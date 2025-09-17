#@desc Subsample a diffusion acquisition scheme, and optionally the associated data. Arguments after the first specify the number of directions to take from each shell, in ascending order by b-value (including b=0). This may be done based on the original order of acquisition (the "sequential" method), randomly or following the model of mutually repelling electrostatic charges, which aims to ensure the subset is maximally separated on the sphere. In the latter case the DecayPower option can be used to choose between several variants of the model: 1 corresponds to Coulombic energy, 2 (the default) corresponds to electrostatic force, and larger values correspond to ultra-repulsive variants. The UnweightedMethod option determines the method for unweighted (b=0) shells, and the WeightedMethod determines all others. If the source argument is a session directory, and Data is TRUE, then (raw) image data will also be subsetted and extracted. The results are written to file using the specified SubsetName as a basename.
#@args source, [number of directions per shell]
#@group Diffusion processing

library(tractor.session)

runExperiment <- function ()
{
	requireArguments("source")
	
	unweightedMethod <- getConfigVariable("UnweightedMethod", "random", validValues=c("sequential","random"))
	weightedMethod <- getConfigVariable("WeightedMethod", "electrostatic", validValues=c("sequential","random","electrostatic"))
	decayPower <- getConfigVariable("DecayPower", 2)
	subsetName <- getConfigVariable("SubsetName", NULL, "character")
	includeData <- getConfigVariable("Data", TRUE)
	createSession <- getConfigVariable("CreateSession", FALSE)
	
	coulombForce <- function (dirs)
	{
		N <- nrow(dirs)
		force <- N * 2^(1-decayPower)
		for (i in seq_len(N-1))
		{
			for (j in (i+1):N)
			{
				force <- force + 2 * (vectorLength(dirs[i,]-dirs[j,])^(-decayPower) + vectorLength(dirs[i,]+dirs[j,])^(-decayPower))
			}
		}
		
		return (force)
	}
	
	coulombSample <- function (dirs, n)
	{
		# Sample directions following Koay et al., 2011 (DOI 10.1118/1.3615163)
		N <- nrow(dirs)
		crossTerms <- matrix(NA, N, N)
		
		for (i in seq_len(N-1))
		{
			for (j in (i+1):N)
			{
				crossTerms[i,j] <- crossTerms[j,i] <- 2 * (vectorLength(dirs[i,]-dirs[j,])^(-decayPower) + vectorLength(dirs[i,]+dirs[j,])^(-decayPower))
			}
		}
		
		energy <- rep(2^(1-decayPower), N)
		index <- sample(N, 1)
		indices <- index
		for (i in seq_len(n-1))
		{
			energy[index] <- Inf
			energy[-indices] <- energy[-indices] + crossTerms[index, -indices]
			index <- which.min(energy)
			indices <- c(indices, index)
		}
		
		return (indices)
	}
	
	n <- as(Arguments[-1], "integer")
	indices <- integer(0L)
	
	session <- NULL
	if (isTRUE(file.info(Arguments["source"])$isdir))
	{
		session <- attachMriSession(Arguments["source"])
		scheme <- session$getDiffusionScheme()
	}
	else
		scheme <- readDiffusionScheme(Arguments["source"])
	
	shells <- scheme$getShellIndices()
	directions <- scheme$getGradientDirections()
	bValues <- scheme$getBValues()
	
	for (i in seq_along(n))
	{
		if (any(shells == i-1) && n[i] > 0L)
		{
			shellIndices <- which(shells == i-1)
			currentDirections <- directions[shellIndices,,drop=FALSE]
			if (n[i] > length(shellIndices))
			{
				report(OL$Warning, "More volumes requested than there are in shell #{i-1} - selecting all")
				n[i] <- length(shellIndices)
			}
			method <- ifelse(i==1, unweightedMethod, weightedMethod)
			shellIndices <- switch(method, sequential=shellIndices[1:n[i]], random=sample(shellIndices,n[i]), electrostatic=shellIndices[coulombSample(currentDirections,n[i])])
			
			if (i == 1)
				report(OL$Info, "Extracted #{length(shellIndices)} unweighted volumes")
			else
			{
				force <- coulombForce(directions[shellIndices,,drop=FALSE])
				bValue <- unique(bValues[shellIndices])
				report(OL$Info, "Extracted #{length(shellIndices)} volumes from shell #{i-1} (b = #{bValue} s/mm^2), with Coulomb force #{force}", signif=3)
			}
			
			indices <- c(indices, shellIndices)
		}
	}
	
	assert(length(indices) > 0, "No volumes selected")
	
	scheme <- asDiffusionScheme(directions[indices,,drop=FALSE], bValues=bValues[indices])
	if (includeData && !is.null(session))
	{
		image <- session$getImageByType("rawdata", "diffusion", volumes=indices)
		image$setTags(bVectors=scheme$getGradientDirections(), bValues=scheme$getBValues())
		if (createSession)
		{
			if (!file.exists(subsetName))
				dir.create(subsetName, recursive=TRUE)
			session <- attachMriSession(subsetName)
			session$getDirectory("diffusion", createIfMissing=TRUE)
			writeImageFile(image, session$getImageFileNameByType("rawdata","diffusion"), writeTags=TRUE)
		}
		else
			writeImageFile(image, subsetName, writeTags=TRUE)
	}
	else
		writeDiffusionScheme(scheme, subsetName)
}
