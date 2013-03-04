#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "tractor.h"

char *script_file = NULL, *working_dir = NULL, *report_file = NULL, *output_level = NULL, *config_file = NULL, *script_args = NULL;
int parallelisation_factor = 1;

char * allocate_and_copy_string (const char *from)
{
    char *to;
    
    if (from != NULL)
    {
        to = (char *) malloc(strlen(from) + 1);
        strcpy(to, from);
    }
    
    return to;
}

void parse_arguments (int argc, const char **argv)
{
    int i, to_drop, to_skip, current_arg;
    size_t script_args_len = 0, script_args_index = 0;
    
    // First pass: identify and remove flagged options
    current_arg = 1;
    while (current_arg < argc)
    {
        to_drop = 0;
        to_skip = 0;
        
        if (strncmp(argv[current_arg],"-",1) != 0)
        {
            if (script_file == NULL)
            {
                script_file = allocate_and_copy_string(argv[current_arg]);
                to_drop = 1;
            }
            else
                to_skip = 1;
        }  
        else if (strcmp(argv[current_arg], "-w") == 0)
        {
            working_dir = allocate_and_copy_string(argv[current_arg+1]);
            to_drop = 2;
        }
        else if (strcmp(argv[current_arg], "-r") == 0)
        {
            report_file = allocate_and_copy_string(argv[current_arg+1]);
            to_drop = 2;
        }
        else if (strcmp(argv[current_arg], "-l") == 0)
        {
            output_level = allocate_and_copy_string(argv[current_arg+1]);
            to_drop = 2;
        }
        else if (strcmp(argv[current_arg], "-c") == 0)
        {
            config_file = allocate_and_copy_string(argv[current_arg+1]);
            to_drop = 2;
        }
        else if (strcmp(argv[current_arg], "-p") == 0)
        {
            if (argv[current_arg+1] != NULL)
            {
                parallelisation_factor = atoi(argv[current_arg+1]);
                to_drop = 2;
            }
        }
        else if (strncmp(argv[current_arg],"-",1) == 0)
        {
            fprintf(stderr, "\x1b[33mUnrecognised option: %s\x1b[0m\n", argv[current_arg]);
            to_drop = 1;
        }
        
        for (i=0; i<to_drop; i++)
            argv[current_arg+i] = NULL;
        current_arg += to_drop + to_skip;
    }
    
    // Second pass: sum up the lengths of unflagged arguments
    current_arg = 1;
    while (current_arg < argc)
    {
        if (argv[current_arg] != NULL)
            script_args_len += strlen(argv[current_arg]) + 1;
        current_arg++;
    }
    
    if (script_args_len > 0)
        script_args = (char *) malloc(script_args_len);
    
    // Third pass: copy unflagged arguments into "script_args"
    current_arg = 1;
    while (current_arg < argc)
    {
        if (argv[current_arg] != NULL)
        {
            strcpy(script_args + script_args_index, argv[current_arg]);
            script_args_index += strlen(argv[current_arg]);
            strcpy(script_args + script_args_index, " ");
            script_args_index++;
        }
        current_arg++;
    }
}

int main (int argc, char **argv)
{
    parse_arguments(argc, (const char **) argv);
    
    if (script_file == NULL)
    {
        fputs("\x1b[31mError: Script file must be specified\x1b[0m\n", stderr);
        tidy_up();
        return 1;
    }
    
    char *R_args[3] = { "tractor", "--quiet", "--vanilla" };
    Rf_initialize_R(3, R_args);
    
    // Store the addresses of the default callbacks
    // These will be called by our functions after they have had a chance to intervene
    ptr_R_ReadConsole_default = ptr_R_ReadConsole;
    ptr_R_CleanUp_default = ptr_R_CleanUp;
    
    // Set up our callbacks
    ptr_R_ReadConsole = &read_console;
    ptr_R_WriteConsole = NULL;
    ptr_R_WriteConsoleEx = &write_console;
    ptr_R_CleanUp = &tidy_up_all;
    R_running_as_main_program = 1;
    
    Rf_mainloop();
    
    // For the compiler only: Rf_mainloop() does not return
    return 0;
}

char * build_bootstrap_string ()
{
    // R prototype is: function (scriptFile, workingDirectory = getwd(), reportFile = NULL, outputLevel = OL$Warning,
    //           configFiles = NULL, configText = NULL, parallelisationFactor = 1, standalone = TRUE, debug = FALSE)
    
    size_t len, offset;
    char *bootstrap_string;
    const char *fixed_part = "library(utils); library(tractor.utils); bootstrapExperiment(";
    
    // Work out the length of string required
    len = strlen(fixed_part) +      // Length of fixed part of the string
          3 +                       // Closing bracket, newline and terminating characters
          strlen(script_file) + 2;  // Length of script file name and surrounding quotes
    
    // Two added to each length for quotes (where needed)
    if (working_dir != NULL)
        len += strlen(working_dir) + strlen(", workingDirectory=") + 2;
    if (report_file != NULL)
        len += strlen(report_file) + strlen(", reportFile=") + 2;
    if (output_level != NULL)
        len += strlen(output_level) + strlen(", outputLevel=OL$");
    if (config_file != NULL)
        len += strlen(config_file) + strlen(", configFiles=") + 2;
    if (script_args != NULL)
        len += strlen(script_args) + strlen(", configText=") + 2;
    if (parallelisation_factor > 1)
        len += ((size_t) ceil(log10(parallelisation_factor))) + strlen(", parallelisationFactor=");
    
    // Allocate space for the string
    bootstrap_string = (char *) malloc(len);
    
    // Write into the final string
    offset = (size_t) sprintf(bootstrap_string, "%s'%s'", fixed_part, script_file);
    
    if (working_dir != NULL)
        offset += sprintf(bootstrap_string + offset, ", workingDirectory='%s'", working_dir);
    if (report_file != NULL)
        offset += sprintf(bootstrap_string + offset, ", reportFile='%s'", report_file);
    if (output_level != NULL)
        offset += sprintf(bootstrap_string + offset, ", outputLevel=OL$%s", output_level);
    if (config_file != NULL)
        offset += sprintf(bootstrap_string + offset, ", configFiles='%s'", config_file);
    if (script_args != NULL)
        offset += sprintf(bootstrap_string + offset, ", configText='%s'", script_args);
    if (parallelisation_factor > 1)
        offset += sprintf(bootstrap_string + offset, ", parallelisationFactor=%d", parallelisation_factor);
    
    sprintf(bootstrap_string + offset, ")\n");
    
    return bootstrap_string;
}

int read_console (const char *prompt, unsigned char *buffer, int buffer_len, int add_to_history)
{
    // Preserved across calls to this function
    static size_t remaining_len = -1;
    static size_t current_offset = 0;
    
    char *bootstrap_string;
    int return_value = 1;
    
    // First time: build bootstrap string
    if (remaining_len == -1)
    {
        bootstrap_string = build_bootstrap_string();
        remaining_len = strlen(bootstrap_string);
        
        // Print bootstrap string to stdout if running in debug mode
        if (output_level != NULL && strcmp(output_level,"Debug") == 0)
        {
            fprintf(stdout, "\x1b[32m%s\x1b[0m", bootstrap_string);
            fflush(stdout);
        }
    }
    
    // First and subsequent times until whole string is transferred: copy (part of) string to buffer
    if (remaining_len != 0)
    {
        if (remaining_len < buffer_len)
        {
            strcpy((char *) buffer, bootstrap_string + current_offset);
            remaining_len = 0;
        }
        else
        {
            strncpy((char *) buffer, bootstrap_string + current_offset, buffer_len);
            remaining_len -= buffer_len;
            current_offset += buffer_len;
        }
    }
    else
    {
        // Once bootstrap string is written, revert to usual prompt
        return_value = (*ptr_R_ReadConsole_default)(prompt, buffer, buffer_len, add_to_history);
    }
    
    return return_value;
}

void write_console (const char *buffer, int buffer_len, int output_type)
{
    if (output_type == 0)
        fputs(buffer, stdout);
    else if (strncmp(buffer,"Error",5) == 0 || strncmp(buffer,"ERROR",5) == 0)
        fprintf(stderr, "\x1b[31m%s\x1b[0m", buffer);
    else
        fprintf(stderr, "\x1b[33m%s\x1b[0m", buffer);
    
    fflush(stdout);
}

void tidy_up ()
{
    if (script_file != NULL)
        free(script_file);
    if (working_dir != NULL)
        free(working_dir);
    if (report_file != NULL)
        free(report_file);
    if (output_level != NULL)
        free(output_level);
    if (config_file != NULL)
        free(config_file);
    if (script_args != NULL)
        free(script_args);
}

void tidy_up_all (SA_TYPE save_action, int status, int run_last)
{
    tidy_up();
    (*ptr_R_CleanUp_default)(save_action, status, run_last);
}
