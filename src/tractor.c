#include <stdio.h>
#include <string.h>
#include <stdlib.h>

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
    int i, to_drop, current_arg;
    size_t script_args_len = 0, script_args_index = 0;
    
    // First pass: identify and remove flagged options
    current_arg = 1;
    while (current_arg < argc)
    {
        to_drop = 0;
        
        if (strncmp(argv[current_arg],"-",1) != 0 && script_file == NULL)
        {
            script_file = allocate_and_copy_string(argv[current_arg]);
            to_drop = 1;
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
        current_arg += to_drop;
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
    ptr_R_CleanUp = &tidy_up;
    R_running_as_main_program = 1;
    
    Rf_mainloop();
    
    // For the compiler only: Rf_mainloop() does not return
    return 0;
}

int read_console (const char *prompt, unsigned char *buffer, int buffer_len, int add_to_history)
{
    // Preserved across calls to this function
    static int previous_calls = 0;
    
    char *bootstrap_string;
    int return_value = 1;
    
    if (previous_calls == 0)
    {
        bootstrap_string = (char *) malloc(buffer_len);
        sprintf(bootstrap_string, "library(utils); library(tractor.utils); bootstrapExperiment('%s', '%s', '%s', OL$%s, '%s', '%s', %d)\n", script_file, working_dir, report_file, output_level, config_file, script_args, parallelisation_factor);
        strcpy((char *) buffer, bootstrap_string);
    }
    else
        return_value = (*ptr_R_ReadConsole_default)(prompt, buffer, buffer_len, add_to_history);
    
    previous_calls++;
    
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

void tidy_up (SA_TYPE save_action, int status, int run_last)
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
    
    (*ptr_R_CleanUp_default)(save_action, status, run_last);
}
