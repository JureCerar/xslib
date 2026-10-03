#include <dirent.h>
#include <stddef.h>
#include <string.h>
#include <sys/stat.h>

// Returns next directory entry in directory stream pointed to by `dirp`
// It returns NULL on reaching the end of the directory stream or if an 
// error occurred. This is wrapper around `readdir` function.
//
// @param dirp Pointer to the directory stream. See `opendir()` for more info.
// @return Next directory entry in the directory stream pointed to by dirp
//
// @note
// Layout of `dirent` struct cannot not be assumed to be portable (depends on vendor implementation).
// Purpose of this wrapper is to avoid using `dirent` on Fortran side of code.
const char *f_readdir(void *dirp)
{
    struct dirent *entry;

    entry = readdir((DIR *)dirp);
    if (entry == NULL)
        return NULL;

    if (entry == NULL)
        return NULL;

    return entry->d_name;
}

// Inquire about provided path.
//
// @param path Path to inquire.
// @return Returns 1 if path is directory, 0 if path is file, and -1 if path is invalid, inaccessible or missing.
int file_info(const char *path)
{
    struct stat st;

    if (stat(path, &st) != 0)
        return -1;

    return S_ISDIR(st.st_mode);
}
