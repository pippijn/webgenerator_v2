#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

value
ml_Cwd_abs_path (value path)
{
  CAMLparam1 (path);
  CAMLlocal1 (result);

  char buf[PATH_MAX];
  if (!realpath (String_val (path), buf))
    {
      snprintf (buf, sizeof buf, "%s: %s",
                String_val (path),
                strerror (errno));
      failwith (buf);
    }
  result = caml_copy_string (buf);

  CAMLreturn (result);
}
