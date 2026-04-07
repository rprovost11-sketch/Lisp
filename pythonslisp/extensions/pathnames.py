"""Pathname extension: file path utilities and directory operations."""
from __future__ import annotations
import os
import tempfile
from pathlib import Path, PurePath
from typing import Any

from pythonslisp.Environment import Environment
from pythonslisp.AST import got_str, L_T, L_NIL
from pythonslisp.Context import Context
from pythonslisp.Exceptions import LRuntimeUsageError
from pythonslisp.extensions import primitive


def _require_string( arg, pos, fn ):
   if not isinstance( arg, str ):
      raise LRuntimeUsageError( fn, f'Invalid argument {pos}. STRING expected{got_str(arg)}.' )


# ── Existing (moved from io.py) ─────────────────────────────────────────

@primitive( 'tmpdir', '()' )
def LP_tmpdir( ctx: Context, env: Environment, args: list[Any] ) -> str:
   """Returns the system temporary directory as a string."""
   return tempfile.gettempdir()

@primitive( 'path-join', '(path-segment &rest more-segments)' )
def LP_path_join( ctx: Context, env: Environment, args: list[Any] ) -> str:
   """Joins path segments using the OS path separator.  Returns the result
as a string."""
   for i, arg in enumerate( args ):
      _require_string( arg, i + 1, LP_path_join )
   return os.path.join( *args )

@primitive( 'directory-files', '(dir &optional extension)' )
def LP_directory_files( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns a sorted list of full file paths in dir.
With an optional extension string (e.g. \".log\"), only files with that
extension are returned.  Directories are excluded.  Returns NIL if dir
does not exist or is empty."""
   dirPath = args[0]
   _require_string( dirPath, 1, LP_directory_files )
   ext = None
   if len(args) == 2:
      ext = args[1]
      _require_string( ext, 2, LP_directory_files )
   if not os.path.isdir( dirPath ):
      return L_NIL
   entries = sorted( os.listdir( dirPath ) )
   result = []
   for name in entries:
      fullPath = os.path.join( dirPath, name )
      if not os.path.isfile( fullPath ):
         continue
      if ext is not None and not name.endswith( ext ):
         continue
      result.append( fullPath )
   return result

@primitive( 'make-directory', '(path)' )
def LP_make_directory( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Creates the directory at path, including any missing parent directories.
Does nothing if the directory already exists.  Returns the path string."""
   path = args[0]
   _require_string( path, 1, LP_make_directory )
   os.makedirs( path, exist_ok=True )
   return path

@primitive( 'file-basename', '(path)' )
def LP_file_basename( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the final component of a file path (the filename without
the directory prefix).  E.g. (file-basename \"/foo/bar/baz.log\") => \"baz.log\"."""
   path = args[0]
   _require_string( path, 1, LP_file_basename )
   return os.path.basename( path )


# ── New pathname utilities ───────────────────────────────────────────────

@primitive( 'pathname-directory', '(path)' )
def LP_pathname_directory( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the directory portion of a file path as a string.
Returns an empty string if the path has no directory component."""
   path = args[0]
   _require_string( path, 1, LP_pathname_directory )
   return str( PurePath( path ).parent )

@primitive( 'pathname-name', '(path)' )
def LP_pathname_name( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the filename without extension from a file path.
E.g. (pathname-name \"/foo/bar.txt\") => \"bar\"."""
   path = args[0]
   _require_string( path, 1, LP_pathname_name )
   return PurePath( path ).stem

@primitive( 'pathname-type', '(path)' )
def LP_pathname_type( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the file extension (without the dot) from a file path.
Returns an empty string if there is no extension.
E.g. (pathname-type \"/foo/bar.txt\") => \"txt\"."""
   path = args[0]
   _require_string( path, 1, LP_pathname_type )
   suffix = PurePath( path ).suffix
   return suffix[1:] if suffix else ''

@primitive( 'merge-pathnames', '(pathname &optional default-pathname)' )
def LP_merge_pathnames( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Merges a relative pathname with a default directory.  If pathname is
absolute, it is returned unchanged.  Otherwise default-pathname provides
the directory context.  Default-pathname defaults to the current directory.
E.g. (merge-pathnames \"bar.txt\" \"/foo/\") => \"/foo/bar.txt\"."""
   pathname = args[0]
   _require_string( pathname, 1, LP_merge_pathnames )
   p = PurePath( pathname )
   if p.is_absolute():
      return str( p )
   if len(args) >= 2:
      default = args[1]
      _require_string( default, 2, LP_merge_pathnames )
   else:
      default = os.getcwd()
   return str( PurePath( default ) / p )

@primitive( 'path-exists-p', '(path)' )
def LP_path_exists_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if path exists on the filesystem, NIL otherwise."""
   path = args[0]
   _require_string( path, 1, LP_path_exists_p )
   return L_T if os.path.exists( path ) else L_NIL

@primitive( 'directory-p', '(path)' )
def LP_directory_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if path exists and is a directory, NIL otherwise."""
   path = args[0]
   _require_string( path, 1, LP_directory_p )
   return L_T if os.path.isdir( path ) else L_NIL

@primitive( 'file-p', '(path)' )
def LP_file_p( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns T if path exists and is a regular file, NIL otherwise."""
   path = args[0]
   _require_string( path, 1, LP_file_p )
   return L_T if os.path.isfile( path ) else L_NIL

@primitive( 'user-homedir-pathname', '()' )
def LP_user_homedir( ctx: Context, env: Environment, args: list[Any] ) -> Any:
   """Returns the current user's home directory as a string."""
   return str( Path.home() )
