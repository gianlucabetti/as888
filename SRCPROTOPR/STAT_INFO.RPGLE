000100081209     D*--------------------------------------------------------------------
000200081209     D* Reperimento attributi d un file
000300081209     D*
000400081209     D* stat(*address of a pathfilename, *address of memory area into a stat_info)
000500081209     D*--------------------------------------------------------------------
000600081209     Dstat_info        DS                  inz qualified
000700081209     D st_mode                       10U 0
000800081209     D st_ino                        10U 0
000900081209     D st_nlink                       5U 0
001000081209     D st_uid                        10U 0 OVERLAY(stat_info:13)
001100081209     D st_gid                        10U 0
001200081209     D st_size                       10U 0
001300081209     D st_atime                      10U 0
001400081209     D st_mtime                      10U 0
001500081209     D st_ctime                      10U 0
001600081209     D st_dev                        10U 0
001700081209     D st_blksize                    10U 0
001800081209     D st_allocsize                  10U 0
001900081209     D st_objtype                    10A
002000081209     D st_codepage                    5U 0
002100081209     D st_reserved1                  61A
002200081209     D st_ino_gen_id                 10U 0
