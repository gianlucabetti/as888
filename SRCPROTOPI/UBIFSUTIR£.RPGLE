000100081118     D*------------------
000200161017     D* PASSAGGIO PARAMETRI A PROCEDURE UBIFSUTIR
000300081118     D*------------------
000400161109     DiIFSUTItypeB64   S              3A   INZ
000500161109     DiIFSUTIfile      S           1024A   INZ
000600161109     DiIFSUTIsizeByte  S             10I 0 INZ
000700170516     DiIFSUTIoFlg      S             10I 0 INZ
000800170516     DiIFSUTImode      S             10U 0 INZ
000900170516     DiIFSUTIcodePage  S             10U 0 INZ
001000161021     DoIFSUTIpath      S            512A   INZ
001100161021     DoIFSUTIfullName  S            512A   INZ
001200161021     DoIFSUTIName      S            512A   INZ
001300161021     DoIFSUTIExt       S             32A   INZ
001400161021     DoIFSUTIsizeByte  S             10I 0 INZ
001500161021     DoIFSUTIdata16K   S          16384A   INZ VARYING
001600161021     DoIFSUTIdata32K   S          32768A   INZ VARYING
001700161030     DoIFSUTIdata64K   S          65536A   INZ VARYING
001800170516     DoIFSUTIdata4MB   S               A   INZ VARYING LEN(4194304)
001900161026     DoIFSUTIskCnt     S              5U 0 INZ
002000161030     DoIFSUTILenLast   S             10I 0 INZ
002100161102     DoIFSUTIskD256K   S           8192A   INZ VARYING
002200161102     D                                     DIM(32)
002300161102     DoIFSUTIskD512K   S          16384A   INZ VARYING
002400161030     D                                     DIM(32)
002500161030     DoIFSUTIskLen     S             10I 0 INZ
002600161030     D                                     DIM(32)
002700161021     DoIFSUTIcodePage  S              5U 0 INZ
002800161021     DoIFSUTI_CCSID    S              5U 0 INZ
