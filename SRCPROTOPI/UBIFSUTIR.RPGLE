000100081118     D*------------------
000200161017     D* PASSAGGIO PARAMETRI A PROCEDURE UBIFSUTIR
000300081118     D*------------------
000400161109     DiIFSUTIfile      S           1024A   INZ
000500161109     DiIFSUTIsizeByte  S             10I 0 INZ
000600170516     DiIFSUTIoFlg      S             10I 0 INZ
000700170516     DiIFSUTImode      S             10U 0 INZ
000800170516     DiIFSUTIcodePage  S             10U 0 INZ
000900161021     DoIFSUTIpath      S            512A   INZ
001000161021     DoIFSUTIfullName  S            512A   INZ
001100161021     DoIFSUTIName      S            512A   INZ
001200161021     DoIFSUTIExt       S             32A   INZ
001300161021     DoIFSUTIsizeByte  S             10I 0 INZ
001400161030     DoIFSUTIdata64K   S          65536A   INZ VARYING
001500170516     DoIFSUTIdata4MB   S               A   INZ VARYING LEN(4194304)
001600161021     DoIFSUTIcodePage  S              5U 0 INZ
001700161021     DoIFSUTI_CCSID    S              5U 0 INZ
