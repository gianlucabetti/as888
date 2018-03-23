000100101119     D*--------------------------------------------------
000200101119     D* Procedure name: CBIRX_Init
000300101119     D* Purpose:        Inizializzazione modulo.
000400101119     D* Returns:        Esito.
000500101119     D* Parameter:      piKpjba
000600101119     D*--------------------------------------------------
000700101119     D CBIRX_Init      PR            10I 0
000800101119     D  piSocieta                          LIKE(andiz00f.societa)
000900101119     D  piKpjba                            LIKEDS(kpjba)
001000101119     D                                     OPTIONS(*NOPASS)
001100101119
001200101119     D*--------------------------------------------------
001300101119     D* Procedure name: CBIRX_Finalize
001400101119     D* Purpose:        Chiusura modulo.
001500101119     D* Returns:
001600101119     D*--------------------------------------------------
001700101119     D CBIRX_Finalize  PR
001800101119
001801101119     D*--------------------------------------------------
001802101119     D* Procedure name: CBIRX_NewProvider
001803101119     D* Purpose:        Inizia una sessione per un provider.
001804101119     D* Returns:        Esito.
001805101119     D* Parameter:      piProvider => Provider.
001806101119     D*--------------------------------------------------
001807101119     D CBIRX_NewProvider...
001808101119     D                 PR            10I 0
001809101119     D  piProvider                   15A   CONST
001810101119
001811101122     D*--------------------------------------------------
001812101122     D* Procedure name: CBIRX_EndProvider
001813101122     D* Purpose:        Fine provider.
001814101122     D* Returns:        Esito.
001815101122     D*--------------------------------------------------
001816101122     D CBIRX_EndProvider...
001817101122     D                 PR            10I 0
001828101122
001829101122     D*--------------------------------------------------
001830101122     D* Procedure name: CBIRX_RunProvider
001831101122     D* Purpose:        Elaborazione del provider.
001832101122     D* Returns:        Esito.
001833101122     D* Parameter:      piNumeroFilesElaborati => Numero dei files elaborati.
001834101122     D*--------------------------------------------------
001835101122     D CBIRX_RunProvider...
001836101122     D                 PR            10I 0
001837101122     D  piNumeroFilesElaborati...
001838101122     D                                3U 0
001839101122
001840101122     D*--------------------------------------------------
001841101122     D* Procedure name: CBIRX_GetNextFileName
001842101122     D* Purpose:        Restituisce il nome del prossimo file da elaborare.
001843101122     D* Returns:        File name.
001844101122     D*--------------------------------------------------
001845101122     D CBIRX_GetNextFileName...
001846101122     D                 PR            10I 0
001847101122     D piFileName                   255A   VARYING
001848101122
001849101122     D*--------------------------------------------------
001850101122     D* Procedure name: CBIRX_RunFile
001851101122     D* Purpose:        Elabora un file.
001852101122     D* Returns:        Esito.
001853101122     D* Parameter:      piIfsDir => Indirizzario IFS.
001854101122     D* Parameter:      piFileName => Nome file.
001855101122     D* Parameter:      piDb2Lib => Libreria DB2.
001856101122     D* Parameter:      piDb2File => File DB2.
001857101122     D* Parameter:      piDb2Mbr => Membro DB2.
001858101122     D*--------------------------------------------------
001859101122     D CBIRX_RunFile   PR            10I 0
001860101122     D  piIfsDir                    255A   VARYING
001861101122     D                                     CONST
001862101122     D  piFileName                  255A   VARYING
001863101122     D                                     CONST
001864101122     D  piDb2Lib                     10A   VARYING
001865101122     D                                     CONST
001866101122     D  piDb2File                    10A   VARYING
001867101122     D                                     CONST
001868101122     D  piDb2Mbr                     10A   VARYING
001869101122     D                                     CONST
001870101123     D  piRigheCopiate...
001871101123     D                               10I 0 OPTIONS(*NOPASS)
001872101123
001873101123     D*--------------------------------------------------
001874101123     D* Procedure name: CBIRX_GetDescrizioneProvider
001875101123     D* Purpose:        Restituisce la descrizione del provider.
001876101123     D* Returns:        Descrizione provider.
001877101123     D*--------------------------------------------------
001878101123     D CBIRX_GetDescrizioneProvider...
001879101123     D                 PR            20A   VARYING
001880101123
