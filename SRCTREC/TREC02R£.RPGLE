000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200110511     H* Trec02r *----------------------------------------------------*
000300110511     H* Importa tariffe da foglio EXL/ Stampa controllo tariffe C/E
000400950517     H*--------------------------------------------------------------*
000500110511     Ftrec02d   CF   E             WORKSTN
000501110511     d trec02ds      e ds                  inz
000503110510     d kpjba         e ds
000504110510     D CMD             S             48    DIM(1) CTDATA PERRCD(1)
004600950517     C* Loop di gestione videata
004602050426     C     *ENTRY        PLIST
004603110510     C                   PARM                    kpjba
004604110510     C                   Z-ADD     48            LUNG             15 5
004605110511     c                   if        kcdaz = 'ECX2'
004606110511     c                   eval      titolov = 'Importa tariffe foglio EXL'
004607110511     c                   eval      Riga1v  = 'Libreria ARRIVO'
004608110511     c                   end
004609110511     c                   if        kcdaz = 'ECC2'
004610110511     c                   eval      titolov = 'Stampa controllo tariffe C/E'
004611110511     c                   eval      Riga1v  = 'Libreria Tariffe'
004612110511     c                   end
004613110511     c*
004618110114     C                   SETON                                        LR
004619110228     c                   do        *hival
004701110510     C                   EXFMT     trec01
004702110510     c                   movea     '0000'        *in(01)
004703110510     C                   IF        *inkc = '1'
004704050426     c                   leave
004900950517     C                   END
004901110510     c* libreria output
004903110510     c                   if        liboutv <> *blanks
004904110510     C                   MOVEl(p)  CMD(1)        COMMAN           80
004905110510     c                   eval      %subst(comman:12:10) = liboutv
004906110510     C                   CALL      'QCMDEXC'                            01
004907110510     C                   PARM                    COMMAN
004908110510     C                   PARM                    LUNG
004909110510     c                   else
004910110510     c                   seton                                        01
004911110510     c                   end
004912110510     c   01              iter
004942110510     c*
004943110510     c                   if        *inkf
004944110510     c                   eval      d01libout = liboutv
004948110511     c                   movel(p)  trec02ds      kpjbu
004949110511     c                   if        kcdaz = 'ECX2'
004950110511     c                   call      'TRECEXL0C'
004951110510     c                   parm                    kpjba
004952110511     c                   end
004953110511     c                   if        kcdaz = 'ECC2'
004954110511     c                   call      'TRECCTR1C'
004955110511     c                   parm                    kpjba
004956110511     c                   end
004957050426     c                   leave
004958050426     c                   end
005000050426     C                   ENDdo
005001110510**
005100110510CHKOBJ OBJ(          ) OBJTYPE(*lib)
