000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200110511     H* Trec02r *----------------------------------------------------*
000300110511     H* Importa tariffe da foglio EXL/ Stampa controllo tariffe C/E
000400950517     H*--------------------------------------------------------------*
000500110511     Ftrec02d   CF   E             WORKSTN
000600110511     d trec02ds      e ds                  inz
000700110510     d kpjba         e ds
000800110510     D CMD             S             48    DIM(1) CTDATA PERRCD(1)
000900950517     C* Loop di gestione videata
001000050426     C     *ENTRY        PLIST
001100110510     C                   PARM                    kpjba
001200110510     C                   Z-ADD     48            LUNG             15 5
001300110511     c                   if        kcdaz = 'ECX2'
001400110511     c                   eval      titolov = 'Importa tariffe foglio EXL'
001500110511     c                   eval      Riga1v  = 'Libreria ARRIVO'
001600110511     c                   end
001700110511     c                   if        kcdaz = 'ECC2'
001800110511     c                   eval      titolov = 'Stampa controllo tariffe C/E'
001900110511     c                   eval      Riga1v  = 'Libreria Tariffe'
002000110511     c                   end
002100110511     c*
002200110114     C                   SETON                                        LR
002300110228     c                   do        *hival
002400110510     C                   EXFMT     trec01
002500110510     c                   movea     '0000'        *in(01)
002600110510     C                   IF        *inkc = '1'
002700050426     c                   leave
002800950517     C                   END
002900110510     c* libreria output
003000110510     c                   if        liboutv <> *blanks
003100140915     c                   if        kcdaz = 'ECC2'
003200110510     C                   MOVEl(p)  CMD(1)        COMMAN           80
003300110510     c                   eval      %subst(comman:12:10) = liboutv
003400110510     C                   CALL      'QCMDEXC'                            01
003500110510     C                   PARM                    COMMAN
003600110510     C                   PARM                    LUNG
003700140915     c                   end
003800110510     c                   else
003900110510     c                   seton                                        01
004000110510     c                   end
004100110510     c   01              iter
004200110510     c*
004300110510     c                   if        *inkf
004400110510     c                   eval      d01libout = liboutv
004500110511     c                   movel(p)  trec02ds      kpjbu
004600110511     c                   if        kcdaz = 'ECX2'
004700110511     c                   call      'TRECEXL0C'
004800110510     c                   parm                    kpjba
004900110511     c                   end
005000110511     c                   if        kcdaz = 'ECC2'
005100110511     c                   call      'TRECCTR1C'
005200110511     c                   parm                    kpjba
005300110511     c                   end
005400050426     c                   leave
005500050426     c                   end
005600050426     C                   ENDdo
005700110510**
005800110510CHKOBJ OBJ(          ) OBJTYPE(*lib)
