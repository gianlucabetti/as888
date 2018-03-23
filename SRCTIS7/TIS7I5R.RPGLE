000100010419      *============================================================================================
000200010419      * NOTA: Per mettere in debug questo PGM (ovvero per controllare le singole bolle - TITAS00F)
000300010419      *       occorre - STRSRVJOB TRECD0
000400020521      *               - STRDBG TIS7I5R
000500010419      *============================================================================================
000600991102     h*--------------------------------------------------------------------------------------------*
000700991209     h* Allineamento estratto bolle -TIVTA-                                                        *
000800991102     h*--------------------------------------------------------------------------------------------*
000900991026     h DECEDIT('0,') DATEDIT(*DMY.)
001000991102     f*--------------------------------------------------------------------------------------------*
001100991102     f* Data base
001200991102     f*--------------------------------------------------------------------------------------------*
001300991102     d*--------------------------------------------------------------------------------------------*
001400991102     d* Data structure
001500991102     d*--------------------------------------------------------------------------------------------*
001600991102     d*---
001700991210     d* Variabili riferite al data base
001800991210     d*---
001900010309     d* Dichiarazioni DS
002000991102     d*---
002100010309     d* DS di procedura
002200010309     d*---
002300010309     d  tis799dsi    e ds
002400010309     d*---
002500991209     d* DS reperimento parametri
002600010309     d*---
002700991210     d  ds$eco       e ds
002800991209     d  ds£eco       e ds
002900010309     d*---
003000991209     d* DS composizione data spedizione
003100010309     d*---
003200991026     d                 DS                  INZ
003300991210     d  priaas                 1      4  0                                       -annp
003400991210     d  primgs                 5      8  0                                       -mese/giorno
003500991210     d d$pdsp                  1      8  0                                      *data spedizione
003600991210     d                 DS                  INZ
003700991210     d  dopaas                 1      4  0                                       -annp
003800991210     d  dopmgs                 5      8  0                                       -mese/giorno
003900991210     d d$ddsp                  1      8  0                                      *data spedizione
004000010309     d*---
004100991215     d* DS (ridefinizione) x lettura immagini "Prima" e "Dopo" TITAS00F
004200010309     d*---
004300991215     d ds$tas0       e ds
004400991215     d  prima1                 1    250
004500991215     d  prima2               251    500
004600991215     d  dopo1                501    750
004700991215     d  dopo2                751   1000
004800991102     c*--------------------------------------------------------------------------------------------*
004900991102     c* Main lines
005000991102     c*--------------------------------------------------------------------------------------------*
005100000208     c*
005200000208     c* verifica se modifiche sono significative ai fini TIVTA00F
005300000208     c* o condizioni di SHTDWN
005400010309     c                   exsr      chktas                                       *verifica immagini
005500991209     c*
005600991209     c     tasOK         ifeq      1
005700010309     c* chiama il programma di ri-generazione "Estratto Bolle"
005800010410     c                   call      'TIS799R'                                    *Estratto Bolle
005900010309     c                   parm                    tis799dsi
006000000208     c                   endif
006100991209     c*
006200010309     c                   seton                                            RT
006300991214     c*
006400991214     c     chiudi        tag
006500010309     c                   seton                                            LR
006600991209     c*--------------------------------------------------------------------------------------------*
006700991209     c* verifica campi TITAS00F modificati
006800991209     c*--------------------------------------------------------------------------------------------*
006900991209     c     chktas        BEGSR
007000991209     c*
007100000208     c                   z-add     0             tasOK             1 0
007200000208     c*
007300991229     c     d$gsts        ifeq      'C'
007400991229     c     d$gsts        oreq      'S'
007500991214     c                   goto      chiudi
007600991214     c                   else
007700000208     c*
007800000208     c* verifico che le modifiche al TITAS00F non siano state generate dal *PGM "TNSB23R"
007900020619     c* o dal "TRECE9R" (usa il campo ll2 per l'UPDATE e per l'INSERT verificare di volta in volta)
008000020619     c     d$gpgf        ifne      'TNSB23R'
008100020619     c     d$gpgf        andne     'TRECE9R'
008200020619     c*
008300000208     c                   z-add     1             tasOK
008400991209     c*
008500991229     c                   movel     d$pre1        prima1
008600991229     c                   movel     d$pre2        prima2
008700991229     c                   movel     d$dre1        dopo1
008800991229     c                   movel     d$dre2        dopo2
008900991210     c*
009000010309     c* impostazione ds x passaggio parametri al PGM di rigenerazione TIVTA ("Estratto Bolle")
009100010309     c                   clear                   tis799dsi
009200010309     c*
009300010309     c     d$gsts        ifeq      'D'
009400010309     c                   z-add     d$paas        aasI99
009500010309     c                   z-add     d$pmgs        mgsI99
009600010309     c                   z-add     d$plnp        lnpI99
009700010309     c                   z-add     d$pnrs        nrsI99
009800010309     c                   z-add     d$pnsp        nspI99
009900010309     c                   movel     d$ptbl        tblI99
010000991214     c                   endif
010100010309     c*
010200010309     c     d$gsts        ifeq      'I'
010300010309     c                   z-add     d$daas        aasI99
010400010309     c                   z-add     d$dmgs        mgsI99
010500010309     c                   z-add     d$dlnp        lnpI99
010600010309     c                   z-add     d$dnrs        nrsI99
010700010309     c                   z-add     d$dnsp        nspI99
010800010309     c                   movel     d$dtbl        tblI99
010900991214     c                   endif
011000010309     c*
011100001002     c     d$gsts        ifeq      'U'
011200991209     c                   z-add     0             tasOK
011300010202     c     d$prmn        ifne      d$drmn
011400010202     c     d$paas        orne      d$daas
011500010202     c     d$pmgs        orne      d$dmgs
011600010202     c     d$plod        orne      d$dlod
011700010202     c     d$pnzd        orne      d$dnzd
011800010202     c     d$pprd        orne      d$dprd
011900010202     c     d$prsd        orne      d$drsd
012000010202     c     d$pksc        orne      d$dksc
012100010202     c     d$pccm        orne      d$dccm
012200010202     c     d$plna        orne      d$dlna
012300010202     c     d$pfgc        orne      d$dfgc
012400020619     c     d$pll2        orne      d$dll2
012500010309     c                   z-add     d$daas        aasI99
012600010309     c                   z-add     d$dmgs        mgsI99
012700010309     c                   z-add     d$dlnp        lnpI99
012800010309     c                   z-add     d$dnrs        nrsI99
012900010309     c                   z-add     d$dnsp        nspI99
013000010309     c                   movel     d$dtbl        tblI99
013100010309     c                   z-add     1             tasOK
013200010309     c                   endif
013300010309     c                   endif
013400991210     c*
013500000208     c                   else
013600000208     c                   z-add     0             tasOK
013700000208     c                   endif
013800010309     c                   endif
013900991214     c*
014000991210     c                   ENDSR
014100991102     c*--------------------------------------------------------------------------------------------*
014200991102     c* operazioni iniziali
014300991102     c*--------------------------------------------------------------------------------------------*
014400991209     c     *inzsr        BEGSR
014500991102     c*
014600991102     c* ricevimento parametri
014700010309     c     *entry        plist
014800010309     c                   parm                    ds$eco
014900010309     c                   parm                    ds£eco
015000991026     c*
015100991026     c                   ENDSR
