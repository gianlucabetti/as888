000100050608     H DECEDIT('0,') DATEDIT(*yMd.) option(*nodebugio)
000200020409      *------------------------------------------------------------------------*
000300050608      * Cancella tariffa RCV <=6,2                                             *
000400020409      *------------------------------------------------------------------------*
000500050608     FTITAV00F  O  A E             DISK
000600050608     ftitpt01l  if   e           k disk
000700050608     F                                     INFDS(tpt1)
000800050608     ftitpt00F  uf   e             disk    rename(titpt000:titptfis)
000900050608     ftiopt01l  if   e           k disk    rename(titpt000:tiopt000)
001000050608     F                                     INFDS(opt1)
001100050608     ftiopt00F  uf   e             disk    rename(titpt000:tioptfis)
001200050608     f
001300020409     ftitpd01l  uf   e           k disk
001400050608     ftiopd01l  uf   e           k disk    rename(titpd000:tiopd000)
001500020411     fprtf198   o    f  198        printer oflind(*inof) usropn
001600020409
001700050608     D tpt1            DS
001800050608     D  tptNRR               397    400B 0
001900050608     D opt1            DS
002000050608     D  optNRR               397    400B 0
002100020411     d cmd             s              1    dim(122) ctdata perrcd(62)
002200020411
002300020411     d lung            s             15  5
002400020411     d comman          s            110
002500050608     d
002600050608     D PGMSTATUS      SDS
002700050608     D JOB_NAME              244    253
002800050608     D USER                  254    263
002900020411
003000020411      * File titpt
003100020411      *------------
003200050608     c     *loval        setll     titpt01l
003300050608     c                   read      titpt01l
003400050608     c                   dow       not %eof(titpt01l)
003500050608     c                   if        tptftc='R ' and tptvlm<=6,2 and tptatb=' '
003600050608     c
003700050608     c     ktpd          setll     titpd01l
003800050608     c     ktpd          reade     titpd01l
003900050608     c                   dow       not %eof(titpd01l)
004000050608     c
004100050608     c                   if        tpdatb=' '
004200050608     c                   movel     'A'           tpdatb
004300050608     c                   movel     *date         tpddtr
004400050608     c                   movel     ' '           tpdftr
004500050608     C                   UPDATE    TITPD000
004600050608     c                   endif
004700050608     c
004800050608     c     ktpd          reade     titpd01l
004900050608     c                   enddo
005000050608     c
005100050608     c     tptnrr        chain     titptfis                           99
005200050608     c                   movel     'A'           tptatb
005300050608     c                   movel     *date         tptdtr
005400050608     c                   movel     ' '           tptftr
005500050608     C  n99              UPDATE    TITPtfis
005600050608     c
005700050608     c                   except    stampa
005800050608     c* Scrivo storico TITAV
005900050608     c                   exsr      SUB_storico
006000020409     c                   endif
006100050608     c                   read      titpt01l
006200050608     c                   enddo
006300050608      * File tiOPT
006400050608      *------------
006500050608     c     *loval        setll     tiopt01l
006600050608     c                   read      tiopt01l
006700050608     c                   dow       not %eof(tiopt01l)
006800050608     c                   if        tptftc='R ' and tptvlm<=6,2
006900050608     c
007000050608     c     ktpd          setll     tiopd01l
007100050608     c     ktpd          reade     tiopd01l
007200050608     c                   dow       not %eof(titpd01l)
007300050608     C                   delete    TIoPD000
007400050608     c
007500050608     c     ktpd          reade     tiopd01l
007600050608     c                   enddo
007700050608     c
007800050608     c     optnrr        delete    tioptfis                           99
007900050608     c
008000050608     c                   except    stampaoff
008100050608     c                   endif
008200050608     c
008300050608     c                   read      tiopt01l
008400050608     c                   enddo
008500050608     c
008600050608     c                   seton                                        lr
008700020411
008800020411      *---------------------------------------------------------------*
008900020411      *   ROUTINE INIZIALE
009000020411      *---------------------------------------------------------------*
009100020411     c     *inzsr        begsr
009200020411
009300020411      * Ovrprtf
009400020411     c                   z-add     103           lung
009500020411     c                   clear                   comman
009600020411     c                   movea     cmd(1)        comman
009700020411     c                   call      'QCMDEXC'
009800020411     c                   parm                    comman
009900020411     c                   parm                    lung
010000020411     c                   open      prtf198
010100020411
010200020411     c     ktpd          klist
010300050608     c                   kfld                    tptksc
010400050608     c                   kfld                    tptctr
010500050608     c                   kfld                    tptprg
010600050608     c                   kfld                    tptftc
010700020411
010800020411     c                   endsr
010900050608     C*---------------------------------------------------------------*
011000050608     C* ROUTINE DI SCRITTURA FILE STORICO TITAV00F
011100050608     C*---------------------------------------------------------------*
011200050608     C     SUB_STORICO   BEGSR
011300050608     C                   eval      TAVksc= tptksc
011400050608     C                   move      tptctr        TAVctr
011500050608     C                   eval      TAVprg= tptprg
011600050608      * data variazione
011700050608     C                   movel     *date         tavdav
011800050608     C                   eval      TAVnoj= job_name
011900050608     C                   eval      TAVpru= user
012000050608      *
012100050608     c                   clear                   TAVatb
012200050608      * recupero ora di variazione
012300050608     c                   time                    w0120            12 0
012400050608     c                   movel     w0120         TAVorv
012500050608     c                   movel     'R '          TAVftc
012600050608     c                   movel     'PAR'         TAVtrd
012700050608     c                   movel     'Ann'         TAVCTA
012800050608      *
012900050608     c                   movel     *date         TAVdtr
013000050608      *
013100050608     c                   write     titav000
013200050608     C*
013300050608     C                   ENDSR
013400020411
013500020409     oprtf198   e            stampa      1
013600050608     o                                              'Annullamento RCV da'
013700050608     o                                         +  1 'TARIFFE:'
013800050608     o                       tptksc            +  3
013900050608     o                       tptctr            +  2
014000050608     o                       tptprg        2   +  2
014100050608     o               99                        +  2 'Non trovata testata'
014200050608     oprtf198   e            stampaoff   1
014300050608     o                                              'Annullamento RCV da'
014400050608     o                                         +  1 'OFFERTE:'
014500050608     o                       tptksc            +  2 '      / '
014600050608     o                       tptctr            +  2
014700050608     o               99                        +  2 'Non trovata testata'
014800020411** cmd
014900020411OVRPRTF FILE(PRTF198) OUTQ(QPRINTS)
