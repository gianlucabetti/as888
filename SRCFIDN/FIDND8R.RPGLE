000100000630      **************************************************************************
000200000630      *                                                                        *
000300000630      *   *----------------------------------------------------------------*   *
000400050517      *    PULIZIA Archivi Richiesta Assistenza clienti                        *
000500000630      *   *----------------------------------------------------------------*   *
000600000630      *                                                                        *
000700000630      **************************************************************************
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000630     FTABEL00F  IF   E           K DISK
001000150324     fFITGD38L  iF   E           K DISK
001100050519     Ffnorm01l  iF   E           K DISK
001200050519     Ffnblp01l  iF   E           K DISK
001300050519     Ffnarb01l  iF   E           K DISK
001400000630      *
001500000630      * DS PER CONTROLLO E INVERSIONE DATE
001600000630      *
001700000630     D WLBDAT          DS
001800000630     D  G02DAT                 1      8  0
001900000630     D  G02INV                 9     16  0
002000000630     D  G02ERR                17     17
002100000630     D  G02TGI                18     22  0
002200000630     D WGIDAT          DS
002300000630     D  GIODAT                 1      8  0
002400000630     D  GIOINV                 9     16  0
002500000630     D  GIOTGI                17     21  0
002600050519      * - Esplosione oggetto spedizione
002700050519     D DS_spe          DS                  INZ
002800050519     D  DSspe_lnp              1      3  0
002900050519     D  DSspe_nrs              4      5  0
003000050519     D  DSspe_nsp              6     12  0
003100050519     D  DSspe_aas             13     16  0
003200050519      * - Esplosione oggetto ORM
003300050519     D DS_orm          DS                  INZ
003400050519     D  DSorm_poe              1      3  0
003500050519     D  DSorm_nsr              4      5  0
003600050519     D  DSorm_nor              6     12  0
003700050519     D  DSorm_nrv             13     14  0
003800000630     D KPJBA         E DS
003900000630      *
004000000630      * DS PER TABELLA 5A (TABEL00F)
004100000630      *
004200050517     D DS5A2         E DS
004300050520      *
004400150623     d fidnCads      e ds
004500050401
004600050404     d sav_tgi         s                   like(g02tgi)
004700050520     d wmsgedp         s              1
004800050520     D MSG             S             70    DIM(2) CTDATA PERRCD(1)
004900000630      **************************************************************************
005000000630      *                                                                        *
005100000630      *         O P E R A Z I O N I   I N I Z I A L I                          *
005200000630      *                                                                        *
005300000630      **************************************************************************
005400000630     C     *ENTRY        PLIST
005500000630     C                   PARM                    KPJBA
005600000630     C                   Z-ADD     1             CODUT
005700000630      *
005800000630     C     KTAB          KLIST
005900000630     C                   KFLD                    CODUT             1 0
006000000630     C                   KFLD                    COD
006100000630     C                   KFLD                    KEY
006200050519     c     korm          klist
006300050519     C                   KFLD                    DSorm_poe
006400050519     C                   KFLD                    DSorm_nsr
006500050519     C                   KFLD                    DSorm_nor
006600050519     C                   KFLD                    DSorm_nrv
006700050519     c     kbol          klist
006800050519     C                   KFLD                    DSspe_aas
006900050519     C                   KFLD                    DSspe_lnp
007000050519     C                   KFLD                    DSspe_nrs
007100050519     C                   KFLD                    DSspe_nsp
007200000630      *
007300000630     C     *LIKE         DEFINE    TBLCOD        COD
007400000630     C     *LIKE         DEFINE    TBLKEY        KEY
007500000630      *
007600000630      * RECUPERO DA TABELLA 5A I PARAMETRI DI PULIZIA
007700000630      *
007800000630     C                   MOVEL     '5A'          COD
007900050517     C                   MOVEL     '2       '    KEY
008000000630     C     KTAB          CHAIN     TABEL00F                           30
008100050517     C  N30              MOVEL     TBLUNI        DS5A2
008200050517     C   30              CLEAR                   DS5A2
008300000630      *
008400000630      **************************************************************************
008500000630      *                                                                        *
008600000630      *         R O U T I N E   P R I N C I P A L E                            *
008700000630      *                                                                        *
008800000630      **************************************************************************
008900000630      *
009000000630      * RICAVO IL NR. DI GIORNI DALLA DATA ODIERNA
009100000630      *
009200000630     C                   TIME                    WTIME            14 0          ORA E DATA
009300000630     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
009400000630     C                   MOVE      WDATE         G02DAT
009500000630     C                   MOVEL     *BLANK        G02ERR
009600000630     C                   CALL      'XSRDA8'
009700000630     C                   PARM                    WLBDAT
009800050404     c                   z-add     g02tgi        sav_tgi
009900000630      *
010000050517      * Data di pulizia del file FITGD00F
010100050401      * =================================
010200050517     C     sav_tgi       SUB       §5Atgd        GIOTGI
010300050401     C                   CALL      'XSRGI8'
010400050401     C                   PARM                    WGIDAT
010500050517     C                   Z-ADD     GIOinv        WRKDAT            8 0          GG/MM/AAAA
010600050517     c*
010700050517     c                   if        wrkdat > 0
010800050517     c                   exsr      sr_pultgd
010900050401     C                   ENDIF
011000000630      *
011100150623      * Richiamo FIDNCA per la chiuusra dei file
011200150623      * ========================================
011300150623     c                   clear                   fidncads
011400150623     c                   eval      ic0tla = 'C'
011500150623     c                   call      'FIDNCAR'
011600150623     c                   parm                    fidncads
011700150623      *
011800000630     C                   SETON                                        LR
011900050520     c*
012000050519      *---------------------------------------------------------------*
012100050519     c* Pulizia fitgd e file collegati
012200050520      *---------------------------------------------------------------*
012300050517     C     sr_pultgd     begsr
012400050520     c                   clear                   wmsgedp
012500150324     c     wrkdat        setll     FITGD000
012600050519     c                   do        *hival
012700150324     c                   readp     FITGD000
012800150324     c                   if        %eof(FITGD38L) or tgddcd = *zeros
012900050519     c                   leave
013000050519     c                   endif
013100050519     c                   clear                   wcanc             1
013200050519     c                   SELECT
013300050519     c* Se tipo oggetto G=Generico sempre cancellabile
013400050519     c                   when      tgdtor = 'G'
013500050519     c* Se tipo oggetto O=O.R.M. Posso cancellare solo se non presnete orm
013600050519     c                   when      tgdtor = 'O'
013700050519     c                   movel     tgdogg        DS_ORM
013800150604     c     korm          chain     fnorm01l
013900150604     c                   if        %found(fnorm01l)
014000050519     c                   eval      wcanc = 'N'
014100050519     c                   endif
014200050519     c* Se tipo oggetto S=Spedizione posso cancellare solo se non presente
014300161118     c* bolla in blp e in arb e se utente diverso da EDPFAT e P.O. inserimento = 046
014400050519     c                   when      tgdtor = 'S'
014401161118      *
014402161118     c                   If        tgdpru = 'EDPFAT     ' and tgdpot = 46
014403161118     c                   eval      wcanc = 'N'
014404161118     c                   else
014405161118      *
014500050519     c                   movel     tgdogg        DS_SPE
014600150604     c     kbol          chain     fnblp01l
014700150604     c                   if        %found(fnblp01l)
014800150604     c                   eval      wcanc = 'N'
014900150604     c                   else
015000150604     c     kbol          chain     fnarb01l
015100150604     c                   if        %found(fnarb01l)
015200050519     c                   eval      wcanc = 'N'
015300150604     c                   endif
015400050519     c                   endif
015401161118      *
015402161118     c                   endif
015500050519     c                   other
015600050519     c* se tipo oggetto diverso non cancello e segnalo
015700050519     c                   eval      wcanc = 'N'
015800050520     c                   eval      wmsgedp = 'S'
015900050519     c                   endsl
016000050519     c
016100050519     c* se record cancellabile procedo
016200050519     c                   if        wcanc = *blanks
016300150623     c                   clear                   fidncads
016400050520     c                   z-add     tgdant        ic0ant
016500050520     c                   z-add     tgdnut        ic0nut
016600050520     c                   z-add     tgdpdt        ic0pdt
016700150623     c                   call      'FIDNCAR'
016800150623     c                   parm                    fidncads
016900050519     c                   endif
017000050519     c                   enddo
017100050520     c* Se trovati tipi oggetto non gestiti invio messaggio di posta elettro
017200050520     c* nica a profilo EDP
017300050520     c                   if        wmsgedp = 'S'
017400050520     C                   MOVEL     MSG(1)        MSGTES
017500111103     C                   MOVEL(P)  MSG(2)        MSGDET
017600120302     C                   eval      wrkEml= 'ced@brt.it'
017700111103     C                   call(e)   'TIS701C'
017800111103     C                   parm                    wrkEml          253
017900111103     C                   PARM                    MSGTES           44
018000111103     C                   PARM                    MSGDET         5000
018100050520     c                   endif
018200050517     C                   endsr
018300050520** MSG
018400050523PULIZIA RICHIESTE ASSISTENZA (FIDND8R)
018500050523Trovati tipi oggetto non gestiti e quindi non puliti:VERIFICARE
