000001090211     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000      *****************************************************************
000300000000      *
000400011031      *  Nome programma:  ficn90R
000500000000      *  Descrizione   :  Riorganizzazione tariffe padroncini
000600000000      *                   Programma di esecuzione
000700000000      *
000800000000      *****************************************************************
000801090624     Ffiadt00f  UF   E             DISK    commit
000802090624     Ffiadt01L  IF   E           K DISK
000803090624     F                                     RENAME(fiadt000:fiadtL)
000804090624     F                                     INFDS($fiadt)
000805090624     Ffiadd01L  UF   E           K DISK    commit
000806090624     Ffitgt01L  UF   E           K DISK    commit
000900090211     Ffifgt00F  UF   E             DISK    commit
001000011031     Ffifgt01L  IF   E           K DISK
001100011031     F                                     RENAME(fifgt000:fifgtL)
001200011031     F                                     INFDS($fifgt)
001300090211     Ffifpt01L  UF   E           K DISK    commit
001400090211     Ffifpd01L  UF   E           K DISK    commit
001500090211     Ffifgts0F  o    E             DISK    commit
001501090624     Ffiadts0F  o    E             DISK    commit
001600020729     FTABEL00F  IF   E           K DISK
001700000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001800000000     F                                     USROPN
001900000000      *
002000000000     D TE              S             71    DIM(1) CTDATA PERRCD(1)              RICHE DI STAMP
002001090528     d maxddc          s              8  0
002100000000      *
002200090624      * Numero relativo di record per il file testate tariffe città
002300011031     D $fifgt          DS
002400000000     D  $FGTR                397    400B 0
002401090624      * Numero relativo di record per il file testate tariffe aff/def
002402090624     D $fiadt          DS
002403090624     D  $adtR                397    400B 0
002500000000     D PARAM           DS
002600011031     D  cn90D1                 1      8  0
002700011031     D  cn90D2                 9     16  0
002800011031     D  cn90ST                17     17
002900000000     D WLBDAT          DS
003000020729     D  G02DAT                 1      8  0
003100020729     D  G02INV                 9     16  0
003200020729     D  G02ERR                17     17
003300020729     D  G02TGI                18     22  0
003400020729     D WGIDAT          DS
003500020729     D  GIODAT                 1      8  0
003600020729     D  GIOINV                 9     16  0
003700020729     D  GIOTGI                17     21  0
003800020729      *
003900020729     D DS5A          E DS
004000000000      * Predispongo una DS che conterra' la chiave del record
004700940926     D*
004800940926     D                 DS
004900940926     D  AA                     1      4  0
005000940926     D  MM                     5      6  0
005100940926     D  GG                     7      8  0
005200940926     D  DATA                   1      8  0
005300940926     D*
005301090624     d tipo            s              1
005400000000     D KPJBA         E DS
005500000000     D CNCR80        E DS
005600000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005700000000      *****************************************************************
005800000000      * RIEPILOGO INDICATORI
005900000000      *****************************************************************
006000000000      * 40    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD : SE
006100000000      *         IL RECORD E' ALLOCATO (CIOE' *IN40 ACCESO) SALTO LA
006200000000      *         TARIFFA (CHE VERRA' ANNULLATA LA PROSSIMA VOLTA)
006300000000      * 30/34 - DI COMODO
006400000000      *****************************************************************
006500000000     C     *ENTRY        PLIST
006600000000     C                   PARM                    KPJBA
006700020729     C*                  MOVEL     KPJBU         PARAM
006800000000     C                   Z-ADD     1             CODUT
006900000000     C                   CALL      'X§PARUT'                            99
007000000000     C                   PARM                    UT§DSE
007100000000     C                   MOVEL     RAGUT         RSUT             20
007200000000     C                   MOVEL     REC80         CNCR80
007300020729     c                   exsr      limiti
007400000000      *
007500011031     C     fifgdK        KLIST                                                  fifgt01L
007600000000     C                   KFLD                    FGTPDR                         CD.PADRONCINO
007700000000     C                   KFLD                    FGTSML                         SIMULAZIONE
007800000000     C                   KFLD                    FGTTSR                         TIPO PRESTAZION
007900000000     C                   KFLD                    FGTCTR                         CD.TARIFFA
008000000000     C                   KFLD                    FGTPRG                         PROGR.TARIFFA
008001090114     C     ktgt          KLIST                                                  fifgt01L
008002090114     C                   KFLD                    FGTPDR                         CD.PADRONCINO
008003090114     C                   KFLD                    FGTSML                         SIMULAZIONE
008004090114     C                   KFLD                    FGTprg                         TIPO PRESTAZION
008005090624     C     fiaddK        KLIST                                                  fifgt01L
008006090624     C                   KFLD                    adtPDR                         CD.PADRONCINO
008007090624     C                   KFLD                    adtPRG                         PROGR.TARIFFA
008100000000      *
008200000000      *****************************************************************
008300000000      *               M A I N     L I N E
008400000000      *****************************************************************
008401090624     c                   eval      tipo = 'C'
008500000000      *
008600011031     C     cn90ST        IFEQ      'S'
008700000000     C                   OPEN      QSYSPRT
008800000000     C                   EXCEPT    TESTA                                        TESTATA
008900000000     C                   ENDIF
009000000000     C                   SETOFF                                       40        NESSUNO
009001090624     c* autisti ritiri e consegne
009002090624     c                   exsr      srcitta
009003090624     c* autisti aff/defl
009004090624     c                   eval      tipo = 'A'
009005090706     c                   seton                                        of
009006090624     c                   exsr      sraffdef
009007090624      *
009008090624     C     cn90ST        IFEQ      'S'
009009090624     C  N40              EXCEPT    NOELEM                                       NESSUNO
009010090624     C                   EXCEPT    FINEST                                       FINE STAMPA
009011090624     C                   ENDIF
009012090624     c                   commit
009013090624     C                   SETON                                        LR
009014090624     c**********************************************************************
009015090624     c     srcitta       begsr
009016090624     c**********************************************************************
009300000000      *
009400000000      * MI POSIZIONO A FINE FILE, QUINDI LEGGO ALL'INDIETRO
009500011031     C     *HIVAL        SETLL     fifgt01L
009600000000     C                   DO        *HIVAL
009700011031     C                   READP     fifgt01L                               30
010200000000      * CONTROLLO SE DTA SCADENZA E' NEL LIMITE
010300011031     C     FGTDST        IFLE      cn90D1                                       SCADUTA
010400000000     C     FGTATB        ANDEQ     *BLANK
010500000000      *
010600011031     C     $FGTR         CHAIN     fifgt00F                           3140
010700000000      *
010800000000      * 40 ON - RECORD ALLOCATO PER CUI SALTO LA TARIFFA
010801090708     c                   clear                   tgtnrc
011000090528     C                   if        not *in40 and not *in31
011001090528     c* verifico se già presente la supertesta, se c'è annullo
011002090528     c     ktgt          chain     fitgt01l
011003090528     c                   if        %found(fitgt01l) and tgtatb = ' '
011004090528     c* se tariffa mai stampata verifico se ho valorizzazioni
011005090528     c* nel qual caso non posso annullare la tariffa
011007090528     c                   if        tgtdcn = 0 and tgtdts <> 0
011008090528     c                   exsr      srval
011009090528     c                   if        maxddc <> 0
011010090528     c                   iter
011011090528     c                   end
011012090528     c                   end
011013090708     c* se tariffa stampata ma non ho ricevuto la copia firmata
011014090708     c* non posso annullare la tariffa
011015090708     c                   if        tgtdcn <> 0 and tgtdrc = 0
011016090708     c                   iter
011017090708     c                   end
011018090528     C                   MOVE      'A'           TGTATB                         'A'=ANNULLATO
011019090528     c                   update    fitgt000
011020090528     c                   end
011100000000      *
011200000000      * ANNULLO DETTAGLIO E TESTATA TARIFFA PARTICOLARE
011300000000     C                   EXSR      ANNULL
011500000000      * ANNULLO TESTATA TARIFFA
011600000000     C                   MOVE      'A'           FGTATB                         'A'=ANNULLATO
011601090708     c                   if        tgtnrc <> 0
011602090708     c                   eval      fgtdtr = tgtnrc
011603090708     c                   end
011900020528     c                   write     fifgts00
012000000000     C                   EXCEPT    TESTAR
012100000000      *
012200000000      * STAMPO PROGRESSIVO
012300011031     C     cn90ST        IFEQ      'S'
012400000000     C                   EXSR      STAMPA
012500000000     C                   ENDIF
012600000000     C                   SETON                                        40        ALMENO UNO
012601090706     c                   commit
012700000000      *
012800090528     C                   END
012900000000     C                   ENDIF
013900000000     C  N30              ENDDO
013901090624     c                   endsr
013902090624     c**********************************************************************
013903090624     c     sraffdef      begsr
013904090624     c**********************************************************************
013905090624      *
013906090624      * MI POSIZIONO A FINE FILE, QUINDI LEGGO ALL'INDIETRO
013907090624     C     *HIVAL        SETLL     fiadt01L
013908090624     C                   DO        *HIVAL
013909090624     C                   READP     fiadt01L                               30
013910090624      * CONTROLLO SE DTA SCADENZA E' NEL LIMITE
013911090624     C     adtDST        IFLE      cn90D1                                       SCADUTA
013912090624     C     adtATB        ANDEQ     *BLANK
013913090624     c* se tariffa mai stampata verifico se ho valorizzazioni
013914090624     c* nel qual caso non posso annullare la tariffa
013915090624     c                   if        adtdcn = 0 and adtdts <> 0
013916090624     c                   exsr      srvala
013917090624     c                   if        maxddc <> 0
013918090624     c                   iter
013919090624     c                   end
013920090624     c                   end
013921090708     c* se tariffa stampata ma non ho ricevuto la copia firmata
013922090708     c* non posso annullare la tariffa
013923090708     c                   if        adtdcn <> 0 and adtdrc = 0
013924090708     c                   iter
013925090708     c                   end
013926090624      *
013927090624     C     $adtR         CHAIN     fiadt00F                           3140
013928090624      *
013929090624      * 40 ON - RECORD ALLOCATO PER CUI SALTO LA TARIFFA
013930090624     C                   if        not *in40 and not *in31
013931090624     C                   MOVE      'A'           adtATB                         'A'=ANNULLATO
013932090624     c                   update    fiadt000
013933090624      *
013934090624      * ANNULLO DETTAGLIO E TESTATA TARIFFA PARTICOLARE
013935090624     C                   EXSR      ANNULLa
013937090624     c                   write     fiadts00
013939090624     c                   end
013940090624      *
013941090624      * STAMPO PROGRESSIVO
013942090624     C     cn90ST        IFEQ      'S'
013943090624     C                   EXSR      STAMPA
013944090624     C                   ENDIF
013945090624     C                   SETON                                        40        ALMENO UNO
013946090706     c                   commit
013947090624     c                   end
013949090624     C  N30              ENDDO
013950090624     c                   endsr
014600000000      *****************************************************************
014700090528      * verifico se presenti valorizzazioni per tariffa non stampata
014800000000      *****************************************************************
014900090528     C     srval         BEGSR
014901090528     c                   clear                   maxddc
014902090528     C/EXEC SQL
014903090528     C+ SELECT max(fttddc) INTO :maxddc FROM fiftt00f WHERE fttpdr =
014904090528     C+ :tgtpdr and fttddc between :tgtddt and :tgtdst
014906090528     C/END-EXEC
014907090528     c                   endsr
014908090624      *****************************************************************
014909090624      * verifico se presenti valorizzazioni per tariffa aff/def non stampata
014910090624      *****************************************************************
014911090624     C     srvala        BEGSR
014912090624     c                   clear                   maxddc
014913090624     C/EXEC SQL
014914090624     C+ SELECT max(attddc) INTO :maxddc FROM fiatt00f WHERE attpdr =
014915090624     C+ :adtpdr and attddc between :adtddt and :adtdst
014916090624     C/END-EXEC
014917090624     c                   endsr
014918090528      *****************************************************************
014919090528      *    ANNULLO TESTATA E DETTAGLIO TARIFFA PARTICOLARE
014920090528      *****************************************************************
014921090528     C     ANNULL        BEGSR
015000000000      *
015100000000      * ANNULLO DETTAGLIO
015200011031     C     fifgdK        SETLL     fifpd01L
015300011031     C     fifgdK        READE     fifpd01L                               33
015400000000     C     *IN33         DOWEQ     '0'
015500000000     C                   MOVE      'A'           FPDATB                         'A'=ANNULLATO
015800000000     C                   EXCEPT    DETPAR
015900000000      *
016000011031     C     fifgdK        READE     fifpd01L                               33
016100000000     C                   END
016200000000      *
016300000000      * ANNULLO TESTATA
016400011031     C     fifgdK        SETLL     fifpt01L
016500011031     C     fifgdK        READE     fifpt01L                               34
016600000000     C     *IN34         DOWEQ     '0'
016700000000     C                   MOVE      'A'           FPTATB                         'A'=ANNULLATO
016800000000     C*                    MOVE *BLANKS   TPTFTR           FLAG TRASMISS.
016900000000     C*                    Z-ADD0         FPTDTR           NR.TRASMISSIONE
017000000000     C                   EXCEPT    TESPAR
017100000000      *
017200011031     C     fifgdK        READE     fifpt01L                               34
017300000000     C                   END
017400000000      *
017500000000     C                   ENDSR
017501090624      *****************************************************************
017502090624      *    ANNULLO DETTAGLIO TARIFFA aff/def
017503090624      *****************************************************************
017504090624     C     ANNULLa       BEGSR
017505090624      *
017506090624      * ANNULLO DETTAGLIO
017507090624     C     fiaddK        SETLL     fiadd01L
017508090624     C     fiaddK        READE     fiadd01L                               33
017509090624     C     *IN33         DOWEQ     '0'
017510090624     C                   MOVE      'A'           addATB                         'A'=ANNULLATO
017511090624     C                   update    fiadd000
017512090624      *
017513090624     C     fiaddK        READE     fiadd01L                               33
017514090624     C                   END
017527090624      *
017528090624     C                   ENDSR
017600000000      *****************************************************************
017700000000      *    STAMPA PROGRESSIVO ANNULLATO
017800000000      *****************************************************************
017900000000     C     STAMPA        BEGSR
018000000000      *
018001090624     c                   if        tipo ='C'
018100000000      * GIRO DATA DECORRENZA
018200940926     C                   Z-ADD     FGTDDT        DATA
018300940926     C                   Z-ADD     MM            GGMM              4 0
018400940926     C                   MOVEL     GG            GGMM
018500940926     C                   Z-ADD     AA            STADDT            8 0
018600940926     C                   MOVEL     GGMM          STADDT
018700000000      *
018800000000      * GIRO DATA SCADENZA
018900940926     C                   Z-ADD     FGTDST        DATA
019000940926     C                   Z-ADD     MM            GGMM
019100940926     C                   MOVEL     GG            GGMM
019200940926     C                   Z-ADD     AA            STADST            8 0
019300940926     C                   MOVEL     GGMM          STADST
019400940926     C*
019500000000      * STAMPO DETTAGLIO
019600000000     C                   EXCEPT    DET
019601090624     c                   else
019602090624      * GIRO DATA DECORRENZA
019603090624     C                   Z-ADD     adtDDT        DATA
019604090624     C                   Z-ADD     MM            GGMM              4 0
019605090624     C                   MOVEL     GG            GGMM
019606090624     C                   Z-ADD     AA            STADDT            8 0
019607090624     C                   MOVEL     GGMM          STADDT
019608090624      *
019609090624      * GIRO DATA SCADENZA
019610090624     C                   Z-ADD     adtDST        DATA
019611090624     C                   Z-ADD     MM            GGMM
019612090624     C                   MOVEL     GG            GGMM
019613090624     C                   Z-ADD     AA            STADST            8 0
019614090624     C                   MOVEL     GGMM          STADST
019615090624     C*
019616090624      * STAMPO DETTAGLIO
019617090624     C                   EXCEPT    DETa
019618090624     c                   end
019700000000      *
019800000000     C                   ENDSR
019900020729      *-------------------------------------------------
020000020729     C     limiti        begsr
020100020729      *-------------------------------------------------
020200020729     C     KTAB          KLIST
020300020729     C                   KFLD                    CODUT
020400020729     C                   KFLD                    COD               2
020500020729     C                   KFLD                    KEY               8
020600020729     C* LA DATA DEL GIORNO LA PRENDO DA TIME
020700020729     C                   TIME                    W0120            14 0
020800020729     C                   MOVE      W0120         WDAT              8 0
020900020729     C*
021000020729     c                   clear                   wlbdat
021100020729     C                   Z-ADD     WDAT          G02DAT
021200020729     C                   MOVEL     *BLANK        G02ERR
021300020729     C                   CALL      'XSRDA8'
021400020729     C                   PARM                    WLBDAT
021500020729      * DATE PULIZIE: REPERISCO GIORNI DA DS5A
021600020729     C                   MOVEL     '5A'          COD
021700020729     C                   MOVEL     '1       '    KEY
021800020729     C     KTAB          CHAIN     TABEL                              43
021900020729     C  N43              MOVEL     TBLUNI        DS5A
022000020729     C   43              MOVEL     *ZEROS        DS5A
022100020729      *
022200020729      * SOTTRAGGO §5AFGT AI GIORNI
022300020729     C     G02TGI        SUB       §5AFGT        GIOTGI
022400020729      *
022500020729     C                   CALL      'XSRGI8'
022600020729     C                   PARM                    WGIDAT
022700020729     C                   Z-ADD     GIOinv        cn90d1
022800020729     C                   Z-ADD     GIOdat        cn90d2
022900020729     c                   move      'S'           cn90st
023000020729     C                   endsr
023100000000      *****************************************************************
023200011031     Ofifgt000  E            TESTAR
023300000000     O                       FGTATB
023400011031     Ofifpt000  E            TESPAR
023500000000     O                       FPTATB
023600011031     Ofifpd000  E            DETPAR
023700000000     O                       FPDATB
023800000000      *****************************************************************
023900000000     OQSYSPRT   E            TESTA            02
024000000000     O         OR    OF
024100000000     O                       RSUT                20
024200000000     O                       TE(1)              101
024300090706     O                       cn90D2              91 '  /  /    '
024400000000     O          E            TESTA            04 05
024500000000     O         OR    OF
024600090706     O                                            4 'TIPO'
024601090706     O                                           17 'AUTOTRAP. '
024700000000     O                                           30 'SIMULAZIONE'
024800000000     O                                           43 'PRESTAZIONE'
024900000000     O                                           65 'CD.TARIFFA'
025000000000     O                                           78 'PROGRESSIVO'
025100000000     O                                           90 'DECORRENZA'
025200000000     O                                          100 'SCADENZA'
025300000000     O          EF           DET            1
025301090706     O                                            3 'A/C'
025400090706     O                       FGTPDR              15
025500000000     O                       FGTSML              23
025600000000     O                       FGTTSR              38
025700000000     O                       FGTCTR              62
025800000000     O                       FGTPRG        Z     73
025900940926     O                       STADDT              89 '  /  /    '
026000940926     O                       STADST             100 '  /  /    '
026001090624     O          EF           DETa           1
026002090706     O                                            2 'D'
026003090706     O                       adtPDR              15
026006090624     O                       adtPRG        Z     73
026007090624     O                       STADDT              89 '  /  /    '
026008090624     O                       STADST             100 '  /  /    '
026100000000     O          E            NOELEM      1
026200940926     O                                           57 '( Nessun elemento annull'
026300940926     O                                         +  0 'ato )'
026400000000     O          E            FINEST      3
026500000000     O                                           59 '***   FINE  STAMPA  ***'
026600940926**
026700940926 **  PROGRESSIVI TARIFFA ANNULLATI FINO ALLA DATA  XX/XX/XXXX  **
