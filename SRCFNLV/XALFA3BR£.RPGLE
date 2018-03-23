000100000000     H
000200991203     F* XALFA3BR *----------------------------------------------------*
000300961016     F*     RICERCA ALFABETICA PIANO DEI CONTI
000400980701     F* CON O SENZA CAPOCONTO E CON S.I.
000500961016     F*---------------------------------------------------------------*
000600961030     FTABEL00F  IF   E           K DISK
000700980701     FAZORG01L  IF   E           K DISK
000800030401     FCNACO01L  IF   E           K DISK    RENAME(CNACO000:CNACO001)
000900980701     FCNACO04L  IF   E           K DISK
001000030401     FCNACO06L  IF   E           K DISK    RENAME(CNACO000:CNACO006)
001100050927     FCNACO00f  IF   E           K DISK    RENAME(CNACO000:CNACO0)
001200030401     FTFNTC01L  IF   E           K DISK
001300030401     FCNIND00F  IF   E           K DISK
001400050927     FCNIND02L  IF   E           K DISK    RENAME(CNIND000:CNIND2)
001500050927     FCNIND03L  IF   E           K DISK    RENAME(CNIND000:CNIND3)
001600030401     FXALFA3FM  CF   E             WORKSTN SFILE(XALFAB02:RECNUM)
001700050317     F                                     maxdev(*file)
001800030401
001900030401     D*-------------------
002000960705     D* DEFINIZIONE SCHIERE
002100030401     D*-------------------
002200961016     D FIL             S              3    DIM(30)
002300980701     D ELA             S              3  0 DIM(30)
002400980701     D ESI             S              3    DIM(30)
002500961030     D L4              S              4  0 DIM(23)
002600961030     D L4C             S              4    DIM(22)
002700990107     D KSM             S              7    DIM(20)
002800990107     D KCM             S              4    DIM(20)
002900990107     D KDM             S              3    DIM(20)
003000000912     D*-------------------
003100000912     D* DEFINIZIONE DS
003200000912     D*-------------------
003300000912     D* REPERIMENTO S.I.
003400000912     D BS10DS        E DS                  EXTNAME(TIBS10DS)
003500000912     D* CONTROLLO DATA
003600000912     D WLBDA8          DS
003700000912     D  G08DAT                 1      8  0
003800000912     D  G08INV                 9     16  0
003900000912     D  G08ERR                17     17
004000000912     D  G08TGI                18     22  0
004100050317     d
004200050317     D wesci           S              1
004300050928
004400050928     d wiva            s             16
004500050928     d wcdf            s             16
004600000000     C*---------------------------------------------------------------*
004700000000     C     RAGSO         KLIST
004800000912     C                   KFLD                    KKUT
004900000000     C                   KFLD                    PARKCC
005000000000     C                   KFLD                    PARRAG
005100961016     C     RAGSO1        KLIST
005200000912     C                   KFLD                    KKUT
005300961016     C                   KFLD                    PARKCC
005400980701     C     RAGSO3        KLIST
005500000912     C                   KFLD                    KKUT
005600980701     C                   KFLD                    PARRAG
005700961016     C     KACO62        KLIST
005800000912     C                   KFLD                    KKUT
005900961016     C                   KFLD                    PARFTT
006000961016     C                   KFLD                    PARFLT
006100961016     C                   KFLD                    PARKCC
006200961016     C     KACO6         KLIST
006300000912     C                   KFLD                    KKUT
006400961016     C                   KFLD                    PARFTT
006500961016     C                   KFLD                    PARFLT
006600961016     C                   KFLD                    PARKCC
006700961016     C                   KFLD                    PARRAG
006800030401     C     KIND          KLIST
006900030401     C                   KFLD                    acokut
007000030401     C                   KFLD                    acokcc
007100030401     C                   KFLD                    acoksc
007200960610     C     KNTC          KLIST
007300960610     C                   KFLD                    KAPL
007400960610     C                   KFLD                    KNK1
007500960610     C                   KFLD                    KNK2
007600960610     C                   KFLD                    KTNT
007700961030     C     KTAB          KLIST
007800961030     C                   KFLD                    CODUT
007900961030     C                   KFLD                    COD
008000961030     C                   KFLD                    KEY
008100050927      * CNIND00F X PARTITA IVA
008200050927MV---c     kIva          klist
008300050928MV---c                   kfld                    kkut
008400050927     c                   kfld                    parKCC
008500050927MV---c                   kfld                    indiva
008600050927      * CNIND00F X CODICE FISCALE
008700050927MV---C     kCdf          klist
008800050928MV---C                   kfld                    kkut
008900050927     C                   kfld                    parkcc
009000050927MV---C                   kfld                    indcdf
009100050927      * CNACO00F X CHIAVE COMPLETA
009200050927     c     kAco          klist
009300050927     c                   kfld                    indkut
009400050927     c                   kfld                    indkcc
009500050927     c                   kfld                    indksc
009600960610     C*
009700000912     C     *LIKE         DEFINE    PARKUT        KKUT
009800000912     C     *LIKE         DEFINE    ACOFTT        PARFTT
009900961016     C     *LIKE         DEFINE    ACOFLT        PARFLT
010000961016     C     *LIKE         DEFINE    NTCNK1        KNK1
010100960610     C     *LIKE         DEFINE    NTCNK2        KNK2
010200960610     C     *LIKE         DEFINE    NTCAPL        KAPL
010300960610     C     *LIKE         DEFINE    NTCTNT        KTNT
010400961016     C*
010500960610     C                   MOVEL     'C'           KAPL
010600960611     C                   MOVEL     'DC'          KTNT
010700961016     C                   MOVEL     '3'           PARFTT
010800000912     C                   MOVEL     1             KKUT
010900980701     C*
011000980701     C* PER CAPOCONTO CLIENTI VISUALIZZO ANCHE LA DESCRIZIONE
011100960611     C*---------------------------------------------------------------*
011200000000     C     *ENTRY        PLIST
011300990107     C* INPUT : NOME AZIENDA
011400000000     C                   PARM                    PARDUT           30
011500000912     C* INPUT : RICHIESTA COD CLIENTE UNIFICANTE : "PADRE"
011600040116     C*         1-NON VISUALIZZA CLIENTE UNIFICANTE PADRE
011700040116     C*         2-VISUALIZZA CLIENTE UNIFICANTE PADRE
011800040116     C*         3-ESTRAE E VISUALIZZA SOLO CLIENTE UNIFICANTE PADRE
011900000000     C                   PARM                    PARKUT            1 0
012000000804     C* INPUT: RICERCA ALFABETICA (ANCHE IN OUTPUT SE SCELTO SOLO 1 CODICE)
012100000000     C                   PARM                    PARRAG           48
012200990107     C* INPUT: CAPOCONTO
012300000000     C                   PARM                    PARKCC            4 0
012400990107     C* INPUT: ESCLUSIONE CODICI BLOCCATI O ANNULATI
012500000000     C                   PARM                    PARSTA            1 0
012600990107     C* INPUT: P.O. IN CUI RICERCARE
012700961016     C                   PARM                    PARFLR           90
012800990107     C* INPUT: AZIENDA IN CUI RICERCARE
012900980701     C                   PARM                    PARDIT            3
013000991203     C* INPUT: NUMERO COD.DA RITORNARE
013100991203     C                   PARM                    PARNUM            2 0
013200990107     C* OUTPUT: FINO A 20 CAPOCONTI
013300990107     C                   PARM                    PARKCM           80
013400990107     C* OUTPUT: FINO A 20 CODICI
013500990107     C                   PARM                    PARKSM          140
013600990107     C* OUTPUT: FINO A 20 DITTE CODICI
013700990107     C                   PARM                    PARKDM           60
013800050317     c* Nuovo parametro di input:
013900050317     c*              S = gestisco uscita da videata di ricerca dopo 50 sec
014000050317     c*                  restituisce l'errore: PARERR = 'SP'-suepro tempo
014100050317     C                   PARM                    PAResci           1
014200050317     C                   PARM                    PARerr            2
014300050927      * Nuovi parametri per ricerca p.IVA o codice fiscale
014400050927     c                   Parm                    pariva           16
014500050927     c                   Parm                    parcdf           16
014600050317     c
014700050928
014800050928     c                   Clear                   wiva
014900050928     c                   Clear                   wcdf
015000050928
015100050927     c**!!!              if        %parms<12
015200050927     c                   Select
015300050927     c                   when      %Parms < 12
015400050317     c                   eval      wesci=' '
015500050927     c                   When      %Parms > 13
015600050927     c                   Eval      wesci = ' '
015700050928     c                   Eval      wiva = pariva
015800050928     c                   Eval      wcdf = parcdf
015900050927     c**!!!              else
016000050927     c                   Other
016100050317     c                   eval      wesci=paresci
016200050317     c                   clear                   parerr
016300050927     c**!!!              endif
016400050927     c                   EndSl
016500980701     C** PULISCO SFL
016600980701     C                   EXSR      PULSF
016700000912     C                   SETOFF                                       7010
016800990107     C                   CLEAR                   KSM
016900990107     C                   CLEAR                   KCM
017000990107     C                   CLEAR                   KDM
017100990107     C                   CLEAR                   PARKSM
017200990107     C                   CLEAR                   PARKCM
017300991203     C                   CLEAR                   PARKDM
017400000912     C* ACCENDO INDICATORE CHE MI CONDIZIONE LA VISUALIZZAZIONE DEL
017500040116     C*  CLIENTE UNIFICANTE
017600040116     C                   IF        PARKUT = 2 or PARKUT = 3
017700000912     C                   SETON                                        10
017800000912     C                   ENDIF
017900991203     C*
018000991203     C     PARNUM        IFEQ      0
018100991203     C                   Z-ADD     1             PARNUM
018200991203     C                   ENDIF
018300000207     C* SE PARNUM=1 IMPOSTO AVANZAMENTO AUTOMATICO RECORD
018400000207     C     PARNUM        IFEQ      1
018500000207     C                   SETON                                        15
018600000207     C                   ELSE
018700000207     C                   SETOFF                                       15
018800000207     C                   ENDIF
018900980701     C**
019000980701     C* SE CAPOCONTO RICHIESTO NON E' DELLA L4 --> NON CARICO NULLA
019100980701     C**
019200980701    0C     PARKCC        IFGT      0
019300980701     C     PARKCC        LOOKUP    L4                                     20
019400980701    0C                   ENDIF
019500980701     C*
019600980701     C* CONTROLLO LE SELEZIONI
019700980701     C                   EXSR      CTRSEL
019800980701     C**
019900980701     C* SE IMMESSA UN P.O. E PARDIT DEVONO ESSERE CONGRUENTI
020000980701    1C     SAVFIL        IFGT      0
020100980701     C     PARDIT        ANDNE     *BLANKS
020200980701     C     PARDIT        ANDNE     ESI(1)
020300980701     C                   SETON                                        70
020400980701    1C                   ENDIF
020500961030     C**
020600961016     C**
020700000000     C     INIZIO        TAG
020800980701     C**
020900980701    0C     *IN70         IFEQ      *OFF
021000961016     C*
021100980701    1C                   SELECT
021200050927      * Con partita IVA
021300050928     c                   When      wiva <> *Blanks
021400050928     c                   Movel     wiva          indiva
021500050927MV---c     kIva          Setll     cnind02l                           74
021600050927      * Con codice fiscale
021700050928     c                   When      wcdf <> *Blanks
021800050928     c                   Movel     wcdf          indcdf
021900050927MV---c     kCdf          Setll     cnind03l                           74
022000980701     C* SENZA CAPOCONTO
022100980701     C     PARKCC        WHENEQ    0
022200980701     C     RAGSO3        SETLL     CNACO001                           74
022300980701     C* CON CAPOCONTO E 1 P.O. SPECIFICO
022400980701     C     SAVFIL        WHENEQ    0
022500000000     C     RAGSO         SETLL     CNACO000                           74
022600980701     C                   OTHER
022700980701     C* CON CAPOCONTO E PIU' P.O.
022800961016     C                   Z-ADD     SAVFIL        PARFLT
022900961016     C     KACO6         SETLL     CNACO006                           74
023000980701    1C                   ENDSL
023100980701     C**
023200980701     C* 74 ON - NON TROVATO NULLA
023300980701    1C     *IN74         IFEQ      *ON
023400980701     C                   SETON                                        70
023500980701   X1C                   ELSE
023600960611     C**
023700900618     C                   MOVE      *ZERO         RECNUM            4 0
023800960611     C**
023900900618     C     TORNA         TAG
024000900618     C                   MOVE      *ZERO         §REC              3 0
024100980701    2C     §REC          DOUEQ     18
024200980701    3C                   SELECT
024300050928     c                   When      wiva <> *Blanks
024400050927MV---c     kIva          Reade     cnind02l                               74
024500050928     c                   When      wcdf <> *Blanks
024600050927MV---c     kCdf          Reade     cnind03l                               74
024700980701     C     PARKCC        WHENEQ    0
024800000912     C     KKUT          READE     CNACO001                               74
024900980701     C     SAVFIL        WHENEQ    0
025000980701     C     RAGSO1        READE     CNACO000                               74
025100980701     C                   OTHER
025200961016     C     KACO62        READE     CNACO006                               74
025300980701    3C                   ENDSL
025400050927
025500050927      * se ricerca per partiva iva o codice fiscale chain su cnaco
025600050928     c  n74              If        wiva <> *Blanks or wcdf <> *Blanks
025700050927     c     kAco          Chain     Cnaco00f
025800050927     c                   If        Not %Found(Cnaco00f)
025900050927     c                   Eval      *In74 = *On
026000050927     c                   EndIf
026100050927     c                   EndIf
026200960611     C* FINE LETTURA
026300980701    3C     *IN74         IFEQ      *ON
026400960611     C                   MOVEL     'FINE'        MESSAG
026500960611     C                   MOVE      'CONTI'       MESSAG
026600960611     C                   Z-ADD     18            §REC
026700980701   X3C                   ELSE
026800980701     C**
026900980701     C* CONTROLLO SE IL SOTTOCONTO RIENTRA NELLE SELEZIONI
027000980701     C                   EXSR      CONFR
027100960611     C**
027200960611     C** RECORD DA SELEZIONARE
027300980701    4C     *IN91         IFEQ      *OFF
027400040116      *
027500040116     C* REPERISCE IL CODICE PADRE DEI CLIENTI
027600040116    5C                   IF        PARKUT = 2 or PARKUT = 3
027700040116     C                   CLEAR                   BS10DS
027800040116     C                   Z-ADD     DATCOR        D10DRF
027900040116     C                   MOVEL     'ST'          D10TLE
028000040116     C                   MOVEL     'P'           D10PAF
028100040116     C                   Z-ADD     ACOKSC        D10COD
028200040116     C                   CALL      'TIBS10R'
028300040116     C                   PARM                    BS10DS
028400040116IF  6C     D10ERR        IFEQ      *BLANKS
028500040116     C     D10COP        ANDGT     *ZEROS
028600040116     C                   Z-ADD     D10COP        VIDCOP
028700040116X   6C                   ELSE
028800040116     C                   Z-ADD     ACOKSC        VIDCOP
028900040116E   6C                   ENDIF
029000040116     C*
029100040116     C* IMPOSTA SE IL CLIENTE E' UNIFICANTE O NO
029200040116     C                   MOVEL     *BLANKS       VIDUNI
029300040116     C                   Z-ADD     VIDCOP        N7                7 0
029400040116IF  6C     N7            IFEQ      ACOKSC
029500040116     C                   MOVEL     'U'           VIDUNI
029600040116E   6C                   ENDIF
029700040116E   5C                   ENDIF
029800980701     C**
029900040116     C** SE richiesti solo unificanti proseguo se è il caso
030000040116    5C                   IF        PARKUT <> 3 or
030100040116     C                             (PARKUT = 3 and VIDUNI = 'U')
030200040116     C**
030300980701     C                   MOVEL(P)  WDIT          VIDREF
030400980701     C**
030500980701     C* SE NON C'E' IL CAPOCONTO METTO NEL RIFERIMENTO DEL SOTTOCONTO
030600980701     C*  IL SUO CAPOCONTO, ALTRIMENTI SE CLIENTI METTO ORGDIT
030700980701     C   83              MOVEL     ACOKCC        VIDREF
030800980701     C* RAGIONE SOCIALE
030900960610     C                   MOVEL     ACORAG        VIDRAG
031000030403     C*                  MOVE      ACORAG        VIDRG1
031100960611     C                   CLEAR                   VIDNT1
031200030403     C*                  CLEAR                   VIDNT2
031300960610     C* DESCRIZIONE
031400960610     C* VEDO SE C'E' LA DESCRIZIONE CODICE SOLO PER CLIENTE
031500960611     C                   MOVE      ACOKSC        KNK1
031600010131      *
031700010131     C     KNTC          SETLL     TFNTC01L
031800010131     C                   do        *hival
031900010131     C     KNTC          READE     TFNTC01L                               30
032000010131     C   30              leave
032100010131     C     ntcflt        ifne      'A'
032200030401     C                   MOVEL     NTCRNT        VIDNT1
032300030403     C*                  MOVE      NTCRNT        VIDNT2
032400010131     C                   leave
032500010131     C                   ENDIF
032600010131     C                   enddo
032700900618     C*
032800030401     C* RECUPERA INDIRIZZO
032900030401     c     kind          chain     cnind000
033000040116    6c                   IF        %found
033100030401     c                   movel     indvia        vidind
033200030401     c                   movel     indcit        vidloc
033300030401     c                   movel     indcae        vidcap
033400040116    7c                   If        indsta <> *blanks
033500030401     c                   movel(P)  indsta        vidprn
033600040116   X7c                   Else
033700030401     c                   movel(P)  indprv        vidprn
033800040116    7c                   Endif
033900040116   X6c                   ELSE
034000030401     c                   clear                   vidind
034100030401     c                   clear                   vidloc
034200030401     c                   clear                   vidcap
034300030401     c                   clear                   vidprn
034400040116    6C                   ENDIF
034500030401      *
034600030401     C*
034700960610     C                   ADD       1             §REC
034800960610     C                   ADD       1             RECNUM
034900960610     C                   WRITE     XALFAB02
035000040116    5C                   ENDIF
035100040116    4C                   ENDIF
035200980701    3C                   ENDIF
035300900911     C*
035400980701    2C                   ENDDO
035500900618     C*
035600980701    2C     *IN74         IFEQ      *ON
035700980701MV-- C     RECNUM        ANDEQ     0
035800980701     C                   SETON                                        70
035900980701MV- 2C                   END
036000980701MV- 1C                   END
036100980701MV- 0C                   END
036200900911     C*
036300000000     C     VISUAL        TAG
036400980701     C*
036500000000     C                   WRITE     XALFAB03
036600980701     C   70              WRITE     XALFAB04
036700980701     C                   CLEAR                   MESSAG
036800980701     C**
036900050317     c                   if        wesci<>'S'
037000000000     C                   EXFMT     XALFAB01
037100050317     c                   else
037200050317     C                   write     XALFAB01
037300050317     c                   read(E)   xalfa3fm
037400050317     c                   endif
037500050317     c
037600050317     c                   if        %error
037700050317     C                   Z-SUB     1             PARSTA
037800050317     c                   eval      parerr='SP'
037900050317     C                   goto      fine
038000050317    1C                   ENDIF
038100960611     C*
038200960611     C** F12 - RITORNO
038300980701    1C     *INKL         IFEQ      *ON
038400980701     C                   Z-SUB     1             PARSTA
038500980701     C                   RETURN
038600980701    1C                   ENDIF
038700960611     C** F7  - ALTRA RICERCA
038800980701     C   07
038900980701     CANN70              EXSR      PULSF
039000980701     C   07
039100980701     CANN70              GOTO      INIZIO
039200960611     C** ROLLA
039300980701     C   06
039400980701     CANN70              GOTO      TORNA
039500980701     C   06
039600980701     CAN 70              GOTO      VISUAL
039700960611     C**
039800980701    1C     *IN70         IFEQ      *OFF
039900990107     C                   Z-ADD     1             BB                2 0
040000990107     C**
040100990107     C                   READC     XALFAB02                               73
040200990107    2C     *IN73         DOWEQ     *OFF
040300991203     C     BB            ANDLE     PARNUM
040400990107     C* SELEZIONE
040500990107    3C     SCELTA        IFEQ      '1'
040600000207     C     *IN15         ANDEQ     *OFF
040700000207     C     *IN15         OREQ      *ON
040800000914     C* CAPOCONTO NON PASATO: LO PRENDO DA VIDREF
040900000914     C   83              MOVE      VIDREF        KCM(BB)
041000000914     C* N83 - UNI KCC IN PARKCC
041100000914     C  N83              MOVE      PARKCC        KCM(BB)
041200990107     C                   MOVE      ACOKSC        KSM(BB)
041300990107     C  N83              MOVEL     VIDREF        KDM(BB)
041400000804     C   15              MOVEL     VIDRAG        PARRAG
041500990107     C                   ADD       1             BB
041600000915     c* se e' riamsto impostato -1 clearo il campo
041700050317     c     parsta        iflt      0
041800000915     c                   clear                   parsta
041900000915    3C                   ENDIF
042000000915    3C                   ENDIF
042100990107     C**
042200990107     C                   READC     XALFAB02                               73
042300990107    2C                   ENDDO
042400990107     C* NESSUNA SELEZIONE
042500990107    2C     BB            IFEQ      1
042600990107     C                   MOVEL     'NESSUNA'     MESSAG
042700990107     C                   MOVE      'SELEZ.'      MESSAG
042800990107     C                   GOTO      VISUAL
042900990107    2C                   ENDIF
043000990107     C**
043100990107   X1C                   ELSE
043200980702     C* 70 ON - NON CI SONO DATI
043300980702MV-- C                   Z-SUB     1             PARSTA
043400980701    1C                   END
043500990107     C**
043600990107     C                   MOVEA     KSM           PARKSM
043700990107     C                   MOVEA     KCM           PARKCM
043800990107     C                   MOVEA     KDM           PARKDM
043900050317     c     fine          tag
044000000000     C                   RETURN
044100960705     C**-------------------------------------------------------------**
044200961030     C     *INZSR        BEGSR
044300961030     C                   Z-ADD     1             CODUT             1 0
044400961030     C                   MOVEL     '£4'          COD               2
044500961030     C                   MOVEL     '       1'    KEY               8
044600961030     C                   Z-ADD     1             C
044700961030     C                   CLEAR                   L4
044800961030     C**
044900961030     C     KTAB          CHAIN     TABEL                              30
045000961030     C*
045100961030     C     *IN30         IFEQ      *OFF
045200961030     C     TBLFLG        ANDEQ     ' '
045300961030     C                   MOVEA     TBLUNI        L4C
045400961030     C                   DO        22            YY                2 0
045500961030     C     L4C(YY)       IFGT      *ZEROS
045600961030     C                   MOVEL     L4C(YY)       L4(C)
045700961030     C                   ADD       1             C                 2 0
045800961030     C                   ENDIF
045900961030     C                   ENDDO
046000961030     C*
046100961030     C                   ENDIF
046200000912     C*
046300000912     C* REPERISCE LA DATA CORRENTE
046400000912     C                   TIME                    N14              14 0
046500000912     C                   MOVE      N14           G08DAT
046600000912     C                   Z-ADD     *ZEROS        G08INV
046700000912     C                   MOVEL     '0'           G08ERR
046800000912     C                   CALL      'XSRDA8'
046900000912     C                   PARM                    WLBDA8
047000000912     C                   Z-ADD     G08INV        DATCOR            8 0
047100000912     C*
047200961030     C                   ENDSR
047300980701     C**-------------------------------------------------------------**
047400980701     C**  CONTROLLO SELEZIONI IMMESSE
047500980701     C**-------------------------------------------------------------**
047600980701     C     CTRSEL        BEGSR
047700980701     C*
047800980701     C                   MOVEL     PARDUT        RSUT
047900980701     C                   CLEAR                   KNK1
048000980701     C                   MOVEL     PARKCC        KNK1
048100000914     C                   MOVEL     'XALFA3BR'    §PGM
048200980701     C                   SETOFF                                       9091
048300980701     C**
048400980701     C* PARSTA = 9 : NON SI VOGLIONO VISUALIZZATI GLI ANN.TI
048500980701     C*              E NON ABILITATI
048600980701     C     PARSTA        COMP      9                                      90
048700980701     C**
048800980701     C* 83 OFF- CAPOCONTO IMMESSO
048900980701     C     PARKCC        COMP      0                                      83
049000980701     C**
049100980701     C* SE NON PASSATA 1 FILIALE VEDO SE PASSATE NELLA SCHIERA
049200980701     C                   SETOFF                                       95
049300980701     C                   CLEAR                   ELA
049400980701     C                   CLEAR                   ESI
049500000207     C                   CLEAR                   SAVFIL
049600980701     C                   Z-ADD     0             Y
049700980701     C** FILIALI PASSATE
049800980701     C                   MOVEA     PARFLR        FIL
049900980701    2C                   DO        30            X                 3 0
050000980701    3C     FIL(X)        IFGT      *ZEROS
050100980701     C                   ADD       1             Y                 3 0
050200980701     C                   MOVEL     FIL(X)        ELA(Y)
050300980701     C     ELA(Y)        CHAIN     AZORG01L                           30
050400980701     C  N30              MOVEL     ORGDIT        ESI(Y)
050500980701    3C                   ENDIF
050600980701    2C                   ENDDO
050700980701     C*
050800980701     C* SE C'E' SOLO UNA FILIALE IMPOSTO SAVFIL
050900980701    2C     ELA(2)        IFEQ      0
051000980701     C     ELA(1)        ANDGT     0
051100990107     C                   Z-ADD     ELA(1)        SAVFIL            3 0
051200980701     C                   SETON                                        95
051300980701   X2C                   ELSE
051400980701     C*
051500980701     C* SE HO PASSATO PIU' DI UNA FILIALE --> MEMORIZZO
051600980701    3C     ELA(2)        IFGT      0
051700990107     C                   SETON                                        95
051800980701    3C                   ENDIF
051900980701    2C                   ENDIF
052000980701     C                   ENDSR
052100980701     C**-------------------------------------------------------------**
052200980701     C**  CONFRONTO CON LE SELEZIONI IMMESSE
052300980701     C**-------------------------------------------------------------**
052400980701     C     CONFR         BEGSR
052500980701     C                   SETOFF                                         91
052600980701     C                   CLEAR                   WDIT
052700130315    1C***  ACOABL        IFEQ      '0'
052800130315     C***  ACOABL        OREQ      '9'
052900130315     C***                MOVE      *BLANK        ACOABL
053000130315    1C***                END
053100980701     C*
053200980701    1C   90ACOFLG        IFEQ      '*'
053300130315     C**   ACOABL        OREQ      '*'
053400130315     C**   ACOABL        OREQ      '8'
053500130315     c     acoabl        orne      *blanks
053600980701     C                   SETON                                        91
053700980701    1C                   ENDIF
053800980701    C**
053900980701     C                   MOVEL     ACOKSC        COMFIL            3 0
054000980701     C*
054100980701     C* SE C'E' FILIALE CONTROLLO
054200980701     C     ACOKCC        LOOKUP    L4                                     25
054300980701    1C   25COMFIL        IFGT      0
054400980701     C**
054500980701    2C     *IN95         IFEQ      *ON
054600980701     C                   Z-ADD     1             Y
054700980701     C     COMFIL        LOOKUP    ELA(Y)                                 30
054800980701     C  N30              SETON                                        91
054900980701     C   30              MOVEL     ESI(Y)        WDIT              3
055000980701   X2C                   ELSE
055100980701     C     COMFIL        CHAIN     AZORG01L                           30
055200980701     C  N30              MOVEL     ORGDIT        WDIT              3
055300980701    2C                   ENDIF
055400980701     C**
055500980701     C* VEDO SE IMPOSTATO PARDIT
055600980701     C*  CONTROLLO SOLO SE LE PRIME 3 CIFRE DEL SOTTOCONTO SONO PIENE
055700980701    2C     PARDIT        IFNE      *BLANKS
055800980701     C     WDIT          ANDNE     PARDIT
055900980701     C                   SETON                                        91
056000980701    2C                   ENDIF
056100980701    1C                   ENDIF
056200980701     C**
056300980701     C* SE NON TROVATO CAPOCONTO NON CARICO
056400980706     C**N25                SETON                     91
056500980701     C**
056600980701     C                   ENDSR
056700980701     C**-------------------------------------------------------------**
056800980701     C**  PULIZIA SFL
056900980701     C**-------------------------------------------------------------**
057000980701     C     PULSF         BEGSR
057100980701     C                   SETON                                        71
057200980701     C                   WRITE     XALFAB01
057300980701     C                   SETOFF                                       71
057400980701     C                   ENDSR
057500000912     C**-------------------------------------------------------------**
