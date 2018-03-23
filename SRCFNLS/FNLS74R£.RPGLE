000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020208      * FNLS74R *----------------------------------------------------*
000300020208      *         - STAMPA SEGNACOLLI E RISTAMPA
000400020208      *--------------------------------------------------------------*
000500910205     FAZORG01L  IF   E           K DISK
000600971216     FTABEL00F  IF   E           K DISK
000700051012     FFIARS01L  IF   E           K DISK
000800081006     FFiar401L  IF   E           K DISK
000900941228     FFNBLD01L  UF   E           K DISK
001000020208     FFNBLD02J  IF   E           K DISK    RENAME(FNBLD000:FNBLD002)
001100020208     FFNBLD03J  IF   E           K DISK    RENAME(FNBLD000:FNBLD003)
001200020703     ffnbld06j  if   e           k disk    rename(fnbld000:fnbld006)
001300020703     ffnbld07j  if   e           k disk    rename(fnbld000:fnbld007)
001400050418     fFIAR501L  if   e           k disk    usropn
001500941228     FFNLV22P   O    F  132        PRINTER
001600020208
001700020208      *--------------------------------------------------------------*
001800020208      *       S C H I E R E
001900020208      *--------------------------------------------------------------*
002000951102     D K15             S              1    DIM(15)
002100980330     D W35             S              1    DIM(35)
002200020208
002300020208      *--------------------------------------------------------------*
002400020208      *       D S
002500020208      *--------------------------------------------------------------*
002600900523     D PARAM           DS
002700910205     D  VIDKSC                11     17
002800941228     D  VIDCPR                18     24
002900020208      * ORDINAMENTO PADRONCINO: M - MITTENTE
003000020208      *                         D - DESTINATARIO
003100960320     D  VIDOR1                25     25
003200020208      * DATA SPEDIZIONE AAMMGG
003300941228     D  VIDDSP                26     33  0
003400020208      * S= RISTAMPA
003500941228     D  VIDRSN                34     34
003600020208      * PARZIALIZZAZIONE PER RAGIONE SOCIALE
003700951102     D  VIDRSM                35     49
003800020208      * ORDINAMENTO MITTENTE  : C - CODICE PRODOTTO
003900020208      *                         T - TERMINAL DI ARRIVO
004000960320     D  VIDOR2                50     50
004100020208      * PGM STAMPA SEGNACOLLI
004200941228     D  PSS                  227    236
004300020208      *
004400910412     D                 DS
004500941228     D  BLDAAS                 1      4  0
004600941228     D  BLPMGS                 5      8  0
004700941228     D  BLPDSP                 1      8  0
004800020208      *
004900971216     D DS03          E DS
005000971217     D DS15          E DS
005100971216     D DS3A          E DS
005400060519     D DSBL4I        E DS
005500060519     D DSBL4K        E DS
005600991206     D OG143         E DS
005700040525     D OG150         E DS
005800971217     D DSBLD         E DS
005900971216     D DSBLDE        E DS
006000050418     d dAR5gen       e ds                  inz
006100910205     D KPJBA         E DS
006200020208      *
006300991214     D                 DS
006400991214     D  DPDSTP                 1     14
006500991214     D  DPDS1                  1      3
006600991214     D  DPDV1                  4      4    INZ
006700991214     D  DPDS2                  5     12
006800991214     D  DPDV2                 13     13    INZ
006900991214     D  DPDS3                 14     14
007000020208      *
007100991214     D                 DS
007200991214     D  DPDPAR                 1     12
007300991214     D  DPD1                   1      3
007400991214     D  DPD2                   4     11
007500060522     D**DPD3                  12     12
007600020703
007700020703      *--------
007800020703      * Ds di riferimento al file esterno AzUte00F
007900020703      *--------
008000020703     d AzuteDs       e ds                  ExtName(AzUte00F)
008100020703      *--------
008200020703      * Ds per dati organigramma
008300020703      *--------
008400020703     d DDatiUte      e ds
008500020703      *--------
008600020703      * Parametri per richiamo al pgm di controllo profilo utenti
008700020703      *--------
008800020703     d Tibs34Ds      e ds                  Inz
008900020703      *--------
009000020703      * Parametri per richiamo al pgm trul06r
009100020703      *--------
009200020703     d Trul06Ds      e ds
009300020703     d  lin                    1     90  0 dim(30)
009400020208
009500020208      *--------------------------------------------------------------*
009600020208      *       C A M P I
009700020208      *--------------------------------------------------------------*
009800020208     D COD             S                   like(TBLCOD)
009900020208     D key             S                   like(TBLKEY)
010000020208     D FST2            S                   like(BLDFST)
010100020208     D LNANTW          S                   like(§ogntw)
010200020208     D LNPNTW          S                   like(§ogntw)
010300060220     D WTRC            S                   like(ar4TRC)
010400020208     D comscd          S              8  0
010500020208     D KSC             S              7  0
010600020208     D cpr             S              7  0
010700020208     D wpos            S              2  0
010800020208     D x               S              2  0
010900020208     D §z              S              2  0
011000020208     D bb              S              2  0
011100020208     D fst             S              1
011200020912     D savfst          S              1
011300020208     D wsta            S              1
011400020208     D w015A           S             15
011500020208     D w005A           S              5
011900020208     D flgtb1          S              1
012000020703     d xx              s              2  0 inz
012100020703     d klnp            s                   like(bldlnp)
012200020208
012300020208      ****************************************************************
012400020208      *  RIEPILOGO INDICATORI
012500020208      ***************************************************************
012600020208      * 04    - RICHIESTA PER PADR. IN ORDINE DI MITTENTE/DESTINATARIO
012700020208      * 05    - RICHIESTA RISTAMPA
012800020208      * 07    - RICHIESTA PER PADR. IN ORDINE DI DESTINATARIO/LNP
012900020208      * 08    - RICHIESTA PER MITT. IN ORDINE DI CODICE PRODOTTO/TFA
013000020208      * 09    - RICHIESTA PER MITT. IN ORDINE DI TER.ARR/LNA
013100020208      * 30    - COMODO
013200020208      * 31    - FINE LETTURA FILE
013300020208
013400020208      *****************************************************************
013500000000     C     *ENTRY        PLIST
013600000000     C                   PARM                    KPJBA
013700020703     C                   MOVEL     KPJBU         PARAM
013800020703
013900020703      * reperisco i dati utente
014000020703     c     *dtaara       define    §azute        AzuteDs
014100020703     c     *dtaara       define    §datiute      DDatiUte
014200020703
014300020703     c                   in(E)     *dtaara
014400020703     c                   IF        %Error or RSUT = *blanks
014500020703     c                   call      'TIBS34R'
014600020703     c                   parm                    Tibs34Ds
014700020703     c                   in        *dtaara
014800020703     c                   ENDIF
014900020703
015000020208      *---------------------------------------------------------------*
015100910205     C     KBLD          KLIST
015200910205     C                   KFLD                    BLDAAS
015300910205     C                   KFLD                    BLDLNP
015400910205     C                   KFLD                    BLDNRS
015500910205     C                   KFLD                    BLDNSP
015600910219     C                   KFLD                    BLDCDP
015700060220     C     Kar4          KLIST
015800971216     C                   KFLD                    BLDAAS
015900971216     C                   KFLD                    BLDLNP
016000971216     C                   KFLD                    BLDNRS
016100971216     C                   KFLD                    BLDNSP
016200971216     C                   KFLD                    WTRC
016300910207     C     KBLD1         KLIST
016400020703     c                   kfld                    klnp
016500910207     C                   KFLD                    KSC
016600020208     C                   KFLD                    FST
016700910207     C     KBLD2         KLIST
016800910207     C                   KFLD                    CPR
016900020208     C                   KFLD                    FST
017000971216     C     KTAB2         KLIST
017100971217     C                   KFLD                    CODUT             1 0
017200020208     C                   KFLD                    COD
017300020208     C                   KFLD                    KEY
017400020208      *
017500051012     C     KARS01        KLIST
017600980324     C                   KFLD                    BLPFLS
017700980324     C                   KFLD                    BLDLNA
017800980324     C                   KFLD                    BLDNRS
017900980324     C                   KFLD                    §LDNCD
018000980324     C                   KFLD                    WTRC
018100050418      * FIAR501L
018200050418     c     k05AR501      klist
018300050418     c                   kfld                    BLDaas
018400050418     c                   kfld                    BLDlnp
018500050418     c                   kfld                    BLDnrs
018600050418     c                   kfld                    BLDnsp
018700050418     c                   kfld                    AR5trd
018800050418     c                   eval      AR5trd = 'GEN'
018900020208      *
019000971217     C                   Z-ADD     1             CODUT
019100020208      *
019200910205     C*---------------------------------------------------------------*
019300971217     C** SIGNIFICATI DEL CAMPO BLDFST:
019400971217     C*  ' ' -->   STAMPARE SIA  SEGNACOLLI ITA CHE EST
019500971217     C*  'E' -->   STAMPARE SOLO SEGNACOLLO         EST
019600971217     C*  'S' --> RISTAMPATE SIA  SEGNACOLLI ITA CHE EST
019700971217     C*  'Z' --> RISTAMPARE SOLO SEGNACOLLO         EST
019800971216     C**
019900971216     C                   CLEAR                   DSBLD
020000960320     C**
020100050418     C**  IMMESSO  C O D I C E   C L I E N T E   M I T T E N T E
020200910205     C     VIDKSC        IFGT      *ZEROS
020300020208     C                   MOVEL     VIDKSC        KSC
020400960320     C* 08 ON - ORDINATO PER CODICE PRODOTTO
020500960320     C     VIDOR2        IFEQ      'P'
020600020208     C                   eval      *in08 = *on
020700960320     C                   ENDIF
020800960320     C* 09 ON - ORDINATO PER TERMINAL ARRIVO
020900960320     C     VIDOR2        IFEQ      'T'
021000020208     C                   eval      *in09 = *on
021100960320     C                   ENDIF
021200960320     C*
021300910205     C                   END
021400960320     C**
021500960320     C** IMMESSO  C O D I C E   P A D R O N C I N O
021600910205     C     VIDCPR        IFGT      *ZEROS
021700020208     C                   MOVEL     VIDCPR        CPR
021800960320     C*
021900960320     C* 04 ON - ORDINATO PER MITTENTE
022000960320     C     VIDOR1        IFEQ      'M'
022100020208     C                   eval      *in04 = *on
022200951102     C* VEDO SE IMMESSA LA RAGIONE SOCIALE
022300951102     C     VIDRSM        IFNE      *BLANKS
022400020208     C     ' '           CHECKR    VIDRSM        WPOS
022500960320     C                   ENDIF
022600960320     C                   ENDIF
022700960320     C* 07 ON - ORDINATO PER DESTINATARIO
022800960320     C     VIDOR1        IFEQ      'D'
022900020208     C                   eval      *in07 = *on
023000960320     C                   ENDIF
023100960320     C*
023200960320     C                   ENDIF
023300910412     C*
023400020208      * VEDO SE RICHIESTA  LA RISTAMPA
023500910412     C     VIDRSN        IFEQ      'S'
023600020208     C                   eval      *in05 = *on
023700050418     ***C                   MOVEL     'R'           §LDRST
023800910412     C                   MOVEL     'S'           FST
023900971217     C                   MOVEL     'Z'           FST2
024000910412     C                   ELSE
024100050418     ***C                   MOVEL     *BLANKS       §LDRST
024200971217     C                   MOVEL     ' '           FST
024300971217     C                   MOVEL     'E'           FST2
024400050418i    C                   END
024500020703
024600020703      * richiesta stampa per mittente
024700020703     c                   if        *in08 or *in09
024800020703      * se p.o. di secondo livello imposto nella sk delle lnp se
024900020703      * stesso
025000020703     c                   clear                   lin
025100020703     c                   if        dutlpo = '2' or dutlpo = *blanks
025200020703     c                   eval      lin(1) = dutpou
025300020703      * se p.o. di primo livello carico tutti i p.o. del terminal di partenza
025400020703     c                   else
025500020703     c                   clear                   Trul06Ds
025600020703     c                   eval      D06Cod = '£1'
025700020703     c                   eval      D06Tla = 'L'
025800020703     c                   movel     Simfel        D06Key
025900020703     c                   movel(p)  Trul06Ds      Kpjbu
026000020703     c                   call      'TRUL06R'
026100020703     c                   parm                    Kpjba
026200020703     c                   movel     Kpjbu         Trul06Ds
026300020703     c                   endif
026400020703     c                   endif
026500020912
026600020912      * Salvo il flag fst
026700020912     c                   Eval      savfst = fst
026800020703
026900020703     c     leggi         tag
027000020703     c                   if        *in08 or *in09
027100020703      * imposto la prima linea di partenza
027200020703     c                   add       1             xx
027300020703     c                   if        lin(xx) = *zeros
027400020703     c                   goto      fine
027500020703     c                   endif
027600020912      * reimposto il flag fst
027700020912     c                   Eval      fst = savfst
027800020703     c                   eval      klnp = lin(xx)
027900020703     c                   endif
028000020703
028100020208      **
028200020208      * FACCIO SEMPRE 2 GIRI:
028300020208      *  PRIMA I COLLI ITA/EST
028400020208      *  POI  SOLO I COLLI EST
028500971217    1C                   DO        2
028600020208     C* PADRONCINO
028700960320     C   07KBLD2         SETLL     FNBLD02J
028800960320     C   04KBLD2         SETLL     FNBLD03J
028900960320     C* MITTENTE
029000020703     C   08KBLD1         SETLL     FNBLD06J
029100020703     C   09KBLD1         SETLL     FNBLD07J
029200960320     C*
029300960320     C* PADRONCINO
029400960320     C   07KBLD2         READE     FNBLD02J                               31
029500960320     C   04KBLD2         READE     FNBLD03J                               31
029600960320     C* MITTENTE
029700020703     C   08KBLD1         READE     FNBLD06J                               31
029800020703     C   09KBLD1         READE     FNBLD07J                               31
029900910205     C*
030000971217    2C     *IN31         DOWEQ     *OFF
030100951102     C*
030200951102     C* S T A M P A
030300971217    3C     *IN05         IFEQ      *OFF
030400951102     C*
030500980204AG*  C* NEL CASO IN CUI SIA VALORIZZATA LA DATA DI SPEDIZIONE ANCHE SE NON
030600980204AG*  C* SONO IN RISTAMPA CONTROLLO CHE LA DATA SPEDIZIONE SEGNACOLLI
030700980204AG*  C* LETTI SIA UGUALE A QUELLA RISCHIESTA A VIDEO
030800980204AG*  C     VIDDSP        IFNE      *ZEROS
030900980204AG*  C     BLPDSP        ANDEQ     VIDDSP
031000980204AG*  C     VIDDSP        OREQ      *ZEROS
031100980204AG*  C*
031200020208     C                   MOVEL     'S'           WSTA
031300951102     C* PER MITTENTE CON RAGIONE SOCIALE CONTROLLO LA PARZIALIZZAZ.
031400971217    4C   04VIDRSM        IFNE      *BLANKS
031500960320     C                   EXSR      CTRPAR
031600971217    4C                   ENDIF
031700951102     C*
031800971217    4C     WSTA          IFEQ      'S'
031900910205     C                   EXSR      STASCD
032000910412     C* AGGIORNO FLAG
032100941228     C     KBLD          CHAIN     FNBLD000                           30
032200971217     C**
032300971217     C  N30BLDFST        IFEQ      ' '
032400971217     C                   MOVEL     'S'           BLDFST
032500971217     C                   ELSE
032600971217     C                   MOVEL     'Z'           BLDFST
032700971217     C                   ENDIF
032800910412     C  N30              EXCEPT    UPBLD
032900971219     C**
033900000201     C**
034900971219    4C                   ENDIF
035000980204AG*  C*
035100980204AG*  C                   ENDIF
035200980204     C*
035300951102     C* R I S T A M P A
035400971217   X3C                   ELSE
035500971217    4C     BLPDSP        IFEQ      VIDDSP
035600951102     C*
035700020208     C                   MOVEL     'S'           WSTA
035800951102     C* PER MITTENTE CON RAGIONE SOCIALE CONTROLLO LA PARZIALIZZAZ.
035900971217    5C   04VIDRSM        IFNE      *BLANKS
036000960320     C                   EXSR      CTRPAR
036100971217    5C                   ENDIF
036200951102     C*
036300971217    5C     WSTA          IFEQ      'S'
036400910412     C                   EXSR      STASCD
036500971217    5C                   ENDIF
036600971217    4C                   ENDIF
036700971217    3C                   ENDIF
036800951102     C**
036900960320     C* PADRONCINO
037000960320     C   07KBLD2         READE     FNBLD02J                               31
037100960320     C   04KBLD2         READE     FNBLD03J                               31
037200960320     C* MITTENTE
037300020703     C   08KBLD1         READE     FNBLD06J                               31
037400020703     C   09KBLD1         READE     FNBLD07J                               31
037500971217    2C                   ENDDO
037600971217     C                   MOVEL     FST2          FST
037700971217    1C                   ENDDO
037800020703
037900020703      * se richiesta stampa per mittente devo tornare a leggere la linea
038000020703      * di partenza sucessiva
038100020703     c                   if        *in08 or *in09
038200020703     c                   goto      leggi
038300020703     c                   endif
038400910412     C**
038500000000     C     FINE          TAG
038600020208     C                   eval       *inLR = *on
038700951102     C*
038800951102     C*--- CONTROLLO PARZIALIZZAZIONE PER RAGIONE SOCIALE -----------*
038900951102     C     CTRPAR        BEGSR
039000951122     C     WPOS          IFLT      15
039100020208     C     WPOS          ADD       1             X
039200951102     C                   MOVEA     BLPRSM        K15
039300951102     C                   MOVEA     *BLANKS       K15(X)
039400020208     C                   MOVEA     K15           W015A
039500951122     C                   ELSE
039600951122     C                   MOVEL     BLPRSM        W015A
039700951122     C                   ENDIF
039800951102     C*
039900951102     C     W015A         IFNE      VIDRSM
040000951102     C                   CLEAR                   WSTA
040100951102     C                   ENDIF
040200951102     C                   ENDSR
040300910205     C*
040400910205     C*--- STAMPA BOLLA ---------------------------------------------*
040500910205     C     STASCD        BEGSR
040600000201     C                   CLEAR                   DSBLDE
040800020211     C* RICHIAMO DIRETTAMENTE IL PROGRAMMA DI STAMPA SEGNACOLLO
040900020208     C                   Z-ADD     BLDNCD        COMSCD
041000910205     C     BLDNCA        IFEQ      0
041100910205     C                   Z-ADD     BLDNCD        BLDNCA
041200910205     C                   END
041300050418      *
041400050418      * Verifica se spedizione "di valore"
041500050418     c                   exsr      CtrSpeVal
041600050418      *
041700050418      * impostazione flag ristampa/di_valore
041800050418     c                   clear                   §LDrst
041900050418sel 1c                   select
042000050418      * - ristampa collo appartenente a spedizione "di valore"
042100050418w   1c                   when          VIDrsn  =  'S'
042200050418     c                             and §AR5bva =  'V'
042300050418     c                   movel     'X'           §LDrst
042400050418      * - ristampa collo appartenente a spedizione NON "di valore"
042500050418w   1c                   when          VIDrsn  =  'S'
042600050418     c                             and §AR5bva =  *blanks
042700050418     c                   movel     'R'           §LDrst
042800050418      * - stampa   collo appartenente a spedizione "di valore"
042900050418w   1c                   when      §AR5bva =  'V'
043000050418     c                   movel     'V'           §LDrst
043100050418      *** stampa   collo appartenente a spedizione NON "di valore"
043200050418w   1c***                when      §AR5bva =  *blanks
043300050418     c***                movel     *blanks       §LDrst
043400050418e   1c                   endsl
043500020211      *
043600910219     C                   Z-ADD     BLDNC1        §LDNC1
043700910205     C                   MOVEL     BLPFLS        §LDLNP
043800020208     C     BLPFLS        CHAIN     AZORG
043900020208     c                   if        %found(AZORG01L)
044000020208     C                   MOVEL     ORGDES        §LDDLP
044100020208     c                   else
044200020208     C                   MOVEL     *BLANKS       §LDDLP
044300020208     c                   endif
044400910205     C                   MOVEL     BLDCDP        §LDCDP
044500910205     C                   MOVEL     BLDNRS        §LDNRS
044600910205     C                   MOVEL     BLDLNA        §LDLNA
044700020208      *
044800020208     C     BLDLNA        CHAIN     AZORG
044900020208     C                   clear                   §LDDLA
045000020208     c                   if        %found(AZORG01L)
045100020208     C     ORGDE5        IFNE      *BLANKS
045200910426     C                   MOVEL     ORGDE5        §LDDLA
045300910426     C                   ELSE
045400910426     C                   MOVEL     ORGDES        §LDDLA
045500910426     C                   END
045600991206     C                   MOVEL     ORGDE3        OG143
045700040525     C                   MOVEL     ORGDF0        OG150
045800020208     C                   MOVEL     §ogntw        LNAntw
045900040525     c                   movel     orgtel        dpdtel           15
046000040525     c                   movel     orgfax        dpdfax           15
046100020208     c                   endif
046200020208      **
046300020208     C     BLDLNP        CHAIN     AZORG
046400020208     C                   MOVEL     ORGDE3        OG143
046500020208     C                   MOVEL     §ogntw        LNPntw
046600971224     C**
046700910205     C                   MOVEL     BLDTFA        §LDTNA
046800910205     C                   MOVEL     BLPTSP        §LDTSP
046900910205     C                   MOVEL     BLPZNC        §LDZSC
047000910219     C                   MOVE      BLPNCL        §LDNC2
047100910205     C                   MOVEL     BLPRSM        §LDRSM
047200910205     C                   MOVEL     BLPPRM        §LDPRM
047300910205     C                   MOVEL     BLPRSD        §LDRSD
047400910205     C                   MOVEL     BLPCAD        §LDCAD
047500910205     C                   MOVEL     BLPLOD        §LDLOD
047600910404     C                   MOVEL     BLPIND        §LDIND
047700910205     C                   MOVEL     BLPPRD        §LDPRD
047800941228     C                   MOVE      BLDPDR        §LDCPD
047900910205     C                   MOVEL     *ZEROS        §LDNCP
048000910205     C                   MOVEL     *ZEROS        §LDNCU
048100941228     C                   Z-ADD     BLPPKF        §LDPKG
048200941228     C                   MOVEL     BLPFVF        §LDFVR
048300941228     C                   Z-ADD     BLPVLF        §LDVOL
048400950116     C                   Z-ADD     BLDAAS        §LDAAS
048500950116     C                   Z-ADD     BLPMGS        §LDMGS
048600000201      * SE LNA DPD
048700991206      * IMPOSTO NUMERO PARCEL DPD IN RIFERIMENTI SEGNACOLLO
048800020211    1C                   if        LNANTW = 'DPD'
048900060522     C                   MOVEL     'I'           WTRC
049000081006     C     Kar4          CHAIN     Fiar401L
049100060220     c                   if        %found(fiar401L)
049200060522     C                   MOVEL     ar4NOT        DSBL4I
049300020211     c                   else
049400060522     C                   CLEAR                   DSBL4I
049500020211     c                   endif
049600060522    2C     §B4ipn        IFNE      *BLANKS
049700010302     C* MUOVO ALLINEATO, A SX DELLA RSM, IL RMA SE <> DA BLANKS
049800010302     C* E LO COMPATTO PERDENDO CIOÈ I BLANK NON SIGNIFICATIVI
049900010302     C     BLDCDP        IFNE      *BLANKS
050000010302     C                   CLEAR                   W35
050100010302     C                   MOVEA     BLDCDP        W35
050200020208     C     ' '           CHECKR    BLDCDP        §Z
050300010302     C                   ADD       2             §Z
050400010302     C                   MOVEA     BLPRSM        W35(§Z)
050500010302     C                   MOVEA     W35           §LDRSM
050600010302     C                   ENDIF
050800060522     C                   MOVEL     §B4ipn        DPDPAR
050900991214     C                   MOVEL     DPD1          DPDS1
051000991214     C                   MOVEL     DPD2          DPDS2
051100060522     C                   MOVEL     §b4icd        DPDS3
051200991214     C                   MOVEL     DPDSTP        §LDCDP
051300000201     C*
052300000201     C     BLPPKF        IFLE      3
052400000201     C                   MOVEL     'X'           §DESSE
052500000201     C                   ENDIF
052600060523     c*
052700060523     C                   MOVEL     'K'           WTRC
052800081006     C     Kar4          CHAIN     Fiar401L
052900060523     c                   if        %found(fiar401L)
053000060523     C                   MOVEL     ar4NOT        DSBL4K
053100060523     c                   else
053200060523     C                   CLEAR                   DSBL4K
053300060523     c                   endif
053400000201     C*
053500060523     C                   MOVEL     blpnzd        §DESIA
053600060523     C                   MOVEL     §B4krt        §DEAWB
053700060523     C                   MOVEL     §B4ipn        §DEEUR
053800060523     c                   eval      %subst(§deeur:12:1) = §b4icd
053900000201     C                   MOVEL     DPDTEL        §DEXX2
054000000201     C                   MOVE      DPDFAX        §DEXX2
054100060523     c                   eval      w005a = %subst(§b4ida:2:3) + §b4iza
054200000201     C                   MOVE      W005A         §DEXX1
054300991206    2C                   ENDIF
054400991206    1C                   ENDIF
056600020211      *
056700020208      * SE ARRIVO EUROEXPRESS
056800020208      * IMPOSTO EUR PER EUROEXPRESS
056900040506      * 06/05/05: Imposto "EEX" per euroexpress
057000020211     C     LNANTW        IFEQ      'EEX'
057100040506     C                   MOVEL     'EEX'         §DETSE
057200020211     C                   ENDIF
059200020208      *
059400020208      **
060700020211      **
060800020208    2C                   IF        FST = *blank  OR  FST = 'S'
060900971216     C                   MOVEL     'ITA'         §DETST
061000040506     c     lnantw        ifeq      'EEX'
061100040506     c                   exsr      etiest
061200040506     c                   endif
061300020208   x2C                   ELSE
061400971217     C                   MOVEL     '   '         §DETST
061500020208    2C                   ENDIF
061600020208      **
061800020211      *
061900971217     C     §DETST        IFNE      *BLANKS
062000910208     C     COMSCD        DOWLE     BLDNCA
062100990318     C     COMSCD        ANDGT     0
062200911122     C                   Z-ADD     COMSCD        §LDNCD
062300020211      *
062400020211      * Particolarità:
062500020211      *    EUROEXPRESS: imposta §DEEUR e §LDCDP
062600020211      *          FEDEX: imposta §LDCDP
062700020211     C                   SELECT
062800020211     c                   When      LNAntw = 'FED'
062900020211     c                   exsr      Partic_Fed
063000040506     c                   When      LNPntw = 'EEX'
063100020211     c                   exsr      Partic_Eex
063200020211     c                   ENDSL
063300910205     C*
063400941228     C                   CALL      PSS
063500910205     C                   PARM                    DSBLD
063600971217     C                   PARM                    DSBLDE
063700910205     C*
063800910205     C                   ADD       1             COMSCD
063900910219     C                   ADD       1             §LDNC1
064000910205     C*
064100971217     C                   ENDDO
064200971217     C                   ENDIF
064300020211      *
064400910205     C                   ENDSR
064500971216     C* STAMPA ETICHETTA ESTERA SDI O BARTOLINI ----------------------*
064600971216     C     ETIEST        BEGSR
064700971216     C*
064900081007    1C     §detse        ifeq      'EEX'
065000020211      * CHAIN SULLE NAZIONI
065100971217     C                   MOVEL     '15'          COD
065200971217     C                   MOVEL(P)  BLPNZD        KEY
065300020211     C     KTAB2         CHAIN     TABEL
065400020215     c                   IF        %FOUND(tabel00f) and TBLFLG =  *blanks
065500020211     C                   MOVEL     TBLUNI        DS15
065600020211     C                   MOVEL     §15COD        §DEISA
065700020211     c                   ELSE
065800020211     C                   CLEAR                   DS15
065900020211     c                   ENDIF
066000020211      *
068100020211     C**  DPD - EUROEXPRESS
068300971216     C**
068400971216     C                   MOVEL     '3A'          COD
068500971216     C                   MOVEL(P)  BLPCBO        KEY
068600020211     C     KTAB2         CHAIN     TABEL
068700971216     C                   MOVEL     TBLUNI        DS3A
068800020208     C                   MOVEL     §3ATB1        FLGTB1
068900971216     C**
069000971216    3C                   SELECT
069100971216     C     FLGTB1        WHENEQ    'A'
069200971216     C     §3AFCA        ANDEQ     0
069300971216     C                   MOVEL     'A'           §DEFCA
069400971216     C     FLGTB1        WHENEQ    'A'
069500971216     C     §3AFCA        ANDNE     0
069600971216     C                   MOVEL     'X'           §DEFCA
069700971216     C     §3AFCA        WHENNE    0
069800971216     C                   MOVEL     'S'           §DEFCA
069900971216    3C                   ENDSL
070100971216    1C                   ENDIF
070200971216     C**
070300971216     C                   ENDSR
070400020211      *---------------------------------------------------------------*
070500020211      *--- PARICOLARTITA' FEDEX -------------------------------------*
070600020211      *---------------------------------------------------------------*
070700020211     C     Partic_Fed    BEGSR
070800020211      **
070900020211      * IMPOSTO AWB NEL CODICE PRODOTTO
071000020211     C                   MOVEL     'F'           WTRC
071100051012     C***  KBLS01        CHAIN     FNBLS01L
071200051012     C***                if        %found(FNBLS01L) and BLSnot <> *blanks
071300051012     C***                MOVEL     BLSnot        §LDCDP
071400051012    2C***                endif
071500051012     C     KARS01        CHAIN     FIARS01L
071600051012     C                   if        %found(FIARS01L) and ARSnot <> *blanks
071700051012     C                   MOVEL     ARSnot        §LDCDP
071800051012    2C                   endif
071900020211      **
072000020211     C                   ENDSR
072100020211      *---------------------------------------------------------------*
072200020211      *--- PARICOLARTITA' EUROEXPRESS  ------------------------------*
072300020211      *---------------------------------------------------------------*
072400020211     C     Partic_Eex    BEGSR
072500020208      **
072600991206     C                   MOVEL     'E'           WTRC
072700051012     C     KarS01        CHAIN     FIarS01L
072800051012    2C                   IF        %FOUND(FIarS01L)
072900020211      * PASSO AL PGM DI STAMPA ETICHETTE IL N. SEGNACOLLO EUROEXPRESS
073000051012     C                   MOVEL     arSNOT        §DEEUR
073100020211      * SE IMPORT METTO NEL CODICE PRODOTTO DELL'ETICHETTA
073200020211      * ITALIA IL N.SEGNACOLLO EUROEXPRESS
073300020208    3C     LNPntw        IFEQ      'EEX'
073400051012     C                   MOVEA     arSNOT        W35
073500020208     C                   Z-ADD     1             BB
073600020208     C     ' '           LOOKUP    W35(BB)                                30
073700980330     C   30              MOVEA     *BLANKS       W35(BB)
073800980330     C                   MOVEA     W35(6)        §LDCDP
073900980324    3C                   ENDIF
074000980324    2C                   ENDIF
074100020208      *
074200980324     C                   ENDSR
074300050418      *---------------------------------------------------------------*
074400050418      *  Verifica se spedizione "DI VALORE"                           *
074500050418      *---------------------------------------------------------------*
074600050418     c     CtrSpeVal     BEGSR
074700050418      *
074800050418if  1c                   if        NOT %open(FIAR501L)
074900050418     c                   open      FIAR501L
075000050418e   1c                   endif
075100050418      *
075200050418      * Verifica se spedizione in esame = ultima "reperita" su FIAR5
075300050418if  1c                   if            BLDaas = AR5aas
075400050418     c                             and BLDlnp = AR5lnp
075500050418     c                             and BLDnrs = AR5nrs
075600050418     c                             and BLDnsp = AR5nsp
075700050418     c                   leavesr
075800050418e   1c                   endif
075900050418      *
076000050418      * Reperisce i dati da FIAR5
076100050418     c                   clear                   dAR5gen
076200050418     c     K05AR501      setgt     FIAR5000
076300050418do  1c                   dou            %eof(FIAR501L)
076400050418     c                             or   AR5atb =  *blanks
076500050418     c     K05AR501      readpe    FIAR5000
076600050418e   1c                   enddo
076700050418if  1c                   if             NOT %eof(FIAR501L)
076800050418     c                             and  AR5atb =  *blanks
076900050418     c                   movel     AR5uni        dAR5gen
077000050418e   1c                   endif
077100050418      *
077200050418     c                   ENDSR
077300050418      *
077400020211      *-------------------------------------------------------------------
077500941228     OFNBLD000  E            UPBLD
077600910205     O                       BLDFST
077700941228     OFNLV22P   T    LRNLR
