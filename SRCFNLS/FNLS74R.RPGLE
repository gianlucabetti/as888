000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020208      * FNLS74R *----------------------------------------------------*
000300020208      *         - STAMPA SEGNACOLLI E RISTAMPA
000400020208      *--------------------------------------------------------------*
000500910205     FAZORG01L  IF   E           K DISK
000600971216     FTABEL00F  IF   E           K DISK
000700051012     FFIARS01L  IF   E           K DISK
000800171011     F*Fiar401L  IF   E           K DISK
000900171011     FFipnd01L  IF   E           K DISK
001000941228     FFNBLD01L  UF   E           K DISK
001100020208     FFNBLD02J  IF   E           K DISK    RENAME(FNBLD000:FNBLD002)
001200020208     FFNBLD03J  IF   E           K DISK    RENAME(FNBLD000:FNBLD003)
001300020703     ffnbld06j  if   e           k disk    rename(fnbld000:fnbld006)
001400020703     ffnbld07j  if   e           k disk    rename(fnbld000:fnbld007)
001500050418     fFIAR501L  if   e           k disk    usropn
001600941228     FFNLV22P   O    F  132        PRINTER
001700020208
001800020208      *--------------------------------------------------------------*
001900020208      *       S C H I E R E
002000020208      *--------------------------------------------------------------*
002100951102     D K15             S              1    DIM(15)
002200980330     D W35             S              1    DIM(35)
002300020208
002400020208      *--------------------------------------------------------------*
002500020208      *       D S
002600020208      *--------------------------------------------------------------*
002700900523     D PARAM           DS
002800910205     D  VIDKSC                11     17
002900941228     D  VIDCPR                18     24
003000020208      * ORDINAMENTO PADRONCINO: M - MITTENTE
003100020208      *                         D - DESTINATARIO
003200960320     D  VIDOR1                25     25
003300020208      * DATA SPEDIZIONE AAMMGG
003400941228     D  VIDDSP                26     33  0
003500020208      * S= RISTAMPA
003600941228     D  VIDRSN                34     34
003700020208      * PARZIALIZZAZIONE PER RAGIONE SOCIALE
003800951102     D  VIDRSM                35     49
003900020208      * ORDINAMENTO MITTENTE  : C - CODICE PRODOTTO
004000020208      *                         T - TERMINAL DI ARRIVO
004100960320     D  VIDOR2                50     50
004200020208      * PGM STAMPA SEGNACOLLI
004300941228     D  PSS                  227    236
004400020208      *
004500910412     D                 DS
004600941228     D  BLDAAS                 1      4  0
004700941228     D  BLPMGS                 5      8  0
004800941228     D  BLPDSP                 1      8  0
004900020208      *
005000971216     D DS03          E DS
005100971217     D DS15          E DS
005200971216     D DS3A          E DS
005300171011     D*DSBL4I        E DS
005400171011     D*DSBL4K        E DS
005500991206     D OG143         E DS
005600040525     D OG150         E DS
005700971217     D DSBLD         E DS
005800971216     D DSBLDE        E DS
005900050418     d dAR5gen       e ds                  inz
006000910205     D KPJBA         E DS
006100020208      *
006200171011     D*                DS
006300171011     D* DPDSTP                 1     14
006400171011     D* DPDS1                  1      3
006500171011     D* DPDV1                  4      4    INZ
006600171011     D* DPDS2                  5     12
006700171011     D* DPDV2                 13     13    INZ
006800171011     D* DPDS3                 14     14
006900020208      *
007000171011     D*                DS
007100171011     D* DPDPAR                 1     12
007200171011     D* DPD1                   1      3
007300171011     D* DPD2                   4     11
007400060522     D**DPD3                  12     12
007500020703
007600020703      *--------
007700020703      * Ds di riferimento al file esterno AzUte00F
007800020703      *--------
007900020703     d AzuteDs       e ds                  ExtName(AzUte00F)
008000020703      *--------
008100020703      * Ds per dati organigramma
008200020703      *--------
008300020703     d DDatiUte      e ds
008400020703      *--------
008500020703      * Parametri per richiamo al pgm di controllo profilo utenti
008600020703      *--------
008700020703     d Tibs34Ds      e ds                  Inz
008800020703      *--------
008900020703      * Parametri per richiamo al pgm trul06r
009000020703      *--------
009100020703     d Trul06Ds      e ds
009200020703     d  lin                    1     90  0 dim(30)
009300020208
009400020208      *--------------------------------------------------------------*
009500020208      *       C A M P I
009600020208      *--------------------------------------------------------------*
009700020208     D COD             S                   like(TBLCOD)
009800020208     D key             S                   like(TBLKEY)
009900020208     D FST2            S                   like(BLDFST)
010000020208     D LNANTW          S                   like(§ogntw)
010100020208     D LNPNTW          S                   like(§ogntw)
010200171011     D WTRC            S                   like(arsTRC)
010300020208     D comscd          S              8  0
010400020208     D KSC             S              7  0
010500020208     D cpr             S              7  0
010600020208     D wpos            S              2  0
010700020208     D x               S              2  0
010800020208     D §z              S              2  0
010900020208     D bb              S              2  0
011000020208     D fst             S              1
011100020912     D savfst          S              1
011200020208     D wsta            S              1
011300020208     D w015A           S             15
011400171011     D*w005A           S              5
011500020208     D flgtb1          S              1
011600020703     d xx              s              2  0 inz
011700020703     d klnp            s                   like(bldlnp)
011800020208
011900020208      ****************************************************************
012000020208      *  RIEPILOGO INDICATORI
012100020208      ***************************************************************
012200020208      * 04    - RICHIESTA PER PADR. IN ORDINE DI MITTENTE/DESTINATARIO
012300020208      * 05    - RICHIESTA RISTAMPA
012400020208      * 07    - RICHIESTA PER PADR. IN ORDINE DI DESTINATARIO/LNP
012500020208      * 08    - RICHIESTA PER MITT. IN ORDINE DI CODICE PRODOTTO/TFA
012600020208      * 09    - RICHIESTA PER MITT. IN ORDINE DI TER.ARR/LNA
012700020208      * 30    - COMODO
012800020208      * 31    - FINE LETTURA FILE
012900020208
013000020208      *****************************************************************
013100000000     C     *ENTRY        PLIST
013200000000     C                   PARM                    KPJBA
013300020703     C                   MOVEL     KPJBU         PARAM
013400020703
013500020703      * reperisco i dati utente
013600020703     c     *dtaara       define    §azute        AzuteDs
013700020703     c     *dtaara       define    §datiute      DDatiUte
013800020703
013900020703     c                   in(E)     *dtaara
014000020703     c                   IF        %Error or RSUT = *blanks
014100020703     c                   call      'TIBS34R'
014200020703     c                   parm                    Tibs34Ds
014300020703     c                   in        *dtaara
014400020703     c                   ENDIF
014500020703
014600020208      *---------------------------------------------------------------*
014700910205     C     KBLD          KLIST
014800910205     C                   KFLD                    BLDAAS
014900910205     C                   KFLD                    BLDLNP
015000910205     C                   KFLD                    BLDNRS
015100910205     C                   KFLD                    BLDNSP
015200910219     C                   KFLD                    BLDCDP
015300171011     C*    Kar4          KLIST
015400171011     C*                  KFLD                    BLDAAS
015500171011     C*                  KFLD                    BLDLNP
015600171011     C*                  KFLD                    BLDNRS
015700171011     C*                  KFLD                    BLDNSP
015800171011     C*                  KFLD                    WTRC
015900910207     C     KBLD1         KLIST
016000020703     c                   kfld                    klnp
016100910207     C                   KFLD                    KSC
016200020208     C                   KFLD                    FST
016300910207     C     KBLD2         KLIST
016400910207     C                   KFLD                    CPR
016500020208     C                   KFLD                    FST
016600971216     C     KTAB2         KLIST
016700971217     C                   KFLD                    CODUT             1 0
016800020208     C                   KFLD                    COD
016900020208     C                   KFLD                    KEY
017000020208      *
017100051012     C     KARS01        KLIST
017200980324     C                   KFLD                    BLPFLS
017300980324     C                   KFLD                    BLDLNA
017400980324     C                   KFLD                    BLDNRS
017500980324     C                   KFLD                    §LDNCD
017600980324     C                   KFLD                    WTRC
017700050418      * FIAR501L
017800050418     c     k05AR501      klist
017900050418     c                   kfld                    BLDaas
018000050418     c                   kfld                    BLDlnp
018100050418     c                   kfld                    BLDnrs
018200050418     c                   kfld                    BLDnsp
018300050418     c                   kfld                    AR5trd
018400050418     c                   eval      AR5trd = 'GEN'
018500020208      *
018600971217     C                   Z-ADD     1             CODUT
018700020208      *
018800910205     C*---------------------------------------------------------------*
018900971217     C** SIGNIFICATI DEL CAMPO BLDFST:
019000971217     C*  ' ' -->   STAMPARE SIA  SEGNACOLLI ITA CHE EST
019100971217     C*  'E' -->   STAMPARE SOLO SEGNACOLLO         EST
019200971217     C*  'S' --> RISTAMPATE SIA  SEGNACOLLI ITA CHE EST
019300971217     C*  'Z' --> RISTAMPARE SOLO SEGNACOLLO         EST
019400971216     C**
019500971216     C                   CLEAR                   DSBLD
019600960320     C**
019700050418     C**  IMMESSO  C O D I C E   C L I E N T E   M I T T E N T E
019800910205     C     VIDKSC        IFGT      *ZEROS
019900020208     C                   MOVEL     VIDKSC        KSC
020000960320     C* 08 ON - ORDINATO PER CODICE PRODOTTO
020100960320     C     VIDOR2        IFEQ      'P'
020200020208     C                   eval      *in08 = *on
020300960320     C                   ENDIF
020400960320     C* 09 ON - ORDINATO PER TERMINAL ARRIVO
020500960320     C     VIDOR2        IFEQ      'T'
020600020208     C                   eval      *in09 = *on
020700960320     C                   ENDIF
020800960320     C*
020900910205     C                   END
021000960320     C**
021100960320     C** IMMESSO  C O D I C E   P A D R O N C I N O
021200910205     C     VIDCPR        IFGT      *ZEROS
021300020208     C                   MOVEL     VIDCPR        CPR
021400960320     C*
021500960320     C* 04 ON - ORDINATO PER MITTENTE
021600960320     C     VIDOR1        IFEQ      'M'
021700020208     C                   eval      *in04 = *on
021800951102     C* VEDO SE IMMESSA LA RAGIONE SOCIALE
021900951102     C     VIDRSM        IFNE      *BLANKS
022000020208     C     ' '           CHECKR    VIDRSM        WPOS
022100960320     C                   ENDIF
022200960320     C                   ENDIF
022300960320     C* 07 ON - ORDINATO PER DESTINATARIO
022400960320     C     VIDOR1        IFEQ      'D'
022500020208     C                   eval      *in07 = *on
022600960320     C                   ENDIF
022700960320     C*
022800960320     C                   ENDIF
022900910412     C*
023000020208      * VEDO SE RICHIESTA  LA RISTAMPA
023100910412     C     VIDRSN        IFEQ      'S'
023200020208     C                   eval      *in05 = *on
023300050418     ***C                   MOVEL     'R'           §LDRST
023400910412     C                   MOVEL     'S'           FST
023500971217     C                   MOVEL     'Z'           FST2
023600910412     C                   ELSE
023700050418     ***C                   MOVEL     *BLANKS       §LDRST
023800971217     C                   MOVEL     ' '           FST
023900971217     C                   MOVEL     'E'           FST2
024000050418i    C                   END
024100020703
024200020703      * richiesta stampa per mittente
024300020703     c                   if        *in08 or *in09
024400020703      * se p.o. di secondo livello imposto nella sk delle lnp se
024500020703      * stesso
024600020703     c                   clear                   lin
024700020703     c                   if        dutlpo = '2' or dutlpo = *blanks
024800020703     c                   eval      lin(1) = dutpou
024900020703      * se p.o. di primo livello carico tutti i p.o. del terminal di partenza
025000020703     c                   else
025100020703     c                   clear                   Trul06Ds
025200020703     c                   eval      D06Cod = '£1'
025300020703     c                   eval      D06Tla = 'L'
025400020703     c                   movel     Simfel        D06Key
025500020703     c                   movel(p)  Trul06Ds      Kpjbu
025600020703     c                   call      'TRUL06R'
025700020703     c                   parm                    Kpjba
025800020703     c                   movel     Kpjbu         Trul06Ds
025900020703     c                   endif
026000020703     c                   endif
026100020912
026200020912      * Salvo il flag fst
026300020912     c                   Eval      savfst = fst
026400020703
026500020703     c     leggi         tag
026600020703     c                   if        *in08 or *in09
026700020703      * imposto la prima linea di partenza
026800020703     c                   add       1             xx
026900020703     c                   if        lin(xx) = *zeros
027000020703     c                   goto      fine
027100020703     c                   endif
027200020912      * reimposto il flag fst
027300020912     c                   Eval      fst = savfst
027400020703     c                   eval      klnp = lin(xx)
027500020703     c                   endif
027600020703
027700020208      **
027800020208      * FACCIO SEMPRE 2 GIRI:
027900020208      *  PRIMA I COLLI ITA/EST
028000020208      *  POI  SOLO I COLLI EST
028100971217    1C                   DO        2
028200020208     C* PADRONCINO
028300960320     C   07KBLD2         SETLL     FNBLD02J
028400960320     C   04KBLD2         SETLL     FNBLD03J
028500960320     C* MITTENTE
028600020703     C   08KBLD1         SETLL     FNBLD06J
028700020703     C   09KBLD1         SETLL     FNBLD07J
028800960320     C*
028900960320     C* PADRONCINO
029000960320     C   07KBLD2         READE     FNBLD02J                               31
029100960320     C   04KBLD2         READE     FNBLD03J                               31
029200960320     C* MITTENTE
029300020703     C   08KBLD1         READE     FNBLD06J                               31
029400020703     C   09KBLD1         READE     FNBLD07J                               31
029500910205     C*
029600971217    2C     *IN31         DOWEQ     *OFF
029700951102     C*
029800951102     C* S T A M P A
029900971217    3C     *IN05         IFEQ      *OFF
030000951102     C*
030100980204AG*  C* NEL CASO IN CUI SIA VALORIZZATA LA DATA DI SPEDIZIONE ANCHE SE NON
030200980204AG*  C* SONO IN RISTAMPA CONTROLLO CHE LA DATA SPEDIZIONE SEGNACOLLI
030300980204AG*  C* LETTI SIA UGUALE A QUELLA RISCHIESTA A VIDEO
030400980204AG*  C     VIDDSP        IFNE      *ZEROS
030500980204AG*  C     BLPDSP        ANDEQ     VIDDSP
030600980204AG*  C     VIDDSP        OREQ      *ZEROS
030700980204AG*  C*
030800020208     C                   MOVEL     'S'           WSTA
030900951102     C* PER MITTENTE CON RAGIONE SOCIALE CONTROLLO LA PARZIALIZZAZ.
031000971217    4C   04VIDRSM        IFNE      *BLANKS
031100960320     C                   EXSR      CTRPAR
031200971217    4C                   ENDIF
031300951102     C*
031400971217    4C     WSTA          IFEQ      'S'
031500910205     C                   EXSR      STASCD
031600910412     C* AGGIORNO FLAG
031700941228     C     KBLD          CHAIN     FNBLD000                           30
031800971217     C**
031900971217     C  N30BLDFST        IFEQ      ' '
032000971217     C                   MOVEL     'S'           BLDFST
032100971217     C                   ELSE
032200971217     C                   MOVEL     'Z'           BLDFST
032300971217     C                   ENDIF
032400910412     C  N30              EXCEPT    UPBLD
032500971219     C**
032600000201     C**
032700971219    4C                   ENDIF
032800980204AG*  C*
032900980204AG*  C                   ENDIF
033000980204     C*
033100951102     C* R I S T A M P A
033200971217   X3C                   ELSE
033300971217    4C     BLPDSP        IFEQ      VIDDSP
033400951102     C*
033500020208     C                   MOVEL     'S'           WSTA
033600951102     C* PER MITTENTE CON RAGIONE SOCIALE CONTROLLO LA PARZIALIZZAZ.
033700971217    5C   04VIDRSM        IFNE      *BLANKS
033800960320     C                   EXSR      CTRPAR
033900971217    5C                   ENDIF
034000951102     C*
034100971217    5C     WSTA          IFEQ      'S'
034200910412     C                   EXSR      STASCD
034300971217    5C                   ENDIF
034400971217    4C                   ENDIF
034500971217    3C                   ENDIF
034600951102     C**
034700960320     C* PADRONCINO
034800960320     C   07KBLD2         READE     FNBLD02J                               31
034900960320     C   04KBLD2         READE     FNBLD03J                               31
035000960320     C* MITTENTE
035100020703     C   08KBLD1         READE     FNBLD06J                               31
035200020703     C   09KBLD1         READE     FNBLD07J                               31
035300971217    2C                   ENDDO
035400971217     C                   MOVEL     FST2          FST
035500971217    1C                   ENDDO
035600020703
035700020703      * se richiesta stampa per mittente devo tornare a leggere la linea
035800020703      * di partenza sucessiva
035900020703     c                   if        *in08 or *in09
036000020703     c                   goto      leggi
036100020703     c                   endif
036200910412     C**
036300000000     C     FINE          TAG
036400020208     C                   eval       *inLR = *on
036500951102     C*
036600951102     C*--- CONTROLLO PARZIALIZZAZIONE PER RAGIONE SOCIALE -----------*
036700951102     C     CTRPAR        BEGSR
036800951122     C     WPOS          IFLT      15
036900020208     C     WPOS          ADD       1             X
037000951102     C                   MOVEA     BLPRSM        K15
037100951102     C                   MOVEA     *BLANKS       K15(X)
037200020208     C                   MOVEA     K15           W015A
037300951122     C                   ELSE
037400951122     C                   MOVEL     BLPRSM        W015A
037500951122     C                   ENDIF
037600951102     C*
037700951102     C     W015A         IFNE      VIDRSM
037800951102     C                   CLEAR                   WSTA
037900951102     C                   ENDIF
038000951102     C                   ENDSR
038100910205     C*
038200910205     C*--- STAMPA BOLLA ---------------------------------------------*
038300910205     C     STASCD        BEGSR
038400000201     C                   CLEAR                   DSBLDE
038500020211     C* RICHIAMO DIRETTAMENTE IL PROGRAMMA DI STAMPA SEGNACOLLO
038600020208     C                   Z-ADD     BLDNCD        COMSCD
038700910205     C     BLDNCA        IFEQ      0
038800910205     C                   Z-ADD     BLDNCD        BLDNCA
038900910205     C                   END
039000050418      *
039100050418      * Verifica se spedizione "di valore"
039200050418     c                   exsr      CtrSpeVal
039300050418      *
039400050418      * impostazione flag ristampa/di_valore
039500050418     c                   clear                   §LDrst
039600050418sel 1c                   select
039700050418      * - ristampa collo appartenente a spedizione "di valore"
039800050418w   1c                   when          VIDrsn  =  'S'
039900050418     c                             and §AR5bva =  'V'
040000050418     c                   movel     'X'           §LDrst
040100050418      * - ristampa collo appartenente a spedizione NON "di valore"
040200050418w   1c                   when          VIDrsn  =  'S'
040300050418     c                             and §AR5bva =  *blanks
040400050418     c                   movel     'R'           §LDrst
040500050418      * - stampa   collo appartenente a spedizione "di valore"
040600050418w   1c                   when      §AR5bva =  'V'
040700050418     c                   movel     'V'           §LDrst
040800050418      *** stampa   collo appartenente a spedizione NON "di valore"
040900050418w   1c***                when      §AR5bva =  *blanks
041000050418     c***                movel     *blanks       §LDrst
041100050418e   1c                   endsl
041200020211      *
041300910219     C                   Z-ADD     BLDNC1        §LDNC1
041400910205     C                   MOVEL     BLPFLS        §LDLNP
041500020208     C     BLPFLS        CHAIN     AZORG
041600020208     c                   if        %found(AZORG01L)
041700020208     C                   MOVEL     ORGDES        §LDDLP
041800020208     c                   else
041900020208     C                   MOVEL     *BLANKS       §LDDLP
042000020208     c                   endif
042100910205     C                   MOVEL     BLDCDP        §LDCDP
042200910205     C                   MOVEL     BLDNRS        §LDNRS
042300910205     C                   MOVEL     BLDLNA        §LDLNA
042400020208      *
042500020208     C     BLDLNA        CHAIN     AZORG
042600020208     C                   clear                   §LDDLA
042700020208     c                   if        %found(AZORG01L)
042800020208     C     ORGDE5        IFNE      *BLANKS
042900910426     C                   MOVEL     ORGDE5        §LDDLA
043000910426     C                   ELSE
043100910426     C                   MOVEL     ORGDES        §LDDLA
043200910426     C                   END
043300991206     C                   MOVEL     ORGDE3        OG143
043400040525     C                   MOVEL     ORGDF0        OG150
043500020208     C                   MOVEL     §ogntw        LNAntw
043600171011     c**                 movel     orgtel        dpdtel           15
043700171011     c**                 movel     orgfax        dpdfax           15
043800020208     c                   endif
043900020208      **
044000020208     C     BLDLNP        CHAIN     AZORG
044100020208     C                   MOVEL     ORGDE3        OG143
044200020208     C                   MOVEL     §ogntw        LNPntw
044300971224     C**
044400910205     C                   MOVEL     BLDTFA        §LDTNA
044500910205     C                   MOVEL     BLPTSP        §LDTSP
044600910205     C                   MOVEL     BLPZNC        §LDZSC
044700910219     C                   MOVE      BLPNCL        §LDNC2
044800910205     C                   MOVEL     BLPRSM        §LDRSM
044900910205     C                   MOVEL     BLPPRM        §LDPRM
045000910205     C                   MOVEL     BLPRSD        §LDRSD
045100910205     C                   MOVEL     BLPCAD        §LDCAD
045200910205     C                   MOVEL     BLPLOD        §LDLOD
045300910404     C                   MOVEL     BLPIND        §LDIND
045400910205     C                   MOVEL     BLPPRD        §LDPRD
045500941228     C                   MOVE      BLDPDR        §LDCPD
045600910205     C                   MOVEL     *ZEROS        §LDNCP
045700910205     C                   MOVEL     *ZEROS        §LDNCU
045800941228     C                   Z-ADD     BLPPKF        §LDPKG
045900941228     C                   MOVEL     BLPFVF        §LDFVR
046000941228     C                   Z-ADD     BLPVLF        §LDVOL
046100950116     C                   Z-ADD     BLDAAS        §LDAAS
046200950116     C                   Z-ADD     BLPMGS        §LDMGS
046300171020     c* etichetta ITAlia
046400000201      * SE LNA DPD
046500991206      * IMPOSTO NUMERO PARCEL DPD IN RIFERIMENTI SEGNACOLLO
046600171020      *
046700171020      * per import è impostato dalla immisisone bolle il parcel
046800171020      * per export invece c'è RMA e quindi, solo per dpd, devo
046900171020      * impostare il parcel DPD
047000171020      *
047100020211    1C                   if        LNANTW = 'DPD'
047200171011     C**                 MOVEL     'I'           WTRC
047300171011     C**   Kar4          CHAIN     Fiar401L
047400171011     c**                 if        %found(fiar401L)
047500171011     C**                 MOVEL     ar4NOT        DSBL4I
047600171011     c**                 else
047700171011     C**                 CLEAR                   DSBL4I
047800171011     c**                 endif
047900171011    2C**   §B4ipn        IFNE      *BLANKS
048000171011        chain (bldaas:bldlnp:bldnrs:bldnsp) fipnd01l ;
048100171011        if %found(fipnd01l)     ;
048200171011            eval §ldcdp=pndipn   ;
048300171011        endif;
048400010302     C* MUOVO ALLINEATO, A SX DELLA RSM, IL RMA SE <> DA BLANKS
048500010302     C* E LO COMPATTO PERDENDO CIOÈ I BLANK NON SIGNIFICATIVI
048600171012     C**   BLDCDP        IFNE      *BLANKS
048700171012     C**                 CLEAR                   W35
048800171012     C**                 MOVEA     BLDCDP        W35
048900171012     C**   ' '           CHECKR    BLDCDP        §Z
049000171012     C**                 ADD       2             §Z
049100171012     C**                 MOVEA     BLPRSM        W35(§Z)
049200171012     C**                 MOVEA     W35           §LDRSM
049300171012     C**                 ENDIF
049400171011     C**                 MOVEL     §B4ipn        DPDPAR
049500171011     C**                 MOVEL     DPD1          DPDS1
049600171011     C**                 MOVEL     DPD2          DPDS2
049700171011     C**                 MOVEL     §b4icd        DPDS3
049800171011     C**                 MOVEL     DPDSTP        §LDCDP
049900000201     C*
050000171011     C*    BLPPKF        IFLE      3
050100171011     C*                  MOVEL     'X'           §DESSE
050200171011     C*                  ENDIF
050300060523     c*
050400171011     C**                 MOVEL     'K'           WTRC
050500171011     C**   Kar4          CHAIN     Fiar401L
050600171011     c**                 if        %found(fiar401L)
050700171011     C**                 MOVEL     ar4NOT        DSBL4K
050800171011     c**                 else
050900171011     C**                 CLEAR                   DSBL4K
051000171011     c**                 endif
051100000201     C*
051200171011     C**                 MOVEL     blpnzd        §DESIA
051300171011     C**                 MOVEL     §B4krt        §DEAWB
051400171011     C**                 MOVEL     §B4ipn        §DEEUR
051500171011     c**                 eval      %subst(§deeur:12:1) = §b4icd
051600171011     C**                 MOVEL     DPDTEL        §DEXX2
051700171011     C**                 MOVE      DPDFAX        §DEXX2
051800171011     c**                 eval      w005a = %subst(§b4ida:2:3) + §b4iza
051900171011     C**                 MOVE      W005A         §DEXX1
052000171011    2C**                 ENDIF
052100991206    1C                   ENDIF
052200020211      *
052300020208      * SE ARRIVO EUROEXPRESS
052400020208      * IMPOSTO EUR PER EUROEXPRESS
052500040506      * 06/05/05: Imposto "EEX" per euroexpress
052600020211     C     LNANTW        IFEQ      'EEX'
052700040506     C                   MOVEL     'EEX'         §DETSE
052800020211     C                   ENDIF
052900020208      *
053000020208      **
053100020211      **
053200020208    2C                   IF        FST = *blank  OR  FST = 'S'
053300971216     C                   MOVEL     'ITA'         §DETST
053400040506     c     lnantw        ifeq      'EEX'
053500040506     c                   exsr      etiest
053600040506     c                   endif
053700020208   x2C                   ELSE
053800971217     C                   MOVEL     '   '         §DETST
053900020208    2C                   ENDIF
054000020208      **
054100020211      *
054200971217     C     §DETST        IFNE      *BLANKS
054300910208     C     COMSCD        DOWLE     BLDNCA
054400990318     C     COMSCD        ANDGT     0
054500911122     C                   Z-ADD     COMSCD        §LDNCD
054600020211      *
054700020211      * Particolarità:
054800020211      *    EUROEXPRESS: imposta §DEEUR e §LDCDP
054900020211      *          FEDEX: imposta §LDCDP
055000020211     C                   SELECT
055100020211     c                   When      LNAntw = 'FED'
055200020211     c                   exsr      Partic_Fed
055300040506     c                   When      LNPntw = 'EEX'
055400020211     c                   exsr      Partic_Eex
055500020211     c                   ENDSL
055600910205     C*
055700941228     C                   CALL      PSS
055800910205     C                   PARM                    DSBLD
055900971217     C                   PARM                    DSBLDE
056000910205     C*
056100910205     C                   ADD       1             COMSCD
056200910219     C                   ADD       1             §LDNC1
056300910205     C*
056400971217     C                   ENDDO
056500971217     C                   ENDIF
056600020211      *
056700910205     C                   ENDSR
056800971216     C* STAMPA ETICHETTA ESTERA SDI O BARTOLINI ----------------------*
056900971216     C     ETIEST        BEGSR
057000971216     C*
057100081007    1C     §detse        ifeq      'EEX'
057200020211      * CHAIN SULLE NAZIONI
057300971217     C                   MOVEL     '15'          COD
057400971217     C                   MOVEL(P)  BLPNZD        KEY
057500020211     C     KTAB2         CHAIN     TABEL
057600020215     c                   IF        %FOUND(tabel00f) and TBLFLG =  *blanks
057700020211     C                   MOVEL     TBLUNI        DS15
057800020211     C                   MOVEL     §15COD        §DEISA
057900020211     c                   ELSE
058000020211     C                   CLEAR                   DS15
058100020211     c                   ENDIF
058200020211      *
058300020211     C**  DPD - EUROEXPRESS
058400971216     C**
058500971216     C                   MOVEL     '3A'          COD
058600971216     C                   MOVEL(P)  BLPCBO        KEY
058700020211     C     KTAB2         CHAIN     TABEL
058800971216     C                   MOVEL     TBLUNI        DS3A
058900020208     C                   MOVEL     §3ATB1        FLGTB1
059000971216     C**
059100971216    3C                   SELECT
059200971216     C     FLGTB1        WHENEQ    'A'
059300971216     C     §3AFCA        ANDEQ     0
059400971216     C                   MOVEL     'A'           §DEFCA
059500971216     C     FLGTB1        WHENEQ    'A'
059600971216     C     §3AFCA        ANDNE     0
059700971216     C                   MOVEL     'X'           §DEFCA
059800971216     C     §3AFCA        WHENNE    0
059900971216     C                   MOVEL     'S'           §DEFCA
060000971216    3C                   ENDSL
060100971216    1C                   ENDIF
060200971216     C**
060300971216     C                   ENDSR
060400020211      *---------------------------------------------------------------*
060500020211      *--- PARICOLARTITA' FEDEX -------------------------------------*
060600020211      *---------------------------------------------------------------*
060700020211     C     Partic_Fed    BEGSR
060800020211      **
060900020211      * IMPOSTO AWB NEL CODICE PRODOTTO
061000020211     C                   MOVEL     'F'           WTRC
061100051012     C***  KBLS01        CHAIN     FNBLS01L
061200051012     C***                if        %found(FNBLS01L) and BLSnot <> *blanks
061300051012     C***                MOVEL     BLSnot        §LDCDP
061400051012    2C***                endif
061500051012     C     KARS01        CHAIN     FIARS01L
061600051012     C                   if        %found(FIARS01L) and ARSnot <> *blanks
061700051012     C                   MOVEL     ARSnot        §LDCDP
061800051012    2C                   endif
061900020211      **
062000020211     C                   ENDSR
062100020211      *---------------------------------------------------------------*
062200020211      *--- PARICOLARTITA' EUROEXPRESS  ------------------------------*
062300020211      *---------------------------------------------------------------*
062400020211     C     Partic_Eex    BEGSR
062500020208      **
062600991206     C                   MOVEL     'E'           WTRC
062700051012     C     KarS01        CHAIN     FIarS01L
062800051012    2C                   IF        %FOUND(FIarS01L)
062900020211      * PASSO AL PGM DI STAMPA ETICHETTE IL N. SEGNACOLLO EUROEXPRESS
063000051012     C                   MOVEL     arSNOT        §DEEUR
063100020211      * SE IMPORT METTO NEL CODICE PRODOTTO DELL'ETICHETTA
063200020211      * ITALIA IL N.SEGNACOLLO EUROEXPRESS
063300020208    3C     LNPntw        IFEQ      'EEX'
063400051012     C                   MOVEA     arSNOT        W35
063500020208     C                   Z-ADD     1             BB
063600020208     C     ' '           LOOKUP    W35(BB)                                30
063700980330     C   30              MOVEA     *BLANKS       W35(BB)
063800980330     C                   MOVEA     W35(6)        §LDCDP
063900980324    3C                   ENDIF
064000980324    2C                   ENDIF
064100020208      *
064200980324     C                   ENDSR
064300050418      *---------------------------------------------------------------*
064400050418      *  Verifica se spedizione "DI VALORE"                           *
064500050418      *---------------------------------------------------------------*
064600050418     c     CtrSpeVal     BEGSR
064700050418      *
064800050418if  1c                   if        NOT %open(FIAR501L)
064900050418     c                   open      FIAR501L
065000050418e   1c                   endif
065100050418      *
065200050418      * Verifica se spedizione in esame = ultima "reperita" su FIAR5
065300050418if  1c                   if            BLDaas = AR5aas
065400050418     c                             and BLDlnp = AR5lnp
065500050418     c                             and BLDnrs = AR5nrs
065600050418     c                             and BLDnsp = AR5nsp
065700050418     c                   leavesr
065800050418e   1c                   endif
065900050418      *
066000050418      * Reperisce i dati da FIAR5
066100050418     c                   clear                   dAR5gen
066200050418     c     K05AR501      setgt     FIAR5000
066300050418do  1c                   dou            %eof(FIAR501L)
066400050418     c                             or   AR5atb =  *blanks
066500050418     c     K05AR501      readpe    FIAR5000
066600050418e   1c                   enddo
066700050418if  1c                   if             NOT %eof(FIAR501L)
066800050418     c                             and  AR5atb =  *blanks
066900050418     c                   movel     AR5uni        dAR5gen
067000050418e   1c                   endif
067100050418      *
067200050418     c                   ENDSR
067300050418      *
067400020211      *-------------------------------------------------------------------
067500941228     OFNBLD000  E            UPBLD
067600910205     O                       BLDFST
067700941228     OFNLV22P   T    LRNLR
