000100160229     H DECEDIT('0,') DATEDIT(*DMY.) Option(*nodebugio)
000200941112     H* TNSF20R  *---------------------------------------------------*
000300941112     H*             - TASSAZIONE SINGOLA SPEDIZIONE
000400031121     H*--------------------------------------------------------------*
000401160314     H* PGM STORICIZZATO NELLA SRCOLD2016 IN DATA 14/03/16           *
000500031121     H* PGM STORICIZZATO NELLA SRCOLD2003 IN DATA 21/11/03           *
000600000000     H*--------------------------------------------------------------*
000700061219     H* IN TASFLO DELLA DS DTAS E' STATA AGGIUNTA UNA DATA PER LA    *
000800061219     H* RICERCA DEL PROGRESSIVO DELLA TARIFFA. QUESTA DATA VIENE     *
000900061219     H* PASSATA DAL PROGRAMMA DI RITASSAZIONE E SOLO DA LUI          *
001000990722     H*                                                              *
001100990722     H* ATTENZIONE QUANDO VERRANNO MODIFICATE LE LUNGHEZZE DEL CODICE*
001200990722     H* TARIFFE PARTICOLARI MODIFICARE LE LUNGHEZZE DELLE SCHIERE    *
001300990722     H* E DEI CAMPI CHE LI RICEVONO                                  *
001400990722     H*--------------------------------------------------------------*
001500980605     FTABEL00F  IF   E           K DISK    USROPN
001600040505     FTNTBE01L  IF   E           K DISK    USROPN
001700941107     FTNTAM00L  IF   E           K DISK
001800990721     FTITAD00L  IF   E           K DISK
001900990721     FTITPT01L  IF   E           K DISK
002000990721     FTITPD02L  IF   E           K DISK
002100060906     FTIPMG01L  IF   E           K DISK
002200140224     FTIDPB01L  IF   E           K DISK
002300020319     FAZORG01L  IF   E           K DISK
002400030205     D CTR             S              1    DIM(50)                              CODICI TARIFFA
002500020114     D CP              S              1    DIM(50)                              TARIFFE PARTICOLARI
002600941112     D TS              S              1    DIM(10)                              TIPI SERVIZIO
002700041220     D CTS             S              2    DIM(600)                             CODICI TASSAZIONE
002800990916     D SVW             S              1    DIM(20)                              SALVO SIGLE VARIE
002900990916     D VAW             S             11  3 DIM(20)                              SALVO IMPORTI VARIE
003000941116     D CV              S              3    DIM(50)                              CAMBI
003100980429     D TIC             S              2    DIM(20)                              TIPI INCASSO
003200031120     D* VARIE LEGATE ALLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
003300031120     D T3A             S              1    DIM(20)                              VARIE X BOLLE SING.V
003400980605     D* TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
003500980605     D TBN             S              2    DIM(20)                              TIPI BOLLA NO ASSIC
003600050504     D* P.O. ESTERI senza DPD
003700020319     D EST             S              3  0 DIM(300)                             P.O. ESTERI
003800050504     D* P.O. ESTERI tutti con DPD
003900050504     D ESTtot          S              3  0 DIM(300)                             P.O. ESTERI
004000000103     D* SIGLE VARIE ESENTI IVA
004100000103     D VES             S              1    DIM(20)                              SIGLE VARIE ESENTI D
004200020416     D* SIGLE VARIE DA RESTITUIRE CON IMPORTO =0
004300020416     D SV0             S              1    DIM(20)                              SIGLE VARIE IMP0 I D
004400020221     D* TARIFFE DI CARTELLO PER OGNI NETWORK
004500020221     D NTW             S              3    DIM(10)                              NETWORK
004600020221     D TAC             S              7  0 DIM(10)                              TARIFFA CARTELLO
004700020221     D CTC             S              3  0 DIM(10)                              CODICE TARIFFA CARTE
004800020221     D PRC             S              3  0 DIM(10)                              PROGRESSIVO TARIFFA
004900020222     D DIC             S              3    DIM(10)                              DIVISA      TARIFFA
004901160226     D TIP             S              2    DIM(10)                              Tipologia FEDEX
005000040506      * date decorrenza + supplemento carburante
005100040506     D DSCA            S             13  0 DIM(999) DESCEND                     DTA DEC. + % carbur
005200080617      * filiali abilitate addebito lasciato avviso  + date inizio tassazione
005300080617     D SFilLav         S              3  0 DIM(999)                             Filiale
005400080617     D SDtaLav         S              8  0 DIM(999)                             Data tassazione
005500111102      * filiali abilitate addebito centro storico   + date inizio tassazione
005600111102     D SFilZtl         S              3  0 DIM(999)                             Filiale
005700111102     D SDtaZtl         S              8  0 DIM(999)                             Data tassazione
005800040506
005900040505
006000980605     D* MESSAGGI DI ERRORE DEI MANCA TARIFFA
006100080220     D MSG             S             78    DIM(19) CTDATA PERRCD(1)             MSG DI ERRORE
006200040505
006300040505
006400941112     D WLBDAT          DS                  INZ
006500941112     D  G02DAT                 1      8  0
006600941112     D  G02INV                 9     16  0
006700941112     D  G02ERR                17     17
006800941115     D  G02TGI                18     22  0
006900941107     D ACDS            DS                  OCCURS(100)
007000941112     D  ASGL                   1     13  3
007100941112     D  AITR                  14     24  3
007200990921     D  AMIN                  25     35  3
007300990921     D  AMAX                  36     46  3
007400990921     D  AAIN                  47     47
007500990721     D** VALORI TITPT DELLE TARIFFE PARTICOLARI
007600020218     D TARDS           DS                  OCCURS(200)
007700941107     D  TLNP                   1      3  0
007800941113     D  TCTS                   4      5
007900990721     D  TNAZ                   6      8
008000990721     D  TCAP                   9     17
008100990721     D  TCTR                  18     20  0
008200990721     D  TSCG                  21     33  3
008300990721     D  TITR                  34     44  3
008400990721     D  TINO                  45     55  3
008500990721     D  TRPV                  56     58  1
008600990921     D  TMIN                  59     69  3
008700990921     D  TMAX                  70     80  3
008800040507      * DS SUPPLEMMENTO CARBURANTE
008900040507     D WDSPSC          DS
009000040507     D  DS_DSC                 1      8  0
009100040507     D  DS_PSC                 9     13  2
009200941111     D** CODICI DI TASSAZIONE
009300041220     D DSCT          E DS                  OCCURS(600) INZ
009400941112     D*  TARIFFE PARTICOLARI
009500020114     D DS1P          E DS                  OCCURS(50) INZ
009600000103     D** SIGLE VARIE
009700000103     D DS$2          E DS
009800000103     D** VARIA ESENTE
009900000103     D DSCC          E DS
010000031120     D** 3A TIPIO BOLLE
010100031120     D DS3A          E DS
010200941112     D** CAMBI
010300980504     D DSCV          E DS                  OCCURS(50)
010400980504     D** DS DEL CAMPO TAMFLO DI TNTAM
010500980504     D DSTA01        E DS
010600140224     D** DS DEL CAMPO TPTFLO DI TITPT
010700140224     D DTPT01        E DS
010800140224      *
010900020618     D OG147         E DS
011000980504     D** DS PASSATE DAI PGM RICHIAMANTI
011100990916     D DTAS          E DS
011200050928
011300060922     D DTAS01        E DS
011400060922
011500990916     D DTASV         E DS
011600941112     D  SV                     1     20
011700941112     D                                     DIM(20)                              SIGLE VARIE
011800990916     D  VA                    21    140P 3
011900990916     D                                     DIM(20)                              IMPORTI VARIE
012000990916     D  ES                   141    160
012100011127     D*
012200941110     D TAMDS         E DS                  EXTNAME(TNTAM00F) INZ
012300000000     D**  FILE TARIFFE  - MASTER
012400000000     D KPJBA         E DS
012500000125     D OG143         E DS                  INZ
012600000125     D DSQT1         E DS                  INZ
012700941107     D* DATI VARI TASSAZIONE 1
012800980429     D DS1A          E DS                  INZ
012900980429     D** TIPI INCASSO C/A
013000030205     D DSTR          E DS                  OCCURS(50) INZ
013100941107     D** TIPI TARIFFA
013200980603     D DSTB          E DS
013300980603     D** TIPI BOLLA
013400980429     D DS5E          E DS                  OCCURS(10) INZ
013500990917     D** MONETA DI CONTO/MONETA CORRENTE
013600990917     D DGED          E DS
013700990922     D** IMPORTI STANDARD NELLE MONETE DI CONTO
013800990922     D DGEI          E DS
013900040505     D** PERCENTUALE SUPPLEMENTO CARBURANTE
014000040505     D DPSC          E DS
014100080617     D** FILIALI ADDEBITO LASCIATO AVVISO
014200080617     D DLAV          E DS
014300111102     D** FILIALI ADDEBITO CENTRO STORICO
014400111102     D DZTL          E DS
014500990917     D** CALL PGM RICERCA TABELLA
014600990917     D DSBS02        E DS                  EXTNAME(TIBS02DS)
014700990917     D** CAMBIO DIVISA
014800990917     D YEUR          E DS                  EXTNAME(YEURCODS)
014900990922     D** CALL PGM CALCOLO TOTALE IMPONIBILE
015000990922     D DSLV16        E DS                  EXTNAME(FNLV16DS)
015100160226     D** CALL PGM PER IDENTIFICARE NETWORK DELLA SPEDIZIONE
015200160225     D TRUL27        E DS                  EXTNAME(TRUL27DS1)
015201160226     D** CALL PGM PER RECUPERO TARIFFA DI CARTELLO
015202160226     D TRULC7        E DS                  EXTNAME(TRULC7DS)
015300980526     D PARAMT          DS
015400980526     D  PARCA                256    256
015500101014
015600101014     d TrulISTds     e ds
015700040505
015800040505      * -------------- variabili ------------------------------------
015900040505
016000040507     D Keytbe          S              3                                         chiave TBE
016100080617     D indx            S              3  0                                      Indice schiera
016200040507     D KX              S              3  0                                      Indice schiera
016300040507     D IMP_SCA         S             15  3                                      Impon.sup.carb.
016400040507     D PER_SCA         S              5  2                                      Perc.sup.carb.
016500060414     d applicarevers   s              1
016501160216     D NtwTam          S              3                                         Network di TNTAM
016502160226     D TipTam          S              2                                         tipo tariffa Fedex
016503160606     D UsaVlf          S                   like(tasvlf)                         Volume tassazione
016600060906
016700060906     d Sca_pb          s              3  0                                      scag.prez.base gasol
016800060906     d Sca_spe         s              3  0                                      scag.prez.gasol sped
016900060906     d Sca_sca         s              3  0                                      scaglioni scattati
017000080909     d Pmg_sgl         s             13  3                                      prezzo medio gasolio
017100140224     d Variafuel_min   s             11  3                                      Fuel minimo
017200040505
017300061219     d Sav_dsp         s              8  0                                      Salv. dta spedizione
017400080220
017500080220     d Wrksv           s             20                                         salvataggio sigle va
017600090922     d WvariaMag       s              1                                         salvataggio sigle va
017700090924     d WTSP            s                   like(taditr)
017800090924     d IMPON           s                   like(taditr)
017900090924     d WimpoMag        s                   like(taditr)
018000990916     C****************************************************************
018100990916     C*  RIEPILOGO INDICATORI
018200990916     C***************************************************************
018300000125     C* 05    - DI COMODO
018400990916     C* 07    - LETTURA GENERICO
018500990916     C* 08    - OFF MANCA TARIFFA CLIENTE
018600990916     C* 09    - LETTURA TITAD
018700000103     C* 10    - LOKUP
018800990923     C* 12    - MONETA CHE ACCETTA IMPORTI CON DECIMALI
018900990916     C* 15    - GENERICO
019000990916     C* 17    - TARIFFA SCADUTA
019100990916     C* 18    - TARIFFA NON TROVATA
019200990916     C* 28    - LETTURA TARIFFE PARTICOLARI
019300990916     C* 49    - TROVATA TARIFFA DEL CAP DI ARRIVO
019400001009     C* 56    - TROVATA TARIFFA  (TARIFFA REVERSIBILE)
019500001009     C* 57    - ASSEGNATO      (NON + IN USO *!!*)
019600020225     C* 90    - MANCA TARIFFA DI CARTELLO
019700990916     C* 94    - MANCA TARIFFA
019800990916     C* 96    - SCHIERE VARIE
019900990916     C*****************************************************************
020000000000     C     *ENTRY        PLIST
020100941111     C                   PARM                    KPJBA
020200990922     C                   PARM                    DTAS
020300990922     C                   PARM                    DTASV
020400060922
020500060922     C                   MOVEL     TASFLO        DTAS01
020600110624      * verifico data spedizione con data attivazione varia email avviso destinatario
020700130920     c***                if        §asemd = 'S' and tasdsp < 20110901
020800130920     c***                clear                   §asemd
020900130920     c***                endif
021000110624
021100150922      * pulisco i campi della DTAS che sono di solo output
021200150922     c                   clear                   tastap
021300150922     c                   clear                   tasepd
021301151118     c                   clear                   tasprg
021400060922
021500920225     C                   Z-ADD     1             CODUT             1 0
021600000000     C*---------------------------------------------------------------*
021700020226     C*
021800020226     C** ACCESSO TITPT01L
021900941107     C     KISOT         KLIST
022000941107     C                   KFLD                    TAMKSC
022100941107     C                   KFLD                    TAMCTR
022200941107     C                   KFLD                    TAMPRG
022300990722     C                   KFLD                    FISO              2
022400020226     C     KTPTC         KLIST
022500020226     C                   KFLD                    CARKSC
022600020226     C                   KFLD                    CARCTR
022700020226     C                   KFLD                    CARPRG
022800020226     C                   KFLD                    FISO
022900930419     C*
023000020222     C** ACCESSO TFTPD02L
023100941107     C     KISOD         KLIST
023200941107     C                   KFLD                    TPTKSC
023300941107     C                   KFLD                    TPTCTR
023400941107     C                   KFLD                    TPTPRG
023500941107     C                   KFLD                    TPTFTC
023600941107     C                   KFLD                    ISOCTS
023700020412     C** ACCESSO TFTPD02L
023800020412     C     KISOD2        KLIST
023900020412     C                   KFLD                    TPTKSC
024000020412     C                   KFLD                    TPTCTR
024100020412     C                   KFLD                    TPTPRG
024200020412     C                   KFLD                    TPTFTC
024300020222     C** ACCESSO TABEL X CODICE
024400941107     C     KTAB          KLIST
024500941107     C                   KFLD                    CODUT
024600941107     C                   KFLD                    COD               2
024700020222     C** ACCESSO TABEL X CODICE + CHIAVE
024800941107     C     KTABC         KLIST
024900941107     C                   KFLD                    CODUT
025000941107     C                   KFLD                    COD
025100941107     C                   KFLD                    KEY               8
025200020222     C** ACCESSO TNTAM  TARIFFA DI CARTELLO
025300020222     C     KTAMC         KLIST
025400020226     C                   KFLD                    CARKSC
025500020226     C                   KFLD                    CARCTR
025600020226     C                   KFLD                    CARPRG
025700020222     C** ACCESSO TNTAM TARIFFE SCADUTA
025800941107     C     KTAM2         KLIST
025900941107     C                   KFLD                    KSC2
026000941111     C                   KFLD                    CTR2
026100941107     C                   KFLD                    PRG2
026200020222     C** ACCESSO TNTAM X CLIENTE
026300941107     C     KTAM          KLIST
026400941107     C                   KFLD                    TMCLI
026500941107     C                   KFLD                    TMCTR
026600020222     C** ACCESO TITAD
026700941107     C     KTAD          KLIST
026800941107     C                   KFLD                    TDCLI
026900941107     C                   KFLD                    TDPRG
027000020618     C                   KFLD                    WLNP
027100941112     C                   KFLD                    COMCTS
027200990722     C                   KFLD                    COMNAZ
027300990722     C                   KFLD                    COMCAP
027400941107     C                   KFLD                    TDCTR
027500020222     C** ACCESSO TITAD PER TARIFFA REVERSIBILE
027600941112     C     KTDREV        KLIST
027700941112     C                   KFLD                    TDCLI
027800941112     C                   KFLD                    TDPRG
027900981027     C                   KFLD                    KFIL
028000941112     C                   KFLD                    COMCTS
028100990722     C                   KFLD                    COMNAZ
028200941112     C                   KFLD                    COMCAP
028300941112     C                   KFLD                    TDCTR
028400020826     C     *LIKE         DEFINE    TASPKF        USAPKF
028500020827     C     *LIKE         DEFINE    §qttpc        WTARA
028600020827     C     *LIKE         DEFINE    TASPKF        WNTARA
028700020826     C     *LIKE         DEFINE    KSCFIL        KFIL
028800020618     C     *LIKE         DEFINE    TASLNP        SAVLNP
028900020618     C     *LIKE         DEFINE    TASLNP        WLNP
029000020618     C     *LIKE         DEFINE    TASCTS        COMCTS
029100941107     C***********     -------------       ***********
029200930419     C                   MOVEL     ' '           RT
029300930419     C*
029400941125     C* TASTLA = ' ' ELABORO E CHIUDO PGM  CON RETRN
029500941125     C* TASTLA = 'L' ELABORO E CHIUDO PGM  CON LR
029600941125     C* TASTLA = 'C' NO ELAB.  CHIUDO PGM  CON LR PER CHIUSURA FILE
029700941116     C     TASTLA        IFEQ      ' '
029800941116     C     TASTLA        OREQ      'L'
029900941107     C*
030000941116     C     TASTLA        IFEQ      ' '
030100930419     C                   MOVEL     'R'           RT                1
030200930419     C                   END
030300980505     C* C A M P I   D I   O U T P U T
030400980505     C                   CLEAR                   TASERR
030500980505     C                   CLEAR                   TASMSG
030600980505     C                   CLEAR                   TASIAL
030700980526     C                   MOVEL     KPJBU         PARAMT
030800030902      * clearizzo indicatiori x manca tariffa e inoltro
030900030902     C                   SETOFF                                       9456
030901151119      * verifico data spedizione con data attivazione varia t Diritto di chiamata prenotazione
030902151119      * ritiro e packing list
030903151204     c                   if        Tasdsp < 20160101
030904151119     c                   clear                   TASPRT
030905151119     c                   clear                   TASSPL
030906151119     c                   Endif
030907151119
031000030211
031100030211     c                   Clear                   wFiso
031200990930     C** CONTROLLO LA DIVISA CHE MI VIENE PASSATA SE DIVERSA DA BLANK VERIFICO
031300990930     C** CHE SIA CORRETTA
031400990930     C     TASDIV        IFNE      *BLANKS
031500990930     C                   CLEAR                   DSLV16
031600990930     C                   MOVEL     'D'           D17FIM
031700990930     C                   MOVE      TASDIV        D17DIV
031800991012     C****                 MOVE TASDSP    D17DSP
031900990930     C                   CALL      'FNLV16R'
032000990930     C                   PARM                    DSLV16
032100990930     C                   PARM                    DTASV
032200990930     C* CONTROLLO IL CODICE DI RITORNO
032300990930     C     O17ERR        IFNE      *BLANKS
032400990930     C                   MOVEL     'E'           TASERR
032500990930     C                   MOVEL     O17MSG        TASMSG
032600990930     C                   SETON                                        94
032700990930     C                   GOTO      FINE
032800990930     C                   ENDIF
032900990930     C                   ENDIF
033000941125     C*
033100980605     C** CARICA TABELLE SOLO LA 1° VOLTA
033200941125     C     CARTBL        IFEQ      *BLANK
033300941125     C                   MOVE      '1'           CARTBL            1
033400941125     C                   EXSR      CARTAB
033500011127      * RECUPERO UDATE SOLO LA PRIMA VOLTA
033600011127     C                   EXSR      RUDATE
033700020320     C     TASMSG        IFNE      *BLANKS
033800020320     C                   SETON                                        94
033900020320     C                   GOTO      FINE
034000020320     C                   ENDIF
034100941125     C                   END
034200011127     C* R E C U P E R O   D A T A   D I   T A S S A Z I O N E
034300030131     C**!!!              EXSR      RECDAT
034400991012     C*
034500991012     C** SALVO GLI IMPORTI CHE MI VENGONO PASSATI
034600991012     C                   EXSR      SAVDS
034700941125     C*
034800941110     C                   SETOFF                                       94
034900030925      *
035000030925      * se richiamato per il solo calcolo della varia D  VADO A FINE
035100030925     C                   IF        TASSVA = 'D'
035200030925     C                   GOTO      FINE
035300030925     C                   ENDIF
035400030925      *+
035500941124     C                   MOVEL     TASTBL        TIPBOL            1
035600941124     C                   MOVEL     TASKSC        KSCFIL            3 0
035700941124     C                   MOVE      TASKSC        KSCCOD            4 0
035800080617      * da giugno 2008 viene riattivato per alcune filiali l'addebito del lasciato avviso
035900080617      * se tasric diverso da blank .. quindi la spedizione ha un evento di lasciato avviso
036000080617      * verifico se la filiale del cliente è presente nella tabella LAV oppure nella tabella
036100080617      * LAV esiste la filiale 999
036200080617    1c                   If        TasRic <> ' '
036300080617     c                   clear                   indx
036400080617     c                   eval      indx = %lookup(kscfil:sfillav)
036500080617      * se indice =  a zero cerco con la filiale 999
036600080617    2c                   if        indx = *zeros
036700080617     c                   eval      indx = %lookup(999:sfillav)
036800080617    2c                   endif
036900080617      * pulisco il flag del calcolo dell'addebito del lasciato avviso perchè filiale cliente
037000080617      * non abilitato all'addebito
037100080617    2c                   If        indx = *zeros
037200080617     c                   clear                   tasric
037300080617      * altrimenti verifico la data attivazione tassazione
037400080617   x2c                   else
037500080617      * se data spedizione inferiore all'attivazione pulisco il flag
037600080618    3c                   if        tasdsp < sdtalav(indx)
037700080617     c                   clear                   tasric
037800080617    3c                   endif
037900080617    2c                   endif
038000080617    1c                   endif
038100080617     c
038200980429     C* VEDO SE TIPO INCASSO E' INTESTATO AL MITTENTE
038300980430     C                   CLEAR                   TASBM
038400061016      * se il primo carattere del campo TASTIC è uguale a blank
038500061016      * ci troviamo a tassare una spedizione in sede in quanto
038600061016      * in TNCSB (file contrassegni ) la combinazione di tpa (tipo assegno)
038700061018      * e tpi (tipo intestazione) o fus (tipo intestazione in bolla)
038800061018      * potrebbe non esistere in tabella  1A tipo incasso c/assegno
038900061016      * a questo punto prendo solo il tipo intestazione assegno che
039000061016      * è uguale a  M per mittente  e "B" o " " per Vettore
039100061016      * In Tasbm passo solo se è uguale a M
039200061016
039300061016     C                   If        %subst(tastic:1:1) = ' '
039400061016     C                   If        %subst(tastic:2:1) = 'M'
039500061016     c                   eval      TASBM = 'M'
039600061016     c                   endif
039700061016
039800061016     c                   else
039900980429     C     TASTIC        LOOKUP    TIC                                    96
040000980429     C   96              MOVEL     'M'           TASBM             1
040100061016     c                   endif
040200980605     C** TASSAZIONE
040300941107     C                   EXSR      TARIFF
040400941110     C     FINE          TAG
040500980605     C** 94 ON - MANCA TARIFFA
040600990922     C     *IN94         IFEQ      '1'
040700990929     C* E NON C'E' TOTALE IMPONIBILE  DO ERRORE
040800990929     C     TASIMV        IFEQ      *ZEROS
040900990922     C                   MOVEL     'E'           TASERR            1
041000990929     C                   ENDIF
041100990922     C* REIMPOSTO LA DS COME PRIMA SENZA CONVERSIONI IN MONETA
041200990922     C*    MI SEMBRA INUTILE ADESSO EVENTUALMENTE LO FACCIO DOPO
041300990922     C*
041400990922     C                   ELSE
041500990929     C* CALCOLO IL TOTALE IMPONIBILE SE E' UGUALE A ZEROS
041600990929     C*
041700990929     C     TASIMV        IFEQ      *ZEROS
041800990923     C                   CLEAR                   DSLV16
041900990923     C                   MOVEL     'T'           D17FIM
042000990923     C                   Z-ADD     TASPOR        D17POR
042100990923     C                   CALL      'FNLV16R'
042200990923     C                   PARM                    DSLV16
042300990923     C                   PARM                    DTASV
042400001205     C*
042500001205     C* SE SPEDIZIONI CON DATA MAGGIORE O UGUALE AL 01012001 CALCOLO L'ADDIZIONALE DI GESTIONE
042600001211     C* SUL TOTALE IMPONIBILE E DOPO CALCOLO SUL TOTALE IMPONIBILE + ADDIZ. GESTIONE L'ISTAT
042700001205     C*
042800001221     C     TASDSP        IFGE      20010101
042900020416     C* ESEGUO SOLO SE NON HO RICHIAMATO LA TASSAZIONE PER UNA VARIA
043000031120     C*  SOLA SE VARIA NON PRESENTE NELLA TABELLA T3A (TABELLA DELLE VARIE DELLE BOLLE CHE
043100031121     C* ACCETTANO UNA SINGOLA  VARIA ESEMPIO "O" "Y" "*".....)
043200031120     C*    TASSVA        ANDEQ     ' '
043300031120      *
043400031120     C                   IF        TASSVA <> ' '
043500031120     C                   SETOFF                                       11
043600031120     C     TASSVA        LOOKUP    T3A(1)                                 11
043700031120     C                   ENDIF
043800040113      *
043900040113      *
044000040113      * SE VARIA UGUALE A BLANK                                             OPPURE
044100040113      * VARIA PRESENTE NELLA TABELLA T3A                                    OPPURE
044200040113      * TASSAZIONE 2° BOLLA     TASSVA = 'G' E TASCVAG = 'S'                OPPURE
044300040113      * VARIA UGUALE A "Z"                                                  OPPURE
044400040113      * CALCOLO A.G.+ ISTAT
044500040113      *
044600040113     C                   IF        TASSVA = ' ' OR
044700040113     C                             %FOUND       OR
044800040113     C                             (TASSVA = 'G' AND TASCVAG = 'S') OR
044900040113     C                             TASSVA = 'Z'
045000001205     C                   EXSR      ADGIST
045100001205     C                   ENDIF
045200031120      *
045300031120     C                   ENDIF
045400001205     C*
045500990923     C                   Z-ADD     O17IMV        TASIMV
045600001205     C*
045700990929     C                   ENDIF
045800030924      *
045900030924      * DIRITTO DI FATTURAZIONE lo calcolo solo alla fine della TASSAZIONE senza aggiunte dell'addi-
046000030924      *                         zionale di gestione
046100030924      *                         se totale imponibile maggiore di zero
046200030924     C                   IF        TASIMV > 0 AND
046300030924     C                             (TASSVA = §$2VRD OR TASSVDF= §$2VRD)
046400030924      *
046500030924     C                   EXSR      DIRFAT
046600030924      *
046700030924     C                   ENDIF
046800990922     C* MUOVO LA DIVISA DELLA TARIFFA
046900020416     C*  SE NON PASSATA LA VARIA O SE PASSATA E IMPORTO >0
047000020416     C*
047100020416     C     TASSVA        IFEQ      *BLANKS
047200020416     C     O17IMV        ORGT      0
047300990929     C                   MOVE      §TADIV        TASDIV
047400020416     C                   ENDIF
047500020416     C*
047600020416     C* SE SI TRATTA DI UNA VARIA DA PASSARE ANCHE A 0 CONTROLLO
047700020416     C     TASSVA        IFNE      *BLANKS
047800020416     C     O17IMV        ANDEQ     0
047900020416     C     TASSVA        LOOKUP    SV0                                    05
048000020416     C     *IN05         IFEQ      *ON
048100020416     C                   MOVEL     TASSVA        SV(1)
048200020416     C                   ENDIF
048300020416     C                   ENDIF
048400080221
048500080221      * VERIFICO SE IMPONIBILE A ZERO SENZA SIGLE DI VARIE PERCHÈ QUESTO TIPO DI CONTROLLO
048600080221      * VIENE ESEGUITO DAI PGM CHIAMANTI
048700080221     c                   clear                   wrksv
048800080221     c                   movea     sv            wrksv
048900080221     C                   IF        TASIMV = 0 AND wrkSV = *BLANKS
049000080221     C                   MOVEL     'E'           TASERR            1
049100080221     C                   MOVEL     MSG(19)       TASMSG
049200080221     C                   SETON                                        94
049300080221     C                   GOTO      FINE
049400080221     C                   ENDIF
049500990922     C*
049600990922     C                   ENDIF
049700011127     C* CONTROLLO LA DIVISA IN BASE ALLA DATA TASSAZIONE SE DA CONVERTIRE OPPURE NO
049800011127     C* SE STO TASSANDO CON DATA > DEL 2001 UNA BOLLA CON DATA < DEL 2002 E LA DIVISA NON E'
049900011127     C* UGUALE ALLA MONETA DI CONTO AZIENDALE CONVERTO TUTTI GLI IMPORTI ANCHE LA QTA DA
050000011127     C* FATTURARE NELLA DIVISA DI CONTO
050100011127     C*
050200020826     C**         DATTAS    IFGE 20020101
050300020826     C     TASDIV        IFNE      §GEDCN
050400011127     C     TASDSP        ANDLT     20020101
050500011127     C* CONVERTO
050600011127     C                   MOVE      TASDIV        DIVINP
050700011127     C                   MOVE      §GEDCN        DIVOUT
050800011127     C                   MOVE      'S'           CONQTA
050900011127     C                   MOVE      'S'           CONIAL
051000011127     C*
051100011127     C* RECUPERO LE CARATTERISTICHE DELLA MONETA DI CONTO
051200011127     C                   SETOFF                                       12
051300011127     C                   Z-ADD     1             K
051400011127     C     §GEDCN        LOOKUP    CV(K)                                  10
051500011127     C     *IN10         IFEQ      *ON
051600011127     C     K             OCCUR     DSCV
051700011127     C     §CVFDC        IFEQ      'S'
051800011127     C* DIVISA CHE ACCETTA DECIMALI
051900011127     C                   SETON                                        12
052000011127     C                   ENDIF
052100011127     C                   ENDIF
052200011127     C*
052300011127     C                   EXSR      CNVTAS
052400011127     C* VALORIZZO  LA DIVISA
052500011127     C                   MOVEL     §GEDCN        TASDIV
052600011127     C*
052700011127     C                   ENDIF
052800990922     C*
052900941107     C                   END
053000941107     C*
053100941107     C     RT            IFEQ      'R'
053200941107     C                   RETURN
053300941107     C                   ELSE
053400160226     C* CHIUDO PGM TRUL27R1
053500020319     C                   CLEAR                   TRUL27
053600020319     C                   MOVE      'C'           I27TLA
053700160225     C                   CALL      'TRUL27R1'
053800020319     C                   PARM                    TRUL27
053801160226     C* CHIUDO PGM TRULC7R
053802160226     C                   CLEAR                   TRULC7
053803160226     C                   MOVE      'C'           IC7TLA
053804160226     C                   CALL      'TRULC7R'
053805160226     C                   PARM                    TRULC7
053900941107     C                   SETON                                        LR
054000941107     C                   END
054100941107     C************       -----------       *************
054200941107     C**
054300941112     C******************************************************
054400941110     C**  CALCOLA TASSAZIONE BOLLA
054500941112     C******************************************************
054600941110     C     TARIFF        BEGSR
054700980605     C                   MOVEL     'I'           WBOEST
054800000125     C                   CLEAR                   WBODPD
054900020318     C                   CLEAR                   WBOFED
054901160315     C                   CLEAR                   tasfie
054902160218     C                   CLEAR                   TRUL27
054903160226     C                   CLEAR                   TRULC7
055000991014     C                   SETOFF                                       12
061600001201     C*
061700001201     C* VERIFICO SE CODICE APPARTIENE ALLA SDI
061800001201     C*
061900001201     C                   CLEAR                   KSDIT
062000001201     C     KSCFIL        CHAIN     AZORG01L                           07
062100001201     C     *IN07         IFEQ      *OFF
062200001201     C                   MOVEL     ORGDIT        KSDIT             3
062300001201     C                   ENDIF
062400980605     C**
062500980605     C**
062600020222     C* PER 8888 E 9999 --> CERCO LA TARIFFA DI CARTELLO RELATIVA AL TIPO DI SPEDIZIONE
062700980605    1C     KSCCOD        IFEQ      8888
062800980527     C     KSCCOD        OREQ      9999
062900160216      * CERCO LA TARIFFA DI CARTELLO in base al network della spedizione
062901160216     C** PER VERIFICARE SE SONO BOLLA DPD FEDEX O ESTERA CHIAMO TRUL27
062902160216     C**  SENZA LA DATA
062903160216     C                   CLEAR                   TRUL27
062904160216     C                   MOVEL     TASTSP        I27TSP
062905160216     C                   MOVEL     TASLNA        I27LNA
062906160216     C                   MOVEL     TASLNP        I27LNP
062907160216     C                   MOVEL     TASKSC        I27CLI
062908160216     c                   Movel     Tasctr        I27Ctb
062909160225     C                   CALL      'TRUL27R1'
062910160216     C                   PARM                    TRUL27
062911160216      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
062912160216     C     O27ERR        IFEQ      *BLANKS
062913160216     C     O27FIE        IFEQ      'E'
062914160216     C                   MOVEL     'E'           WBOEST            1
062915160216     C                   ENDIF
062916160216     C*
062917160216     C     O27FIE        IFEQ      'D'
062918160216     C                   MOVEL     'S'           WBODPD            1
062919160216     C                   MOVEL     'I'           WBOEST            1
062920160216     C                   ENDIF
062921160226     C*
062922160226     C     O27FIE        IFEQ      'X'
062923160226     C     O27NTW        ANDEQ     'DPD'
062924160226     C                   MOVEL     'S'           WBODPD
062925160226     C                   MOVEL     'I'           WBOEST
062926160226     C                   ENDIF
062927160226     C*
062928160226     C     O27FIE        IFEQ      'X'
062929160226     C     O27NTW        ANDEQ     'EEX'
062931160226     C                   MOVEL     'E'           WBOEST
062932160226     C                   ENDIF
062933160216     C*
062934160216     C     O27FIE        IFEQ      'F'
062935160216     C     O27FIE        orEQ      'M'
062936160216     C     O27FIE        orEQ      'N'
062937160216     C                   MOVEL     'S'           WBOFED            1
062938160216     C                   MOVEL     'E'           WBOEST            1
062939160216     C                   ENDIF
062940160216     C                   ELSE
062941160216     C                   SETON                                        94
062942160216     C                   MOVEL     O27MSG        TASMSG
062943160216     C                   GOTO      FINE
062944160216     C                   ENDIF
062945160216     C**
062946160226     C* LO PASSO IN OUTPUT (ha senso solo per fedex)
062947160216     c                   MOVEL     O27FIE        TASFIE
062948160216      * Per tutte le tipologie di bolla FedEx imposto sempre 'F' xchè il
062949160226      * TNSF07 testa 'F' x definire che è una bolla FedEx (descrizione varie)
062950160216     c                   If        O27Fie = 'M' or O27Fie = 'N'
062951160216     c                   Movel     'F'           TAsFie
062952160216     c                   EndIf
062953160216     c
062954160216     c                   eval      NtwTam = O27ntw
062955160301     c                   clear                   tiptam
062956160226      * imposto il tipo tariffa fedex
062957160226     c                   if        NtwTam = 'FED' and
062958160226     c                             o27fie = 'N'
062959160226     c                   eval      TIPTam = 'FD'
062960160226     c                   endif
062962160216
062963160226     c                   if        NtwTam = 'FED' and
062964160226     c                             (o27fie = 'M' or o27fie = 'F')
062965160226     c                   eval      TIPTam = 'FM'
062966160226     c                   endif
062967160226
062968160216
063000020225     C                   EXSR      CERTCA
063100020319     C                   Z-ADD     CARKSC        KSC2
063200020319     C                   Z-ADD     CARCTR        CTR2
063300020319     C                   Z-ADD     CARCTR        CODCTR
063400020319     C                   Z-ADD     CARPRG        PRG2
063500020225      *
063600980605     C     KTAM2         CHAIN     TNTAM00L                           94
063700020222      *
063800980605     C** NON TROVATA (IMPOSSIBILE!!!)
063900980605    2C     *IN94         IFEQ      *ON
064000980605     C                   MOVEL     MSG(10)       TASMSG
064100980605     C                   GOTO      FINE
064200980605    2C                   ENDIF
064300980605     C* KSCCOD NON LO IMPOSTO PERCHE'PER I 9999 VOGLIO CHE RIMANGA TALE
064400980605     C**
064500980605   X1C                   ELSE
064600941112     C** ACCESSO TNTAM
064700980605     C                   Z-ADD     TASCTR        CODCTR            3 0
064800980605     C                   Z-ADD     TASKSC        CODCLI            7 0
064900980605     C**
065000941110     C                   Z-ADD     CODCLI        TMCLI             7 0
065100980515     C                   Z-ADD     CODCTR        TMCTR             3 0
065200941110     C                   EXSR      CERTAM
065300020227     C** SE 90 /94 SONO SPENTI NON HO TROVATO LA TARIFFA DI CARTELLO RELATIVA ALLA TARIFFA
065400020319     C** DEL CLIENTE   (QUESTO CASO L'HO ELIMINATO)
065500020227     C     *IN90         IFEQ      *OFF
065600020227     C     *IN94         ANDEQ     *OFF
065700020225     C*** NON SO COSA FARE ANCHE PERCHE' E' IMPOSSIBILE MA VADO A FINE LO STESSO
065800020225     C                   SETON                                        94
065900020225     C                   GOTO      FINE
066000020225     C                   ENDIF
066100980604     C** NON C'E' TARIFFA CLIENTE
066200980605    2C     *IN94         IFEQ      *ON
066300980604     C**
066400980605     C* SE NON HO CHIAMATO PER UNA VARIA  -- > ERRORE
066500020416     C* NON DO ERRORE SOLO PER LA VARIA "R"
066600020416    3C     TASSVA        IFNE      'R'
066700980604     C                   GOTO      FINE
066800980605   X3C                   ELSE
066900980604     C**
067000020416     C** SE HO CHIAMATO PER LA VARIA 'R' --> IMPOSTO LA TAR.CARTELLO
067100980605     C* IMPOSTO ANCHE KSCCOD perche' voglio che me lo tratti come
067200980605     C*  cliente generico da tariffa di cartello per l'assicurazione
067300980605     c*  Non ho problemi per i 9999 èerche' avendoli gia' impostati
067400980605     c*  la prima volta che fa  certam trova la tariffa (di cartello)
067500980605     C                   MOVE      8888          KSCCOD
067501160216     C                   CLEAR                   TRUL27
067502160216     C                   MOVEL     TASTSP        I27TSP
067503160216     C                   MOVEL     TASLNA        I27LNA
067504160216     C                   MOVEL     TASLNP        I27LNP
067505160216     C                   MOVEL     TASKSC        I27CLI
067506160216     c                   Movel     Tasctr        I27Ctb
067507160225     C                   CALL      'TRUL27R1'
067508160216     C                   PARM                    TRUL27
067509160216      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
067510160216     C     O27ERR        IFEQ      *BLANKS
067511160216     C     O27FIE        IFEQ      'E'
067512160216     C                   MOVEL     'E'           WBOEST            1
067513160216     C                   ENDIF
067514160216     C*
067515160216     C     O27FIE        IFEQ      'D'
067516160216     C                   MOVEL     'S'           WBODPD            1
067517160216     C                   MOVEL     'I'           WBOEST            1
067518160216     C                   ENDIF
067519160226     C*
067520160226     C     O27FIE        IFEQ      'X'
067521160226     C     O27NTW        ANDEQ     'DPD'
067522160226     C                   MOVEL     'S'           WBODPD
067523160226     C                   MOVEL     'I'           WBOEST
067524160226     C                   ENDIF
067525160226     C*
067526160226     C     O27FIE        IFEQ      'X'
067527160226     C     O27NTW        ANDEQ     'EEX'
067528160226     C                   MOVEL     'E'           WBOEST
067529160226     C                   ENDIF
067530160216     C*
067531160216     C     O27FIE        IFEQ      'F'
067532160216     C     O27FIE        orEQ      'M'
067533160216     C     O27FIE        orEQ      'N'
067534160216     C                   MOVEL     'S'           WBOFED            1
067535160216     C                   MOVEL     'E'           WBOEST            1
067536160216     C                   ENDIF
067537160216     C                   ELSE
067538160216     C                   SETON                                        94
067539160216     C                   MOVEL     O27MSG        TASMSG
067540160216     C                   GOTO      FINE
067541160216     C                   ENDIF
067542160216     C**
067543160216     C* LO PASSO IN OUTPUT
067544160216     c                   MOVEL     O27FIE        TASFIE
067545160216      * Per tutte le tipologie di bolla FedEx imposto sempre 'F' xchè il
067546160216      * TNSF02 testa 'F' x definire che è una bolla FedEx (descrizione varie)
067547160216     c                   If        O27Fie = 'M' or O27Fie = 'N'
067548160216     c                   Movel     'F'           TAsFie
067549160216     c                   EndIf
067550160216     c
067551160216     c                   eval      NtwTam = O27ntw
067552160301     c                   clear                   tiptam
067553160226      * imposto il tipo tariffa fedex
067554160226     c                   if        NtwTam = 'FED' and
067555160226     c                             o27fie = 'N'
067556160226     c                   eval      TIPTam = 'FD'
067557160226     c                   endif
067558160226
067559160226     c                   if        NtwTam = 'FED' and
067560160226     c                             (o27fie = 'M' or o27fie = 'F')
067561160226     c                   eval      TIPTam = 'FM'
067562160226     c                   endif
067563160226
067564160216
067600020225     C* CERCO LA CARTELLO IN BASE ALLE CARATTERISTICHE DELLA SPEDIZIONE EW DEL CLIENTE
067700020225     C                   EXSR      CERTCA
067800020319     C                   Z-ADD     CARKSC        KSC2
067900020319     C                   Z-ADD     CARCTR        CTR2
068000020319     C                   Z-ADD     CARCTR        CODCTR
068100020319     C                   Z-ADD     CARPRG        PRG2
068200020225     C*
068300980605     C     KTAM2         CHAIN     TNTAM00L                           94
068400980605     C** NON TROVATA (IMPOSSIBILE!!!)
068500980605    4C     *IN94         IFEQ      *ON
068600980605     C                   MOVEL     MSG(10)       TASMSG
068700980605     C                   GOTO      FINE
068800980605    4C                   ENDIF
068900020225     C* MI CARICO LA DIVISA DELLA CARTELLO
069000020225     C                   MOVEL     TAMFLO        DSTA01
069100020225     C                   MOVEL     §TADIV        DIVCAR
069200020225     C*
069300980605    3C                   ENDIF
069400980605    2C                   ENDIF
069500980605    1C                   ENDIF
069600980604     C**
069700941110     C                   Z-ADD     TAMKSC        TDCLI             7 0
069800941110     C                   Z-ADD     TAMPRG        TDPRG             3 0
069900941110     C                   Z-ADD     TAMCTR        TDCTR             3 0
070000980609     C* CAMPO DI OUTPUT
070100000125     C                   MOVEL     TAMBAP        TASBAP
070200980609     C                   MOVEL     TAMFLO        DSTA01
070300000125     C*
070400000125     C* SE BOLLA DPD E TARIFFA DI CARTELLO --> ERRORE SE NON RICHIAMO
070500020319     C*  PER UNA VARIA E SE LA CARTELLO NON E' UNA CARTELLO DPD
070600000125     C     KSCCOD        IFEQ      8888
070700000125     C     KSCCOD        OREQ      9999
070800020416     C     TASSVA        IFNE      'R'
070900000125     C     WBODPD        ANDEQ     'S'
071000020225      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
071100020225     C     §TADPD        ANDNE     'S'
071200000125     C                   SETON                                        94
071300000125     C                   MOVEL     MSG(13)       TASMSG
071400000125     C                   GOTO      FINE
071500000125     C                   ENDIF
071600020416     C     TASSVA        IFNE      'R'
071700020319     C     WBOFED        ANDEQ     'S'
071800020319      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
071900020319     C     §TAFED        ANDNE     'S'
072000020319     C                   SETON                                        94
072100020319     C                   MOVEL     MSG(16)       TASMSG
072200020319     C                   GOTO      FINE
072300020319     C                   ENDIF
072400000125     C                   ENDIF
072500000125     C*
072600000125     C* SE TARIFFA O CLIENTE TARIFFA DPD E BOLLA NON LO E'--> MANCA TAR
072700000125     C     §TADPD        IFEQ      'S'
072800000125     C     WBODPD        IFEQ      ' '
072900000125     C                   SETON                                        94
073000000125     C                   MOVEL     MSG(12)       TASMSG
073100000125     C                   GOTO      FINE
073200000125     C                   ENDIF
073300000125     C                   ELSE
073400000125     C     WBODPD        IFEQ      'S'
073500000125     C                   SETON                                        94
073600000125     C                   MOVEL     MSG(13)       TASMSG
073700000125     C                   GOTO      FINE
073800000125     C                   ENDIF
073900000125     C                   ENDIF
074000020319     C* SE TARIFFA O CLIENTE TARIFFA FED E BOLLA NON LO E'--> MANCA TAR
074100020319     C     §TAFED        IFEQ      'S'
074200020319     C     WBOFED        IFEQ      ' '
074300020319     C                   SETON                                        94
074400020319     C                   MOVEL     MSG(15)       TASMSG
074500020319     C                   GOTO      FINE
074600020319     C                   ENDIF
074700020319     C                   ELSE
074800020319     C     WBOFED        IFEQ      'S'
074900020319     C                   SETON                                        94
075000020319     C                   MOVEL     MSG(16)       TASMSG
075100020319     C                   GOTO      FINE
075200020319     C                   ENDIF
075300020319     C                   ENDIF
075400000125     C**
075500990927     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
075600990927     C     §TADIV        IFEQ      *BLANK
075700990927     C                   MOVE      'ITL'         §TADIV
075800990927     C                   ENDIF
075900011127      * VERIFICO IN BASE ALLA DATA TASSAZIONE E DATA SPEDIZIONE SE LA DIVISA DELLA TARIFFA
076000011127      * E' CORRETTA
076100011127      *
076200011127      * SE STO TASSANDO con data > del 2001 un bolla con data > del 2001 E LA DIVISA NON E'
076300011127      * UGUALE ALLA MONETA DI CONTO AZIENDALE
076400020826     C***        DATTAS    IFGT 20011231
076500020826     C     §TADIV        IFNE      §GEDCN
076600011127     C     TASDSP        ANDGT     20011231
076700011127      * MANCA TARIFFA
076800011127     C                   SETON                                        94
076900011127     C                   MOVEL     MSG(14)       TASMSG
077000011127     C                   GOTO      FINE
077100011127     C                   ENDIF
077200991014     C*
077300991014     C* RECUPERO LE CARATTERISTICHE DELLA DIVISA
077400991014     C                   Z-ADD     1             K
077500991014     C     §TADIV        LOOKUP    CV(K)                                  10
077600991014     C     *IN10         IFEQ      *ON
077700991014     C     K             OCCUR     DSCV
077800991014     C     §CVFDC        IFEQ      'S'
077900991014     C* DIVISA CHE ACCETTA DECIMALI
078000991014     C                   SETON                                        12
078100991014     C                   ENDIF
078200991014     C                   ENDIF
078300990921     C** RICERCA TNTAM
078400990921     C** VERIFICO SE LA MONETA PASSATA E'= A QUELLA  DELLA TARIFFA
078500990921     C     TASDIV        IFNE      §TADIV
078600990928     C* E LA TARIFFA PASSAT NON E' UGUALE A BLANK
078700990928     C     TASDIV        ANDNE     *BLANKS
078800990921     C** CONVERTO GLI IMPORTI NELLA MONETA DELLA TARIFFA CLIENTE
078900990921     C*  ANCHE LE VARIE
079000011127     C*
079100011127     C* PASSO LE MONETE PER LA CONVERSIONE ED IN PIÙ IL FLAG SE DEVO
079200011127     C* CONVERTIRE LA QUANTITA' DA FATTURARE OPPURE NO
079300011127     C* NEL CASO IN CUI CONVERTO NELLA MONETA DELLA TARIFFA
079400011127     C* NON E' NECESSARIO CONVERTIRE LA QTA DA FATTURARE E
079500011127     C* L'IMPORTO D'ASSICURARE CALCOLATO IN TASSAZIONE
079600011127     C                   MOVE      TASDIV        DIVINP            3
079700011127     C                   MOVE      §TADIV        DIVOUT            3
079800011127     C                   MOVE      'N'           CONQTA            1
079900011127     C                   MOVE      'N'           CONIAL            1
080000990921     C                   EXSR      CNVTAS
080100990921     C*
080200990921     C                   ENDIF
080300990929     C*------------------------------------------------------
080400990929     C* SE IMPONIBILE VALORIZZATO VADO A FINE
080500990929     C     TASIMV        IFNE      *ZEROS
080600000103     C* E NON E' STATA RICHIESTA NESSUNA VARIA
080700000103     C     TASSVA        ANDEQ     ' '
080800990929     C                   GOTO      FINE
080900990929     C                   ENDIF
081000980506     C*------------------------------------------------------
081100020826     C* VERIFICO SE APPLICARE IL PESO VDL O MENO
081200020826     C                   EXSR      CTRPES
081201160606     C* VERIFICO SE APPLICARE IL VOLUME VDL O MENO
081202160606     C                   EXSR      CTRVOL
081300020826     C*------------------------------------------------------
081400980506     C** PORTO O INOLTRO PRETASSATO
081500990721     C**  OPPURE DEVO CALCOLARE UNA VARIA SPECIFICA --> NON CERCO TITAD
081600941110     C     TASPOR        IFGT      0
081700980506     C     TASINL        ORGT      0
081800980506     C     TASSVA        ORNE      ' '
081900000516     C**                   Z-ADD0         TASPVL
082000941110     C                   GOTO      TASSAT
082100941110     C                   END
082200941110     C*
082300941112     C                   MOVEL     CODCTR        UNOCTR            1 0
082400980506     C* QUANTITA' DA FATTURARE
082500950119     C     UNOCTR        IFEQ      5
082600950210     C     TASQFT        COMP      0                                      94
082700980512     C   94              MOVEL     MSG(6)        TASMSG
082800980512     C   94              GOTO      FINE
082900950119     C                   END
083000950119     C** TARIFFA A QUANTITA' - MANCA Q.TA' DA FATTURARE
083100950119     C     UNOCTR        IFEQ      4
083200950119     C     TAMFVD        IFEQ      *BLANKS
083300950119     C     TAMFVD        OREQ      '2'
083400961007     C     TAMFVD        OREQ      '4'
083500950210     C     TASQFT        COMP      0                                      94
083600980512     C   94              MOVEL     MSG(6)        TASMSG
083700950119     C   94              GOTO      FINE
083800950120     C                   GOTO      POIQTF
083900950119     C                   END
084000950119     C** TARIFFA A VALORE FLAG 2 E BLANK
084100950119     C     TAMFVD        IFEQ      '1'
084200950210     C     TASQFT        CABGT     0             POIQTF
084300950119     C                   END
084400950119     C*
084500950210     C                   Z-ADD     TASIAS        TASQFT
084600950119     C                   Z-ADD     1             K
084700950119     C     TASVAS        LOOKUP    CV(K)                                  05
084800980527     C     *IN05         IFEQ      *OFF
084900980527     C                   SETON                                        94
085000980527     C                   MOVEL     MSG(2)        TASMSG
085100980527     C                   GOTO      FINE
085200980527     C                   ENDIF
085300990917     C**
085400990917     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA
085500990917     C                   CLEAR                   YEUR
085600990917     C                   MOVEL     TASVAS        YECDVI
085700990917     C                   MOVEL     §TADIV        YECDVO
085800990917     C                   Z-ADD     TASQFT        YECIMI
085900991014     C   12              MOVE      '3'           YECDCO
086000991014     C  N12              MOVE      '0'           YECDCO
086100990917     C*
086200990917     C                   CALL      'YEURCO'
086300990917     C                   PARM                    YEUR
086400990917     C*
086500990917     C     YECESI        IFEQ      ' '
086600990917     C                   Z-ADD     YECIMO        TASQFT
086700990917     C                   END
086800990923     C****                 ENDIF
086900990917     C****       K         OCUR DSCV
087000990917     C****       TASVAS    IFNE §CVITA
087100990917     C****       TASQFT    MULT §CVCMB    TASQFT
087200990917     C****                 END
087300950119     C* CAMBIO
087400950210     C     TASQFT        IFEQ      0
087500950119     C                   EXSR      CALVAS
087600950210     C                   Z-ADD     IMPVAS        TASQFT
087700950119     C                   END
087800950210     C     TASQFT        COMP      0                                      94
087900980527     C   94              MOVEL     MSG(6)        TASMSG
088000950119     C   94              GOTO      FINE
088100950119     C** TARIFFA A VALORE FLAG 1 E 3 - NON ESISTE VALORE ASSICURATIVO
088200950119     C                   END
088300950119     C     POIQTF        TAG
088400980518     C* MI SERVE MINTAS PER IL CARICAMENTO DELLO SCAGLIONE DEL
088500980518     C*  MINIMO TASSABILE
088600980518     C                   Z-ADD     1             A
088700980518     C                   MOVEL     UNOCTR        WTPG              1
088800980518     C     WTPG          LOOKUP    CTR(A)                                 05
088900980518     C   05A             OCCUR     DSTR
089000980518     C   05              Z-ADD     §TRATA        MINTAS
089100990722     C*
089200990722     C** RECUPERO CODICE NAZIONE MITTENTE E DESTINATARIO DAL CODIEC TASSAZIONE
089300990722     C*
089400990923     C***                  Z-ADD1         A
089500990923     C***        TASCTS    LOKUPCTS,A                    05
089600990923     C***05      A         OCUR DSCT
089700990923     C***05                MOVEL§CTNAR    TASNAD  3
089800990722     C*
089900990923     C***                  Z-ADD1         A
090000990923     C***        TASMCT    LOKUPCTS,A                    05
090100990923     C***05      A         OCUR DSCT
090200990923     C***05                MOVEL§CTNAR    TASNAM  3
090300980518     C*
090400990721     C** RICERCA TITAD
090500980518     C                   EXSR      CARTAR
090600980518     C*
090700980313     C** PESO TASSABILE E QUANTITA' DA FATTURARE
090800980429     C                   MOVEL     UNOCTR        WTPG              1
090900980312     C                   CLEAR                   WRPV
091000040603     C                   CLEAR                   RAPPV
091100980312     C                   MOVEL     'TAD'         WDET              3
091200980313     C                   Z-ADD     TAMARL        WARL
091300980313     C                   Z-ADD     TAMARF        WARF
091400980313     C                   Z-ADD     TAMARO        WARO
091500980312     C                   EXSR      CALCOL
091600980313     C                   Z-ADD     ISOPES        PESTAS
091700941112     C*--------------------------------------------------------------
091800980313     C*  PESO TASSABILE PER TARIFFE A PESO
091900980313     C                   EXSR      CALPT
092000941112     C*--------------------------------------------------------------
092100941110     C     TASSAT        TAG
092200990922     C** ESEGUE TASSAZIONE BOLLA
092300941110     C                   EXSR      TASSA
092400990922     C** MANCA TARIFFA
092500941110     C   94              GOTO      FINE
092600941110     C                   ENDSR
092700941112     C******************************************************
092800941112     C** GUIDA PER TASSAZIONE SPEDIZIONE
092900941112     C******************************************************
093000941110     C     TASSA         BEGSR
093100941110     C                   SETOFF                                       94
093200090923     C                   clear                   WvariaMag
093300090924     c                   clear                   WImpoMag
093400090923
093500970522     C** FLAG PER APPLICAZIONE INOLTRO
093600001009     C*!!* PRIMA SI FACEVA COSÌ *!!*
093700001009     C*!!* ** ASSEGNATI-MITTENTE FRANCHI-DESTINATARIO
093800001009     C*!!* ** PER 99 SEMPRE DESTINATARIO
093900001009     C*!!*           TIPBOL    COMP 'A'                      57
094000001009     C*!!*   57      KSCCOD    COMP 9999                 5757
094100001009     C*!!*   57                MOVELTASFAP    FLGINO  1
094200001009     C*!!*  N57                MOVELTASFIN    FLGINO
094300001009     C*!!*
094400001009     C*!!* ORA CONTROLLO LA TARIFFA :
094500001009     C*!!* SE E' USO LA TARIFFA REVERSIBILE (56 ON)  PRENDO COME INOLTRO L'ANTEPORTO TASFAP
094600001009     C*!!* SE USO LA TARIFFA NORMALE (56 OFF) USO L'INOLTRO TASFIN
094700001009     C*!!*
094800001009     C  N56              MOVEL     TASFIN        FLGINO            1
094900001009     C   56              MOVEL     TASFAP        FLGINO
095000001009     C*!!*
095100001009     C*!!*   FINE   *!!*
095200970522     C**
095300980506     C** SE DEVO CALCOLARE UNA VARIA VADO SUBITO DA QUELLA
095400980506     C     TASSVA        IFNE      '  '
095500990916     C     *LIKE         DEFINE    TASQFT        WQFT
095600980506     C** VARIA 'G' --> CONTRASSEGNO
095700980506     C     TASSVA        IFEQ      §$2VRG
095800980506     C                   GOTO      VARIAG
095900980506     C                   ENDIF
096000980525     C** VARIA 'R' --> ASSICURAZIONE PER CONTO
096100980526     C     TASSVA        IFEQ      §$2VRR
096200980525     C                   GOTO      VARIAR
096300980525     C                   ENDIF
096400020412     C** VARIA 'O' --> RICERCA DOCUMENTI
096500020412     C     TASSVA        IFEQ      §$2VRO
096600020412     C                   GOTO      VARIAO
096700020412     C                   ENDIF
096800020516     C** VARIA 'Y' --> RITIRI ANNULLATI
096900020516     C     TASSVA        IFEQ      §$2VRY
097000020516     C                   GOTO      VARIAY
097100020516     C                   ENDIF
097200020516     C** VARIA '*' --> DIROTTAMENTI
097300020523     C     TASSVA        IFEQ      §$2AST
097400020523     C                   GOTO      VARAST
097500020516     C                   ENDIF
097600030403     C** VARIA 'W' --> RECAPITO C/ASSEGNI
097700030403     C     TASSVA        IFEQ      §$2VRW
097800030403     C                   GOTO      VARVRW
097900030403     C                   ENDIF
098000030527     C** VARIA 'M' --> ADDEBITO INSOLUTI INTERESSI DI MORA
098100030527     C     TASSVA        IFEQ      §$2VRM
098200030527     C                   GOTO      VARVRM
098300030527     C                   ENDIF
098400030626     C** VARIA 'T' --> ORM COMMISSIONATO
098500030626     C     TASSVA        IFEQ      §$2VRT
098600030626     C                   GOTO      VARVRT
098700030626     C                   ENDIF
098800040910     C** VARIA 'a' --> P.O.D IMMAGE
098900040910     C     TASSVA        IFEQ      §$2POD
099000040910     C                   GOTO      VARPOD
099100040910     C                   ENDIF
099200030604     C** VARIA 'D' --> DIRITTO DI FATTURAZIONE
099300030604     C     TASSVA        IFEQ      §$2VRD
099400030922     C                   GOTO      FINE
099500030604     C                   ENDIF
099600040113     C** VARIA 'Z' --> ADDIZIONALE DI GESTIONE + ISTAT
099700040113     C     TASSVA        IFEQ      §$2VRZ
099800040113     C                   GOTO      FINE
099900040113     C                   ENDIF
100000980506     C                   ENDIF
100100970522     C**
100200941123     C** SE IL PORTO O L'INOLTRO SONO PRETASSATI
100300941110     C** DEBBO ESCLUDERE LA FASE DI CALCOLO TARIFFA
100400090924     C     TASPOR        CABGT     0             DOPOIN
100500090924     C     TASINL        CABGT     0             DOPOIN
100600980508     C*****
100700980508     C** CALCOLO    P O R T O     ED    I N O L T R O
100800980508     C*****
100900980506     C**
101000020218     C                   DO        200           A
101100941110     C     A             OCCUR     TARDS
101200941110     C     TSCG          IFGE      PESTAS
101300941110     C                   GOTO      ENDTAS
101400941110     C                   END
101500941110     C                   END
101600980508     C** NON TROVATO SCAGLIONE ESATTO
101700941110     C     TSCG          IFLT      PESTAS
101800941110     C                   SETON                                        94
101900980512     C                   MOVEL     MSG(5)        TASMSG
102000150930     c                   eval      %subst(tasmsg: 37)= %trim(%editc(PESTAS:'3'))
102100980512     C                   GOTO      FINE
102200941110     C                   END
102300941110     C     ENDTAS        TAG
102400980512     C** MI SALVO LA POSIZIONE DELLO SCAGLIO TROVATO CHE MI
102500980512     C*  SERVE PER IL CALCOLO TARIFFA CON GARANTITO MAX SCAGLIONE PREC
102600980512     C     *LIKE         DEFINE    A             WA
102700980512     C                   Z-ADD     A             WA
102800980508     C**
102900980508     C* CALCOLO PER QUALE VALORE MOLTIPLICARE LA TARIFFA
103000980508     C                   Z-ADD     PESTAS        WPTAS3
103100980508     C                   EXSR      CALQLI
103200980508     C**
103300980508     C* MODO APPLICAZIONE TARIFFA
103400980508     C                   MOVEL     TAMFLO        DSTA01
103500990927     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
103600990927     C     §TADIV        IFEQ      *BLANK
103700990927     C                   MOVE      'ITL'         §TADIV
103800990927     C                   ENDIF
103900980512     C**
104000980512     C* CALCOLO NORMALE ANCHE PER SCALARE AL DI SOTTO
104100980512     C*  DELL'APPLICAZIONE TARIFFA FINITA E PER MAX SCAGLIONE PREC
104200980512     C     §TAF02        IFNE      'S'
104300980512     C     §TAF02        OREQ      'S'
104400980512     C     WATA          ANDNE     ' '
104500980508     C** MOLTIPLICAZIONE TARIFFA
104600980508     C                   EXSR      MULTAR
104700980515     C* APPLICAZIONE MINIMO E MASSIMO
104800980515     C                   EXSR      MINMAX
104900980508     C                   Z-ADD     WPOR          TASPOR
105000980508     C                   Z-ADD     WINL          TASINL
105100980514     C                   ENDIF
105200980508     C**
105300980508     C** S C A L A R E
105400980512     C     §TAF02        IFEQ      'S'
105500980508     C                   EXSR      SCALAR
105600980512     C                   ENDIF
105700980508     C**
105800980508     C** M A X   S C A G L I O N E   P R E C E D E N TE
105900980512     C     §TAF02        IFEQ      'G'
106000980512     C                   EXSR      MAXSCA
106100980512     C                   ENDIF
106200990923     C**
106300990923     C** SE DIVISA DELLA TARIFFA NON ACCETTA I DECIMALI PREPARO IL
106400990923     C** DEL PORTO E DELL'INOLTRO SENZA DECIMALI
106500990923     C     *IN12         IFEQ      '0'
106600990923     C* PORTO
106700991112     C     TASPOR        ADD       0,5           CAMPOW           11 0
106800990923     C                   Z-ADD     CAMPOW        TASPOR
106900990923     C* INOLTRO
107000991112     C     TASINL        ADD       0,5           CAMPOW
107100990923     C                   Z-ADD     CAMPOW        TASINL
107200990923     C                   ENDIF
107300090924     c                   eval      WImpoMag=taspor+tasinl
107400980508     C**
107500970521     C*******************************
107600990921     C****************
107700990921     C** TIPO SERVIZIO
107800990921     C****************
107900001127     C*
108000090924     C**                 Z-ADD     1             A
108100090924     C**                 clear                   ESP
108200001127     C*
108300001127     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
108400001127     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
108500001127     C*
108600090924     C**   KSDIT         IFEQ      'SDI'
108700090924     C**   TASDSP        ANDLE     20001231
108800090924     C**   TASTSP        ANDEQ     'C'
108900090924     C**   'E'           LOOKUP    TS(A)                                  05
109000090924     C**                 ELSE
109100090924     C**   TASTSP        LOOKUP    TS(A)                                  05
109200090924     C**                 ENDIF
109300001127     C*
109400090924     C**N05              GOTO      DOPOIN
109500090924     C**   A             OCCUR     DS5E
109600090924     C**   §5EFTC        CABEQ     *BLANKS       DOPOIN
109700090922
109800980430     C** TIPO SERVIZIO SENZA TARIFFA PARTICOLARE
109900990923     C** PORTO
110000090924     C**                 Z-ADD     0             WTSP
110100090924    1C**   TASPOR        IFGT      0
110200090924     c**                 exsr      CerESPRESSO
110300090924     c**
110400060414     c* Se c'e' già la varia, non la calcolo
110500090924    2c**                 if        giaespr<>'S'
110600090924     C**                 Z-ADD     TASPOR        IMPON
110700090924     c**                 if        *in56
110800090924     c**                 eval      applicarevers='S'
110900090924     c**                 endif
111000090924     C**                 EXSR      CALTS
111100090924     c**                 clear                   applicarevers
111200090924    3C**   WTSP          IFGT      0
111300060414     C*****              ADD       WTSP          TASPOR
111400060414     c
111500090924    4c**                 if        esp<>999
111600090924     C**                 MOVEL     wVariaMag     SV(ESP)
111700090924     C**                 add       wtsp          VA(ESP)
111800090924     c**                 else
111900060414     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
112000090924     C**                 ADD       wtsp          TASPOR
112100090924    4c**                 endif
112200090924     c**
112300090924    3C**                 ENDif
112400090924    2C**                 ENDif
112500090924    1C**                 ENDif
112600990923     C** INOLTRO
112700090924     C**                 Z-ADD     0             WTSP
112800090924    1C**   TASINL        IFGT      0
112900090924     c**                 EXSR      CerESPRESSO
113000090924    2c**                 if        giaespr<>'S'
113100090924     C**                 Z-ADD     TASINL        IMPON
113200090924     c**                 if        *in56
113300090924     c**                 eval      applicarevers='S'
113400090924     c*+                 endif
113500090924     C**                 EXSR      CALTS
113600090924     c**                 clear                   applicarevers
113700090924    3C**   WTSP          IFGT      0
113800060414     C*****              ADD       WTSP          TASINL
113900090924    4c**                 if        esp<>999
114000090924     C**                 MOVEL     wVariaMag     SV(ESP)
114100090924     C**                 add       wtsp          VA(ESP)
114200090924   x4c**                 else
114300060414     C** SE NON CI SONO VARIE LIBERE AGGIUNGO ALla voce
114400090924     C**                 ADD       WTSP          TASINL
114500090924    4c**                 endif
114600090924    3C**                 END
114700090924    2C**                 END
114800090924    1C**                 END
114900090924     c
115000991012     C     DOPOIN        TAG
115100991012     C**
115200990923     C* VALORIZZO LA VARIA DELL'INOLTRO NELLA SCHIERA DELLE VARIE
115300991012     C     TASINL        IFGT      0
115400990923     C                   Z-ADD     1             X                 2 0
115500990923     C     §$2FIN        LOOKUP    SV(X)                                  30
115600990923     C* SE NON ESISTE CARICOLA SCHIERA
115700990923     C     *IN30         IFEQ      '0'
115800990923     C                   Z-ADD     1             X
115900990923     C     ' '           LOOKUP    SV(X)                                  30
116000990923     C   30              MOVEA     §$2FIN        SV(X)
116100990923     C                   ENDIF
116200990923     C* SE LO TROVO VALORIZZO TASINL
116300990923     C   30              Z-ADD     TASINL        VA(X)
116400991012     C                   END
116500990921     C****************************************
116600990921     C** ISOLA  LOCALITA' DISAGIATE C.STORICI
116700990921     C****************************************
116800970521     C** SE TROVATA TARIFFA DEL PAESE APPLICO LO STESSO L'ISOLA
116900970521     C**
117000970721    1C     FLGINO        IFEQ      'I'
117100970521     C     FLGINO        OREQ      'D'
117200110624      * sostituisco il controllo del centro storico con i nuovi flag T o Z
117300110624      * T = centro storico in città
117400110624      * Z = centro storico in provincia
117500110624     C**   FLGINO        OREQ      'S'
117600110624     C     FLGINO        OREQ      'T'
117700110624     C     FLGINO        OREQ      'Z'
117800970521     C* IMPOSTO LA TARIFFA PARTICOLARE
117900970521     C                   SELECT
118000970521     C     FLGINO        WHENEQ    'I'                                          ISOLA
118100970521     C                   MOVEL     'I'           TASTC
118200970521     C     FLGINO        WHENEQ    'D'                                          LOCALITA' DI
118300970521     C                   MOVEL     'K'           TASTC
118400110624      * sostituisco il controllo del centro storico con i nuovi flag T o Z
118500110624      * T = centro storico in città
118600110624      * Z = centro storico in provincia
118700110624     C**   FLGINO        WHENEQ    'S'                                          C.STORICO
118800110624     C     FLGINO        WHENEQ    'T'                                          C.STORICO
118900970521     C                   MOVEL     'Q'           TASTC
119000110624     C     FLGINO        WHENEQ    'Z'                                          C.STORICO
119100110624     C                   MOVEL     'Q'           TASTC
119200970521     C                   ENDSL
119300110624      ** visto che l'attivazione del calcolo del centro storico dovrebbe essere
119400110909      ** il 3/10/11  attivo il flag "Q" in tastc confrontando la dta spedizione
119500110909     c                   if        tasdsp < 20111003 and tastc = 'Q'
119600110630     c                   clear                   tastc
119700110630     c                   endif
119800110628      ** Se TASTC è valorizzato calcolo la varia. Questa specifica si può togliere
119900110628      ** quando abilitiamo il centro storico
120000110628 bis1c                   If        tastc <> ' '
120100110628     c
120200970521     C**
120300060414     c* 14/4/06-ES--> il pgm per tassare questa tar.particolare usa
120400060414     c*           sempre il cod.tassazione mittente anche se non
120500060414     c*           applica la reversibilità. Da sempre lo fa.
120600060414     c*           Abbiamo valutato che è sbagliato ma deve seguire
120700060414     c*           la regola della reversibilità. Per cui non lo devo
120800060414     c*           più fare per tipbol='A' ma per 56 ON
120900060414     C**   TIPBOL        IFEQ      'A'
121000060414     C                   IF        *in56
121100981008     C                   MOVEL     TASCTS        WCTS              2
121200941123     C                   MOVEL     TASMCT        TASCTS
121300060414     C                   ENDif
121400970521     C*
121500970521     C                   EXSR      TASPAR
121600060414     C**   TIPBOL        IFEQ      'A'
121700060414     C                   IF        *in56
121800970721     C                   MOVEL     WCTS          TASCTS
121900970721     C                   END
122000970721     C**
122100970721     C* SOLO SE L'HO CALCOLATA FACCIO LE COSE SOTTOSTANTI
122200970721    2C     WNOEST        IFEQ      'S'
122300970721     C     TASCP         ANDGT     0
122400970522     C* MEMORIZZO PORTO E POSIZIONE VARIA DOVE HO MEMORIZZATO
122500970522     C                   Z-ADD     TASCP         TASISO
122600090924     c* imponibile per maggiorazione "E" o "H"
122700090924     c                   eval      wImpoMag=wImpoMag+TASISO
122800970522     C*
122900970522     C  N96              MOVEL     'S'           WPORTO            1
123000970522     C* SALVO L'INDICE DELLA SCHIERA DELLA VARIA
123100970522     C   96              Z-ADD     A             ISO               2 0
123200970522     C   96              MOVEL     ' '           WPORTO            1
123300970521     C*
123400970521     C** SE VA IN SOSTITUZIONE CLEARO L'INOLTRO
123500970521     C** FLAG=S INOLTRO+ISOLA
123600970521     C** FLAG=N SOLO ISOLA
123700110628     c** In caso di centro storico  non considero il flag FLGISO in quanto non
123800110628     c** non deve andare in sostituzione dell'inoltro ed in tariffa non viene
123900110628     c** richiesta la valorizzazione quindi è sempre diverso da "S"
124000970721    3C     FLGISO        IFNE      'S'
124100110628     c     TASTC         ANDNE     'Q'
124200090924     c* tolgo dall'imponibile per la maggiornazione l'Inoltro
124300090924     c                   eval      wImpoMag=wImpoMag-TASINL
124400090924     c*
124500090924     C                   CLEAR                   TASINL
124600990921     C** RICHIAMO LA VECCHIA POSIZIONE DELLA VARIA E LA SOSTIUISCO CON IL
124700990921     C**  VALORE DELL'ISOLA
124800990921     C** L'INDICE "X" PUNTA ALLA VARIA DELL'INOLTRO
124900990921     C     X             IFGT      *ZERO
125000990921     C* PULISCO L'INOLTRO
125100990921     C                   CLEAR                   VA(X)
125200990921     C                   CLEAR                   SV(X)
125300990921     C* UTILIZZO LA POSIZIONE VUOTA DELLA SCHIERA PER METTERE L'IMPORTO DELLA
125400990921     C* VARIA DELL'ISOLA
125500990921     C                   MOVEL     §1PVAR        SV(X)
125600990921     C                   Z-ADD     TASCP         VA(X)
125700990921     C* PULISCO LA POSIZIONE DELLA SCHIERA PRECEDENTE
125800990921     C   96              CLEAR                   VA(A)
125900990921     C   96              CLEAR                   SV(A)
126000990921     C* SALVO L'INDICE ATTUALE DELLA SCHIERA DELLA VARIA
126100990921     C   96              Z-ADD     X             ISO
126200990922     C                   END
126300970721    3C                   END
126400990921     C***********************
126500990921     C** IS/L.D./C.S  - TIPO SERVIZIO
126600990921     C***********************
126700090924     C**                 Z-ADD     1             A                 5 0
126800001127     C*
126900001127     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
127000001127     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
127100001127     C*
127200090924     C**   KSDIT         IFEQ      'SDI'
127300090924     C**   TASDSP        ANDLE     20001231
127400090924     C**   TASTSP        ANDEQ     'C'
127500090924     C**   'E'           LOOKUP    TS(A)                                  05
127600090924     C**                 ELSE
127700090924     C**   TASTSP        LOOKUP    TS(A)                                  05
127800090924     C*+                 ENDIF
127900001127     C*
128000090924     C** 05A             OCCUR     DS5E
128100090924    3C** 05§5EFTC        IFNE      *BLANKS
128200970521     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
128300090924     c**                 EXSR      CerESPRESSO
128400090924     c**
128500090924   3ac**                 if        giaespr<>'S'
128600090924     C**                 Z-ADD     0             WTSP
128700090924     C**                 Z-ADD     TASISO        IMPON
128800060414     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
128900090924     C**                 IF        *in56
129000090924     c**                 eval      applicarevers='S'
129100090924     c**                 endif
129200090924     C**                 EXSR      CALTS
129300090924     c**                 clear                   applicarevers
129400060414     c
129500090924    4C**   WTSP          IFGT      0
129600090924    5c**                 if        esp<>999
129700090924     C**                 MOVEL     WVariaMag     SV(ESP)
129800090924     C**                 add       wtsp          VA(ESP)
129900090924   x5c**                 else
130000060414     c* Se non c'e' una varia disponibile, sommo al posto o alla
130100060414     c*  varia dell' isola
130200090924    6C**   WPORTO        IFEQ      'S'
130300090924     C**                 ADD       WTSP          TASPOR
130400090924   x6C**                 ELSE
130500090924     C**                 ADD       WTSP          TASISO
130600090924     C**                 Z-ADD     TASISO        VA(ISO)
130700090924    6C**                 END
130800090924    5C**                 END
130900090924    4C**                 END
131000060414     c
131100090924   3aC**                 ENDif
131200090924    3C**                 END
131300090924     c
131400970721    2C                   END
131500110628 bis1C                   ENDIF
131600970721    1C                   ENDIF
131700090924     c
131800090924     C***********************
131900090924     C** Maggiorazione per tipo servizio sul totale imponibile da maggiorare
132000110628     c** ovvero:  PORTO + INOLTRO + ISOLA/DISAGIATE/CENTRO STORICO
132100110628     C** Se porto o inoltro già presenti: solo su ISOLA/DISAGIATE/CENTRO STORICO
132200110628     c** Se isola/disagiate/centro storico già presente: solo su PORTO + INOLTRO
132300090924     C***********************
132400090924     C                   Z-ADD     1             A                 5 0
132500090924     C                   clear                   ESP
132600090924     c
132700090924    1c                   if        WimpoMag>0
132800090924     C*
132900090924     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
133000090924     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
133100090924     C*
133200090924    2C     KSDIT         IFEQ      'SDI'
133300090924     C     TASDSP        ANDLE     20001231
133400090924     C     TASTSP        ANDEQ     'C'
133500090924     C     'E'           LOOKUP    TS(A)                                  05
133600090924     C                   ELSE
133700090924     C     TASTSP        LOOKUP    TS(A)                                  05
133800090924    2C                   ENDIF
133900090924     C*
134000090924     C   05A             OCCUR     DS5E
134100090924    2C   05§5EFTC        IFNE      *BLANKS
134200090924     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
134300090924     c                   EXSR      CerESPRESSO
134400090924     c
134500090924    3c                   if        giaespr<>'S'
134600090924     C                   Z-ADD     0             WTSP
134700090924     c
134800090924     C                   Z-ADD     WImpoMag      IMPON
134900090924     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
135000090924     C                   IF        *in56
135100090924     c                   eval      applicarevers='S'
135200090924     c                   endif
135300090924     C                   EXSR      CALTS
135400090924     c                   clear                   applicarevers
135500090924     c
135600090924    4C     WTSP          IFGT      0
135700090924    5c                   if        esp<>999
135800090924     C                   MOVEL     WVariaMag     SV(ESP)
135900090924     C                   add       wtsp          VA(ESP)
136000090924   x5c                   else
136100090924     c* Se non c'e' una varia disponibile, sommo nel porto
136200090924     C                   ADD       WTSP          TASPOR
136300090924    5C                   END
136400090924    4C                   END
136500090924     c
136600090924    3C                   ENDif
136700090924    2C                   END
136800090924    1C                   END
136900990921     C****************
137000990921     C** DIRITTO FISSO
137100990921     C****************
137200980424     C     TASDIF        IFEQ      0
137300980424     C                   MOVEL     'H'           TASTC
137400990921     C* RICHIAMO LA ROUTINE DI CALCOLO DELLE TARIFFE PARTICOLARI
137500990921     C                   EXSR      TASPAR
137600990921     C   96              Z-ADD     TASCP         TASDIF
137700990921     C***
137800980424     C                   ENDIF
137900990921     C***********************
138000990921     C** PROVVIGIONE ASSEGNO
138100990921     C***********************
138200980506     C     VARIAG        TAG
138300980506     C     *LIKE         DEFINE    TASVA1        PROVAS
138400980506     C                   Z-ADD     0             PROVAS
138500980429     C** MANCA C/A
138600941112     C     TASCAS        CABEQ     0             POIT01
138700980429     C** PROVV. ASSEGNO GIA' PRESENTE
138800941112     C                   Z-ADD     1             A
138900941112     C     §$2VRG        LOOKUP    SV(A)                                  05
139000980429    1C  N05              DO
139100941116     C*
139200941116     C                   SETOFF                                       94
139300941116     C     *LIKE         DEFINE    TASCAS        CASLIT
139400941116     C                   Z-ADD     TASCAS        CASLIT
139500980429     C**
139600990921    2C*!*        TASCMB    IFEQ 0
139700990921     C*!*                  Z-ADD1         K
139800990921     C*!*        TASVCA    LOKUPCV,K                     05
139900990921    3C*!*        *IN05     IFEQ *OFF
140000990921     C*!*                  SETON                     94
140100990921     C*!*                  MOVELMSG,1     TASMSG
140200990921     C*!*                  GOTO FINE
140300990921    3C*!*                  ENDIF
140400941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
140500990921     C*!*        K         OCUR DSCV
140600990921    3C*!*        TASVCA    IFNE §CVITA
140700990921     C*!*        TASCAS    MULT §CVCMB    CASLIT    H
140800941116     C** CAMBIO IN BASE ALLA DIVISA STANDARD
140900990921    3C*!*                  END
141000990921   X2C*!*                  ELSE
141100990921     C*!*        TASCAS    MULT TASCMB    CASLIT    H
141200941116     C** CAMBIO IN BASE AL VALORE ESISTENTE IN BOLLA
141300990921    2C*!*                  END
141400990921     C* CALCOLO IL VALORE DEL CONTRASSEGNO  IN BASE ALLA MONETA
141500020319     C*******
141600020319     C*******
141700020319     C* SE PRESENTE IL C/A ANNULLATO VEDO IN BASE ALLA TARIFFA CLIENTE
141800020319     C*  SE FATTURARE O MENO
141900020319     C*  §TAPCA='N' NON FATTURO GLI ANNULLATI
142000030131     C**!!!§ASSTA        IFEQ      '9'
142100030131     C     TASSTA        IFEQ      '9'
142200030131     C     §TAPCA        ANDEQ     'N'
142300060922      * calcolo sempre la provvigione del C/A  annullato se si tratta di consegna anomala dirottata
142400060922     C     §ASCCA        ANDNE     '1'
142500060922      *
142600020319     C                   GOTO      POIT01
142700020319     C                   ENDIF
142800020319     C**
142900990921     C     TASVCA        IFNE      §TADIV
143000990921     C                   CLEAR                   YEUR
143100990921     C                   MOVEL     TASVCA        YECDVI
143200990921     C                   MOVEL     §TADIV        YECDVO
143300990921     C                   Z-ADD     TASCAS        YECIMI
143400991014     C   12              MOVE      '3'           YECDCO
143500991014     C  N12              MOVE      '0'           YECDCO
143600990921     C*
143700990921     C                   CALL      'YEURCO'
143800990921     C                   PARM                    YEUR
143900990921     C*
144000990921     C     YECESI        IFEQ      ' '
144100990921     C                   Z-ADD     YECIMO        CASLIT
144200990921     C                   ELSE
144300990921     C                   SETON                                        94
144400990921     C                   MOVEL     MSG(1)        TASMSG
144500990921     C                   GOTO      FINE
144600990923     C                   END
144700990921     C                   ENDIF
144800980429     C* PROVV ASSEGNO MITTENTE
144900980429    2C     TASBM         IFEQ      'M'
145000980429     C                   MOVEL     'W'           TASTC
145100980429     C                   ELSE
145200980429     C* PROVV ASSEGNO VETTORE
145300980429     C                   MOVEL     'V'           TASTC
145400980429    2C                   ENDIF
145500060712      * se spedizione EEX cerco sempre provvigione assegno mittente
145600160218     c***                If        o27fie = 'E'
145601160218     c                   if        WBOEST='E'and WBOFED=' '
145700060712     c                   movel     'W'           tastc
145800060712     c                   endif
145900980429     C**
146000980429     C                   MOVEL     TASTC         FISO
146100980506     C                   MOVEL     'S'           WCAR
146200980429     C                   EXSR      CERTPT
146300980429    2C     WEND          IFEQ      ' '
146400980429     C**
146500980429    3C     TPTTPG        IFEQ      'V'                                          VALORE
146600980429     C                   Z-ADD     CASLIT        TPDPES
146700980429     C                   ELSE                                                   ALTRO
146800980429     C                   Z-ADD     ISOPES        TPDPES
146900980429    3C                   ENDIF
147000980429     C**
147100980429     C                   EXSR      CALACD
147200941116     C*
147300980429    3C     VARIA         IFGT      0
147400941112     C                   Z-ADD     1             A
147500941112     C     ' '           LOOKUP    SV(A)                                  96
147600941112     C   96              MOVEL     §$2VRG        SV(A)
147700980429     C   96              Z-ADD     VARIA         VA(A)
147800980429     C  N96              ADD       VARIA         TASPOR
147900980429     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
148000980429    3C                   ENDIF
148100980429    2C                   ENDIF
148200980429    1C                   ENDDO
148300980506     C**
148400980506     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
148500980506     C     TASSVA        IFEQ      §$2VRG
148600980506     C                   GOTO      FINE
148700980506     C                   ENDIF
148800990921     C***********************
148900990921     C** DIRITTO ASSICURATIVO
149000990921     C***********************
149100941110     C     POIT01        TAG
149200941112     C                   Z-ADD     1             A
149300941112     C     §$2VRE        LOOKUP    SV(A)                                  05
149400941112     C  N05              DO
149500941112     C                   Z-ADD     0             DIRITT
149600941112     C     *LIKE         DEFINE    TASVA1        DIRITT
149700941110     C                   EXSR      CALVE
149800941112     C     DIRITT        IFGT      0
149900941112     C                   Z-ADD     1             A
150000941112     C     ' '           LOOKUP    SV(A)                                  96
150100941112     C   96              MOVEL     §$2VRE        SV(A)
150200941112     C   96              Z-ADD     DIRITT        VA(A)
150300941112     C  N96              ADD       DIRITT        TASPOR
150400941112     C                   END
150500941112     C                   END
150600941112     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
150700990921     C***********************
150800990921     C** ASS. X CONTO
150900990921     C***********************
151000980525     C     VARIAR        TAG
151100980525     C                   Z-ADD     0             WVARIR
151200060322     C                   Z-ADD     0             WVARIACB
151300980525     C     *LIKE         DEFINE    TASVA1        WVARIR
151400060321     C     *LIKE         DEFINE    TASVA1        WVARIACB
151500060320      * cerco la varia R sia in caso di bolla con importo d'assicurare e sia in caso di
151600060320      * ricerca di mandato
151700941112     C                   EXSR      CALVR
151800941112     C** MANCA TARIFFA:MANDATO VALORE = 0 E TASIAS=0
151900980526     C   94              GOTO      FINE
152000980526     C**
152100980525     C     WVARIR        IFGT      0
152200941112     C                   Z-ADD     1             A
152300941112     C     ' '           LOOKUP    SV(A)                                  96
152400941112     C   96              MOVEL     §$2VRR        SV(A)
152500980525     C   96              Z-ADD     WVARIR        VA(A)
152600060320      ** se non ci sono varie libere aggiungo al porto
152700980525     C  N96              ADD       WVARIR        TASPOR
152800060320
152900060320      * se varia uguale a zero e non ho trovato nessun mandato e non c'è importo d'assicurare in
153000060320      * bolla cerco la nuova AC base
153100060320
153200060320     c                   else
153300060320
153400060321     c                   If        task88 = 'X' and
153500060321      ** 88 = non si calcola
153600060322     c                             (Ksccod <> 8888 and Ksccod <> 9999) and
153700060322      ** data spedizione maggiore del 28 02 2006
153800060322     c                             tasdsp > 20060228
153900060320     c                   exsr      CALVACB
154000060320      ** carico la varia nella schiera
154100060321     c                   If        wvariacb > 0
154200060320     c                   z-add     1             a
154300060320     c     ' '           Lookup    sv(a)                                  96
154400060320     c   96              movel     §$2Acb        sv(a)
154500060321     c   96              z-add     Wvariacb      va(a)
154600060320      ** se non ci sono varie libere aggiungo al porto
154700060321     c  N96              add       Wvariacb      TASpor
154800060320     c                   endif
154900060320
155000060320     c                   endif
155100060320
155200060320     c                   END
155300980525     C**
155400980525     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
155500980525     C     TASSVA        IFEQ      §$2VRR
155600980525     C                   GOTO      FINE
155700980525     C                   ENDIF
155800990722     C***************
155900990722     C* T.C.P.
156000990722     C***************
156100990722     C*
156200990722     C                   MOVEL     §$2VRP        TASTC
156300990722     C                   EXSR      TASPAR
156400030624     C***************************************
156500030624     C* ANNULLAMENTO PORTO ASSEGNATO VARIA &
156600030624     C***************************************
156700030624     C*
156800030624     C* SE IL FLAG DI CALCOLO VARIA & È UGUALE A S
156900030624     C                   IF        TASFCAA = 'S'
157000030624     C                   MOVEL     §$2VPA        TASTC
157100030624     C                   EXSR      TASPAR
157200030624     C                   ENDIF
157300050928     C****************************
157400050928     C* LASCIATO AVVISO  VARIA 'c'
157500050928     C****************************
157600050929     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
157700050928     C                   If        tasric <> ' '
157800090305      * verifico se linea di arrivo o linea di partenza è estera non calcolo la varia "c"
157900051028     c     taslna        lookup    esttot                                 30
158000090305      * se linea di arrivo non è estero verifico la linea di partenza
158100090305     c  n30taslnp        lookup    esttot                                 30
158200090305      * se spedizione italiana calcolo
158300051028     c                   If        not *in30
158400050928     C                   Movel     §$2vla        Tastc
158500050928     C                   Exsr      TASpar
158600051005     c                   Endif
158700051005
158800050928     c                   Endif
158900110628     C****************************
159000110628     C* AVVISO AL DESTINATARIO AFFIDAMENTO SPEDIZIONE VARIA 'm'
159100110628     C****************************
159200110628     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
159300110628     C* in TASFLO §asemd
159400130920     C                   If        §asemd = 'S' or §asemd = 'X' or
159500130920     c                             §asemd = 'E'
159600110628     C                   Movel     §$2vad        Tastc
159700110628     C                   Exsr      TASpar
159800110628     c                   Endif
159900110628
160000141215     C****************************
160100141215     C* PIN CODE  CONSEGNA CON VERIFICA PIN CODE VARIA "p"
160200141215     C****************************
160300141215     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
160400141215     C* in TASFLO §aspin
160500141215     C                   If        §aspin = 'S'
160600141215     C                   Movel     §$2pin        Tastc
160700141215     C                   Exsr      TASpar
160800141215     c                   Endif
160900141215
160901151118     C*************************************************
160902151118     C* DIRITTO DI CHIAMATA PRENOTAZIONE ORM TELEFONICO
160903151118     C*************************************************
160905151118     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
160906151118     C* TASPRT
160907151118     C                   If        TASPRT = 'S'
160908151118     C                   Movel     §$2ORMT       Tastc
160909151118     C                   Exsr      TASpar
160910151118     c                   Endif
160911151118
160912151118     C****************************
160913151118     C* PACKING LIST
160914151118     C****************************
160915151118     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
160916151118     C* TASSPL
160917151118     C                   If        TASSPL = 'S'
160918151118     C                   Movel     §$2pkl        Tastc
160919151118     C                   Exsr      TASpar
160920151118     c                   Endif
160921151118
161000020412     C***************
161100020412     C* RICERCA DOCUMENTI - SOLO SE RICHIESTA DA SOLA
161200020412     C***************
161300020412     C*
161400020412     C     VARIAO        TAG
161500020412     C     TASSVA        IFEQ      §$2VRO
161600020412     C                   MOVEL     §$2VRO        TASTC
161700020412     C                   EXSR      TASPAR
161800020412     C**
161900020412     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
162000020412     C     TASSVA        IFEQ      §$2VRO
162100020412     C                   GOTO      FINE
162200020412     C                   ENDIF
162300020412     C                   ENDIF
162400020516     C***************
162500020516     C* RITIRI ANNULLATI  - SOLO SE RICHIESTA DA SOLA
162600020516     C***************
162700020516     C*
162800020516     C     VARIAY        TAG
162900020516     C     TASSVA        IFEQ      §$2VRY
163000020516     C                   MOVEL     §$2VRY        TASTC
163100020516     C                   EXSR      TASPAR
163200020516     C**
163300020516     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
163400020516     C     TASSVA        IFEQ      §$2VRY
163500020516     C                   GOTO      FINE
163600020516     C                   ENDIF
163700020516     C                   ENDIF
163800020516     C***************
163900020516     C* DIROTTAMENTI      - SOLO SE RICHIESTA DA SOLA
164000020516     C***************
164100020516     C*
164200020523     C     VARAST        TAG
164300020523     C     TASSVA        IFEQ      §$2AST
164400020523     C                   MOVEL     §$2AST        TASTC
164500020516     C                   EXSR      TASPAR
164600020516     C**
164700020516     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
164800020523     C     TASSVA        IFEQ      §$2AST
164900020516     C                   GOTO      FINE
165000020516     C                   ENDIF
165100020516     C                   ENDIF
165200030403     C***************
165300030403     C* RECAPITO C/ASSEGNI- SOLO SE RICHIESTA DA SOLA
165400030403     C***************
165500030403     C*
165600030403     C     VARVRW        TAG
165700030403     C     TASSVA        IFEQ      §$2VRW
165800030403     C                   MOVEL     'G'           TASTC
165900030403     C                   EXSR      TASPAR
166000030403     C**
166100030403     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
166200030403     C     TASSVA        IFEQ      §$2VRW
166300030403     C                   GOTO      FINE
166400030403     C                   ENDIF
166500030403     C                   ENDIF
166600030527     C*
166700030527     C***************
166800030527     C* ADDEBITO INSOLUTI - INT.MORA  SOLO SE RICHIESTA DA SOLA
166900030527     C***************
167000030527     C     VARVRM        TAG
167100030527     C     TASSVA        IFEQ      §$2VRM
167200030527      * SE NON ESISTONO VARIE IMPOSTATE DO MANCA TARIFFA
167300030527     c                   MOVEA     SV            WRKSV            20
167400030527     C                   IF        WRKSV = *BLANKS
167500030527     C                   MOVEL     MSG(18)       TASMSG
167600030527     C                   MOVEL     'E'           TASERR            1
167700030527     C                   SETON                                        94
167800030527     C                   ENDIF
167900030527     C                   GOTO      FINE
168000030527     C                   ENDIF
168100030626     C***************
168200030626     C* ORM COMMISSIONATO  - SOLO SE RICHIESTA DA SOLA
168300030626     C***************
168400030626     C*
168500030626     C     VARVRT        TAG
168600030626     C     TASSVA        IFEQ      §$2VRT
168700030626     C                   MOVEL     §$2VRT        TASTC
168800030626     C                   EXSR      TASPAR
168900030626     C**
169000030626     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
169100030626     C     TASSVA        IFEQ      §$2VRT
169200030626     C                   GOTO      FINE
169300030626     C                   ENDIF
169400030626     C                   ENDIF
169500040910     C***************
169600040910     C* P.O.D IMMAGE      - SOLO SE RICHIESTA DA SOLA
169700040910     C***************
169800040910     C*
169900040910     C     VARPOD        TAG
170000040910     C     TASSVA        IFEQ      §$2POD
170100040910     C                   MOVEL     §$2POD        TASTC
170200040910     C                   EXSR      TASPAR
170300040910     C**
170400040910     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
170500040910     C     TASSVA        IFEQ      §$2POD
170600040910     C                   GOTO      FINE
170700040910     C                   ENDIF
170800040910     C                   ENDIF
170900990922     C************************
171000990922     C* CONSEGNE PARTICOLARI
171100990922     C************************
171200990722     C*
171300990922     C** CONSEGNE PARTICOLARI 1
171400941110     C     TASTC1        IFNE      *BLANKS
171500970521     C                   MOVEL     TASTC1        TASTC
171600970521     C                   EXSR      TASPAR
171700941112     C                   END
171800990922     C** CONSEGNE PARTICOLARI 2
171900941110     C     TASTC2        IFNE      *BLANKS
172000970521     C                   MOVEL     TASTC2        TASTC
172100970521     C                   EXSR      TASPAR
172200941110     C                   END
172300020826     C************************
172400020826     C* DITITTO DI PESATURA
172500020826     C************************
172600020826     C*
172700020826     C     §TATAP        IFEQ      'S'
172800020826     C                   MOVEL     '5'           TASTC
172900020826     C                   EXSR      TASPAR
173000020826     C                   END
173100990922     C************************
173200990922     C* RITIRO
173300990922     C************************
173400050504     C* LA VARIA "U" DI RITIRO  E'
173500050504     C** DA CALCOLARE: SUI PORTI ASSEGNATI
173600050504     C**               SUI PORTI FRANCHI NON CODIFICATI
173700050504     C**                   CON FLAG CONSEGNA A MAGAZZINO = A BLANK
173800050504     C**               SULLE BOLLE EXPORT ASSEGNATO NON DPD
173900050504     C**               SULLE BOLLE IMPORT ASSEGNATO TUTTE (ANCHE DPD)
174000990915     C* FRANCO
174100000919     C*!!*       TIPBOL    IFNE 'A'
174200000919     C*!!*       TASFDN    ANDEQ'S'
174300000919     C*!!*                 GOTO ENDRIT
174400000919     C*!!*                 ENDIF
174500990915     C*
174600990915     C* FRANCO CON CODICE CLIENTE UGUALE A 8888 OPPURE UGUALE ALLA TARIFFA DI
174700990915     C* CARTELLO
174800000919     C     TIPBOL        IFNE      'A'
174900000919     C     KSCCOD        ANDNE     8888
175000020417     C****       TASKSC    ANDNECARKSC
175100020417     C* SE NON SI TRATTA NEMMENO DI UNA TARIFFA DI CARTELLO NON TASSO
175200020417     C     TASKSC        LOOKUP    TAC                                    15
175300020417     C  N15              GOTO      ENDRIT
175400000919     C                   ENDIF
175500990930     C** DA CALCOLARE SOLO PER PORTI ASSEGNATI RITIRATI
175600000919     C*!!*       TIPBOL    CABNE'A'       ENDRIT
175700990930     C*
175800970213     C*
175900990914     C* SE BOLLA ASSEGNATA EXPORT --> NON CONSIDERO IL FLAG CONSEGNA A MAGAZZINO
176000990914     C*                               PERCHE' DEVE SEMPRE CALCOLARE LA VARIA "U"
176100970213     C     TASFDN        IFEQ      'S'
176200020319     C* PER ASSEGNATO EXPORT --> SEMPRE  (ANCHE DPD CHE FINO A OGGI
176300050504     C*                          ESCLUDEVA 4/4/02--> RIESCLUDO DPD)
176400050504     C* PER ASSEGNATO IMPORT --> SEMPRE  (ANCHE DPD 04/05/05)
176500050504     C*
176600001204     C* PER ALTRI ASSEGNATI  --> SOLO SE NON E' PORTATA A MAGAZZINO
176700001204     C     TIPBOL        IFEQ      'A'
176800001204     C*
176900980605     C     TASLNA        LOOKUP    EST                                    05
177000050504     c
177100050504     C  n05TASLNP        LOOKUP    ESTtot                                 05
177200050504     c
177300980605     C  N05              GOTO      ENDRIT
177400001204     C                   ELSE
177500001204     C* PER 8888 --> NON APPLICARE SE PORTATA A MAGAZZINO SIA PER EXP
177600001204     C*              CHE PER LE ALTRE SPEDIZIONI
177700001204     C                   GOTO      ENDRIT
177800970213     C                   ENDIF
177900001204     C                   ENDIF
178000060720      *----------------------------------------------------------------------------*
178100060720      * In caso di bolla di reso non si può NON CALCOLARE la varia U in quanto     *
178200060720      * questa serve per far pagare la tratta int.le della spedizione.             *
178300060720      * Infatti quando in Aprile 2006 è stata tolta la signora Villa ha reclamato. *
178400060720      * asterisco le seguenti specifiche                                           *
178500060720     c*** Per bolla import, non applico varia "U" se bolla di reso                 *
178600060720     C***     TASLNP        LOOKUP    ESTtot                                 05    *
178700060720     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
178800060720     C***                   GOTO      ENDRIT                                       *
178900060720     c***                   endif                                                  *
179000060720     c***                                                                          *
179100060720     c*** Per bolla EXport, non applico varia "U" se bolla di reso                 *
179200060720     C***     TASLNA        LOOKUP    EST                                    05    *
179300060720     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
179400060720     C***                   GOTO      ENDRIT                                       *
179500060720     c***                   endif                                                  *
179600060720      *----------------------------------------------------------------------------*
179700060424     c
179800970213     C**
179900970521     C* TARIFFA PARTICOLARE --> U
180000941121     C                   MOVEL     'U'           TASTC
180100970521     C**
180200110617     C** 17/06/2011 LB DN visto che sono state cambiate le tariffe della cartello
180300110617     c** per la tariffa particolare di ritiro per l'Estero e non essendo + uguale
180400110617     c** a quelle per l'italia in caso di prepagati (quindi non codificati) per la GB
180500110617     c** la tariffa di ritiro diventa di 50 euro circa rispetto i 5 euro circa di prima ?
180600110617     c** quindi risulta necessario correggere un errore che ci portiamo dietro da anni
180700110617     c** che tassiamo controllando il codice tassazione mittente solo per gli assegnati
180800110617     c** invece si deve fare anche per i poveri franchi (prepagati) per i quali viene
180900110617     c** calcolato il ritiro. Quindi sia per i franchi che assegnati il codice tassazione
181000110617     c** per la tariffa particolare di ritiro diventa il codice tassazione mittente
181100110617     c** come è giusto che venga tassata da dove viene ritirata e non dove viene
181200110617     c** consegnata la merce. A.G.
181300110617     C**   TIPBOL        IFEQ      'A'
181400970521     C                   MOVEL     TASCTS        WCTS
181500970521     C                   MOVEL     TASMCT        TASCTS
181600110617     C**                 END
181700970521     C*
181800970521     C                   EXSR      TASPAR
181900970521     C*
182000110617     C**   TIPBOL        IFEQ      'A'
182100970521     C                   MOVEL     WCTS          TASCTS
182200110617     C**                 END
182300941121     C     ENDRIT        TAG
182400990922     C************************
182500990922     C* SI DDT
182600031104      * Tutti i pgm che richiamano il TNSF20R x tassare le bolle arrivi
182700031104      * testano il flag DDT e passano 'S' nel caso di DDT SI, in questi pgm ho aggiunto anche i
182800031104      * nuovi flag 'P' e 'Q'
182900031104      * l'unico che non lo fa è FNLG20R così ho aggiunto anche qua il controllo con i 2 nuovi flag
183000031104      * 'P' è uguale a 'S' e viene impostato in arrivo quando si stampa il ddt si
183100031104      * 'Q' è uguale a 'C' e viene impostato in arrivo quando si stampa il ddt si
183200990922     C************************
183300970409     C     TASDDT        IFEQ      'S'
183400031104     C     TASDDT        orEQ      'P'
183500031104     C     TASDDT        orEQ      'Q'
183600970521     C* TARIFFA PARTICOLARE --> B
183700970521     C                   MOVEL     'B'           TASTC
183800970521     C                   EXSR      TASPAR
183900970409     C                   END
184000030203      ***************
184100030203      * BANCALI
184200030203      ***************
184300030203     C*
184400030203      * Non si tassano i bancali per le bolle in porto assegnato
184500030203     c                   If        TasBan > *Zeros and TipBol <> 'A'
184600030203     c                   Movel     §$2Vba        TASTC
184700030203     c                   ExSr      TasPar
184800030203     c                   EndIf
184900001205     C*
185000040506      ************************************************
185100040506      * SUPPLEMENTO CARBURANTE SPEDIZIONI FEDEX
185200040506      ************************************************
185300040506     C                   IF        WBOFED = 'S'
185400060905     C                   EXSR      SR_SUPCARB
185500060905     C                   ELSE
185600060905      ************************************************
185700060905      * FUEL SURCHARGE  SPEDIZIONI NO FEDEX
185800060905      ************************************************
185900060905     C                   EXSR      SR_FUEL
186000040506     C                   ENDIF
186100040506      *
186200941110     C                   SETOFF                                       49
186300941110     C                   ENDSR
186400060414     C*****************************************************************
186500090922     C** Cerco la posizione per la varia della maggiorazione
186600060414     C*****************************************************************
186700060414     c     CerESPRESSO   BEGSR
186800060414     c                   if        esp=0
186900060414     c                   clear                   giaespr           1
187000090923     c                   clear                   wVariaMag         1
187100060414     C                   Z-ADD     1             A
187200090922
187300090922     c* Maggiorazione per tipo servizio E-priority
187400090922     c                   select
187500090922     c                   when      tastsp='E'
187600090922     c                   eval      wvariaMag=§$2tsp
187700090922
187800090922     c* Maggiorazione per tipo servizio H-H1030
187900090922     c                   when      tastsp='H'
188000090922     c                   eval      wvariaMag=§$2tsh
188100090922     c                   endsl
188200090922
188300090922     C     wvariaMag     LOOKUP    SV(a)                                  96
188400090922     c* gia esiste la varia della maggioraz --> non la calcolo
188500060414     c                   if        *in96
188600060414     c                   z-add     a             ESP               3 0
188700060414     c                   eval      giaespr='S'
188800060414     c                   else
188900060414     C                   Z-ADD     1             A
189000060414     C     ' '           LOOKUP    SV(A)                                  96
189100060414     c   96              z-add     a             ESP               3 0
189200060414     C  n96              ADD       999           ESP
189300060414     c                   endif
189400060414     c                   endif
189500060414     c                   ENDSR
189600980508     C*****************************************************************
189700980508     C** VALORE COL QUALE  MOLTIPLICARE LA TARIFFA
189800980508     C*****************************************************************
189900980508     C     CALQLI        BEGSR
190000991013     C                   Z-ADD     0             PESQLI           14 6
190100980508     C** SE LO SCAGLIONE SCELTO PER TASSARE E' INFERIORE AL MINIMO
190200980508     C** TASSABILE DA TARIFFA FORZO PESO TASSABILE = MINTAS
190300980512     C**
190400980512     C* WATA MI IDENTIFICA SE L'APPLICAZIONE TARIFFA FINITA E' DOVUTA
190500980512     C*  ALLO SCAGLIONE CONFRONTATO CON MINTAR (R)
190600980512     C*  O SE E' DOVUTA DAL PESO TASSABILE CONFRONTATO COL MINIMO
190700980515     C*  TASSABILE DI QUELLA TARIFFA (S) - differenza che ora non serve
190800980515     C*  + ma dal momento che c'e' la lascio
190900980508     C                   CLEAR                   WATA
191000980508    1C                   SELECT
191100980508     C     TSCG          WHENLE    MINTAR
191200980508     C                   Z-ADD     1             PESQLI
191300980514     C                   MOVEL     'R'           WATA              1
191400980508     C**
191500980515     C** SE PESO TASSABILE E' ANCORA INFERIORE
191600980514     C*  AL MINIMO TASSABILE DA TARIFFA IMPOSTO PESO TASS = 1
191700980515     C*  NON LO CONTROLLO PER LA TARIFFA A SCALARE PERCHE' ABBIAMO
191800980515     C*  SEMPRE INSERITO UNO SCAGLIONE=A MINIMO TASSABILE E
191900980515     C*  QUINDI E' GIA' CONTROLLATO NEL WHLE PRECEDENTE
192000980515     C     WPTAS3        WHENLE    MINTAS
192100980515     C     §TAF02        ANDNE     'S'
192200980508     C                   Z-ADD     1             PESQLI
192300980508     C                   MOVEL     'S'           WATA
192400980508   X1C                   OTHER
192500980508     C** CALCOLO : PESO IN Q.LI PER TARIFFA A PESO
192600980508     C** CALCOLO   VALORE/100   PER TARIFFA A VALORE (ESSENDO UNA %)
192700980508     C** PER LE ALTRE TARIFFE PESO TASSABILE INVARIATO
192800980508    2C     UNOCTR        IFEQ      0
192900980508     C     UNOCTR        OREQ      4
193000980508     C     WPTAS3        DIV       100           PESQLI
193100980508   X2C                   ELSE
193200980508     C                   Z-ADD     WPTAS3        PESQLI
193300980508    2C                   END
193400980508    1C                   ENDSL
193500980508     C                   ENDSR
193600980508     C*****************************************************************
193700980508     C* MOLTIPLICATO TARIFFA PER VALORE TROVATO (PESO TASSABILE)
193800980508     C*****************************************************************
193900980508     C     MULTAR        BEGSR
194000980508     C     *LIKE         DEFINE    TASPOR        WPOR
194100980508     C     *LIKE         DEFINE    TASINL        WINL
194200980518     C                   CLEAR                   WPOR
194300980518     C                   CLEAR                   WINL
194400980508     C**
194500980508     C     TITR          MULT(H)   PESQLI        WPOR                           ** PORTO
194600980508     C**
194700980508     C** SE TROVATA TARIFFA DEL PAESE NON SI APPLICA MAI INOLTRO
194800110624     C*****  N49FLGINO        IFNE      'C'
194900110624     C** Sostituisco il controllo del flag inoltro diverso da C=città ma
195000110624     c** anche da "T" Città centro storico
195100110624     C                   IF        NOT *IN49  AND
195200110624     C                             FLGINO <> 'C' AND FLGINO <> 'T'
195300980508     C     TINO          MULT(H)   PESQLI        WINL                           ** INOLTRO
195400980508     C                   END
195500980508     C                   ENDSR
195600980508     C*****************************************************************
195700980508     C* CALCOLO TARIFFA A SCALARE
195800980508     C*****************************************************************
195900980508     C     SCALAR        BEGSR
196000980508     C**
196100980514     C     *LIKE         DEFINE    PESTAS        WPTAS1
196200980508     C     *LIKE         DEFINE    PESTAS        WDIFSC
196300980508     C     *LIKE         DEFINE    PESTAS        WPTAS3
196400980511     C     *LIKE         DEFINE    PESQLI        WPESQ
196500980511     C     *LIKE         DEFINE    PESQLI        WPESSV
196600980511     C     *LIKE         DEFINE    TITR          WTITR
196700980511     C     *LIKE         DEFINE    TITR          WSTITR
196800980511     C     *LIKE         DEFINE    TINO          WTINO
196900980511     C     *LIKE         DEFINE    TINO          WSTINO
197000980508     C     *LIKE         DEFINE    TSCG          WPRESC
197100980508     C**
197200980508     C                   CLEAR                   TASPOR
197300980508     C                   CLEAR                   TASINL
197400980508     C                   CLEAR                   WTROVA
197500980508     C                   CLEAR                   WPTAS3
197600980508     C                   CLEAR                   WDIFSC
197700980508     C                   CLEAR                   WPRESC
197800980511     C                   CLEAR                   WPESSV
197900980511     C                   CLEAR                   WPESQ
198000980511     C                   CLEAR                   WTITR
198100980511     C                   CLEAR                   WSTITR
198200980511     C                   CLEAR                   WTINO
198300980511     C                   CLEAR                   WSTINO
198400980508     C                   Z-ADD     PESTAS        WPTAS1
198500980508     C                   Z-ADD     1             A
198600980508     C**
198700020218    1C     A             DOWLE     200
198800980508     C     WTROVA        ANDEQ     ' '
198900980508     C     TSCG          ANDGT     0
199000980508     C     A             OCCUR     TARDS
199100980512     C**
199200980508     C* FACCIO LA DIFFERENZA TRA LO SCAGLIONE LETTO ED IL PRECEDENTE
199300980508     C     TSCG          SUB       WPRESC        WDIFSC
199400980508     C**
199500980508     C* SOTRAGGO DAL PESO TASSABILE RIMASTO LA PARTE DA TASSARE ORA
199600980508     C* IN WPTAS1 HO SEMPRE LA PARTE DI PESO TASSABILE ANCORA DA TASSAR
199700980508     C                   SUB       WDIFSC        WPTAS1
199800980508     C**
199900980508     C** SE IL VALORE E' DIVENTATO < DI 0 SIGNIFICA CHE HO TERMINATO LA
200000980508     C**  TASSAZIONE PER CUI USO SOLO LA PARTE RIMASTA DI WPTAS1
200100980508    2C     WPTAS1        IFLE      0
200200980511     C     WDIFSC        ADD       WPTAS1        WPTAS3
200300980514     C* PESO TASSABILE GLOBALE DELLA SPEDIZIONE
200400980508     C                   MOVEL     'S'           WTROVA            1
200500980508     C                   ELSE
200600980508     C**
200700980508     C* PARTE DI PESO DA TASSARE CON QUESTO SCAGLIONE
200800980508     C                   Z-ADD     WDIFSC        WPTAS3
200900980508     C* SALVO LO SCAGLIONE PRECEDENTE
201000980508     C                   Z-ADD     TSCG          WPRESC
201100980508    2C                   ENDIF
201200980508     C**
201300980508     C* IMPOSTO IL VALORE DA MOLTIPLICARE PER LA TARIFFA
201400980508     C                   EXSR      CALQLI
201500980512     C**
201600980511     C* SE LO SCAGLIONE E' APPLICAZIONE TARIFFA FINITA DEVO CONSIDERARE
201700980512     C*  SOLO L'ULTIMO SCAGLIONE COSI' E NON I PRECEDENTI
201800980514    2C     WATA          IFNE      ' '
201900980511     C     WTROVA        ANDEQ     ' '
202000980511     C* MI SALVO IL VALORE MA PER ORA NON LO MOLTIPLICO
202100980511     C                   Z-ADD     PESQLI        WPESQ
202200980511     C                   Z-ADD     TITR          WTITR
202300980511     C                   Z-ADD     TINO          WTINO
202400980511   X2C                   ELSE
202500980511     C**
202600980511     C* SE LO SCAGLIONE TROVATO E' L'ULTIMO MA COMUNQUE TARIFFA FINITA
202700980511     C* DEVO MOLTIPLICARE SOLO QUESTO --> AZZERO UN EVENTUALE PRECEDENT
202800980511     C*  CHE AVEVA LA TASSAZIONE TARIFFA FINITA
202900980511    3C     WTROVA        IFEQ      'S'
203000980514     C     WATA          ANDNE     ' '
203100980511     C                   CLEAR                   WPESQ
203200980511     C                   CLEAR                   WTITR
203300980511     C                   CLEAR                   WTINO
203400980511    3C                   ENDIF
203500980511     C*
203600980511     C* PRIMA DI MOLTIPLICARE LA TARIFFA TROVATA VEDO SE CE NE E'
203700980511     C*  UNA PRECEDENTE TARIFFA FINITA DA CONSIDERARE
203800980511    3C     WPESQ         IFGT      0
203900980511     C* SALVO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
204000980511     C*  APPENA LETTO
204100980511     C                   Z-ADD     PESQLI        WPESSV
204200980511     C                   Z-ADD     TITR          WSTITR
204300980511     C                   Z-ADD     TINO          WSTINO
204400980511     C* IMPOSTO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
204500980511     C*  PRECEDENTE PER IL CALCOLO
204600980511     C                   Z-ADD     WPESQ         PESQLI
204700980511     C                   Z-ADD     WTITR         TITR
204800980511     C                   Z-ADD     WTINO         TINO
204900980511     C                   EXSR      MULTAR
205000980511     C                   ADD       WPOR          TASPOR
205100980511     C                   ADD       WINL          TASINL
205200980511     C                   CLEAR                   WPESQ
205300980511     C                   CLEAR                   WTITR
205400980511     C                   CLEAR                   WTINO
205500980511     C* REIMPOSTO I DATI DELLO SCAGLIONE APPENA LETTO
205600980511     C                   Z-ADD     WPESSV        PESQLI
205700980511     C                   Z-ADD     WSTITR        TITR
205800980511     C                   Z-ADD     WSTINO        TINO
205900980511    3C                   ENDIF
206000980511     C**
206100980508     C* MOLTIPLICAZIONE TARIFFA
206200980508     C                   EXSR      MULTAR
206300980508     C                   ADD       WPOR          TASPOR
206400980508     C                   ADD       WINL          TASINL
206500980511    2C                   ENDIF
206600980515     C                   ADD       1             A
206700980508     C**
206800980508    1C                   ENDDO
206900980515     C**
207000980515     C* APPLICO MINIMO E MASSIMO
207100980515     C                   Z-ADD     TASPOR        WPOR
207200980515     C                   Z-ADD     TASINL        WINL
207300980515     C                   EXSR      MINMAX
207400980515     C                   Z-ADD     WPOR          TASPOR
207500980515     C                   Z-ADD     WINL          TASINL
207600980508     C                   ENDSR
207700980512     C*****************************************************************
207800980512     C* CALCOLO MAX SCAGLIONE PRECEDENTE
207900980512     C*****************************************************************
208000980512     C     MAXSCA        BEGSR
208100980513     C* SOLO SE LO SCAGLIONE APPLICATO NON ERA IL PRIMO
208200980513     C     WA            IFGT      1
208300980512     C* PER CONFRONTARLO CON IL VALORE NORMALE CALCOLATO
208400980512     C* IL CONFRONTO E' DATO DALLA SOMMA TRA PORT E INOLTRO
208500980512     C                   SUB       1             WA
208600980512     C     WA            OCCUR     TARDS
208700980512     C* CALCOLO PESO DA MOLTIPLICARE
208800980512     C                   Z-ADD     TSCG          WPTAS3
208900980512     C                   EXSR      CALQLI
209000980515     C** MOLTIPLICO LA TARIFFA
209100980512     C                   EXSR      MULTAR
209200980515     C** MINIMO E MASSIMO
209300980515     C                   EXSR      MINMAX
209400980515     C**
209500990923     C     TASPOR        ADD       TASINL        T0100            15 3
209600990923     C     WPOR          ADD       WINL          W0100            15 3
209700980515     C** PRENDO DELLE 2 LA TARIFFA PIU' ALTA
209800980512     C     W0100         IFGT      T0100
209900980512     C                   Z-ADD     WPOR          TASPOR
210000980512     C                   Z-ADD     WINL          TASINL
210100980512     C                   ENDIF
210200980513     C                   ENDIF
210300980512     C**
210400980512     C                   ENDSR
210500980515     C*****************************************************************
210600980515     C** MINIMO E MASSIMO DA APPLICARE -SE APPLICATO TUTTO NEL PORTO
210700980515     C*****************************************************************
210800980515     C     MINMAX        BEGSR
210900980515     C** NON ESISTE MINIMO E MASSIMO
211000980515     C     TMIN          IFEQ      0
211100980515     C     TMAX          ANDEQ     0
211200980515     C                   GOTO      DOPOMM
211300980515     C                   END
211400980515     C** MINIMO
211500990923     C     WPOR          ADD       WINL          WMAX             15 3
211600980515     C     TMIN          IFGT      WMAX
211700980515     C                   Z-ADD     TMIN          WPOR
211800980515     C                   Z-ADD     0             WINL
211900980515     C                   Z-ADD     TMIN          WMAX
212000980515     C                   END
212100980515     C** MASSIMO
212200980515     C     TMAX          IFGT      0
212300980515     C     WMAX          IFGT      TMAX
212400980515     C                   Z-ADD     TMAX          WPOR
212500980515     C                   Z-ADD     0             WINL
212600980515     C                   END
212700980515     C                   END
212800980515     C     DOPOMM        ENDSR
212900980429     C*****************************************************************
213000980429     C* CERCO VARIA ISOLA LOCALITA' DISAGIATA CENTRI STORICI
213100090922     c* cerco anche la maggiorazione
213200980429     C*****************************************************************
213300980429     C     CERISO        BEGSR
213400980429     C                   CLEAR                   VAISO
213500060414     C                   CLEAR                   VATSP
213600140203      * vaaggfuel varie che vengono sommate solo all'imponibile fuel e supplemento carburante
213700140203     C                   CLEAR                   VAAGGFUEL
213800980429     C     *LIKE         DEFINE    TASVA1        VAISO
213900060414     C     *LIKE         DEFINE    TASVA1        VATSP
214000140203     C     *LIKE         DEFINE    TASVA1        VAAGGFUEL
214100981020     C                   Z-ADD     1             CC                3 0
214200980429     C     §$2VRJ        LOOKUP    SV(CC)                                 96     ISOLA
214300980429     C   96              ADD       VA(CC)        VAISO
214400981008     C                   Z-ADD     1             CC
214500980429     C     §$2VRK        LOOKUP    SV(CC)                                 96     DISAGIATE
214600980429     C   96              ADD       VA(CC)        VAISO
214700981008     C                   Z-ADD     1             CC
214800980429     C     §$2VRQ        LOOKUP    SV(CC)                                 96     C.STORICI
214900980429     C   96              ADD       VA(CC)        VAISO
215000060414     C                   Z-ADD     1             CC                3 0
215100090923     c* "E" o "H"
215200090923     c                   if        WvariaMag<>' '
215300140203     C     WVariaMag     LOOKUP    SV(CC)                                 96     maggiorazione e o h
215400060414     C   96              z-add     VA(CC)        VAtsp
215500090923     c                   endif
215600140203      * CALCOLO LE VARIE  AGGIUNTIVE AL SOLO IMPONIBILE FUEL
215700140203     C                   Z-ADD     1             CC                3 0
215800140203     C     '='           LOOKUP    SV(CC)                                 96     RESO BANCALI
215900140203     C   96              ADD       VA(CC)        VAAGGFUEL
216000140203     C                   Z-ADD     1             CC                3 0
216100140203     C     'c'           LOOKUP    SV(CC)                                 96     LASCIATO AVVISO
216200140203     C   96              ADD       VA(CC)        VAAGGFUEL
216300140203     C                   Z-ADD     1             CC                3 0
216400140203     C     'A'           LOOKUP    SV(CC)                                 96     APPUNTAMENTO
216500140203     C   96              ADD       VA(CC)        VAAGGFUEL
216600140203     C                   Z-ADD     1             CC                3 0
216700140203     C     'B'           LOOKUP    SV(CC)                                 96     CONSEGNA DDT
216800140203     C   96              ADD       VA(CC)        VAAGGFUEL
216900140203     C                   Z-ADD     1             CC                3 0
217000140203     C     'C'           LOOKUP    SV(CC)                                 96     FACCH. ARR.
217100140203     C   96              ADD       VA(CC)        VAAGGFUEL
217200140203     C                   Z-ADD     1             CC                3 0
217300140203     C     'F'           LOOKUP    SV(CC)                                 96     FUORI MISURA
217400140203     C   96              ADD       VA(CC)        VAAGGFUEL
217500140203     C                   Z-ADD     1             CC                3 0
217600140203     C     'H'           LOOKUP    SV(CC)                                 96     DIRITTO FISSO
217700140203     C   96              ADD       VA(CC)        VAAGGFUEL
217800140203     C                   Z-ADD     1             CC                3 0
217900140203     C     'I'           LOOKUP    SV(CC)                                 96     SPESE DI GIACENZA
218000140203     C   96              ADD       VA(CC)        VAAGGFUEL
218100140203     C                   Z-ADD     1             CC                3 0
218200140203     C     'N'           LOOKUP    SV(CC)                                 96     ANTEPORTO
218300140203     C   96              ADD       VA(CC)        VAAGGFUEL
218400140203     C                   Z-ADD     1             CC                3 0
218500140203     C     'P'           LOOKUP    SV(CC)                                 96     AI PIANI
218600140203     C   96              ADD       VA(CC)        VAAGGFUEL
218700140203     C                   Z-ADD     1             CC                3 0
218800140203     C     'S'           LOOKUP    SV(CC)                                 96     SUPERMERCATO
218900140203     C   96              ADD       VA(CC)        VAAGGFUEL
219000140203     C                   Z-ADD     1             CC                3 0
219100140203     C     'U'           LOOKUP    SV(CC)                                 96     RITIRO
219200140203     C   96              ADD       VA(CC)        VAAGGFUEL
219300140203     C                   Z-ADD     1             CC                3 0
219400140203     C     'X'           LOOKUP    SV(CC)                                 96     CONS.DISAG.
219500140203     C   96              ADD       VA(CC)        VAAGGFUEL
219600141215     C                   Z-ADD     1             CC                3 0
219700141215     C     'p'           LOOKUP    SV(CC)                                 96     PIN CODE
219800141215     C   96              ADD       VA(CC)        VAAGGFUEL
219900090923     c
220000150420     C     'z'           LOOKUP    SV(CC)                                 96     EXPO
220100150420     C   96              ADD       VA(CC)        VAAGGFUEL
220101151119     c
220102151119     C     't'           LOOKUP    SV(CC)                                 96     Diritto Pren.ORM
220103151119     C   96              ADD       VA(CC)        VAAGGFUEL
220104151119     c
220105151119     C     'k'           LOOKUP    SV(CC)                                 96     Packing List
220106151119     C   96              ADD       VA(CC)        VAAGGFUEL
220200150420     c
220300980429     C                   ENDSR
220400060905     C*****************************************************************
220500060905     C* Fuel Surcharge
220600060905     C*****************************************************************
220700060905     c     SR_fuel       BEGSR
220800060906     C                   CLEAR                   SCA_PB
220900060906     C                   CLEAR                   SCA_SPE
221000060906     C                   CLEAR                   SCA_SCA
221100060905      *
221200060907      * verifico se esiste già la varia non la calcolo
221300060905      *
221400060905     c     §$2VFS        lookup    sv                                     96
221500060907     c                   IF        %found
221600060907     c                   goto      Endfuel
221700060907     c                   endif
221800060905
221900060905      * verifico se cliente ha in tariffa il calcolo del FUEL Surcharge
222000060907      * altrimenti non vado a cercare nella cartello e vado a fine
222100060906     C                   MOVEL     'f'           FISO
222200060906     C                   MOVEL     'f'           WFISO
222300060905
222400060906     c                   z-add     1             a
222500060906     c     Wfiso         Lookup    Cp(a)                                  05
222600060906     C  N05              GOTO      ENDFUEL
222700060906      * aggancio la ds della tabella 1P
222800060906     c     A             Occur     DS1P
222900060906
223000060905     C     KISOT         CHAIN     TITPT01L
223100060907      * cliente non ha la tariffa  non faccio calcolo
223200060907     C                   IF        not %FOUND(TITPT01L)
223300060907     c                   goto      Endfuel
223400060907     c                   endif
223500140224      * ds x recupero % minima applicabile
223600140224     c                   eval      DTPT01 = TPTflo
223700140224      *
223800140224      * data prezzo base maggiore della data spedizone non faccio calcolo
223900060907     C                   IF        TPTDPB > TASDSP
224000060907     c                   goto      Endfuel
224100060907     c                   endif
224200140224      * Ricerco il valore minimo applicabile e lo scaglione corrispondente in vigore
224300140224      * alla data spedizione
224400150217      * solo se in tariffa non è stato specificato che non si deve applicare il VMA
224500140224
224600150217     c                   If        §TPTvma = 'N'
224700150217     c                   clear                   dpbvma
224800150217     c                   clear                   dpbsvm
224900150217     c                   else
225000150217      * si applicazione VMA
225100140224     C     TASDSP        SETLL     TIDPB01L
225200140224     C                   READ      TIDPB01L
225300140224     c                   if        not %found(tidpb01l)
225400140224     c                   clear                   dpbvma
225500140224     c                   clear                   dpbsvm
225600140224     c                   endif
225700150217     c                   endif
225800060906
225900060905      * Ricerco scaglione di appartenenza del prezzo base
226000060905
226100060911     C     TPTDPB        SETLL     TIPMG01L
226200060906     C                   READ      TIPMG01L
226300060906     C                   IF        NOT %EOF(TIPMG01L)
226400060906      *
226500060906     C                   Z-ADD     PMGSCA        SCA_PB
226600060906     C                   ENDIF
226700060906
226800060906      * Ricerco scaglione di appartenenza del prezzo del gasolio al momento della spedizione
226900060906
227000060911     C     TASDSP        SETLL     TIPMG01L
227100060906     C                   READ      TIPMG01L
227200060906     C                   IF        NOT %EOF(TIPMG01L)
227300060906      *
227400080909      * recupero il prezzo per ricercare il F.D. in tariffa
227500080909      *
227600080909     c                   z-add     pmgpmg        PMG_SGL
227700080909
227800060906     C                   Z-ADD     PMGSCA        SCA_SPE
227900060906     C                   ENDIF
228000140224      * verifico se il prezzo del gasolio al momento della spedizione è minore del valore minimo
228100140224      * applicabile sostituisco il prezzo del gasolio e il suo scaglione con il valore minimo
228200140224      * applicabile e lo scaglione corrispondente
228300140224
228400140224     c                   if        pmgpmg < dpbvma
228500140224     c                   z-add     dpbvma        PMG_SGL
228600140224     c                   z-add     dpbsvm        SCA_SPE
228700140224     c                   Endif
228800060906
228900060906      * calcolo gli scaglioni di aumento
229000060906
229100060906     C                   EVAL      SCA_SCA = SCA_SPE - SCA_PB
229200060906     C                   IF        SCA_SCA < 0
229300060906     C                   CLEAR                   SCA_SCA
229400060906     C                   ENDIF
229500140228      * calcolo dell'importo da addebitare al cliente
229600140228
229700140228     C                   CLEAR                   VARIA
229800140228     C                   CLEAR                   VARIAfuel_min
229900140228     C                   CLEAR                   IMP_SCA
230000140228
230100140228      * calcolo imponibile FUEL
230200140228      * recupero importi di isola e località disagiate ed espresso + nuove varie introdotte
230300140228      * dal 3/2/2014 dv 2473
230400140228     C                   EXSR      CERISO
230500140228      *
230600140228     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
230700140228     c                                       VAAGGFUEL
230800060906
230900060906      * se gli scaglioni scattati sono maggiore di zero cerco l'incidenza percentuale di
231000060906      * aumento nel dettaglio tariffario
231100140228    1C                   IF        SCA_SCA > 0
231200080909      * cambia con il progetto 666 aumento supplemento carburante in tariffa
231300080909      * per cercare il Fattore demoltiplicatore non usiamo più il numero di scaglioni
231400080909      * di aumento ma il prezzo medio del gasolio della settimana della data di spedizione
231500080909     C****               Z-ADD     SCA_SCA       TPDPES
231600080909     C                   Z-ADD     PMG_SGL       TPDPES
231700060906     C                   EXSR      CALACD
231800060907      * Se la percentuale di incidenza è maggiore di zero
231900140228    2C                   IF        AITR > 0
232000060905      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
232100060905      *                     per abbandonare il concetto di monete diverse dall'EURO)
232200060907     C                   EVAL(H)   VARIA = (IMP_SCA * aitr * sca_sca) / 100
232300140224
232400140228    2c                   Endif
232500140228    1c                   Endif
232600140224      * anche se il cliente ha negato la tariffa verifico
232700140224      * se nella tariffa particolare del cliente è stato espressa la % minima di apllicazione
232800140224      * calcolo l'importo del fuel utilizzando questa percentuale
232900140224
233000140224     c                   if        §tptpma > 0
233100140224     c                   eval(H)   variafuel_min =  (IMP_SCA * §TPTpma)/ 100
233200140224     c                   Endif
233300140224
233400140224      * prendo il valore della varia maggiore tra quella calcolata con la % minima applicabile
233500140224      * e quella calcolata con il FD della tariffa
233600140224
233700140224     c                   if        variafuel_min > Varia
233800140224     c                   eval      Varia =  variafuel_min
233900140224     c                   endif
234000140224
234100060905      **
234200140224    1c                   If        Varia > 0
234300060905     C                   Z-ADD     1             A
234400060905     C     ' '           LOOKUP    SV(A)                                  96
234500060907     C   96              MOVEL     §$2VFS        SV(A)
234600060905     C   96              Z-ADD     VARIA         VA(A)
234700060905     C  N96              ADD       VARIA         TASPOR
234800060905    1C                   ENDIF
234900060905      *
235000060905      *
235100060906     C     ENDFUEL       ENDSR
235200040506     C*****************************************************************
235300060905     C* SUPPLEMENTO CARBURANTE FEDEX
235400040506     C*****************************************************************
235500060905     C     SR_SUPCARB    BEGSR
235600040507      *
235700040507      * verifico se esiste già la varia
235800040507      *
235900040507     c     §$2VSC        lookup    sv                                     96
236000040507     c                   IF        not %found
236100040506
236200040506      * RICERCO IN BASE ALLA DATA SPEDIZIONE LA GIUSTA PERCENTUALE DI
236300040506      * SUPPLEMNENTO
236400040506
236500040507     C                   CLEAR                   VARIA
236600040507     C                   CLEAR                   IMP_SCA
236700040507     C                   CLEAR                   PER_SCA
236800040507     C                   Z-ADD     1             KX
236900040507    1c                   DO        *HIVAL
237000040507      * SCHIERA IN ORDINE DISCENDENTE
237100040507     C                   MOVE      DSCA(KX)      WDSPSC
237200040507      * SE DATA UGUALE A ZERO VADO A FINE
237300040507    2C                   IF        DS_DSC = *ZEROS
237400040507     C                   LEAVE
237500040507    2C                   ENDIF
237600040507      * SE DATA SPEDIZIONE MAGGIORE DI QUELLA IN SCHIERA RECUPERO LA PERCENTUALE DI CALCOLO
237700040510    2C                   IF        TASDSP >=  DS_DSC
237800040507     c                   z-add     DS_psc        PER_SCA
237900040507     C                   LEAVE
238000040507     C                   ELSE
238100040507      * SE DATA MINORE LEGGO LA DATA SUCCESSIVA
238200040507    3C                   IF        KX < 999
238300040507     C                   ADD       1             KX
238400040507     C                   ELSE
238500040507     C                   LEAVE
238600040507    3C                   ENDIF
238700040507    2C                   ENDIF
238800040507
238900040507    1C                   ENDDO
239000040507      * SE PERCENTUALE MAGGIORE DI ZERO ESEGUO IL CALCOLO
239100040507    1C                   IF        PER_SCA > 0
239200040507      * CALCOLO L'IMPONIBILE
239300060414      * recupero importi di isola e località disagiate ed espresso
239400040507     C                   EXSR      CERISO
239500040507      *
239600060414     c* Di fatto qui l'espresso non ci sarà mai!!! bolla estera
239700140203     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
239800140203     c                                       VAAGGFUEL
239900040507      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
240000040507      *                     per abbandonare il concetto di monete diverse dall'EURO)
240100040507     C                   EVAL(H)   VARIA = (IMP_SCA * PER_SCA) / 100
240200040507      **
240300040507     C                   Z-ADD     1             A
240400040507     C     ' '           LOOKUP    SV(A)                                  96
240500040507     C   96              MOVEL     §$2VSC        SV(A)
240600040507     C   96              Z-ADD     VARIA         VA(A)
240700040507     C  N96              ADD       VARIA         TASPOR
240800040507    1C                   ENDIF
240900040507      *
241000040507     C                   ENDIF
241100040507      *
241200040507     C                   ENDSR
241300970521     C*****************************************************************
241400970521     C* TASSAZIONE TARIFFE PARTICOLARI
241500970521     C*****************************************************************
241600970521     C     TASPAR        BEGSR
241700970521     C     *LIKE         DEFINE    TASVA1        TASCP
241800970522     C     *LIKE         DEFINE    TASVA1        TASISO
241900970521     C     *LIKE         DEFINE    TASTC1        TASTC
242000970721     C                   CLEAR                   TASCP
242100970721     C                   CLEAR                   WNOEST
242200970721     C* SE NON ESISTE LA SIGLA DELLA VARIA O E' GIA' IN TASSAZIONE
242300970721     C*  NON RITASSO NE APPLICO L'ESPRESSO
242400970521     C**
242500970521     C                   Z-ADD     1             A
242600970521     C     TASTC         LOOKUP    CP(A)                                  05
242700970521     C  N05              GOTO      ENDPAR
242800970521     C**
242900970521     C     A             OCCUR     DS1P
243000970521     C                   Z-ADD     1             A
243100970521     C     §1PVAR        LOOKUP    SV(A)                                  05
243200970521     C* SIGLA VARIA NON PRESENTE --> CALCOLO
243300970521     C     *IN05         IFEQ      *OFF
243400970721     C                   MOVEL     'S'           WNOEST            1
243500970521     C                   EXSR      CALCP
243600970521     C**
243700970521     C     TASCP         IFGT      0
243800130927      * se sto tassando la tariffa particolare m = avviso affidamento spedizione
243900130927      * ed il flag EMD è uguale a "E" (via sms che mail) raddoppio la varia
244000130927     c                   if        §1pvar = §$2VAD and
244100130927     c                             §asemd = 'E'
244200130927     c                   eval      tascp = tascp * 2
244300130927     c                   endif
244400130927      *
244500970521     C                   Z-ADD     1             A
244600970521     C     ' '           LOOKUP    SV(A)                                  96
244700970521     C   96              MOVEL     §1PVAR        SV(A)
244800970521     C   96              Z-ADD     TASCP         VA(A)
244900970521     C  N96              ADD       TASCP         TASPOR
245000970521     C                   END
245100970521     C                   END
245200970521     C     ENDPAR        ENDSR
245300941112     C******************************************************
245400941107     C** CALCOLO PESO TASSABILE PER TARIFFA A PESO
245500941112     C******************************************************
245600941107     C     CALPT         BEGSR
245700980313     C** MI MEMORIZZO SU BLTAS IL PESO TASSABILE SE
245800980313     C** IL PESO E' SUPERIORE AL MINIMO TASSABILE
245900980313     C** SE ESISTE IL RAPPORTO PESO VOLUME IN TARIFFA  E VOL IN BOLLA
246000980313     C** IL PESO TASSABILE SU BLTAS E' SEMPRE ARROTONDATO AL KG SUPERIO
246100980311     C                   CLEAR                   TASPVL
246200050912     C***  UNOCTR        IFEQ      0
246300050912     C**** UNOCTR        OREQ      3
246400050912      * modifico --- il peso tassabile si stampa e si calcola nei seguenti casi :
246500050912      * Tariffa a quintale "0" il peso da fatturare maggiore del  minimo tassabile
246600050912      *                        (definito nella tabella TR = 100) oppure volume da fatturare
246700050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
246800050912      *                        tariffa maggiore di zero
246900050912      * Tariffa a kg       "3" non controllo il peso da fatturare maggiore del minimo tassabile
247000050912      *                        (definito nella tabella TR = 0,1) ma solo volume da fatturare
247100050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
247200050912      *                        tariffa maggiore di zero
247300050912     C***  TARIFFA A QUINTALE
247400050912
247500050912     C     UNOCTR        IFEQ      0
247600050912
247700020826     C     USAPKF        IFGT      MINTAS
247800950901     C     PESTAS        ADD       0,9           TASPVL
247900941107     C                   ELSE
248000160606     C     USAVLF        IFNE      0
248100980311     C     RAPPV         ANDNE     0
248200950901     C     PESTAS        ADD       0,9           TASPVL
248300980313     C                   ENDIF
248400980313     C                   ENDIF
248500980313     C                   ENDIF
248600050912
248700050912     C***  TARIFFA A KG
248800050912
248900050912     C     UNOCTR        IFEQ      3
249000050912
249100160606     C     USAVLF        IFNE      0
249200050912     C     RAPPV         ANDNE     0
249300050912     C     PESTAS        ADD       0,9           TASPVL
249400050912     C                   ENDIF
249500050912     C                   ENDIF
249600050912
249700980313     C                   ENDSR
249800950119     C******************************************************
249900950119     C** CALCOLA VALORE DA ASSICURARE PER TARIFFA A VALORE
250000950119     C******************************************************
250100950119     C     CALVAS        BEGSR
250200950119     C     *LIKE         DEFINE    TASIAS        IMPVAS
250300950119     C                   Z-ADD     0             IMPVAS
250400950119     C                   MOVEL     'C'           FISO
250500990722     C                   MOVEL     'C'           WFISO             1
250600950119     C**FLAG ASSICURAZIONE X CONTO = C
250700950119     C                   Z-ADD     1             A
250800990722     C     WFISO         LOOKUP    CP(A)                                  05
250900950119     C  N05              GOTO      DOPVA1
251000950119     C**----------------------------------------
251100990721     C     KISOT         CHAIN     TITPT01L                           28
251200980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
251300980528     C     *IN28         IFEQ      *ON
251400980528     C     TPTATB        ORNE      ' '
251500980528     C                   GOTO      DOPVA1
251600980528     C                   ENDIF
251700980528     C**
251800950119     C** ESCLUDO GLI ANNULLATI
251900950119     C     TPTFVM        IFEQ      '3'
252000020826     C     TPTVLM        MULT      USAPKF        IMPVAS
252100950119     C                   END
252200950119     C*PESO
252300950119     C     TPTFVM        IFEQ      '1'
252400950119     C     TPTVLM        MULT      TASNCL        IMPVAS
252500950119     C                   END
252600950119     C* COLLI
252700950119     C     TPTFVM        IFEQ      '2'
252800950119     C                   Z-ADD     TPTVLM        IMPVAS
252900950119     C                   END
253000950119     C*SPEDIZIONE
253100950119     C     DOPVA1        ENDSR
253200941112     C******************************************************
253300941107     C** CALCOLA VARIA R  --- ASSICURAZIONE PER CONTO
253400941112     C******************************************************
253500941107     C     CALVR         BEGSR
253600050525     c* Il falg task88 cambia significato:indica se bolla con mandato o no
253700050525     c*
253800050525     c* con mandato significa: x mandato a val variabile--> tutte le bolle
253900050525     c*                        x mandato a val.indicato --> solo bolle con
254000050525     c*                        tasias=0 o con tasias <=al valore del mandat
254100050525     c*
254200050525     c* Imposto task88=N se mandato
254300050525     c* Imposto task88=S se no mandato ma tasias>0
254400050525     c* Imposto task88=X per bolle senza mandato e senza importo
254500050525     c
254600050525     C                   MOVEL     'X'           TASK88
254700980603     C** 88 = SI CALCOLA SOLO SE ESISTE IL VALORE MERCE IN BOLLA
254800941112     C     KSCCOD        IFEQ      8888
254900980527     C     KSCCOD        OREQ      9999
255000980709     C     TASIAS        IFEQ      0
255100050525     C**                 MOVEL     'S'           TASK88
255200980709     C                   GOTO      ENDVR
255300941112     C                   END
255400980709     C                   END
255500980603     C**
255600980603     C** SE TIPO BOLLA CHE NON PREVEDE L'ASSICURAZIONE, ESCO SENZA
255700980603     C**  DARE ERRORE
255800050525     C**   TASIAS        IFEQ      0
255900050525     C**   TASTBL        LOOKUP    TBN                                    05
256000050525     C** 05              MOVEL     'S'           TASK88
256100050525     C** 05              GOTO      ENDVR
256200050525     C**                 ENDIF
256300991012     C**
256400991012     C** SE IMPORTO D'ASSICURARE MAGGIORE DI ZEROS MUOVO 'S' TASK88
256500991012     C** PER STAMPA BOLLE CON ASSICURAZIONE PER TABULATONE DELLA CONSULDANNI
256600050525     C**   TASIAS        IFGT      0
256700050525     C**                 MOVEL     'S'           TASK88
256800050525     C**                 ENDIF
256900980603     C**
257000941116     C                   SETOFF                                       94
257100941116     C     *LIKE         DEFINE    TASIAS        IASLIT
257200941116     C                   Z-ADD     TASIAS        IASLIT
257300941116     C     IASLIT        IFNE      0
257400941116     C                   Z-ADD     1             K
257500941116     C     TASVAS        LOOKUP    CV(K)                                  05
257600980504     C     *IN05         IFEQ      *OFF
257700980504     C                   SETON                                        94
257800980504     C                   MOVEL     MSG(2)        TASMSG
257900980504     C                   GOTO      FINE
258000980504     C                   ENDIF
258100941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
258200990922     C**
258300990922     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA SE E' DIVERSA
258400990922     C* DALLA VALUTA DELLA TARIFFA
258500990922     C     §TADIV        IFNE      TASVAS
258600990922     C                   CLEAR                   YEUR
258700990922     C                   MOVEL     TASVAS        YECDVI
258800990922     C                   MOVEL     §TADIV        YECDVO
258900990922     C                   Z-ADD     IASLIT        YECIMI
259000011129     C   12              MOVE      '2'           YECDCO
259100991014     C  N12              MOVE      '0'           YECDCO
259200990922     C*
259300990922     C                   CALL      'YEURCO'
259400990922     C                   PARM                    YEUR
259500990922     C*
259600990922     C     YECESI        IFEQ      ' '
259700990922     C                   Z-ADD     YECIMO        IASLIT
259800990922     C                   END
259900990922     C                   ENDIF
260000990922     C                   ENDIF
260100941123     C**----------------------------------------
260200941107     C                   MOVEL     'C'           FISO
260300990722     C                   MOVEL     'C'           WFISO
260400941107     C**FLAG ASSICURAZIONE X CONTO = C
260500941123     C                   Z-ADD     1             A
260600990722     C     WFISO         LOOKUP    CP(A)                                  05
260700941123     C  N05              GOTO      ENDVR
260800020226      * AGGANCIO LA DS DELA TABELLA 1P
260900020226     C     A             OCCUR     DS1P
261000941123     C**----------------------------------------
261100990721     C     KISOT         CHAIN     TITPT01L                           28
261200980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
261300980528     C     *IN28         IFEQ      *ON
261400980528     C     TPTATB        ORNE      *BLANKS
261500980528     C                   GOTO      DOPAC1
261600980528     C                   ENDIF
261700980505     C**
261800980505     C** CONTROLLO IL TIPO APPLICAZIONE DELLA TARIFFA ASSIC X CONTO
261900980505     C**  SE INCONGRUENTE CON TIPO BOLLA PRENDO LA CARTELLO
262000980505     C     TPTAPL        IFEQ      'A'
262100980505     C     TIPBOL        ANDEQ     'F'
262200980505     C     TPTAPL        OREQ      'P'
262300980505     C     TIPBOL        ANDEQ     'A'
262400980505     C                   GOTO      DOPAC1
262500980505     C                   ENDIF
262600980505     C**
262700980504     C* VALORE MERCE = 0 --> ESCLUDO SE NON E' UN VERO MANDATO
262800980505     C                   SETOFF                                       94
262900980504    1C     TPTVLM        IFEQ      0
263000980709     C* CHE SIA UN MANDATO FITTIZIO O UN MANDATO A VALORE VARIABILE
263100980709     C*  DEVO SEMPRE SCRIVERE LA BOLLA COME 8888 PERCHE L'ASSICURAZIONE
263200980709     C*  VUOLE VEDERE BOLLA PER BOLLA QUANTO SONO ASSICURATE
263300050525     C****               MOVEL     'S'           TASK88
263400980709     C**
263500980504    2C     IASLIT        IFEQ      0
263600980504     c* MANDATO FITTIZIO
263700990922    3C     TPTTMA        IFEQ      'F'
263800980504     C                   GOTO      ENDVR
263900980504    3C                   ENDIF
264000050525     c**
264100050526     C** ES - non serve: già fatto    sopra!! Ripetizione inutile
264200050526    3C**   TPTAPL        IFEQ      'P'
264300050526     C**   TIPBOL        COMP      'F'                                    94
264400050526    3C**                 END
264500050526    3C**   TPTAPL        IFEQ      'A'
264600050526     C**   TIPBOL        COMP      'A'                                    94
264700050526    3C**                 END
264800050526    3C**   TPTAPL        IFEQ      ' '
264900050526     C**                 SETON                                            94
265000050526    3C**                 END
265100050525     c
265200050525     c
265300050525     c* anche se manca tariffa è comunque con mandato
265400050525     c                   eval      task88='N'
265500050525     c*
265600050525     c* non devo dare manca tariffa se tipo bolla che non prevede
265700050525     c* Assicurazione
265800050525     C     TASTBL        LOOKUP    TBN                                    05
265900050525     c                   if        not *in05
266000050526     c                   eval      *in94='1'
266100050525     C                   MOVEL     MSG(4)        TASMSG
266200050525     C                   GOTO      FINE
266300050525     c                   endif
266400050525     c
266500050525   x2c                   else
266600050525     c* Mandato a valore variabile con importo: se fittizio 8888 altriment
266700050525     c* con mandato
266800050525    3C     TPTTMA        IFEQ      'F'
266900050525     c                   eval      task88='S'
267000050525     c                   else
267100050525     c                   eval      task88='N'
267200050525    3C                   END
267300050525    2C                   END
267400050525     c
267500941107     C** MANCA TARIFFA: MANDATO CON VALORE = 0
267600941107     C**                E NON ESISTE VALORE IN BOLLA (SOLO COD.)
267700980504   X1C                   ELSE
267800980504    2C     IASLIT        IFGT      0
267900941112     C                   Z-ADD     0             VALSPE           15 3
268000980504    3C     TPTFVM        IFEQ      '3'
268100020826     C     TPTVLM        MULT      USAPKF        VALSPE
268200980504    3C                   END
268300941107     C*PESO
268400980504    3C     TPTFVM        IFEQ      '1'
268500941107     C     TPTVLM        MULT      TASNCL        VALSPE
268600980504    3C                   END
268700941107     C* COLLI
268800980504    3C     TPTFVM        IFEQ      '2'
268900941107     C                   Z-ADD     TPTVLM        VALSPE
269000980504    3C                   END
269100030908      *
269200030908      * ARROTONDO AL DECIMALE DELLA TARIFFA
269300030908     C                   CLEAR                   YEUR
269400030908     C                   MOVEL     §TADIV        YECDVI
269500030908     C                   MOVEL     §TADIV        YECDVO
269600030908     C                   Z-ADD     VALSPE        YECIMI
269700030908     C                   CALL      'YEURCO'
269800030908     C                   PARM                    YEUR
269900030908     C*
270000030908     C     YECESI        IFEQ      ' '
270100030908     C                   Z-ADD     YECIMO        VALSPE
270200030908     C                   END
270300941107     C*SPEDIZIONE
270400980504    3C     IASLIT        IFGT      VALSPE
270500941107     C                   GOTO      DOPAC1
270600050526     c                   else
270700050526     c
270800050526     c* Se valore rientra nel mandato, imposto che si tratta di mandato
270900050526     C                   MOVEL     'N'           TASK88
271000980504    3C                   END
271100050526     c
271200050526   x2c                   else
271300050526     c* se senza valore in bolla ma madanto con valore -->ok da mandato
271400050526     c                   eval      task88='N'
271500980504    2C                   END
271600980504    1C                   END
271700050526     c
271800980525     C** SE IL CLIENTE E' 8888 HO IMPOSTATO PRIMA LA TARIFFA DI CARTELL
271900980525     C     KSCCOD        IFEQ      8888
272000050525     C                   MOVEL     'S'           TASK88
272100980525     C                   ENDIF
272200050525     c
272300050525     c** Anche se lo considero con mandato, se non c'e' importo e tipo
272400050525     c**  bolla senza assicurazione, non calcolo varia R
272500050525     C     TASIAS        IFEQ      0
272600050525     C     TASTBL        LOOKUP    TBN                                    05
272700050525     C   05              GOTO      ENDVR
272800050525     C                   ENDIF
272900980505     C**
273000941107     C                   GOTO      DOPAC2
273100941107     C     DOPAC1        TAG
273200941107     C** NON ESISTE TARIFFA AXC DEL CLIENTE
273300941121     C*-----------------------------------------------------------------
273400980505     C* SE USATA SI TRATTA DI TARIFFA DI CARTELLO
273500050525     C***                MOVEL     'S'           TASK88
273600941112     C     IASLIT        CABEQ     0             ENDVR
273700050525     c                   eval      task88='S'
273800020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
273900020226     C     KTPTC         CHAIN     TITPT01L                           28
274000020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
274100020226     C     *IN28         IFEQ      *ON
274200020226     C     TPTATB        OREQ      'A'
274300020226     C                   GOTO      ENDVR
274400020226     C                   ENDIF
274500941107     C*-----------------
274600941107     C     DOPAC2        TAG
274700980504     C** SE NON SERVE --> ESCO DAL CALCOLO
274800980504     C** TIPO APPLICAZIONE
274900980504     C** P - SOLO PER FRANCHI
275000980504     C** A - SOLO PER ASSEGNATI
275100980504    1C     TPTAPL        IFNE      *BLANKS
275200980504     C     IASLIT        ANDEQ     0
275300980504    2C     TIPBOL        IFEQ      'F'
275400980504    3C     TPTAPL        IFEQ      'A'
275500980504     C                   GOTO      ENDVR
275600980504    3C                   ENDIF
275700980504   X2C                   ELSE
275800980504    3C     TPTAPL        IFEQ      'P'
275900980504     C                   GOTO      ENDVR
276000980504    3C                   ENDIF
276100980504    2C                   END
276200980504    1C                   END
276300980504     C**
276400941112     C     *LIKE         DEFINE    TASIAS        VALVR
276500941112     C     *LIKE         DEFINE    TASIAS        ASCPES
276600941112     C     *LIKE         DEFINE    TASIAS        ASCPE2
276700941112     C                   Z-ADD     0             VALVR
276800980526    1C     IASLIT        IFEQ      0
276900980526    2C     TPTFVM        IFEQ      '3'
277000020826     C     USAPKF        MULT      TPTVLM        VALVR
277100980526    2C                   END
277200980526    2C     TPTFVM        IFEQ      '1'
277300941107     C     TASNCL        MULT      TPTVLM        VALVR
277400980526    2C                   END
277500980526    2C     TPTFVM        IFEQ      '2'
277600941107     C                   Z-ADD     TPTVLM        VALVR
277700980526    2C                   END
277800980526   X1C                   ELSE
277900941112     C                   Z-ADD     IASLIT        VALVR
278000980526    1C                   END
278100011119     C***** OBSOLETO NON RESTITUISCO + IL VALORE IN LIRE MA NELLA DIVISA DELLA TARIFFA
278200011129     C***** PERO' ARROTONDO AL DECIMALE DELLA TARIFFA
278300011129     C                   CLEAR                   YEUR
278400011129     C                   MOVEL     §TADIV        YECDVI
278500011129     C                   MOVEL     §TADIV        YECDVO
278600011129     C                   Z-ADD     VALVR         YECIMI
278700011129     C                   CALL      'YEURCO'
278800011129     C                   PARM                    YEUR
278900011129     C*
279000011129     C     YECESI        IFEQ      ' '
279100011129     C                   Z-ADD     YECIMO        VALVR
279200011129     C                   END
279300011129     C* RESTITUISCO IN OUTPUT L'IMPORTO DA ASSICURARE
279400011129     C                   Z-ADD     VALVR         TASIAL
279500941107     C** VALORE SPEDIZIONE PER CALCOLO ASSICURAZIONE
279600980312     C**
279700980526     C** CALCOLO SOLO SE NON HO IMPOSTATO IL FLAG
279800980526    1C     PARCA         IFEQ      ' '
279900980505     C**
280000980505     C** SE C'E' GIA' LA VARIA R NON LA RICALCOLO
280100980505     C                   Z-ADD     1             A
280200980505     C     §$2VRR        LOOKUP    SV(A)                                  05
280300980526    2C     *IN05         IFEQ      *OFF
280400980312     C                   MOVEL     TPTTPG        WTPG
280500980312     C                   MOVEL     TPTRPV        WRPV
280600980313     C                   CLEAR                   WDET
280700980313     C                   Z-ADD     TPTARL        WARL
280800980313     C                   Z-ADD     TPTARF        WARF
280900980313     C                   Z-ADD     TPTARO        WARO
281000980312     C                   EXSR      CALCOL
281100980312     C**
281200980312     C** A VALORE
281300980526    3C     TPTTPG        IFEQ      'V'
281400980312     C                   Z-ADD     VALVR         ISOPES
281500980526    3C                   END
281600980312     C     *LIKE         DEFINE    TASIAS        TPDPES
281700980504     C**
281800980504     C**CALCOLO IMPORTO ASSICURAZIONE PER CONT
281900980504     C                   Z-ADD     ISOPES        TPDPES
282000941107     C                   EXSR      CALACD
282100980525     C                   Z-ADD     VARIA         WVARIR
282200980526    2C                   ENDIF
282300980526    1C                   ENDIF
282400980504     C     ENDVR         ENDSR
282500060320     C******************************************************
282600060320     C** CALCOLA VARIA d  --- A C BASE
282700060320     C******************************************************
282800060321     c     CALVACB       BEGSR
282900060320     c* Il falg task88 cambia significato:indica se bolla con mandato o no
283000060320     c* Imposto task88=N se mandato
283100060321     c* Lascio  task88=X per bolle senza mandato e senza importo
283200060321
283300060321      **----------------------------------------
283400060321     c                   movel     'd'           Fiso
283500060321     c                   movel     'd'           WFiso
283600060321      ** Flag A C BASE  = d
283700060321     c                   z-add     1             a
283800060321     c     Wfiso         Lookup    Cp(a)                                  05
283900060321     c  N05              Goto      EndVacb
284000060321      * aggancio la ds della tabella 1P
284100060321     c     A             Occur     DS1P
284200060321      **----------------------------------------
284300060321     c     Kisot         Chain     TITPT01L
284400060321      ** esiste Tariffa A C base
284500060321    1c                   If        %found(titpt01l)
284600060321      **
284700060321      ** Controllo il tipo applicazione della tariffa A C Base
284800060321      **
284900060321    2c                   If        (tptapl = 'A' and tipbol = 'A') or
285000060321     c                             (tptapl = 'P' and tipbol = 'F') or
285100060321     c                              tptapl = ' '
285200060321      *
285300060321     c                   eval      task88='N'
285400060321      ** Anche se lo considero con mandato,  tipo
285500060321      **  bolla senza assicurazione, non calcolo varia d
285600060321     c     TAStbl        Lookup    Tbn                                    05
285700060321     c   05              Goto      ENDVacb
285800060321
285900060321      * calcolo valore assicurato
286000060321
286100060321     c     *Like         Define    TASias        VALVacb
286200060321
286300060321     c                   clear                   VALVacb
286400060321
286500060321      * Peso
286600060321    3c                   If        TPTfvm = '3'
286700060321     c     TPTvlm        Mult      USAPkf        Valvacb
286800060321    3c                   Endif
286900060321      * Colli
287000060321    3c                   If        TPTfvm = '1'
287100060321     c     TPTvlm        Mult      TASncl        Valvacb
287200060321    3c                   Endif
287300060321      * Spedizione
287400060321    3c                   If        TPTfvm = '2'
287500060321     c                   Z-add     TPTvlm        Valvacb
287600060321    3c                   Endif
287700060321      *
287800060321      * Arrotondo al decimale della tariffa
287900060321     c                   Clear                   YEUR
288000060321     c                   Movel     §TAdiv        YECdvi
288100060321     c                   Movel     §TAdiv        YECdvo
288200060321     c                   Z-add     VALvacb       YECimi
288300060321     c                   call      'YEURCO'
288400060321     c                   Parm                    YEUR
288500060321      *
288600060321     c                   If        YECesi = ' '
288700060321     c                   Z-add     YECimo        VALvacb
288800060321     c                   Endif
288900060321
289000060321      ** Calcolo solo se non ho impostato il flag
289100060321    3c                   IF        Parca = ' '
289200060321
289300060321      ** Se c'è' già la varia non la ricalcolo
289400060321     c                   Z-add     1             a
289500060321     c     §$2acb        Lookup    sv(a)                                  05
289600060321    4c     *IN05         Ifeq      *off
289700060321     c                   movel     TPTtpg        Wtpg
289800060321     c                   Movel     TPTrpv        Wrpv
289900060321     c                   clear                   Wdet
290000060321     c                   Z-add     TPTarl        Warl
290100060321     c                   Z-add     TPTarf        Warf
290200060321     c                   Z-add     TPTaro        Waro
290300060321     c                   Exsr      Calcol
290400060321
290500060321      ** A Valore
290600060321    5c                   IF        tpttpg =  'V'
290700060321     c                   z-add     valvacb       ISOpes
290800060321    5c                   Endif
290900060321
291000060321     c                   Z-add     ISOpes        TPDpes
291100060321     c                   Exsr      Calacd
291200060321     c                   Z-add     varia         Wvariacb
291300060321    4c                   endif
291400060321
291500060321    3c                   endif
291600060321
291700060321    2c                   endif
291800060321    1c                   Endif
291900060321
292000060321     c     EndVacb       ENDSR
292100941112     C******************************************************
292200980312     C** CALCOLA TARIFFA PARTICOLARE
292300941112     C******************************************************
292400941107     C     CALACD        BEGSR
292500990923     C* CAMPO VARIA CON  DECIMALI
292600990923     C                   Z-ADD     0             VARIA            15 3
292700990923     C* CAMPO VARIA SENZA DECIMALI ALTRIMENTI NON FUNZIONA L'ARROTONDAMENTO
292800990923     C                   Z-ADD     0             VARIAD           15 0
292900990923     C*
293000941107     C                   DO        100           A
293100941122     C     A             OCCUR     ACDS
293200941107     C                   CLEAR                   ACDS
293300941107     C                   END
293400941112     C** PULIZIA DS TARIFFA
293500020412     C*
293600020412     C* SE ESISTE ALMENO UNA RIGA DI DETTAGLIO E NON RIESCO AD APPLICAR
293700020412     C*  QUESTA TAR.PARTICOLARE --> MANCA TARIFFA
293800980605     C* BOLLA IMPORT / ESXPORT
293900980605     C     WBOEST        IFEQ      'E'
294000020226     C     §1PCTE        IFNE      *BLANKS
294100020226     C                   MOVEL     §1PCTE        ISOCTS
294200941112     C                   GOTO      POIAC
294300941112     C                   END
294400941112     C                   ELSE
294500020226     C     §1PCTS        IFNE      *BLANKS
294600020226     C                   MOVEL     §1PCTS        ISOCTS
294700941112     C                   GOTO      POIAC
294800941112     C                   END
294900941112     C                   END
295000941112     C** CODICI TASSAZIONE FISSI DA TABELLA TARIFFE PARTICOLARI
295100941110     C                   MOVEL     TASCTS        ISOCTS            2
295200060414     c
295300060414      * Se assegnato e tariffa FEDEX metto in ISOCTS  quello mittente
295400050124     c                   If        TIPBOL = 'A' and WBOFED = 'S'
295500050124     c                   movel     tasmct        isocts
295600050124     c                   endif
295700060414     c* Se devo utilizzare quello del mittente per via della
295800060414     c*  reversibilità --> lo uso
295900060414     c                   if        applicarevers='S'
296000060414     c                   movel     tasmct        isocts
296100060414     c                   endif
296200050124      *
296300990721     C     KISOD         SETLL     TITPD02L                               28
296400941107     C   28              GOTO      DOPAC3
296500941107     C                   Z-ADD     1             A
296600941107     C     ISOCTS        LOOKUP    CTS(A)                                 05
296700941107     C   05A             OCCUR     DSCT
296800941107     C   05              MOVEL     §CTRAP        ISOCTS
296900941107     C  N05              MOVEL     *BLANKS       ISOCTS
297000941107     C** REGIONE
297100990721     C     KISOD         SETLL     TITPD02L                               28
297200941107     C   28              GOTO      DOPAC3
297300980605     C     WBOEST        IFEQ      'E'
297400941112     C                   MOVEL     'EE'          ISOCTS
297500941112     C                   ELSE
297600941112     C                   MOVEL     'IT'          ISOCTS
297700941112     C                   END
297800941112     C     POIAC         TAG
297900990721     C     KISOD         SETLL     TITPD02L                               28
298000020412     C**N28                GOTO ENDACD
298100020412     C** N 28 --> MANCA TARIFFA
298200020412     C     *IN28         IFEQ      *OFF
298300020412     C* SE NON È PRESENTE NESSUNA RIGA DI DETTAGLIO, NON DO
298400020412     C*  MANCA TARIFFA: SIGNIFICA CHE È INSERITA UNA TASSAZIONE =0
298500020412     C     KISOD2        SETLL     TITPD02L                               28
298600020412     C  N28              GOTO      ENDACD
298700020412     C*
298800020412     C                   SETON                                        94
298900020412     C                   MOVEL     MSG(17)       TASMSG
299000160411     c                   eval      %subst(tasmsg: 47: 2) = TPTFTC
299100020412     C                   GOTO      FINE
299200020412     C                   ENDIF
299300020412     C**
299400020412     C** NON TROVATA TARIFFA
299500941107     C     DOPAC3        TAG
299600941107     C                   DO        *HIVAL        A
299700990721     C     KISOD         READE     TITPD02L                               28
299800941107     C   28              GOTO      DOPAC4
299900941107     C     A             OCCUR     ACDS
300000990923     C* VERIFICO SE STO CARICANDO LA TARIFFA DI CARTELLO
300100990923     C     TPTKSC        IFEQ      CARKSC
300200990923     C     TPTCTR        ANDEQ     CARCTR
300300990923     C     TPTPRG        ANDEQ     CARPRG
300400990923     C* E LA DIVISA DELLA TARIFFA DI CARTELLO E' DIVERSA DALLA DIVISA DELLA
300500990923     C* TARIFFA DEL CLIENTE  CONVERTO I VALORI O SCAGLIONI NELLA DIVISA
300600990923     C* DELLA TARIFFA DEL CLIENTE
300700990923     C     §TADIV        ANDNE     DIVCAR
300800990923     C*
300900990923     C                   EXSR      CONTPD
301000990923     C*
301100990923     C                   ENDIF
301200990923     C*
301300941112     C                   Z-ADD     TPDSGL        ASGL
301400941112     C                   Z-ADD     TPDITR        AITR
301500941112     C                   Z-ADD     TPDMIN        AMIN
301600941112     C                   Z-ADD     TPDMAX        AMAX
301700941112     C                   MOVEL     TPDAIN        AAIN
301800941107     C                   END
301900941107     C     DOPAC4        TAG
302000941107     C                   DO        100           A
302100941107     C     A             OCCUR     ACDS
302200941112     C     ASGL          IFGE      TPDPES
302300941107     C                   GOTO      DOPAC5
302400941107     C                   END
302500941107     C                   END
302600941123     C     ASGL          IFLT      TPDPES
302700941122     C                   SETON                                        94
302800980527     C                   MOVEL     MSG(7)        TASMSG
302900140224     c                   eval      %subst(tasmsg: 47: 2) = TPDFTC
303000980527     C                   GOTO      FINE
303100980527     C                   ENDIF
303200941122     C** NON TROVATO SCAGLIONE ESATTO
303300941107     C     DOPAC5        TAG
303400941112     C*-----------
303500941122     C     ASGL          IFLE      MINTAS
303600941122     C                   Z-ADD     MINTAS        TPDPES
303700941112     C                   END
303800941122     C     TPDPES        IFLE      MINTAS
303900941122     C                   Z-ADD     MINTAS        TPDPES
304000941122     C                   END
304100991013     C                   Z-ADD     0             TPDQLI           18 6
304200980429     C* DIVIDO PER 100 TARIFFA 0 PER AVERE QUINTALI
304300980429     C*                TARIFFA 4 PERCHE' E' TARIFFA %
304400980429     C     WTPG          IFEQ      '0'
304500980429     C     WTPG          OREQ      '4'
304600941112     C     TPDPES        DIV       100           TPDQLI
304700941112     C                   ELSE
304800941112     C                   Z-ADD     TPDPES        TPDQLI
304900941112     C                   END
305000980429     C** PESO TASSABILE
305100060906      * se trovato dettaglio tariffario della tariffa "f" vado a fine routine in quanto il calcolo
305200060906      * viene eseguito nella routine FUEL
305300060906     c                   If        wfiso = 'f'
305400060906     c                   goto      endacd
305500060906     c                   endif
305600060906
305700990923     C   12AITR          MULT(H)   TPDQLI        VARIA
305800990923     C  N12AITR          MULT(H)   TPDQLI        VARIAD
305900941112     C                   MOVEL     AAIN          FLGISO
306000980429     C*
306100980430     C     VARIA         IFGT      0
306200991012     C     *IN12         ANDEQ     *ON
306300991012     C     VARIAD        ORGT      0
306400991012     C     *IN12         ANDEQ     *OFF
306500991012     C*
306600980430     C     WTPG          IFEQ      'V'
306700980430     C     WTPG          OREQ      'T'
306800990923     C   12VARIA         DIV(H)    100           VARIA
306900990923     C  N12VARIAD        DIV(H)    100           VARIAD
307000980430     C                   ENDIF
307100980430     C                   ENDIF
307200980429     C**
307300991012     C* SPOSTO L'IMPORTO DEL CAMPO VARIAD NEL CAMPO VARIA SOLO SE E' STATO USATO
307400991012     C  N12              Z-ADD     VARIAD        VARIA
307500991012     C*
307600991012     C** MINIMO
307700941112     C     AMIN          IFGT      VARIA
307800991012     C                   Z-ADD     AMIN          VARIA
307900941107     C                   END
308000991012     C** MASSIMO
308100941115     C     AMAX          IFGT      0
308200941112     C     AMAX          IFLT      VARIA
308300991012     C                   Z-ADD     AMAX          VARIA
308400941112     C                   END
308500941115     C                   END
308600000103     C** SE VARIA CON ESENZIONE IVA DEVE AVERE SOLO 2 DCEIMALI
308700000103     C* CERCO LA SIGLA VARIA CORRISPONDENTE
308800000103     C                   MOVEL     TPTFTC        WFTC              1
308900000103     C                   Z-ADD     1             A
309000000103     C     WFTC          LOOKUP    CP(A)                                  10
309100000103     C     *IN10         IFEQ      *ON
309200000103     C     A             OCCUR     DS1P
309300000103     C     §1PVAR        LOOKUP    VES                                    10
309400000103     C     *IN10         IFEQ      *ON
309500000103     C                   MOVE      VARIA         W001A             1
309600000103     C     W001A         IFNE      '0'
309700000103     C     VARIA         ADD       0,001         COMOD2           15 2
309800000103     C                   Z-ADD     COMOD2        VARIA
309900000103     C                   ENDIF
310000000103     C                   ENDIF
310100000103     C                   ENDIF
310200000103     C**
310300941107     C     ENDACD        TAG
310400990923     C*
310500941107     C                   ENDSR
310600941107     C/SPACE
310700941112     C******************************************************
310800980424     C** CALCOLO TARIFFE  PARTICOLARI
310900970521     C******************************************************
311000980428     C     CALCP         BEGSR
311100990721     C** CERO LA TARIFFA PARTICOLARE IN TITPT O CARTELLO
311200980430     C                   MOVEL     TASTC         FISO
311300980506     C                   MOVEL     'S'           WCAR
311400110908      ** se sto calcolando la varia "Q" = Centro storico o ZTL
311500110908      ** non devo cercare la tariffa nella Cartello in mancanza
311600110908      ** di quella del cliente
311700111102      ** E' stata aggiuna una tabella che pilota il recupero della
311800111102      ** tariffa di cartello se la filiale del cliente di tassazione
311900111102      ** è presente nella tabella ztl e la data spedizione è maggiore
312000111102      ** uguale alla data attivazione della tassazione
312100111102     c                   clear                   indztl            3 0
312200110908     c                   If        tastc = 'Q'
312300111102     c                   movel     'N'           WCAR
312400111102      * cerco la filiale del cliente nella tabella ZTL
312500111102     c                   eval      indztl = %lookup(kscfil:sfilztl)
312600111102      * se indice =  a zero cerco con la filiale 999
312700111102    2c                   if        indztl = *zeros
312800111102     c                   eval      indztl = %lookup(999:sfilztl)
312900111102    2c                   endif
313000111102      * imposto il flag della ricerca tariffa di cartello = 'S'  se filiale cliente
313100111102      * abilitata e la data spedizione > = data attivazione tassazione
313200111102    2c                   If        indztl > *zeros and
313300111102    3c                             tasdsp >= sdtaztl(indztl)
313400111102     c                   movel     'S'           WCAR
313500111102    3c                   endif
313600110908     c                   endif
313700110908      *
313800980428     C                   EXSR      CERTPT
313900980428     C** SOLO SE L'HO TROVATA
314000980428    1C     WEND          IFEQ      ' '
314100970521     C                   Z-ADD     ISOPES        TPDPES
314200970521     C                   EXSR      CALACD
314300970521     C                   Z-ADD     VARIA         TASCP
314400980428    1C                   ENDIF
314500970521     C*
314600970521     C                   ENDSR
314700001205     C/SPACE
314800001205     C******************************************************
314900001205     C** CALCOLO ADDIZIONALE DI GESTIONE + ISTAT
315000001205     C******************************************************
315100001205     C     ADGIST        BEGSR
315200001205     C**
315300001205     C*
315400001205     C****************
315500041129     C** ADDIZIONALE DI GESTIONE  anni precedenti       varia "Z"
315600001205     C****************
315700001205     C*
315800031121     C     *LIKE         DEFINE    TASVA1        TASADG
315900001205     C     TAMPAG        IFNE      0
316000001205     C                   Z-ADD     1             A
316100001205     C     §$2VRZ        LOOKUP    SV(A)                                  05
316200001205     C  N05              DO
316300001205     C                   Z-ADD     0             TASADG
316400001205     C* TOTALE IMPONIBILE
316500001205     C                   Z-ADD     O17IMV        IMPADG           15 3
316600001205     C**
316700001205     C                   Z-ADD     0             COMADG           15 3
316800001205     C     IMPADG        MULT      TAMPAG        COMADG
316900001205     C     COMADG        IFGT      0
317000001205     C     COMADG        DIV(H)    100           TASADG
317100001205     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
317200001205     C     *IN12         IFEQ      '0'
317300001205     C     TASADG        ADD       0,5           CAMPOW           11 0
317400001205     C                   Z-ADD     CAMPOW        TASADG
317500001205     C                   END
317600001205     C                   END
317700001205     C*
317800040128     c     tasadg        ifgt      0
317900040128      *
318000001205     C                   Z-ADD     1             A
318100001205     C     ' '           LOOKUP    SV(A)                                  96
318200001205     C   96              MOVEL     §$2VRZ        SV(A)
318300001205     C   96              Z-ADD     TASADG        VA(A)
318400001205     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
318500001205     C  N96              ADD       TASADG        TASPOR
318600001205     C*
318700001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
318800001205     C*
318900001205     C                   CLEAR                   DSLV16
319000001205     C                   MOVEL     'T'           D17FIM
319100001205     C                   Z-ADD     TASPOR        D17POR
319200001205     C                   CALL      'FNLV16R'
319300001205     C                   PARM                    DSLV16
319400001205     C                   PARM                    DTASV
319500040128      *
319600040128     c                   endif
319700001205     C*
319800001205     C                   END
319900001205     C                   END
320000041129     C*
320100041129     C****************
320200041129     C** ADDIZIONALE DI GESTIONE    anno corrente       varia "b"
320300041129     C****************
320400041129     C*
320500041129     c                   clear                   tasadg
320600041129     c                   clear                   impadg
320700041129     c                   clear                   comadg
320800041129
320900041129     C     TAMPPA        IFNE      0
321000041129     C                   Z-ADD     1             A
321100041129     C     §$2VAG        LOOKUP    SV(A)                                  05
321200041129     C  N05              DO
321300041129     C                   Z-ADD     0             TASADG
321400041129     C* TOTALE IMPONIBILE
321500041129     C                   Z-ADD     O17IMV        IMPADG
321600041129     C**
321700041129     C                   Z-ADD     0             COMADG
321800041129     C     IMPADG        MULT      TAMPPA        COMADG
321900041129     C     COMADG        IFGT      0
322000041129     C     COMADG        DIV(H)    100           TASADG
322100041129     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
322200041129     C     *IN12         IFEQ      '0'
322300041129     C     TASADG        ADD       0,5           CAMPOW           11 0
322400041129     C                   Z-ADD     CAMPOW        TASADG
322500041129     C                   END
322600041129     C                   END
322700041129     C*
322800041129     c     tasadg        ifgt      0
322900041129      *
323000041129     C                   Z-ADD     1             A
323100041129     C     ' '           LOOKUP    SV(A)                                  96
323200041129     C   96              MOVEL     §$2VAG        SV(A)
323300041129     C   96              Z-ADD     TASADG        VA(A)
323400041129     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
323500041129     C  N96              ADD       TASADG        TASPOR
323600041129     C*
323700041129     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
323800041129     C*
323900041129     C                   CLEAR                   DSLV16
324000041129     C                   MOVEL     'T'           D17FIM
324100041129     C                   Z-ADD     TASPOR        D17POR
324200041129     C                   CALL      'FNLV16R'
324300041129     C                   PARM                    DSLV16
324400041129     C                   PARM                    DTASV
324500041129      *
324600041129     c                   endif
324700041129     C*
324800041129     C                   END
324900041129     C                   END
325000001205     C**
325100001205     C***************
325200001205     C** ISTAT
325300001205     C***************
325400001205     C*
325500031121     C     *LIKE         DEFINE    TASVA1        TASIAC
325600001205     C                   Z-ADD     1             A
325700001205     C     §$2VRV        LOOKUP    SV(A)                                  05
325800101014    1C  N05TAMrct        IFGT      0
325900001205     C                   Z-ADD     0             TASIAC
326000001205     C*
326100001205     C* CALCOLO ISTAT SU TOTALE IMPONIBILE
326200001205     C*
326300001205     C                   Z-ADD     O17IMV        IMPIAC           15 3
326400101014     C                   Z-ADD     0             PUNIAC            7 4
326500101014
326600101014      * richiamo nuovo pgm per calcolo % istat da aggiungere all'imponibile
326700101014     c
326800101014     c                   clear                   trulistds
326900101014     c                   eval      IISTsca = tamrct
327000101014     c                   eval      IISTdsp = tasdsp
327100101014     c                   call      'TRULISTR'
327200101014     c                   parm                    trulistds
327300101014     c                   IF        OISTerr = *blanks
327400101014     c                   eval      puniac = OISTiac
327500101014     c                   ENDIF
327600101014     c
327700050629     C                   Z-ADD     0             PERIAC           15 6
327800050629     C                   Z-ADD     0             PERIAX           15 4
327900001205     C     PUNIAC        MULT      TAMPRI        PERIAX
328000001205    3C     PERIAX        IFNE      0
328100031121     C     PERIAX        DIV       100           PERIAC
328200001205    3C                   END
328300001205     C** PERCENTUALE DI ISTAT DA APPLICARE
328400001205     C** APPLICO I PUNTI X TRIMESTRE SOLO SE
328500001205     C** LA DATA DI SPEDIZIONE E'
328600001205     C** MAGGIORE O UGUALE ALLA DATA DI SCATTO
328700001205     C** DEI PUNTI NEL TRIMESTRE
328800001205    3C     PERIAC        IFNE      0
328900031121     C                   DIV       100           PERIAC
329000001205     C     IMPIAC        MULT(H)   PERIAC        TASIAC
329100001205    3C                   END
329200001205     C*
329300001205    3C     TASIAC        IFGT      0
329400001205     C*  SENZA DECIMALI
329500001205    4C     *IN12         IFEQ      *OFF
329600001205     C     TASIAC        ADD       0,5           CAMPOW           11 0
329700001205     C                   Z-ADD     CAMPOW        TASIAC
329800001205    4C                   END
329900001205     C*
330000001205     C                   Z-ADD     1             A
330100001205     C     ' '           LOOKUP    SV(A)                                  96
330200001205     C   96              MOVEL     §$2VRV        SV(A)
330300001205     C   96              Z-ADD     TASIAC        VA(A)
330400001205     C  N96              ADD       TASIAC        TASPOR
330500001205     C*
330600001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
330700001205     C*
330800001205     C                   CLEAR                   DSLV16
330900001205     C                   MOVEL     'T'           D17FIM
331000001205     C                   Z-ADD     TASPOR        D17POR
331100001205     C                   CALL      'FNLV16R'
331200001205     C                   PARM                    DSLV16
331300001205     C                   PARM                    DTASV
331400001205     C*
331500001205    3C                   END
331600001205    1C                   ENDIF
331700001205     C*
331800001205     C                   ENDSR
331900030922     C*******************************************************
332000030922     C** CALCOLO DIRITTO DI FATTURAZIONE  + TOTALE IMPONIBILE
332100030922     C******************************************************
332200030922     C     DIRFAT        BEGSR
332300030922     C**
332400030922      * prima verifico se esiste già
332500030922     C                   Z-ADD     1             A
332600030922     C     §$2VRD        LOOKUP    SV(A)                                  05
332700030922     C* SIGLA VARIA NON PRESENTE --> CALCOLO
332800030922     C     *IN05         IFEQ      *OFF
332900030922     C                   Z-ADD     1             A
333000030922     C     ' '           LOOKUP    SV(A)                                  96
333100030922     C   96              MOVEL     §$2VRD        SV(A)
333200030922     C   96              Z-ADD     TASIVDF       VA(A)
333300030922     C  N96              ADD       TASIVDF       TASPOR
333400030922     C*
333500030922     C* RICALCOLO L'IMPONIBILE SOMMANDO IL DIRITTO DI FATTURAZIONE
333600030922     C*
333700030922     C                   CLEAR                   DSLV16
333800030922     C                   MOVEL     'T'           D17FIM
333900030922     C                   Z-ADD     TASPOR        D17POR
334000030922     C                   CALL      'FNLV16R'
334100030922     C                   PARM                    DSLV16
334200030922     C                   PARM                    DTASV
334300030924     C*
334400030924     C                   Z-ADD     O17IMV        TASIMV
334500030924     C*
334600030922     C*
334700030922     C                   ENDIF
334800030922     C*
334900030922     C                   ENDSR
335000980428     C******************************************************
335100980428     C* CERCO TARIFFA PARTICOLARE
335200980428     C******************************************************
335300980428     C     CERTPT        BEGSR
335400980428     C                   CLEAR                   TASCP
335500980428     C                   CLEAR                   WEND
335600980428     C                   MOVEL     *BLANKS       FLGISO            1            FLAG.APPLICAZI.
335700990722     C                   MOVEL     FISO          WFISO
335800980428     C                   Z-ADD     1             A
335900990722     C     WFISO         LOOKUP    CP(A)                                  05
336000980428     C**
336100980428    1C     *IN05         IFEQ      *ON
336200020226     C     A             OCCUR     DS1P
336300980428     C*-----------------------------------------
336400990721     C     KISOT         CHAIN     TITPT01L                           28
336500980528     C** ESISTE TARIFFA DEL CLIENTE
336600980528     C     *IN28         IFEQ      *OFF
336700980528     C     TPTATB        ANDEQ     *BLANKS
336800980428     C                   GOTO      DOPIS2
336900980528     C                   ENDIF
337000980428     C*-----------------------------------------
337100980428     C** MEMORIZZO DATI DA TARIFFA DI CARTELLO
337200980506     C**  SOLO SE HO DETTO CHE LA POSSO USARE E SE C'E'
337300980428     C*-----------------------------------------
337400020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
337500020226     C     WCAR          IFEQ      'S'
337600020226     C     KTPTC         CHAIN     TITPT01L                           28
337700020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
337800020226     C     *IN28         IFEQ      *ON
337900020226     C     TPTATB        OREQ      'A'
338000020226     C                   MOVEL     'S'           WEND
338100020226     C                   GOTO      DOPIS3
338200020226     C                   ENDIF
338300020226      * E SE NON DEVO CARICARE LA CARTELLO ERRORE NON TROVATA TARIFFA PARTICOLARE
338400020226     C                   ELSE
338500020226     C                   MOVEL     'S'           WEND
338600020226     C                   GOTO      DOPIS3
338700020226     C                   ENDIF
338800980430     C     DOPIS2        TAG
338900980430     C* ARROTONDAMENTI E RPV
339000980430     C                   MOVEL     TPTTPG        WTPG
339100090310      * in caso di pod-immage varia "a" imposto come tipo pagamento a colli
339200090310
339300090310     c                   if        fiso = §$2POD
339400090310     C                   MOVEL     '1'           WTPG
339500090310     c                   endif
339600090310
339700980430     C                   MOVEL     TPTRPV        WRPV
339800980430     C                   CLEAR                   WDET
339900980430     C                   Z-ADD     TPTARL        WARL
340000980430     C                   Z-ADD     TPTARF        WARF
340100980430     C                   Z-ADD     TPTARO        WARO
340200980430     C                   EXSR      CALCOL
340300980428     C** NON TROVATA TARIFFA PARTICOLARE
340400980428   X1C                   ELSE
340500980428     C                   MOVEL     'S'           WEND              1
340600980428    1C                   ENDIF
340700020226     C     DOPIS3        ENDSR
340800980312     C******************************************************
340900980312     C** CALCOLO IL VALORE DA MOLTIPLICARE/RPV/ARROTONDAMENTI
341000980312     C******************************************************
341100980312     C     CALCOL        BEGSR
341200980313     C     *LIKE         DEFINE    TAMARL        ARR
341300980313     C     *LIKE         DEFINE    TASIAS        PESTAS
341400980313     C     *LIKE         DEFINE    TASIAS        ISOPES
341500980312     C     *LIKE         DEFINE    TASIAS        ISOPE2
341600980312     C     *LIKE         DEFINE    TPTRPV        WRPV
341700980313     C     *LIKE         DEFINE    TPTARL        WARL
341800980313     C     *LIKE         DEFINE    TPTARF        WARF
341900980313     C     *LIKE         DEFINE    TPTARO        WARO
342000980312     C                   Z-ADD     0             ISOPES
342100980316     C                   SELECT
342200030203
342300030204     c                   When      wFiso = '='
342400030203     c                   Z-Add     TasBan        Isopes
342500030203
342600980429     C     WTPG          WHENEQ    '0'
342700980429     C     WTPG          OREQ      '3'                                          KG
342800020826     C                   Z-ADD     USAPKF        ISOPES
342900980316     C*
343000980429     C     WTPG          WHENEQ    '1'                                          COLLI
343100980312     C                   Z-ADD     TASNCL        ISOPES
343200980316     C*
343300980429     C     WTPG          WHENEQ    '2'                                          SPEDIZ
343400980312     C                   Z-ADD     1             ISOPES
343500980316     C*
343600980429     C     WTPG          WHENEQ    '4'                                          VALORE
343700980429     C     WTPG          OREQ      '5'                                          Q.TA'
343800980312     C                   Z-ADD     TASQFT        ISOPES
343900980429     C*
344000980429     C     WTPG          WHENEQ    'T'                                          TRASPORTO
344100980429     C                   Z-ADD     TASPOR        ISOPES
344200980429     C                   ADD       TASINL        ISOPES
344300980429     C                   EXSR      CERISO
344400980429     C                   ADD       VAISO         ISOPES
344500060414     C                   ADD       VAtsp         ISOPES
344600980316     C                   ENDSL
344700980312     C** MINIMO TASSABILE STD
344800980312     C                   Z-ADD     0             MINTAS
344900980312     C**
345000980312     C                   Z-ADD     1             A
345100980312     C     WTPG          LOOKUP    CTR(A)                                 05
345200980312     C   05A             OCCUR     DSTR
345300980312     C   05              Z-ADD     §TRATA        MINTAS
345400980312     C**
345500980312     C** SOLO PER TAD (PER TPD NON MI SERVE)
345600980428    1C     WDET          IFEQ      'TAD'
345700980312     C                   CLEAR                   MINTAR
345800980312     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
345900980312     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
346000980312     C   05              Z-ADD     §TRATA        MINTAR
346100980428    2C     TAMATA        IFGT      0
346200980312     C                   Z-ADD     TAMATA        MINTAR
346300980428    2C                   ENDIF
346400980428    1C                   ENDIF
346500980312     C**
346600980428     C** RPV SOLO PER TARIFFA 0 E 3
346700980428    1C     §TRRPV        IFEQ      'S'
346800980428     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
346900980428    2C     WDET          IFEQ      'TAD'
347000160606     C     USAVLF        ANDGT     0
347100020218    3C                   DO        200           A
347200980428     C     A             OCCUR     TARDS
347300980428    4C     TSCG          IFGE      ISOPES
347400980428     C                   GOTO      POITAS
347500980428    4C                   END
347600980428    3C                   END
347700980428     C** RICERCO RPV CON PESO REALE
347800980428    3C     TSCG          IFLT      ISOPES
347900980428     C                   SETON                                        94
348000980527     C                   MOVEL     MSG(5)        TASMSG
348100151001     c                   eval      %subst(tasmsg: 37)= %trim(%editc(ISOPES:'3'))
348200980527     C                   GOTO      FINE
348300980428    3C                   END
348400980428     C     POITAS        TAG
348500980428     C                   Z-ADD     TRPV          WRPV
348600040603     C                   Z-ADD     TRPV          rappv
348700980428    2C                   ENDIF
348800980428     C**
348900160606    2C     USAVLF        IFNE      0
349000980428     C     WRPV          ANDNE     0
349100980428     C                   Z-ADD     0             COMPRV            9 3
349200160606     C     USAVLF        MULT      WRPV          COMPRV
349300980428     C     COMPRV        MULT      100           ISOPE2
349400980428    3C     ISOPE2        IFGT      ISOPES
349500980428     C                   Z-ADD     ISOPE2        ISOPES
349600980428    3C                   ENDIF
349700980428    2C                   ENDIF
349800980428    1C                   ENDIF
349900990923     C**
350000990923     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
350100990923    0C     §TRARR        IFEQ      'S'
350200980313     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
350300980312     C     ISOPES        IFGE      MINTAS
350400980312     C                   Z-ADD     0             ARR
350500980313     C     ISOPES        IFGT      WARL
350600980313     C                   Z-ADD     WARO          ARR
350700980312     C                   ELSE
350800980313     C                   Z-ADD     WARF          ARR
350900980312     C                   END
351000980313     C**
351100980313     C                   Z-ADD     0             RESTO            15 5
351200980312     C                   Z-ADD     0             ISOPEX           13 0
351300980312     C     ARR           IFNE      0
351400980312     C     ISOPES        ANDNE     0
351500980312     C     ISOPES        DIV       ARR           ISOPEX
351600980312     C                   MVR                     RESTO
351700980312     C     RESTO         IFGT      0
351800980312     C     ISOPEX        MULT      ARR           ISOPES
351900980312     C                   ADD       ARR           ISOPES
352000980312     C                   ENDIF
352100980312     C                   ENDIF
352200980312     C                   ENDIF
352300980312     C**
352400980312     C***        GIUARR    TAG
352500980312    0C                   ENDIF
352600980312     C                   ENDSR
352700970521     C******************************************************
352800941107     C** CALCOLA DIRITTO ASSICURATIVO - VARIA E
352900941112     C******************************************************
353000941107     C     CALVE         BEGSR
353100941107     C                   MOVEL     'R'           FISO
353200980506     C                   MOVEL     'N'           WCAR
353300980430     C                   EXSR      CERTPT
353400980430     C     WEND          IFEQ      ' '
353500990922     C** TARIFFA A VALORE TOLGO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DI CONTO
353600990922     C*  E CALCOLO VALORE AL KG (FISSO) PER LA PARTE RESTANTE
353700980428    1C     TPTTPG        IFEQ      'V'
353800980312     C                   CLEAR                   ISOPES
353900990922     C* CONVERTO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DELLA TARIFFA
354000990922     C*
354100990922     C     *LIKE         DEFINE    §GELRP        WLRP
354200990922     C*
354300990922     C     §TADIV        IFNE      §GEDCN
354400990922     C                   CLEAR                   YEUR
354500990922     C                   MOVEL     §GEDCN        YECDVI
354600990922     C                   MOVEL     §TADIV        YECDVO
354700990922     C                   Z-ADD     §GELRP        YECIMI
354800991014     C   12              MOVE      '3'           YECDCO
354900991014     C  N12              MOVE      '0'           YECDCO
355000990922     C*
355100990922     C                   CALL      'YEURCO'
355200990922     C                   PARM                    YEUR
355300990922     C*
355400990922     C     YECESI        IFEQ      ' '
355500990922     C                   Z-ADD     YECIMO        WLRP
355600990922     C                   END
355700991112     C* SE TARIFFE UGUALI VALORIZZO WLRP
355800991112     C                   ELSE
355900991118     C                   Z-ADD     §GELRP        WLRP
356000990922     C                   ENDIF
356100990922     C*
356200990922    2C     TPTVLM        IFGT      WLRP
356300990922     C     TPTVLM        SUB       WLRP          ISOPES
356400020826     C     ISOPES        MULT      USAPKF        ISOPES
356500980312    2C                   ENDIF
356600980312    1C                   ENDIF
356700980430     C**
356800980312     C                   Z-ADD     ISOPES        TPDPES
356900941112     C                   EXSR      CALACD
357000941112     C                   Z-ADD     VARIA         DIRITT
357100941112     C**
357200941112     C     TPTAPL        IFNE      *BLANKS
357300941112     C     TIPBOL        IFEQ      'F'
357400941112     C     TPTAPL        IFEQ      'A'
357500941112     C                   Z-ADD     0             DIRITT
357600941112     C                   END
357700941112     C                   ELSE
357800941112     C     TPTAPL        IFEQ      'P'
357900941112     C                   Z-ADD     0             DIRITT
358000941112     C                   END
358100941112     C                   END
358200941112     C                   END
358300941112     C** TIPO APPLICAZIONE
358400941112     C** P - SOLO PER FRANCHI
358500941112     C** A - SOLO PER ASSEGNATI
358600980430     C                   ENDIF
358700941107     C                   ENDSR
358800941107     C/SPACE
358900941112     C******************************************************
359000941112     C** CALCOLA MAGGIORAZIONE TIPO SERVIZIO
359100941112     C******************************************************
359200941112     C     CALTS         BEGSR
359300980506     C                   MOVEL     'S'           WCAR              1
359400090924     c
359500090924     C* 24/09/09 --> Ora applichiamo sempre la maggiorazine espresso
359600090924     c
359700980506     C* SE TIPO SERVIZIO DELLA TARIFFA = A TIPO SERVIZIO BOLLA
359800980506     C*  NON PRENDO LA TARIFFA DI CARTELLO
359900090922     C***  TAMTSP        IFEQ      TASTSP
360000090922     C***                MOVEL     'N'           WCAR              1
360100090922     C***                ENDIF
360200980506     C* SIGLA VARIA CORRISPONDENTE
360300980506     C                   MOVEL     §5EFTC        FISO
360400980506     C                   EXSR      CERTPT
360500980506     C     WEND          IFEQ      ' '
360600980506     C**
360700980506     C** A TRASPORTO
360800980430     C     TPTTPG        IFEQ      'T'
360900980312     C                   Z-ADD     IMPON         ISOPES
361000941112     C                   END
361100980312     C**
361200980312     C                   Z-ADD     ISOPES        TPDPES
361300941112     C                   EXSR      CALACD
361400941112     C                   Z-ADD     VARIA         WTSP
361500980506     C                   ENDIF
361600941112     C**
361700941112     C                   ENDSR
361800941112     C******************************************************
361900020225     C** CERCA TARIFFA TNTAM PIU' RELATIVA CARTELLO
362000941112     C******************************************************
362100941107     C     CERTAM        BEGSR
362200020225      *
362300941111     C     *LIKE         DEFINE    TAMPRG        PRG2
362400941111     C     *LIKE         DEFINE    TAMKSC        KSC2
362500941111     C     *LIKE         DEFINE    TAMCTR        CTR2
362600061219      * se in tasflo c'è una data riferimento per il recupero della tariffa la imposto
362700061219      * solo temporaneamente al posto della data spedizione
362800061219     c                   clear                   Sav_dsp
362900070108     c                   if        §asdrt  > *zeros
363000061219     c                   eval      Sav_dsp = Tasdsp
363100070108     c***                eval      tasdsp = §asdrt
363200070108     c                   movel     §asdrt        tasdsp
363300061219     c                   endif
363400020225      *
363500941112     C                   SETOFF                                       180717
363600020227     C                   SETOFF                                       0890
363700941111     C                   Z-ADD     0             PRG2
363800941111     C                   Z-ADD     0             KSC2
363900941111     C                   Z-ADD     0             CTR2
364000941107     C                   Z-ADD     0             TAMKSC
364100941107     C                   Z-ADD     0             TAMPRG
364200941107     C                   Z-ADD     0             TAMCTR
364300980605     C     KTAM          SETLL     TNTAM000                               08
364400980605     C* MANCA TARIFFA CLIENTE
364500980605    1C     *IN08         IFEQ      *OFF
364600980605     C                   MOVEL     MSG(3)        TASMSG
364700980527     C                   SETON                                        94
364800980527     C                   GOTO      ENDTAM
364900980605    1C                   ENDIF
365000980605     C**
365100941107     C     SUTAM         TAG
365200980605     C**
365300941107     C     KTAM          READE     TNTAM000                               07
365400980605     c**
365500941107     C** SE HO LETTO UNA TARIFFA SCADUTA (17) E IL
365600980605     C** RECORD SUCCESSIVO MI ACCENDE FINE FILE
365700980605     C** chaino vecchio progressivo per tassare con tariffa scaduTa
365800980605    1C     *IN07         IFEQ      *ON
365900980605     C   17KTAM2         CHAIN     TNTAM000                           18
366000980605     c**
366100980605     C     *IN17         IFEQ      *OFF
366200980605    2C     *IN18         OREQ      *ON
366300980605     C                   MOVEL     MSG(3)        TASMSG
366400980605     C                   SETON                                        94
366500980605    2C                   ENDIF
366600980605     C**
366700980605     C                   GOTO      ENDTAM
366800980605     C**
366900980605     C                   ELSE
367000980605     C**
367100980605     C** SE NON E' TARIFFA ADATTA PER LA BOLLA (ITALIA INVECE CHE ESTER
367200980605     C**  O VICEVERSA) --> RILEGGO COME SE NON AVESSI LETTO
367300160216     C***  TAMFIE        IFNE      WBOEST
367400160216     C***  TAMFIE        ANDNE     ' '
367500160216     C***                GOTO      SUTAM
367600160216     C***                ENDIF
367700160216    1C                   ENDIF
367800980605     C**
367900980605     C** TARIFFA DA DECORRERE : SEGNALO SE NON C'E' UNA TARIFFA SCADUTA
368000980605     C**    (di fatto non e' possibile perche' se c'e' una da decorrere
368100980605     C**    e ce n'e' una prima e' la tariffa valida dal momento che
368200980605     C**    non e' possibile che esista una buco di decorrenze scadenze
368300980605     C**    tra i vari progressivi)
368400980605    1C     TASDSP        IFLT      TAMDDT
368500980605     C   17KTAM2         CHAIN     TNTAM000                           18
368600980605     C* NON C'E' TARIFFA SCADUTA O NON TROVATA DI NUOVO
368700980605    2C     *IN17         IFEQ      *OFF
368800980605     C     *IN18         OREQ      *ON
368900980605     C                   MOVEL     MSG(10)       TASMSG
369000980605     C                   SETON                                        94
369100980605    2C                   ENDIF
369200980605     C**
369300941107     C                   GOTO      ENDTAM
369400980605    1C                   END
369500980605     C**
369600980605     C** TARIFFA SCADUTA: DATA SPEDIZIONE MAGGIORE DELLA DATA  SCADENZA
369700980605    1C     TASDSP        IFGT      TAMDST
369800941107     C                   SETON                                        17
369900980605     C                   Z-ADD     TAMKSC        KSC2
370000980605     C                   Z-ADD     TAMCTR        CTR2
370100980605     C                   Z-ADD     TAMPRG        PRG2
370200941107     C                   GOTO      SUTAM
370300980605    1C                   ENDIF
370400020225     C*
370700020227     C     ENDTAM        TAG
370800020227     C*
370900020225     C*
371000020227    0C     *IN94         IFEQ      *OFF
371001160303     C                   MOVEL     TAMFLO        DSTA01
371002160315     c                   clear                   tasfie
371003160303      * stabilisco il network della tariffa per la ricerca della cartello
371004160303     c                   select
371005160303      * ITALIA
371006160303     c                   when      tamfie = 'I' and §tadpd = ' '
371007160303     c                   eval      NTWTAM = 'COR'
371008160303     C                   MOVEL     'I'           WBOEST
371009160303      * DPD
371010160303     c                   when      tamfie = 'I' and §tadpd = 'S'
371011160303     c                   eval      NTWTAM = 'DPD'
371012160303     C                   movel     'S'           WBODPD
371013160303     C                   MOVEL     'I'           WBOEST
371014160303      * FEDEX
371015160303     c                   when      tamfie = 'E' and §tafed = 'S'
371016160303     c                   eval      NTWTAM = 'FED'
371017160303     c                   eval      tasfie = 'F'
371018160303     C                   movel     'S'           WBOFED
371019160303     C                   MOVEL     'E'           WBOEST
371020160303      * da aggiungere la differenzazione tariffa merce e documenti
371021160303      *
371022160303      * EEX
371023160303     c                   when      tamfie = 'E' and §tafed = ' '
371024160303     c                   eval      NTWTAM = 'EEX'
371025160303     C                   MOVEL     'E'           WBOEST
371026160303
371027160303     c                   Endsl
371028160303      * verifico se tariffa adatta alla bolla solo per quanto riguarda italia e estero e non
371029160303      * network
371030160303     C                   CLEAR                   TRUL27
371031160303     C                   MOVEL     TASTSP        I27TSP
371032160303     C                   MOVEL     TASLNA        I27LNA
371033160303     C                   MOVEL     TASLNP        I27LNP
371034160303     C                   MOVEL     TASKSC        I27CLI
371035160303     c                   Movel     Tasctr        I27Ctb
371036160303     C                   CALL      'TRUL27R1'
371037160303     C                   PARM                    TRUL27
371038160303      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
371039160303     C     O27ERR        IFEQ      *BLANKS
371040160303      * se  o27fie diverso da I e tamfie = 'E' o §tadpd = 'S' tariffa ok
371041160303      * se  o27fie uguale   a I e tamfie = 'I' e §tadpd = ' ' tariffa ok
371042160303      * Bolla italia e tariffa Estera
371043160303     c                   if        o27fie = 'I' and tamfie = 'E'
371044160303     C                   MOVEL     MSG(3)        TASMSG
371045160303     C                   SETON                                        94
371046160303     C                   ENDIF
371047160303      *Bolla italia e tariffa DPD
371048160303     c                   if        o27fie='I' and tamfie='I' and §tadpd='S'
371049160303     C                   MOVEL     MSG(3)        TASMSG
371050160303     C                   SETON                                        94
371051160303     C                   ENDIF
371052160303      *Bolla Estera e tariffa Italia
371053160303     c                   if        o27fie <> 'I' and tamfie='I' and §tadpd=' '
371054160303     C                   MOVEL     MSG(3)        TASMSG
371055160303     C                   SETON                                        94
371056160303     C                   ENDIF
371057160303     c
371058160303     C                   ELSE
371059160303     C                   SETON                                        94
371060160303     C                   MOVEL     O27MSG        TASMSG
371061160303     C                   GOTO      FINE
371062160303     C                   ENDIF
371100020227      *
371101160303     C** CERCO LA CARTELLO RELATIVA ALLA TARIFFA DEL CLIENTE
371102160303     C*  SE NON HO GIA' UNA TARIFFA DI CARTELLO E SE 94 SPENTO
371200020227     C                   SETOFF                                       90
371300020227     C                   Z-ADD     1             T
371400020227     C     TAMKSC        LOOKUP    TAC(T)                                 90
371500020227      *
371600020227      * PER 90 SPENTO VUOL DIRE CHE NON HO TROVATO NESSUNA CARTELLO
371700020227    1C     *IN90         IFEQ      *OFF
371979160226      * imposto il tipo tariffa fedex
371980160301     c                   clear                   tiptam
371981160226     c                   if        NtwTam = 'FED' and
371982160226     c                             o27fie = 'N'
371983160226     c                   eval      TIPTam = 'FD'
371984160226     c                   endif
371985160226
371986160226     c                   if        NtwTam = 'FED' and
371987160226     c                             (o27fie = 'M' or o27fie = 'F')
371988160226     c                   eval      TIPTam = 'FM'
371989160226     c                   endif
371990160226
372000020225     C*
374500020225     C* AGGANCIO LA TABELLA DELLE TARIFFE DI CARTELLO
374600020319     C                   EXSR      CERTCA
374601160216    1C                   ENDIF
376100020227    0C                   ENDIF
376200061220      * se impostato il campo del salvataggio della data spedizione
376300061220     c                   if        Sav_dsp <> *zeros
376400061220     c                   eval      tasdsp = Sav_dsp
376500061220     c                   clear                   Sav_dsp
376600061220     c                   endif
376700020225     C*
376800941107     C*
376900020227     C     ENDTA2        ENDSR
377000020618     C******************************************************
377100020618     C* RICERCA DETTAGLIO SU TUTAD
377200020618     C******************************************************
377300020618     C     RICTAD        BEGSR
377400020618     C     KTAD          SETLL     TITAD000                               09
377500020618     C** 49  TROVATA TARIFFA DEL CAP   DI ARRIVO
377600020618     C   09              SETON                                        49
377700020618     C**
377800020618    1C     *IN09         IFEQ      *OFF
377900020618     C                   MOVEL     *BLANKS       COMNAZ
378000020618     C                   MOVEL     *BLANKS       COMCAP
378100020618     C     KTAD          SETLL     TITAD000                               09
378200020618    2C     *IN09         IFEQ      *OFF
378300020618     C*
378400020618     C                   Z-ADD     1             A
378500020618     C     COMCTS        LOOKUP    CTS(A)                                 05
378600020618     C**
378700020618     C* SE TROVATO COD.TASSAZIONE
378800020618    3C     *IN05         IFEQ      *ON
378900020618     C     A             OCCUR     DSCT
379000020618     C                   MOVEL     §CTRAP        COMCTS
379100020618     C** CARICO REGIONE AL POSTO DEL CODICE DI TASSAZIONE
379200020618     C     KTAD          SETLL     TITAD000                               09
379300020618    3C                   ENDIF
379400020618     C**
379500020618    3C     *IN09         IFEQ      *OFF
379600020618     C*
379700020618     C     WBOEST        IFEQ      'E'
379800020618     C                   MOVEL     'EE'          COMCTS
379900020618     C                   ELSE
380000020618     C                   MOVEL     'IT'          COMCTS
380100020618     C                   END
380200020618     C     KTAD          SETLL     TITAD000                               09
380300020618    3C                   ENDIF
380400020618    2C                   ENDIF
380500020618    1C                   ENDIF
380600020618     C                   ENDSR
380700941107     C/SPACE
380800941112     C******************************************************
380900990721     C** CERCA TARIFFA TITAD DETTAGLIO E CARICA DS
381000941112     C******************************************************
381100941107     C     CARTAR        BEGSR
381200941112C    C                   SETOFF                                       495694
381300941112     C                   MOVEL     TASCAD        COMCAP            9
381400990923     C                   MOVEL     TASNZD        COMNAZ            3
381500941112     C                   MOVEL     TASCTS        COMCTS
381600020618     C                   MOVEL     TASLNP        WLNP
381700020618     C                   EXSR      RICTAD
381800020618     C   09              GOTO      LATAR
381900020618     C**
382000020618     C** SE NON TROVATO TITAD CON LA LINEA DI PARTENZA, CERCO CON LA
382100020618     C**  REGIONE TROVATA DA ORGANIGRANNA DI TASLNP
382200020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
382300020618     C     TASLNP        IFNE      SAVLNP
382400020618     C     TASLNP        CHAIN     AZORG01L                           31
382500020618     C   31              CLEAR                   ORGDE7
382600020618     C                   MOVEL     ORGDE7        OG147
382700020618     C                   MOVEL     TASLNP        SAVLNP
382800020618     C                   ENDIF
382900020618     C**
383000020618     C                   MOVEL     TASCAD        COMCAP            9
383100020618     C                   MOVEL     TASNZD        COMNAZ            3
383200020618     C                   MOVEL     TASCTS        COMCTS
383300020618     C                   MOVEL     §OGRET        WLNP
383400020618     C                   EXSR      RICTAD
383500020618     C   09              GOTO      LATAR
383600020618     C**
383700020618     C** SE NON TROVATO TITAD CON LA REGIONE DI TASLNP, CERCO CON LA
383800020618     C**  LINEA ITALIA 950
383900020618     C                   MOVEL     TASCAD        COMCAP            9
384000020618     C                   MOVEL     TASNZD        COMNAZ            3
384100020618     C                   MOVEL     TASCTS        COMCTS
384200020618     C                   MOVEL     950           WLNP
384300020618     C                   EXSR      RICTAD
384400020618     C   09              GOTO      LATAR
384500941112     C*
384600941112     C*--------------------TARIFFA REVERSIBILE----------------------------------
384700941107     C     TIPBOL        CABNE     'A'           MANTAR
384800941107     C** TARIFFA REVERSIBILE SOLO PER RITORNO(TBL=A)
384900941107     C     TASLNP        CABEQ     68            MANTAR
385000941107     C** NO TARIFFA REVERSIBILE PER 68
385100981027     C     KSCFIL        IFEQ      888
385200981027     C                   Z-ADD     TASLNA        KFIL
385300981027     C                   ELSE
385400981027     C                   MOVEL     KSCFIL        KFIL
385500981027     C                   ENDIF
385600981008     C**
385700941107     C                   SETON                                        56
385800941112     C                   MOVEL     TASCAM        COMCAP
385900990923     C                   MOVEL     TASNZM        COMNAZ
386000941123     C                   MOVEL     TASMCT        COMCTS
386100020618     C                   MOVEL     KFIL          WLNP
386200020618     C                   EXSR      RICTAD
386300020618     C   09              GOTO      LATAR
386400020618     C*
386500020618     C** SE NON TROVATO TITAD CON LA LINEA CLIENTE, CERCO CON LA
386600020618     C**  REGIONE TROVATA DA ORGANIGRANNA DELLA LIENA CLIENTE
386700020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
386800020618     C     KFIL          IFNE      SAVLNP
386900020618     C     KFIL          CHAIN     AZORG01L                           31
387000020618     C   31              CLEAR                   ORGDE7
387100020618     C                   MOVEL     ORGDE7        OG147
387200020618     C                   MOVEL     KFIL          SAVLNP
387300020618     C                   ENDIF
387400020618     C*
387500020618     C                   MOVEL     TASCAM        COMCAP
387600020618     C                   MOVEL     TASNZM        COMNAZ
387700020618     C                   MOVEL     TASMCT        COMCTS
387800020618     C                   MOVEL     §OGRET        WLNP
387900020618     C                   EXSR      RICTAD
388000020618     C   09              GOTO      LATAR
388100020618     C**
388200020618     C** SE NON TROVATO TITAD CON LA REGIONE DI KFIL, CERCO CON LA
388300020618     C**  LINEA ITALIA 950
388400020618     C                   MOVEL     TASCAM        COMCAP
388500020618     C                   MOVEL     TASNZM        COMNAZ
388600020618     C                   MOVEL     TASMCT        COMCTS
388700020618     C                   MOVEL     950           WLNP
388800020618     C                   EXSR      RICTAD
388900020618     C   09              GOTO      LATAR
389000020618     C**
389100020618     C**         KTDREV    SETLLTITAD000                 09
389200020618     C** 09                SETON                     49
389300020618     C** 09                GOTO LATAR
389400020618     C**
389500020618     C**                   MOVEL*BLANKS   COMNAZ
389600020618     C**                   MOVEL*BLANKS   COMCAP
389700020618     C**         KTDREV    SETLLTITAD000                 09
389800020618     C** 09                GOTO LATAR
389900020618     C**
390000020618     C**                   Z-ADD1         A
390100020618     C**         COMCTS    LOKUPCTS,A                    05
390200020618     C**N05                GOTO POIT2
390300020618     C**         A         OCUR DSCT
390400020618     C**                   MOVEL§CTRAP    COMCTS
390500020618     C**         KTDREV    SETLLTITAD000                 09
390600020618     C** 09                GOTO LATAR
390700020618     C**         POIT2     TAG
390800020618     C**         WBOEST    IFEQ 'E'
390900020618     C**                   MOVEL'EE'      COMCTS
391000020618     C**                   ELSE
391100020618     C**                   MOVEL'IT'      COMCTS
391200020618     C**                   END
391300020618     C**         KTDREV    SETLLTITAD000                 09
391400020618     C** 09                GOTO LATAR
391500941107     C** NON TROVATO NEMMENO LA TARIFFA REVERSIBILE
391600941107     C     MANTAR        TAG
391700980527     C     *IN09         IFEQ      *OFF
391800980527     C                   SETON                                        94
391900980527     C                   MOVEL     MSG(8)        TASMSG
392000980527     C                   GOTO      FINE
392100980527     C                   ENDIF
392200941107     C** MANCA TARIFFA
392300941107     C     LATAR         TAG
392400030902     C                   EXSR      CARDST
392500941112     C** CARICA DS DELLA TARIFFA
392600941107     C                   ENDSR
392700941107     C/SPACE
392800941112     C******************************************************
392900941107     C** CARICA DS TARIFFA
393000941112     C******************************************************
393100941107     C     CARDST        BEGSR
393200980514     C     *LIKE         DEFINE    TADRPV        RAPPV
393300980514     C**
393400020218     C                   DO        200           A
393500941112     C     A             OCCUR     TARDS
393600941112     C                   CLEAR                   TARDS
393700941112     C                   END
393800941112     C                   Z-ADD     0             RAPPV
393900980514     C                   CLEAR                   WELAMT
394000941112     C**
394100980514    1C                   DO        *HIVAL        A
394200980514     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
394300980514    2C     WELAMT        IFNE      'S'
394400020618     C** 56      KTDREV    READETITAD000                 15
394500020618     C**N56      KTAD      READETITAD000                 15
394600020618     C     KTAD          READE     TITAD000                               15
394700941107     C   15              GOTO      ENDDST
394800980514    2C                   ENDIF
394900980514     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
395000980514    2C     WELAMT        IFEQ      'S'
395100980515     C                   MOVEL     'E'           WELAMT            1
395200980514    2C                   ENDIF
395300980514     C**
395400941107     C     A             OCCUR     TARDS
395500980514     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
395600980514     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
395700980514     C**
395800941120     C                   Z-ADD     TADLNP        TLNP
395900941107     C                   MOVEL     TADCTS        TCTS
396000990721     C                   MOVEL     TADNAZ        TNAZ
396100941110     C                   MOVEL     TADCAP        TCAP
396200941107     C                   Z-ADD     TADCTR        TCTR
396300941107     C                   Z-ADD     TADRPV        TRPV
396400941122     C                   Z-ADD     TADMIN        TMIN
396500941122     C                   Z-ADD     TADMAX        TMAX
396600040603     C***!!!****         Z-ADD     TADRPV        RAPPV
396700980514     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
396800980514    2C     WELAMT        IFNE      'E'
396900980514     C**
397000980514    3C     TADSGL        IFEQ      MINTAS
397100980514     C                   MOVEL     'E'           WELAMT
397200980514   X3C                   ELSE
397300980514     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
397400980514     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
397500980514     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
397600980514     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
397700980515    4C     TADSGL        IFGT      MINTAS
397800980514     C                   MOVEL     'S'           WELAMT
397900980514     C                   Z-ADD     MINTAS        TSCG
398000980514     C**
398100980522     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
398200980522    5C     TAMATA        IFEQ      0
398300980522     C     TAMATA        ORLE      MINTAS
398400980522     C     TADSGL        ORGT      TAMATA
398500980522     C**
398600980522    6C     UNOCTR        IFEQ      4
398700980514     C     UNOCTR        OREQ      0
398800980514     C     MINTAS        DIV(H)    100           WMTAS            15 5
398900980514     C                   ELSE
399000980514     C                   Z-ADD     MINTAS        WMTAS
399100980522    6C                   ENDIF
399200980514     C* TARIFFA
399300980514     C     WMTAS         MULT(H)   TADITR        TITR
399400980514     C     WMTAS         MULT(H)   TADINO        TINO
399500980522     C                   ELSE
399600980522     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
399700980522     C                   Z-ADD     TADITR        TITR
399800980522     C                   Z-ADD     TADINO        TINO
399900980522    5C                   ENDIF
400000980522    4C                   ENDIF
400100980514    3C                   ENDIF
400200980514    2C                   ENDIF
400300980514     C*
400400980514     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
400500980514    2C     WELAMT        IFNE      'S'
400600980514     C                   Z-ADD     TADSGL        TSCG
400700980514     C                   Z-ADD     TADITR        TITR
400800980514     C                   Z-ADD     TADINO        TINO
400900980514    2C                   ENDIF
401000980514    1C                   ENDDO
401100941107     C     ENDDST        ENDSR
401200941107     C/SPACE
401300941112     C******************************************************
401400941110     C** CARICAMENTO TABELLE SOLO LA 1 VOLTA
401500941112     C******************************************************
401600941110     C     CARTAB        BEGSR
401700980605     C     *LIKE         DEFINE    TAMATA        MINTAS
401800980605     C     *LIKE         DEFINE    TAMATA        MINTAR
401900990923     C     *LIKE         DEFINE    §TADIV        DIVCAR
402000020618     C                   CLEAR                   SAVLNP
402100980605     C**
402200980605     C                   OPEN      TABEL00F
402300980605     C**
402400941110     C                   EXSR      CARQT
402500941110     C** CARICAMENTO IAC
402600941110     C                   EXSR      CARTR
402700941110     C** CARICAMENTO TIPI TARIFFA
402800941110     C                   EXSR      CAR$2
402900941110     C** CARICAMENTO SIGLE VARIE
403000000103     C                   EXSR      CARCC
403100000103     C** CARICAMENTO SIGLE VARIE ESENTI
403200941112     C                   EXSR      CARCV
403300941112     C** CARICAMENTO CAMBI
403400941110     C                   EXSR      CARCT
403500941110     C** CARICAMENTO CODIFICI TASSAZIONE
403600941112     C                   EXSR      CAR5E
403700941112     C** CARICA TIPI SERVIZIO
403800980429     C                   EXSR      CAR1A
403900980429     C** CARICA TIPI INCASSO C/A MITTENTI
404000980603     C                   EXSR      CARTB
404100980603     C** CARICA DS TIPI BOLLA CHE NON PREVEDONO ASSICURAZIONE
404200020319     C                   EXSR      CAREST
404300980605     C** CARICA P.O. ESTERI
404400990917     C                   EXSR      CARDIV
404500990917     C** CARICAMENTO MONETA DI CONTO
404600020320     C                   EXSR      CARPAR
404700020320     C** CARICA TARIFFE DI CARTELLO
404800031120     C                   EXSR      CAR3A
404900031120     C** CARICO VARIE DELLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
405000980605     C                   CLOSE     TABEL00F
405100040505     C**
405200040505     C                   OPEN      TNTBE01L
405300040505     C** caricamento percentuale carburante
405400040505     C                   EXSR      CARPSC
405500080617     C** caricamento filiali addebito lasciato avviso
405600080617     C                   EXSR      CARLAV
405700111102     C** caricamento filiali addebito centro storico da cartello
405800111102     C                   EXSR      CARZTL
405900040505     C**
406000040505     C                   CLOSE     TNTBE01L
406100040505     C**
406200941110     C                   ENDSR
406300941110     C**********************************************************
406400941110     C** CARICAMENTO DS MULTIPLA  CODICI TASSAZIONE
406500941110     C**********************************************************
406600941110     C     CARCT         BEGSR
406700041220     C                   DO        600           A
406800941110     C     A             OCCUR     DSCT
406900941110     C                   CLEAR                   DSCT
407000941110     C                   END
407100941110     C** PULIZIA DS
407200941110     C                   MOVEL     'CT'          COD
407300941110     C     KTAB          SETLL     TABEL                                  07
407400941110     C  N07              SETON                                          H7
407500941110     C   H7              GOTO      FINE
407600941110     C                   Z-ADD     0             K
407700941110     C                   DO        *HIVAL        A
407800941110     C     KTAB          READE     TABEL                                  07
407900941110     C   07              GOTO      ENDCTS
408000941110     C     TBLKEY        IFNE      *BLANKS
408100941110     C                   ADD       1             K
408200941110     C                   MOVEL     TBLKEY        CTS(K)
408300941110     C     K             OCCUR     DSCT
408400941110     C                   MOVEL     TBLUNI        DSCT
408500941110     C                   END
408600941110     C                   END
408700941110     C     ENDCTS        TAG
408800941110     C                   ENDSR
408900941110     C/SPACE
409000941112     C**********************************************************
409100941112     C** CARICAMENTO TIPI SERVIZIO
409200941112     C**********************************************************
409300941112     C     CAR5E         BEGSR
409400941112     C                   DO        10            A
409500941112     C     A             OCCUR     DS5E
409600941112     C                   CLEAR                   DS5E
409700941112     C                   END
409800941112     C** PULIZIA DS
409900941112     C                   MOVEL     '5E'          COD
410000941112     C     KTAB          SETLL     TABEL                                  07
410100941112     C  N07              SETON                                          H7
410200941112     C   H7              GOTO      FINE
410300941112     C                   Z-ADD     0             K
410400941112     C                   DO        *HIVAL        A
410500941112     C     KTAB          READE     TABEL                                  07
410600941112     C   07              GOTO      END5E
410700941112     C     TBLKEY        IFNE      *BLANKS
410800941112     C                   ADD       1             K
410900941112     C                   MOVEL     TBLKEY        TS(K)
411000941112     C     K             OCCUR     DS5E
411100941112     C                   MOVEL     TBLUNI        DS5E
411200941112     C                   END
411300941112     C                   END
411400941112     C     END5E         TAG
411500941112     C                   ENDSR
411600980429     C**********************************************************
411700980429     C** CARICAMENTO TIPI INCASSO C/A
411800980429     C**********************************************************
411900980429     C     CAR1A         BEGSR
412000980430     C                   MOVEA     *ALL'9'       TIC
412100980429     C                   MOVEL     '1A'          COD
412200980429     C                   Z-ADD     0             K
412300980429     C     KTAB          SETLL     TABEL                                  07
412400980429     C     KTAB          READE     TABEL                                  07
412500980430    1C     *IN07         DOWEQ     *OFF
412600980430    2C     TBLKEY        IFNE      *BLANKS
412700980430     C     TBLFLG        ANDEQ     ' '
412800980429     C                   MOVEL     TBLUNI        DS1A
412900980430    3C     §1AFMB        IFEQ      'M'
413000980429     C                   ADD       1             K
413100980429     C                   MOVEL     TBLKEY        TIC(K)
413200980430    3C                   ENDIF
413300980430    2C                   ENDIF
413400980429     C     KTAB          READE     TABEL                                  07
413500980430    1C                   ENDDO
413600980429     C                   ENDSR
413700980603     C**********************************************************
413800980603     C** CARICAMENTO TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
413900980603     C**********************************************************
414000980603     C     CARTB         BEGSR
414100980603     C                   MOVEL     'TB'          COD
414200980603     C                   Z-ADD     0             K
414300980603     C     KTAB          SETLL     TABEL                                  07
414400980603     C     KTAB          READE     TABEL                                  07
414500980603    1C     *IN07         DOWEQ     *OFF
414600980603    2C     TBLKEY        IFNE      *BLANKS
414700980603     C     TBLFLG        ANDEQ     ' '
414800980603     C                   MOVEL     TBLUNI        DSTB
414900980603    3C     §TBFAS        IFEQ      '0'
415000980603     C                   ADD       1             K
415100980603     C                   MOVEL     TBLKEY        TBN(K)
415200980603    3C                   ENDIF
415300980603    2C                   ENDIF
415400980603     C     KTAB          READE     TABEL                                  07
415500980603    1C                   ENDDO
415600980603     C                   ENDSR
415700031120     C**********************************************************
415800031120     C** CARICAMENTO VARIE PER TIPI BOLLE CHE ACCETTANO UNA SINGOLA VARIA
415900031120     C**********************************************************
416000031120     C     CAR3A         BEGSR
416100031120     C                   MOVEL     '3A'          COD
416200031120     C                   Z-ADD     0             K
416300031120     C     KTAB          SETLL     TABEL                                  07
416400031120     C     KTAB          READE     TABEL                                  07
416500031120    1C     *IN07         DOWEQ     *OFF
416600031120    2C     TBLKEY        IFNE      *BLANKS
416700031120     C     TBLFLG        ANDEQ     ' '
416800031120     C                   MOVEL     TBLUNI        DS3A
416900031120    3C     §3ASVA        IFNE      ' '
417000031120     C                   ADD       1             K
417100031120     C                   MOVEL     §3ASVA        T3A(K)
417200031120    3C                   ENDIF
417300031120    2C                   ENDIF
417400031120     C     KTAB          READE     TABEL                                  07
417500031120    1C                   ENDDO
417600031120     C                   ENDSR
417700020319     C**********************************************************
417800020319     C** CARICAMENTO PO ESTERI
417900020319     C**********************************************************
418000020319     C     CAREST        BEGSR
418100020319     C*
418200020319     C                   CLEAR                   K
418300050504     C                   CLEAR                   KK                4 0
418400020319     C**
418500020319     C                   CLEAR                   CC
418600020319     C     *LOVAL        SETLL     AZORG01L
418700020319     C                   READ      AZORG01L                               07
418800020319    1C     *IN07         DOWEQ     *OFF
418900020319     C** FIL O AGENZIA
419000020319    2C     ORGFAG        IFEQ      'F'
419100020319     C     ORGFAG        OREQ      'A'
419200020319     C* NON ANNULLATO
419300020319    3C     ORGFVA        IFEQ      ' '
419400020319    4C                   MOVEL     ORGDE3        OG143
419500050504     c* p.o. esteri senza dpd
419600020319     C     §OGNTW        IFEQ      'EEX'
419700020319     C     §OGNTW        OREQ      'EUP'
419800020404     C*******    §OGNTW    OREQ 'DPD'
419900020319     C     §OGNTW        OREQ      'FED'
420000020319     C                   ADD       1             K
420100020319     C                   MOVEL     ORGFIL        EST(K)
420200020319    4C                   ENDIF
420300050504     c* p.o. estero totale, anche con DPD
420400050504    4C     §OGNTW        IFEQ      'EEX'
420500050504     C     §OGNTW        OREQ      'EUP'
420600050504     C     §OGNTW        OREQ      'DPD'
420700050504     C     §OGNTW        OREQ      'FED'
420800050504     C                   ADD       1             KK
420900050504     C                   MOVEL     ORGFIL        ESTtot(KK)
421000050504    4C                   ENDIF
421100020319    3C                   ENDIF
421200020319    2C                   ENDIF
421300020319     C**
421400020319     C                   READ      AZORG01L                               07
421500020319    1C                   ENDDO
421600020319     C                   ENDSR
421700941110     C********************************************************
421800941110     C** CARICAMENTO SIGLE VARIE
421900941110     C********************************************************
422000941110     C     CAR$2         BEGSR
422100941110     C                   CLEAR                   DS$2
422200941110     C                   MOVEL     '$2'          COD
422300941110     C                   MOVEL     *BLANKS       KEY
422400941110     C                   MOVEL     '1'           KEY
422500941110     C     KTABC         CHAIN     TABEL                              07
422600941110     C   07              SETON                                          H7
422700941110     C   H7              GOTO      FINE
422800941110     C                   MOVEL     TBLUNI        DS$2
422900941110     C                   ENDSR
423000000103     C********************************************************
423100000103     C** CARICAMENTO SIGLE VARIE  ESENTI IVA
423200000103     C********************************************************
423300000103     C     CARCC         BEGSR
423400000103     C                   CLEAR                   DSCC
423500020416     C                   CLEAR                   KK
423600000103     C                   MOVEL     'CC'          COD
423700000103     C                   MOVEL     *BLANKS       KEY
423800000103     C                   MOVEL     'VARIE'       KEY
423900000103     C     KTABC         SETLL     TABEL                                  07
424000020416     C**N07                GOTO ENDCC
424100000103     C                   DO        *HIVAL
424200000103     C     KTAB          READE     TABEL                                  07
424300000103     C   07              LEAVE
424400000103     C                   MOVEL     TBLUNI        DSCC
424500000103     C* CONTROLLO SE VARIA ESENTE CARICO LA SIGLA IN TABELLA
424600000103     C     §CCJEI        IFNE      *BLANKS
424700000103     C                   ADD       1             K
424800000103     C                   MOVE      TBLKEY        VES(K)
424900000103     C                   ENDIF
425000020416     C* CONTROLLO SE DA RESTITUIRE CON SIGAL VARIA E IMPORTO =0
425100020416     C     §CCSV0        IFEQ      'S'
425200050504     C                   ADD       1             KK                4 0
425300020416     C                   MOVE      TBLKEY        SV0(KK)
425400020416     C                   ENDIF
425500000103     C                   ENDDO
425600000103     C                   Z-ADD     0             K
425700020416     C                   Z-ADD     0             KK
425800000103     C     ENDCC         ENDSR
425900941112     C********************************************************
426000941112     C** CARICAMENTO CAMBI
426100941112     C********************************************************
426200941112     C     CARCV         BEGSR
426300941116     C                   Z-ADD     0             K
426400941116     C                   DO        50            A
426500941116     C     A             OCCUR     DSCV
426600941112     C                   CLEAR                   DSCV
426700941116     C                   END
426800941116     C                   MOVEL     *BLANKS       CV
426900941116     C*
427000941112     C                   MOVEL     'CV'          COD
427100941116     C     KTAB          SETLL     TABEL                                  07
427200941116     C  N07              GOTO      ENDCV
427300941116     C                   DO        *HIVAL        A
427400941116     C     KTAB          READE     TABEL                                  07
427500941116     C   07              GOTO      ENDCV
427600941116     C     TBLKEY        IFNE      *BLANKS
427700941116     C                   ADD       1             K
427800941116     C                   MOVEL     TBLKEY        CV(K)
427900941116     C     K             OCCUR     DSCV
428000941116     C                   MOVEL     TBLUNI        DSCV
428100941116     C                   END
428200941116     C                   END
428300941112     C     ENDCV         ENDSR
428400941110     C******************************************************
428500941110     C** CARICAMENTO TIPI TARIFFA
428600941110     C******************************************************
428700941110     C     CARTR         BEGSR
428800030205     C                   DO        50            A
428900941110     C     A             OCCUR     DSTR
429000941110     C                   CLEAR                   DSTR
429100941110     C                   END
429200941110     C**
429300941110     C                   MOVEL     'TR'          COD
429400941112     C                   Z-ADD     0             K
429500941110     C     KTAB          SETLL     TABEL                                  07
429600941110     C  N07              SETON                                          H7
429700941110     C   H7              GOTO      FINE
429800941110     C                   DO        *HIVAL        A
429900941110     C     KTAB          READE     TABEL                                  07
430000941110     C   07              GOTO      TAB1
430100941110     C     TBLKEY        IFNE      *BLANKS
430200941110     C                   ADD       1             K
430300941110     C                   MOVEL     TBLKEY        CTR(K)
430400941110     C     K             OCCUR     DSTR
430500941110     C                   MOVEL     TBLUNI        DSTR
430600941110     C                   END
430700941110     C                   END
430800941110     C     TAB1          ENDSR
430900941110     C**********************************************************
431000020221     C** MEMORIZZA
431100941110     C** TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
431200941110     C**********************************************************
431300941110     C     CARPAR        BEGSR
431400020225      *
431500020225     C     *LIKE         DEFINE    TAMKSC        CARKSC
431600020225     C     *LIKE         DEFINE    TAMCTR        CARCTR
431700020225     C     *LIKE         DEFINE    TAMPRG        CARPRG
431800020221      *
431900020222      * RICHIAMO IL TRUL27R PER OGNI NETWORK PER CARICARMI LE TARIFFE DI CARTELLO IN SCHIERA
432000020221      *
432100020221     C* CORRIERE BARTOLINI
432200160225     C                   CLEAR                   TRULC7
432300160225     C                   MOVEL     'COR'         IC7TNTW
432400020222     C                   EXSR      SRTRUL
432900020222      * DPD
433000160225     C                   CLEAR                   TRULC7
433100160225     C                   MOVEL     'DPD'         IC7TNTW
433200020222     C                   EXSR      SRTRUL
433300160226      * FEDEX MERCI
433400160225     C                   CLEAR                   TRULC7
433401160229     C                   EVAL      IC7TIP = 'FM'
433500160225     C                   MOVEL     'FED'         IC7TNTW
433600020222     C                   EXSR      SRTRUL
433601160226      * FEDEX DOCUMENTI
433602160226     C                   CLEAR                   TRULC7
433603160229     C                   EVAL      IC7TIP = 'FD'
433604160226     C                   MOVEL     'FED'         IC7TNTW
433605160226     C                   EXSR      SRTRUL
433700020222      * EUROEXPRESS
433800160225     C                   CLEAR                   TRULC7
433900160225     C                   MOVE      'L'           IC7TLA
434000160229     C                   MOVEL     'EEX'         IC7TNTW
434100020222     C                   EXSR      SRTRUL
434200020226      *
434300020226     C                   CLEAR                   CARKSC
434400020226     C                   CLEAR                   CARCTR
434500020226     C                   CLEAR                   CARPRG
434600020222      *
434700020222      ** CARICO LE TARIFFE PARTICOLARI
434800941111     C                   EXSR      CAR1P
434900020222      *
435000941110     C                   ENDSR
435100040505     C**********************************************************
435200040505     C** CARICAMENTO SCHIERA SUPPLEMEMNTO CARBURANTE
435300040505     C**********************************************************
435400040505     C     CARPSC        BEGSR
435500040505
435600040507     C                   CLEAR                   KX
435700040505
435800040505     C                   MOVEL     'PSC'         KEYTBE
435900040505     C     KEYTBE        SETLL     TNTBE01L
436000040505     C                   DO        *HIVAL
436100040505     C     KEYTBE        READE     TNTBE01L
436200040505
436300040505     C                   IF        %EOF(TNTBE01L)
436400040505     C                   LEAVE
436500040505     C                   ENDIF
436600040505
436700040505     C                   IF        TBEATB = ' '
436800040507     C                   ADD       1             KX
436900040507     C                   IF        KX <= 999
437000040505     C                   MOVEL     TBEUNI        DPSC
437100040507     C                   Z-ADD     §PSCDAD       DS_DSC
437200040507     C                   Z-ADD     §PSCPER       DS_PSC
437300040507     C                   MOVE      WDSPSC        DSCA(KX)
437400040507     C                   ENDIF
437500040505     C                   ENDIF
437600040505
437700040505     C                   ENDDO
437800040507
437900040507     C                   SORTA     DSCA
438000040505
438100040505     C                   ENDSR
438200080617      *****************************************************************
438300080617      ** CARICAMENTO SCHIERA FILIALI ABILITATE ADDEBITO LASCIATO AVVISO
438400080617      *****************************************************************
438500080617     C     CARLAV        BEGSR
438600080617
438700080617     C                   CLEAR                   KX
438800080617
438900080617     C                   MOVEL     'LAV'         KEYTBE
439000080617     C     KEYTBE        SETLL     TNTBE01L
439100080617     C                   DO        *HIVAL
439200080617     C     KEYTBE        READE     TNTBE01L
439300080617
439400080617     C                   IF        %EOF(TNTBE01L)
439500080617     C                   LEAVE
439600080617     C                   ENDIF
439700080617
439800080617     C                   IF        TBEATB = ' '
439900080617     C                   ADD       1             KX
440000080617     C                   IF        KX <= 999
440100080617     C                   MOVEL     TBEUNI        DLAV
440200080617     C                   EVAL      SFILLAV(KX) = %dec(tbeke1:3:0)
440300080618     C                   EVAL      SDTALAV(KX) = D§LAVTAS
440400080617     C                   ENDIF
440500080617     C                   ENDIF
440600080617
440700080617     C                   ENDDO
440800080617
440900080617
441000080617     C                   ENDSR
441100111102      ******************************************************************
441200111102      ** CARICAMENTO SCHIERA FILIALI ADDEBITO DA CARTELLO CENTRO STORICO
441300111102      ******************************************************************
441400111102     C     CARZTL        BEGSR
441500111102
441600111102     C                   CLEAR                   KX
441700111102
441800111102     C                   MOVEL     'ZTL'         KEYTBE
441900111102     C     KEYTBE        SETLL     TNTBE01L
442000111102     C                   DO        *HIVAL
442100111102     C     KEYTBE        READE     TNTBE01L
442200111102
442300111102     C                   IF        %EOF(TNTBE01L)
442400111102     C                   LEAVE
442500111102     C                   ENDIF
442600111102
442700111102     C                   IF        TBEATB = ' '
442800111102     C                   ADD       1             KX
442900111102     C                   IF        KX <= 999
443000111102     C                   MOVEL     TBEUNI        DZTL
443100111102     C                   EVAL      SFILZTL(KX) = %dec(tbeke1:3:0)
443200111102     C                   EVAL      SDTAZTL(KX) = D§ZTLTAS
443300111102     C                   ENDIF
443400111102     C                   ENDIF
443500111102
443600111102     C                   ENDDO
443700111102
443800111102
443900111102     C                   ENDSR
444000020222     C*************************************************************
444100160225     C** RICHIAMO DEL TRULC7R
444200020222     C*************************************************************
444300020222     C     SRTRUL        BEGSR
444400020222      *
444500160226     C                   Z-ADD     TASDCT        IC7DTA
444600160226     c******             Movel     TasCtr        I27Ctb
444700160226     C                   CALL      'TRULC7R'
444800160226     C                   PARM                    TRULC7
444900020222      *
445000160226     C     OC7ERR        IFEQ      *BLANKS
445100160226     C     OC7PRGC       ANDNE     *BLANKS
445200020222     C                   ADD       1             T                 2 0
445300160226     C                   MOVE      IC7TNTW       NTW(T)
445400160226     C                   Z-ADD     OC7KSCC       TAC(T)
445500160226     C                   Z-ADD     OC7CTRC       CTC(T)
445600160226     C                   MOVE      OC7PRGC       PRC(T)
445601160226     C                   MOVE      IC7TIP        TIP(T)
445700020222      *
445800020222      * AGGANCIO LA TARIFFA PER RECUPERARE LA DIVISA
445900020222      *
446000160226     C                   Z-ADD     OC7KSCC       CARKSC
446100160226     C                   Z-ADD     OC7CTRC       CARCTR
446200160226     C                   MOVE      OC7PRGC       CARPRG
446300020222     C     KTAMC         CHAIN     TNTAM00L                           94
446400020222      *
446500020222     C     *IN94         IFEQ      *OFF
446600020222     C                   MOVEL     TAMFLO        DSTA01
446700020222     C                   MOVE      §TADIV        DIC(T)
446800020226      * SE C'E ERRORE VADO A FINE PROGRAMMA
446900020226     C                   ELSE
447000020226     C                   MOVEL     MSG(9)        TASMSG
447100020226     C                   ENDIF
447200020226      *
447300020222      * SE C'E ERRORE VADO A FINE PROGRAMMA
447400020222     C                   ELSE
447500020222     C                   SETON                                        94
447600020222     C                   MOVEL     MSG(9)        TASMSG
447700020222     C                   ENDIF
447800020222      *
447900020222     C                   ENDSR
448000020225     C*************************************************************
448100020225     C** CERCO LA TARIFFA DI CARTELLO
448200020225     C*************************************************************
448300020225     C     CERTCA        BEGSR
450800020318     C** DALLE SCHIERE CARICO LA TARIFFA DI CARTELLO
450900020318     C                   SETOFF                                       90
451000020319     C                   Z-ADD     1             T
451001160226      * se ho il tipo tariffa fedex valorizzato vado con questo altrimenti vado
451002160226      * con il network della tariffa
451003160226     c     Tiptam        ifne      '  '
451004160226     c     tiptam        lookup    TIP(T)                                 90
451005160226     c                   else
451100160216     C     NtwTam        LOOKUP    NTW(T)                                 90
451101160226     c                   endif
451200020318     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
451300020318     C     *IN90         IFEQ      *OFF
451400020318     C                   MOVEL     MSG(9)        TASMSG
451500020319     C                   SETON                                        94
451600020318     C                   GOTO      FINE
451700020318     C                   ENDIF
451800020318     C*
451900020318     C                   Z-ADD     TAC(T)        CARKSC
452000020318     C                   Z-ADD     CTC(T)        CARCTR
452100020318     C                   Z-ADD     PRC(T)        CARPRG
452200020318     C                   MOVE      DIC(T)        DIVCAR
452300020225      *
452400020225     C                   ENDSR
452500941114     C*************************************************************
452600941121     C** CARICAMENTO CODICI E TARIFFE PARTICOLARI DI CARTELLO
452700941114     C*************************************************************
452800941110     C     CAR1P         BEGSR
452900941111     C*
453000020114     C                   DO        50            A
453100941112     C     A             OCCUR     DS1P
453200941111     C                   CLEAR                   DS1P
453300941112     C                   END
453400941111     C                   MOVEL     *BLANKS       CP
453500941110     C**
453600941110     C                   MOVEL     '1P'          COD
453700941111     C                   Z-ADD     0             K                 5 0
453800941110     C     KTAB          SETLL     TABEL                                  07
453900941110     C  N07              SETON                                          H7
454000941110     C   H7              GOTO      FINE
454100941110     C                   DO        *HIVAL        A
454200941110     C     KTAB          READE     TABEL                                  07
454300941110     C   07              GOTO      TABCP
454400941110     C     TBLKEY        IFNE      *BLANKS
454500941115     C                   MOVEL     TBLUNI        CAMPO            26
454600941115     C                   MOVE      CAMPO         WFCP              1
454700941115     C     WFCP          IFEQ      'S'
454800941110     C                   ADD       1             K
454900941110     C                   MOVEL     TBLKEY        CP(K)
455000941112     C     K             OCCUR     DS1P
455100941112     C                   MOVEL     TBLUNI        DS1P
455200941112     C**
455300941110     C                   END
455400941110     C                   END
455500941111     C                   ENDDO
455600020222     C*
455700941110     C     TABCP         ENDSR
455800941110     C****************************************************
455900941110     C** CARICAMENTO DATI VARI TASSAZIONE
456000941110     C****************************************************
456100941110     C     CARQT         BEGSR
456200941110     C                   MOVEL     'QT'          COD
456300941110     C                   MOVEL     *BLANKS       KEY
456400941110     C                   MOVEL     '1'           KEY
456500941110     C     KTABC         CHAIN     TABEL                              07
456600941113     C   07              SETON                                        H7
456700941113     C   H7              GOTO      FINE
456800941110     C  N07              MOVEL     TBLUNI        DSQT1
456900941110     C*
457000941110     C                   ENDSR
457100990917     C****************************************************
457200990922     C** CARICAMENTO MONETE DI CONTO / IMPORTI STANDARD
457300990917     C****************************************************
457400990917     C     CARDIV        BEGSR
457500990917     C*****
457600990922      *  RICERCA DELLA MONETA DI CONTO
457700990917     C                   CLEAR                   DSBS02
457800990917     C                   CLEAR                   DGED
457900990922     C*
458000990917     C                   MOVEL     'C'           T02MOD
458100990917     C                   MOVEL     KNSIF         T02SIF
458200990917     C                   MOVEL     'GED'         T02COD
458300991112     C                   MOVEL     '1'           T02KE1
458400990917      *
458500990917     C                   CALL      'TIBS02R'
458600990917     C                   PARM                    KPJBA
458700990917     C                   PARM                    DSBS02
458800990917      *
458900990917     C     T02ERR        IFEQ      *BLANKS
459000990917     C                   MOVEL     T02UNI        DGED
459100990917     C                   ENDIF
459200990922      * RICERCA DEGLI IMPORTI STANDARD NELLA MONETA DI CONTO
459300990922     C                   CLEAR                   DSBS02
459400990922     C                   CLEAR                   DGEI
459500990922     C                   MOVEL     'L'           T02TLA
459600990922     C                   MOVEL     'C'           T02MOD
459700990922     C                   MOVEL     KNSIF         T02SIF
459800990922     C                   MOVEL     'GEI'         T02COD
459900991112     C                   MOVEL     §GEDCN        T02KE1
460000990922      *
460100990922     C                   CALL      'TIBS02R'
460200990922     C                   PARM                    KPJBA
460300990922     C                   PARM                    DSBS02
460400990922      *
460500990922     C     T02ERR        IFEQ      *BLANKS
460600990922     C                   MOVEL     T02UNI        DGEI
460700990922     C                   ENDIF
460800990922      *
460900990917      *
461000990917     C                   ENDSR
461100011127     C**********************************************************
461200011127     C** RECUPERO LA DATA DEL GIORNO
461300011127     C**********************************************************
461400011127     C     RUDATE        BEGSR
461500011127      *
461600011127     C                   TIME                    W0140            14 0
461700011127     C* UDATE IN GGMMAAAA
461800011127     C                   MOVE      W0140         WDTGIO            8 0
461900011127     C*
462000011127     C                   Z-ADD     WDTGIO        G02DAT
462100011127     C                   MOVEL     *BLANK        G02ERR
462200011127     C                   CALL      'XSRDA8'
462300011127     C                   PARM                    WLBDAT
462400011127     C*
462500011127     C* UDATE IN AAAAMMGG
462600011127     C                   Z-ADD     G02INV        WDTGIO
462700011127      *
462800011127     C                   ENDSR
462900011127     C**********************************************************
463000011127     C** RECUPERO LA DATA TASSAZIONE DA USARE PER QUESTA BOLLA
463100011127     C**********************************************************
463200030131     C**!!!RECDAT        BEGSR
463300011127      *
463400030131     C**!!!              SETOFF                                       31
463500011127      * VERIFICO SE DATA VALORIZZATA E SOPRATTUTTO NUMERICA
463600020826     C**         §ASDAT    IFNE *BLANK
463700020826     C**         §ASDAT    ANDNE*ZEROS
463800011127      *
463900020826     C**                   TESTN          §ASDAT     31
464000011127      * SE 31 ACCESO VUOL DIRE CHE È NUMERICO MA VERIFICO ANCHE L'ULTIMO CARATTERE
464100020826     C**         *IN31     IFEQ *ON
464200020826     C**                   MOVE §ASDAT    W01A    1
464300020826     C**         W01A      COMP '0'                  31  31
464400020826     C**                   ENDIF
464500011127      *
464600020826     C**                   ENDIF
464700011127      * SE 31 ACCESO DATA VALIDA ALTRIMENTI PRENDO LA DATA DEL GIORNO
464800020826     C**         *IN31     IFEQ *ON
464900020826     C**                   MOVE §ASDAT    DATTAS  80
465000020826     C**                   ELSE
465100020826     C**                   Z-ADDWDTGIO    DATTAS
465200020826     C**                   ENDIF
465300020618     C*
465400011127      *
465500030203     C**!!!              ENDSR
465600990916     C****************************************************
465700990916     C** CARICAMENTO IMPORTI PASSATI DALLE DS
465800990916     C****************************************************
465900990916     C     SAVDS         BEGSR
466000990916     C*
466100990922     C     *LIKE         DEFINE    TASPOR        TASINL
466200990916     C*
466300990916     C* SALVO LE SCHIERE DELLE VARIE (SIGLE VARIE+IMPORTI)
466400990916     C                   MOVEA     SV            SVW
466500990916     C                   MOVEA     VA            VAW
466600991012     C                   CLEAR                   TASINL
466700991012     C                   CLEAR                   TASDIF
466800990916     C*
466900990916     C* RECUPERO L'IMPORTO DELL'INOLTRO SE C'E' NELLA SCHIERA DELLE VARIE
467000990916     C                   Z-ADD     1             X                 2 0
467100990923     C     §$2FIN        LOOKUP    SV(X)                                  30
467200990916     C* SE LO TROVO VALORIZZO TASINL
467300990923     C   30              Z-ADD     VA(X)         TASINL
467400991111     C  N30              Z-ADD     0             X
467500990921     C*
467600990921     C* RECUPERO L'IMPORTO DEL DIRITTO FISSO  NELLA SCHIERA DELLE VARIE
467700990921     C                   Z-ADD     1             Y                 2 0
467800990921     C     §$2VRH        LOOKUP    SV(Y)                                  30
467900990921     C* SE LO TROVO VALORIZZO TASINL
468000990923     C   30              Z-ADD     VA(Y)         TASDIF           11 3
468100990916     C*
468200990916     C                   ENDSR
468300990921     C****************************************************
468400990921     C** CONVERSIONE DELLE VARIE IN MONETA DELLA TARIFFA
468500990921     C****************************************************
468600990921     C     CNVTAS        BEGSR
468700990921     C*
468800990921     C* SE DIVISA PASSATA (DELLA BOLLA) E' DIVERSA DALLA DIVISA DELLA
468900990921     C* TARIFFA CONVERTO IN DIVISA TARIFFA
469000990921     C                   Z-ADD     1             A
469100990923    1C                   DO        20            A
469200990923    2C     VA(A)         IFNE      *ZEROS
469300990921     C                   CLEAR                   YEUR
469400011127     C                   MOVEL     DIVINP        YECDVI
469500011127     C                   MOVEL     DIVOUT        YECDVO
469600990921     C                   Z-ADD     VA(A)         YECIMI
469700991014     C   12              MOVE      '3'           YECDCO
469800991014     C  N12              MOVE      '0'           YECDCO
469900990921     C*
470000990921     C                   CALL      'YEURCO'
470100990921     C                   PARM                    YEUR
470200990921     C*
470300990923    3C     YECESI        IFEQ      ' '
470400990921     C                   Z-ADD     YECIMO        VA(A)
470500990923    3C                   END
470600990923    2C                   END
470700990923    1C                   END
470800990921     C* CONVERTO L'IMPORTO DEL PORTO
470900990921     C* SE PORTO DIVERSO DA 0
471000990923    1C     TASPOR        IFGT      0
471100990921     C                   CLEAR                   YEUR
471200011127     C                   MOVEL     DIVINP        YECDVI
471300011127     C                   MOVEL     DIVOUT        YECDVO
471400990921     C                   Z-ADD     TASPOR        YECIMI
471500991014     C   12              MOVE      '3'           YECDCO
471600991014     C  N12              MOVE      '0'           YECDCO
471700990921     C*
471800990921     C                   CALL      'YEURCO'
471900990921     C                   PARM                    YEUR
472000990921     C*
472100990923    2C     YECESI        IFEQ      ' '
472200990921     C                   Z-ADD     YECIMO        TASPOR
472300990921     C                   ELSE
472400990921     C                   MOVEL     MSG(10)       TASMSG
472500990921     C                   GOTO      FINE
472600990923    2C                   END
472700990923    1C                   ENDIF
472800990921     C* SE INOLTRO DIVERSO DA 0
472900990923    1C     TASINL        IFGT      0
473000990921     C                   CLEAR                   YEUR
473100011127     C                   MOVEL     DIVINP        YECDVI
473200011127     C                   MOVEL     DIVOUT        YECDVO
473300990921     C                   Z-ADD     TASINL        YECIMI
473400991014     C   12              MOVE      '3'           YECDCO
473500991014     C  N12              MOVE      '0'           YECDCO
473600990921     C*
473700990921     C                   CALL      'YEURCO'
473800990921     C                   PARM                    YEUR
473900990921     C*
474000990923    2C     YECESI        IFEQ      ' '
474100990921     C                   Z-ADD     YECIMO        TASINL
474200990921     C                   ELSE
474300990921     C                   MOVEL     MSG(10)       TASMSG
474400990921     C                   GOTO      FINE
474500990923    2C                   END
474600990923    1C                   ENDIF
474700990921     C* SE DIRITTO FISSO DIVERSO DA 0
474800990923    1C     TASDIF        IFGT      0
474900990921     C                   CLEAR                   YEUR
475000011127     C                   MOVEL     DIVINP        YECDVI
475100011127     C                   MOVEL     DIVOUT        YECDVO
475200990921     C                   Z-ADD     TASDIF        YECIMI
475300991014     C   12              MOVE      '3'           YECDCO
475400991014     C  N12              MOVE      '0'           YECDCO
475500990921     C*
475600990921     C                   CALL      'YEURCO'
475700990921     C                   PARM                    YEUR
475800990921     C*
475900990923    2C     YECESI        IFEQ      ' '
476000990921     C                   Z-ADD     YECIMO        TASDIF
476100990923    2C                   END
476200990923    1C                   ENDIF
476300990929     C* SE IMPORTO IMPONIBILE DIVERSO DA ZEROS
476400991014     C* LO RICALCOLO PER SICUREZZA
476500990929    1C     TASIMV        IFGT      0
476600991014     C                   CLEAR                   DSLV16
476700991014     C                   MOVEL     'T'           D17FIM
476800991014     C                   Z-ADD     TASPOR        D17POR
476900991014     C                   CALL      'FNLV16R'
477000991014     C                   PARM                    DSLV16
477100991014     C                   PARM                    DTASV
477200991014     C                   Z-ADD     O17IMV        TASIMV
477300990929    1C                   ENDIF
477400990921     C* SE QUANTITA' DA FATTURARE E' DIVERSA DA 0 E LA TARIFFA E' UNA TARIFFA
477500011127     C* A VALORE SPECIFICO DEVO CONVERTIRE IL CAMPO SE RICHIESTO
477600990921     C*
477700011127    1C     TASQFT        IFGT      0
477800011127     C     TAMFVD        ANDEQ     '4'
477900011127     C     CONQTA        ANDNE     'N'
478000011127     C                   CLEAR                   YEUR
478100011127     C                   MOVEL     DIVINP        YECDVI
478200011127     C                   MOVEL     DIVOUT        YECDVO
478300011127     C                   Z-ADD     TASQFT        YECIMI
478400011127     C   12              MOVE      '3'           YECDCO
478500011127     C  N12              MOVE      '0'           YECDCO
478600011127     C*
478700011127     C                   CALL      'YEURCO'
478800011127     C                   PARM                    YEUR
478900011127     C*
479000011127    2C     YECESI        IFEQ      ' '
479100011127     C                   Z-ADD     YECIMO        TASQFT
479200011127    2C                   END
479300011127    1C                   ENDIF
479400011127     C* SE TASIAL VALORIZZATO DEVO CONVERTIRLO SOLO SE RICHIESTO
479500011127     C*
479600011127    1C     TASIAL        IFGT      0
479700011127     C     CONIAL        ANDNE     'N'
479800011127     C                   CLEAR                   YEUR
479900011127     C                   MOVEL     DIVINP        YECDVI
480000011127     C                   MOVEL     DIVOUT        YECDVO
480100011127     C                   Z-ADD     TASIAL        YECIMI
480200011129     C   12              MOVE      '2'           YECDCO
480300011127     C  N12              MOVE      '0'           YECDCO
480400011127     C*
480500011127     C                   CALL      'YEURCO'
480600011127     C                   PARM                    YEUR
480700011127     C*
480800011127    2C     YECESI        IFEQ      ' '
480900011127     C                   Z-ADD     YECIMO        TASIAL
481000011127    2C                   END
481100011127    1C                   ENDIF
481200990921     C*
481300990921     C                   ENDSR
481400990923     C************************************************************
481500990923     C** CONVERSIONE DEL DETTAGLIO TARIFFA PARTICOLARE DI CARTELLO
481600990923     C************************************************************
481700990923     C     CONTPD        BEGSR
481800990923     C*
481900990923     C                   CLEAR                   YEUR
482000990923     C                   MOVEL     DIVCAR        YECDVI
482100990923     C                   MOVEL     §TADIV        YECDVO
482200991014     C   12              MOVE      '3'           YECDCO
482300991014     C  N12              MOVE      '0'           YECDCO
482400990923     C*
482500990923     C* CONTROLLLO IL TIPO TARIFFA SE E' VALORE OPPURE NO PER CONVERTIRE
482600990923     C* LO SCAGLIONE OPPURE NO
482700990923     C*
482800990923    1C     WTPG          IFEQ      'T'
482900990923     C     WTPG          OREQ      '4'
483000990923     C     WTPG          OREQ      'V'
483100990923     C*
483200990923     C                   Z-ADD     TPDSGL        YECIMI
483300990923     C                   CALL      'YEURCO'
483400990923     C                   PARM                    YEUR
483500990923    2C     YECESI        IFEQ      ' '
483600990923     C                   Z-ADD     YECIMO        TPDSGL
483700990923    2C                   END
483800990923     C* NON CONVERTO L'IMPORTO TARIFFA PERCHE' E' UNA PERCENTUALE
483900990923    1C                   ELSE
484000990923     C*
484100990923     C                   Z-ADD     TPDITR        YECIMI
484200990923     C                   CALL      'YEURCO'
484300990923     C                   PARM                    YEUR
484400990923    2C     YECESI        IFEQ      ' '
484500990923     C                   Z-ADD     YECIMO        TPDITR
484600990923    2C                   END
484700990923    1C                   ENDIF
484800990923     C*
484900990923     C* MINIMO
485000990923     C                   Z-ADD     TPDMIN        YECIMI
485100990923     C                   CALL      'YEURCO'
485200990923     C                   PARM                    YEUR
485300990923    1C     YECESI        IFEQ      ' '
485400990923     C                   Z-ADD     YECIMO        TPDMIN
485500990923    1C                   END
485600990923     C* MASSIMO
485700990923     C                   Z-ADD     TPDMAX        YECIMI
485800990923     C                   CALL      'YEURCO'
485900990923     C                   PARM                    YEUR
486000990923    1C     YECESI        IFEQ      ' '
486100990923     C                   Z-ADD     YECIMO        TPDMAX
486200990923    1C                   END
486300990923     C*
486400990923     C                   ENDSR
486500020826     C************************************************************
486600020826     C** CONTROLLO SE APPLICARE PESO NORMALE O VDL
486700020826     C************************************************************
486800020826     C     CTRPES        BEGSR
486900020826     C                   CLEAR                   USAPKF
487000030131     C**!!!              CLEAR                   §ASSPC
487100030131     C                   CLEAR                   TASSPC
487200030429     C                   CLEAR                   TASPKCN
487300020826     C*
487400020826     C* SE NON C'E' IL PESO VDL, PRENDO QUELLO DA FATTURARE
487500030131     C**!!!§ASPKC        IFEQ      0
487600030131     C     TASPKC        IFEQ      0
487700020826     C                   Z-ADD     TASPKF        USAPKF
487800020826     C                   GOTO      ENDPES
487900020826     C                   ELSE
488000020826     C* SE IL VDL NON + MAI DA APPLICARE --> USO IL DA FATTURARE
488100020826     C     §TATAP        IFEQ      'M'
488200020826     C                   Z-ADD     TASPKF        USAPKF
488300020826     C                   GOTO      ENDPES
488400020826     C                   ENDIF
488500020826     C                   ENDIF
488600020826     C* PESO VDL - TARA
488700030131     C**!!!§QTTPC        MULT      §ASNCP        WTARA
488800030131     C     §QTTPC        MULT      TASNCP        WTARA
488900030131     C**!!!§ASPKC        SUB(h)    WTARA         WNTARA
489000030131     C     TASPKC        SUB(h)    WTARA         WNTARA
489100020826     C* SE PESO DA FATTURARE COMPRESO DA VDL E VDL-TARA
489200020826     C*  APPLICO SEMPRE IL PESO DA FATTURARE
489300020826     C*
489400020827     C     TASPKF        IFGE      WNTARA
489500030131     C**!!!TASPKF        ANDLE     §ASPKC
489600030131     C     TASPKF        ANDLE     TASPKC
489700020826     C                   Z-ADD     TASPKF        USAPKF
489800020826     C                   GOTO      ENDPES
489900020826     C                   ENDIF
490000020826     C* SE VDL-TARA > PESO FATTURARE LO APPLICO SIA PER " " CHE PER "S"
490100020827     C     WNTARA        IFGT      TASPKF
490200020827     C                   Z-ADD     WNTARA        USAPKF
490300030131     C**!!!              MOVEL     'S'           §ASSPC
490400030131     C**!!!              z-add     wntara        §ASpkc
490500030131     C                   MOVEL     'S'           TASSPC
490600030429     C                   z-add     wntara        TASpkcN
490700020826     C                   GOTO      ENDPES
490800020826     C                   ENDIF
490900020826     C* SE VDL-TARA < PESO FATTURARE LO APPLICO SOLO SE TOTALE E SE
491000020826     C* "S"
491100020827     C     WNTARA        IFLT      TASPKF
491200030131     C**!!!§ASNCP        ANDEQ     TASNCL
491300030131     C     TASNCP        ANDEQ     TASNCL
491400020826     C     §TATAP        ANDEQ     'S'
491500020827     C                   Z-ADD     WNTARA        USAPKF
491600030131     C**!!!              MOVEL     'S'           §ASSPC
491700030131     C**!!!              z-add     wntara        §ASpkc
491800030131     C                   MOVEL     'S'           TASSPC
491900030429     C                   z-add     wntara        TASpkcN
492000020826     C                   GOTO      ENDPES
492100020826     C                   ENDIF
492200020826     C* SE ANCORA A 0 (IMPOSSIBILE) USO IL PESO DA FATTURARE
492300020826     C     USAPKF        IFEQ      0
492400020826     C                   Z-ADD     TASPKF        USAPKF
492500020826     C                   ENDIF
492600020827     C**
492700020829     c     endpes        tag
492800150922      * valorizzo il flag di tipo applicazione volume
492900150904      * serve per escludere le bolle con tariffa mai applicazione VDL
493000150922      * dal calcolo del peso desunto (PRG 852)
493100150922     c                   eval      tastap = §tatap
493200150904
493300150922      * valorizzo il flag di KSC / CTR esclusione calcolo peso desunto
493400150922      * serve per escludere le bolle dei clienti che non vogliono
493500150922      * il  calcolo del peso desunto (PRG 852)
493600150925     c                   if        tamtpr = 'NO'
493700150925     c                   eval      tasepd = 'N'
493800150925     c                   else
493900150925     c                   clear                   tasepd
494000150925     c                   endif
494100150922
494101151118      * valorizzo il progressivo tariffa utilizzato in tassazione
494102151119     c                   movel     tamprg        tasprg
494103151118
494200020829     C                   ENDSR
494201160606     C************************************************************
494202160606     C** CONTROLLO SE APPLICARE VOLUME REALE O VDL
494203160606     C************************************************************
494204160606     C     CTRVOL        BEGSR
494205160606     C                   CLEAR                   USAVLF
494207160606     C                   CLEAR                   TASFVT
494208160606     C                   CLEAR                   TASVLT
494209160606
494210160606      * valorizzo il volume da usare in tassazione uguale al volume da fatturare
494211160606     c                   eval      usavlf = tasvlf
494212160606     c                   eval      tasfvt = tasfvf
494213160606     c                   eval      tasvlt = tasvlf
494214160606
494215160606     C                   SELECT
494216160606      *
494217160606      * se in tariffa il cliente ha come apllicazione VDL "MAI VDL"  utilizzo TASVLF
494218160606     c                   WHEN      §tatap = 'M'
494222160606      *
494223160606      * se volume da fatturare non è reale utilizzo TASVLF
494224160606     c                   WHEN      tasfvf <> 'R'
494226160606      *
494227160606      * se volume da fatturare è reale e non c'è stata rilevazioni sul VDL utilizzo TASVLF
494228160606     c                   WHEN      tasvlc = 0
494230160606      *
494231160606      * se volume da fatturare è reale e c'è stata rilevazioni sul VDL
494232160606      * se rilevazione totale prendo volume VDL
494233160606     c                   WHEN      tasvlc > 0 and
494234160606     c                             tasnclb = tasncr
494235160606     c                   eval      usavlf = tasvlc
494236160606     c                   eval      tasfvt = 'T'
494237160606     c                   eval      tasvlt = tasvlc
494238160606      *
494239160606      * se volume da fatturare è reale e c'è stata rilevazioni sul VDL
494240160606      * se rilevazione parziale prendo volume VDL se maggiore del volume da fatturare
494241160606     c                   WHEN      tasvlc > 0 and
494242160606     c                             tasnclb > tasncr and tasvlc > tasvlf
494243160606     c                   eval      usavlf = tasvlc
494244160606     c                   eval      tasfvt = 'Z'
494245160606     c                   eval      tasvlt = tasvlc
494246160606
494247160606     c                   endsl
494306160606
494307160606     C                   ENDSR
494308020412**
494400980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo del CONTRASSEGNO  1
494500980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo da ASSICURARE     2
494600980529Non trovata tariffa cliente                                                    3
494700980512Mancano importo da assicurare in bolla e valore merce in tariffa               4
494800980603Non trovato SCAGLIONE in tariffa                                               5
494900980512Manca la quantita' da fatturare                                                6
495000980603Non trovato SCAGLIONE in tariffa particolare
495100980603Non trovato CODICE TASSAZIONE in tariffa                                       8
495200980529Non trovata tariffa di CARTELLO                                                9
495300000125Tariffa cliente con decorrenza maggiore della data spedizione                 10
495400990917Divisa errata                                                                 11
495500000125Tariffa DPD ma bolla Italia                                                   12
495600000125Bolla import/export DPD  ma  tariffa NON DPD                                  13
495700011127Divisa tariffa del cliente non più utilizzabile                               14
495800020318Tariffa FEDEX ma bolla Italia                                                 15
495900020318Bolla import/export FEDEX ma  tariffa NON FEDEX                               16
496000140224Non trovato COD.TASSAZIONE in tar.PARTICOLARE                                 17
496100030527Non presente tassazione per addebito insoluti                                 18
496200080221Tariffa valida ma imponibile minore di 0,000                                  19
