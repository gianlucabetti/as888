000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941112     H* TNSF20R  *---------------------------------------------------*
000300941112     H*             - TASSAZIONE SINGOLA SPEDIZIONE
000400031121     H*--------------------------------------------------------------*
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
015100020221     D** CALL PGM RECUPERO TARIFFE DI CARTELLO
015200020221     D TRUL27        E DS                  EXTNAME(TRUL27DS)
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
021100060922
021200920225     C                   Z-ADD     1             CODUT             1 0
021300000000     C*---------------------------------------------------------------*
021400020226     C*
021500020226     C** ACCESSO TITPT01L
021600941107     C     KISOT         KLIST
021700941107     C                   KFLD                    TAMKSC
021800941107     C                   KFLD                    TAMCTR
021900941107     C                   KFLD                    TAMPRG
022000990722     C                   KFLD                    FISO              2
022100020226     C     KTPTC         KLIST
022200020226     C                   KFLD                    CARKSC
022300020226     C                   KFLD                    CARCTR
022400020226     C                   KFLD                    CARPRG
022500020226     C                   KFLD                    FISO
022600930419     C*
022700020222     C** ACCESSO TFTPD02L
022800941107     C     KISOD         KLIST
022900941107     C                   KFLD                    TPTKSC
023000941107     C                   KFLD                    TPTCTR
023100941107     C                   KFLD                    TPTPRG
023200941107     C                   KFLD                    TPTFTC
023300941107     C                   KFLD                    ISOCTS
023400020412     C** ACCESSO TFTPD02L
023500020412     C     KISOD2        KLIST
023600020412     C                   KFLD                    TPTKSC
023700020412     C                   KFLD                    TPTCTR
023800020412     C                   KFLD                    TPTPRG
023900020412     C                   KFLD                    TPTFTC
024000020222     C** ACCESSO TABEL X CODICE
024100941107     C     KTAB          KLIST
024200941107     C                   KFLD                    CODUT
024300941107     C                   KFLD                    COD               2
024400020222     C** ACCESSO TABEL X CODICE + CHIAVE
024500941107     C     KTABC         KLIST
024600941107     C                   KFLD                    CODUT
024700941107     C                   KFLD                    COD
024800941107     C                   KFLD                    KEY               8
024900020222     C** ACCESSO TNTAM  TARIFFA DI CARTELLO
025000020222     C     KTAMC         KLIST
025100020226     C                   KFLD                    CARKSC
025200020226     C                   KFLD                    CARCTR
025300020226     C                   KFLD                    CARPRG
025400020222     C** ACCESSO TNTAM TARIFFE SCADUTA
025500941107     C     KTAM2         KLIST
025600941107     C                   KFLD                    KSC2
025700941111     C                   KFLD                    CTR2
025800941107     C                   KFLD                    PRG2
025900020222     C** ACCESSO TNTAM X CLIENTE
026000941107     C     KTAM          KLIST
026100941107     C                   KFLD                    TMCLI
026200941107     C                   KFLD                    TMCTR
026300020222     C** ACCESO TITAD
026400941107     C     KTAD          KLIST
026500941107     C                   KFLD                    TDCLI
026600941107     C                   KFLD                    TDPRG
026700020618     C                   KFLD                    WLNP
026800941112     C                   KFLD                    COMCTS
026900990722     C                   KFLD                    COMNAZ
027000990722     C                   KFLD                    COMCAP
027100941107     C                   KFLD                    TDCTR
027200020222     C** ACCESSO TITAD PER TARIFFA REVERSIBILE
027300941112     C     KTDREV        KLIST
027400941112     C                   KFLD                    TDCLI
027500941112     C                   KFLD                    TDPRG
027600981027     C                   KFLD                    KFIL
027700941112     C                   KFLD                    COMCTS
027800990722     C                   KFLD                    COMNAZ
027900941112     C                   KFLD                    COMCAP
028000941112     C                   KFLD                    TDCTR
028100020826     C     *LIKE         DEFINE    TASPKF        USAPKF
028200020827     C     *LIKE         DEFINE    §qttpc        WTARA
028300020827     C     *LIKE         DEFINE    TASPKF        WNTARA
028400020826     C     *LIKE         DEFINE    KSCFIL        KFIL
028500020618     C     *LIKE         DEFINE    TASLNP        SAVLNP
028600020618     C     *LIKE         DEFINE    TASLNP        WLNP
028700020618     C     *LIKE         DEFINE    TASCTS        COMCTS
028800941107     C***********     -------------       ***********
028900930419     C                   MOVEL     ' '           RT
029000930419     C*
029100941125     C* TASTLA = ' ' ELABORO E CHIUDO PGM  CON RETRN
029200941125     C* TASTLA = 'L' ELABORO E CHIUDO PGM  CON LR
029300941125     C* TASTLA = 'C' NO ELAB.  CHIUDO PGM  CON LR PER CHIUSURA FILE
029400941116     C     TASTLA        IFEQ      ' '
029500941116     C     TASTLA        OREQ      'L'
029600941107     C*
029700941116     C     TASTLA        IFEQ      ' '
029800930419     C                   MOVEL     'R'           RT                1
029900930419     C                   END
030000980505     C* C A M P I   D I   O U T P U T
030100980505     C                   CLEAR                   TASERR
030200980505     C                   CLEAR                   TASMSG
030300980505     C                   CLEAR                   TASIAL
030400980526     C                   MOVEL     KPJBU         PARAMT
030500030902      * clearizzo indicatiori x manca tariffa e inoltro
030600030902     C                   SETOFF                                       9456
030700030211
030800030211     c                   Clear                   wFiso
030900990930     C** CONTROLLO LA DIVISA CHE MI VIENE PASSATA SE DIVERSA DA BLANK VERIFICO
031000990930     C** CHE SIA CORRETTA
031100990930     C     TASDIV        IFNE      *BLANKS
031200990930     C                   CLEAR                   DSLV16
031300990930     C                   MOVEL     'D'           D17FIM
031400990930     C                   MOVE      TASDIV        D17DIV
031500991012     C****                 MOVE TASDSP    D17DSP
031600990930     C                   CALL      'FNLV16R'
031700990930     C                   PARM                    DSLV16
031800990930     C                   PARM                    DTASV
031900990930     C* CONTROLLO IL CODICE DI RITORNO
032000990930     C     O17ERR        IFNE      *BLANKS
032100990930     C                   MOVEL     'E'           TASERR
032200990930     C                   MOVEL     O17MSG        TASMSG
032300990930     C                   SETON                                        94
032400990930     C                   GOTO      FINE
032500990930     C                   ENDIF
032600990930     C                   ENDIF
032700941125     C*
032800980605     C** CARICA TABELLE SOLO LA 1° VOLTA
032900941125     C     CARTBL        IFEQ      *BLANK
033000941125     C                   MOVE      '1'           CARTBL            1
033100941125     C                   EXSR      CARTAB
033200011127      * RECUPERO UDATE SOLO LA PRIMA VOLTA
033300011127     C                   EXSR      RUDATE
033400020320     C     TASMSG        IFNE      *BLANKS
033500020320     C                   SETON                                        94
033600020320     C                   GOTO      FINE
033700020320     C                   ENDIF
033800941125     C                   END
033900011127     C* R E C U P E R O   D A T A   D I   T A S S A Z I O N E
034000030131     C**!!!              EXSR      RECDAT
034100991012     C*
034200991012     C** SALVO GLI IMPORTI CHE MI VENGONO PASSATI
034300991012     C                   EXSR      SAVDS
034400941125     C*
034500941110     C                   SETOFF                                       94
034600030925      *
034700030925      * se richiamato per il solo calcolo della varia D  VADO A FINE
034800030925     C                   IF        TASSVA = 'D'
034900030925     C                   GOTO      FINE
035000030925     C                   ENDIF
035100030925      *+
035200941124     C                   MOVEL     TASTBL        TIPBOL            1
035300941124     C                   MOVEL     TASKSC        KSCFIL            3 0
035400941124     C                   MOVE      TASKSC        KSCCOD            4 0
035500080617      * da giugno 2008 viene riattivato per alcune filiali l'addebito del lasciato avviso
035600080617      * se tasric diverso da blank .. quindi la spedizione ha un evento di lasciato avviso
035700080617      * verifico se la filiale del cliente è presente nella tabella LAV oppure nella tabella
035800080617      * LAV esiste la filiale 999
035900080617    1c                   If        TasRic <> ' '
036000080617     c                   clear                   indx
036100080617     c                   eval      indx = %lookup(kscfil:sfillav)
036200080617      * se indice =  a zero cerco con la filiale 999
036300080617    2c                   if        indx = *zeros
036400080617     c                   eval      indx = %lookup(999:sfillav)
036500080617    2c                   endif
036600080617      * pulisco il flag del calcolo dell'addebito del lasciato avviso perchè filiale cliente
036700080617      * non abilitato all'addebito
036800080617    2c                   If        indx = *zeros
036900080617     c                   clear                   tasric
037000080617      * altrimenti verifico la data attivazione tassazione
037100080617   x2c                   else
037200080617      * se data spedizione inferiore all'attivazione pulisco il flag
037300080618    3c                   if        tasdsp < sdtalav(indx)
037400080617     c                   clear                   tasric
037500080617    3c                   endif
037600080617    2c                   endif
037700080617    1c                   endif
037800080617     c
037900980429     C* VEDO SE TIPO INCASSO E' INTESTATO AL MITTENTE
038000980430     C                   CLEAR                   TASBM
038100061016      * se il primo carattere del campo TASTIC è uguale a blank
038200061016      * ci troviamo a tassare una spedizione in sede in quanto
038300061016      * in TNCSB (file contrassegni ) la combinazione di tpa (tipo assegno)
038400061018      * e tpi (tipo intestazione) o fus (tipo intestazione in bolla)
038500061018      * potrebbe non esistere in tabella  1A tipo incasso c/assegno
038600061016      * a questo punto prendo solo il tipo intestazione assegno che
038700061016      * è uguale a  M per mittente  e "B" o " " per Vettore
038800061016      * In Tasbm passo solo se è uguale a M
038900061016
039000061016     C                   If        %subst(tastic:1:1) = ' '
039100061016     C                   If        %subst(tastic:2:1) = 'M'
039200061016     c                   eval      TASBM = 'M'
039300061016     c                   endif
039400061016
039500061016     c                   else
039600980429     C     TASTIC        LOOKUP    TIC                                    96
039700980429     C   96              MOVEL     'M'           TASBM             1
039800061016     c                   endif
039900980605     C** TASSAZIONE
040000941107     C                   EXSR      TARIFF
040100941110     C     FINE          TAG
040200980605     C** 94 ON - MANCA TARIFFA
040300990922     C     *IN94         IFEQ      '1'
040400990929     C* E NON C'E' TOTALE IMPONIBILE  DO ERRORE
040500990929     C     TASIMV        IFEQ      *ZEROS
040600990922     C                   MOVEL     'E'           TASERR            1
040700990929     C                   ENDIF
040800990922     C* REIMPOSTO LA DS COME PRIMA SENZA CONVERSIONI IN MONETA
040900990922     C*    MI SEMBRA INUTILE ADESSO EVENTUALMENTE LO FACCIO DOPO
041000990922     C*
041100990922     C                   ELSE
041200990929     C* CALCOLO IL TOTALE IMPONIBILE SE E' UGUALE A ZEROS
041300990929     C*
041400990929     C     TASIMV        IFEQ      *ZEROS
041500990923     C                   CLEAR                   DSLV16
041600990923     C                   MOVEL     'T'           D17FIM
041700990923     C                   Z-ADD     TASPOR        D17POR
041800990923     C                   CALL      'FNLV16R'
041900990923     C                   PARM                    DSLV16
042000990923     C                   PARM                    DTASV
042100001205     C*
042200001205     C* SE SPEDIZIONI CON DATA MAGGIORE O UGUALE AL 01012001 CALCOLO L'ADDIZIONALE DI GESTIONE
042300001211     C* SUL TOTALE IMPONIBILE E DOPO CALCOLO SUL TOTALE IMPONIBILE + ADDIZ. GESTIONE L'ISTAT
042400001205     C*
042500001221     C     TASDSP        IFGE      20010101
042600020416     C* ESEGUO SOLO SE NON HO RICHIAMATO LA TASSAZIONE PER UNA VARIA
042700031120     C*  SOLA SE VARIA NON PRESENTE NELLA TABELLA T3A (TABELLA DELLE VARIE DELLE BOLLE CHE
042800031121     C* ACCETTANO UNA SINGOLA  VARIA ESEMPIO "O" "Y" "*".....)
042900031120     C*    TASSVA        ANDEQ     ' '
043000031120      *
043100031120     C                   IF        TASSVA <> ' '
043200031120     C                   SETOFF                                       11
043300031120     C     TASSVA        LOOKUP    T3A(1)                                 11
043400031120     C                   ENDIF
043500040113      *
043600040113      *
043700040113      * SE VARIA UGUALE A BLANK                                             OPPURE
043800040113      * VARIA PRESENTE NELLA TABELLA T3A                                    OPPURE
043900040113      * TASSAZIONE 2° BOLLA     TASSVA = 'G' E TASCVAG = 'S'                OPPURE
044000040113      * VARIA UGUALE A "Z"                                                  OPPURE
044100040113      * CALCOLO A.G.+ ISTAT
044200040113      *
044300040113     C                   IF        TASSVA = ' ' OR
044400040113     C                             %FOUND       OR
044500040113     C                             (TASSVA = 'G' AND TASCVAG = 'S') OR
044600040113     C                             TASSVA = 'Z'
044700001205     C                   EXSR      ADGIST
044800001205     C                   ENDIF
044900031120      *
045000031120     C                   ENDIF
045100001205     C*
045200990923     C                   Z-ADD     O17IMV        TASIMV
045300001205     C*
045400990929     C                   ENDIF
045500030924      *
045600030924      * DIRITTO DI FATTURAZIONE lo calcolo solo alla fine della TASSAZIONE senza aggiunte dell'addi-
045700030924      *                         zionale di gestione
045800030924      *                         se totale imponibile maggiore di zero
045900030924     C                   IF        TASIMV > 0 AND
046000030924     C                             (TASSVA = §$2VRD OR TASSVDF= §$2VRD)
046100030924      *
046200030924     C                   EXSR      DIRFAT
046300030924      *
046400030924     C                   ENDIF
046500990922     C* MUOVO LA DIVISA DELLA TARIFFA
046600020416     C*  SE NON PASSATA LA VARIA O SE PASSATA E IMPORTO >0
046700020416     C*
046800020416     C     TASSVA        IFEQ      *BLANKS
046900020416     C     O17IMV        ORGT      0
047000990929     C                   MOVE      §TADIV        TASDIV
047100020416     C                   ENDIF
047200020416     C*
047300020416     C* SE SI TRATTA DI UNA VARIA DA PASSARE ANCHE A 0 CONTROLLO
047400020416     C     TASSVA        IFNE      *BLANKS
047500020416     C     O17IMV        ANDEQ     0
047600020416     C     TASSVA        LOOKUP    SV0                                    05
047700020416     C     *IN05         IFEQ      *ON
047800020416     C                   MOVEL     TASSVA        SV(1)
047900020416     C                   ENDIF
048000020416     C                   ENDIF
048100080221
048200080221      * VERIFICO SE IMPONIBILE A ZERO SENZA SIGLE DI VARIE PERCHÈ QUESTO TIPO DI CONTROLLO
048300080221      * VIENE ESEGUITO DAI PGM CHIAMANTI
048400080221     c                   clear                   wrksv
048500080221     c                   movea     sv            wrksv
048600080221     C                   IF        TASIMV = 0 AND wrkSV = *BLANKS
048700080221     C                   MOVEL     'E'           TASERR            1
048800080221     C                   MOVEL     MSG(19)       TASMSG
048900080221     C                   SETON                                        94
049000080221     C                   GOTO      FINE
049100080221     C                   ENDIF
049200990922     C*
049300990922     C                   ENDIF
049400011127     C* CONTROLLO LA DIVISA IN BASE ALLA DATA TASSAZIONE SE DA CONVERTIRE OPPURE NO
049500011127     C* SE STO TASSANDO CON DATA > DEL 2001 UNA BOLLA CON DATA < DEL 2002 E LA DIVISA NON E'
049600011127     C* UGUALE ALLA MONETA DI CONTO AZIENDALE CONVERTO TUTTI GLI IMPORTI ANCHE LA QTA DA
049700011127     C* FATTURARE NELLA DIVISA DI CONTO
049800011127     C*
049900020826     C**         DATTAS    IFGE 20020101
050000020826     C     TASDIV        IFNE      §GEDCN
050100011127     C     TASDSP        ANDLT     20020101
050200011127     C* CONVERTO
050300011127     C                   MOVE      TASDIV        DIVINP
050400011127     C                   MOVE      §GEDCN        DIVOUT
050500011127     C                   MOVE      'S'           CONQTA
050600011127     C                   MOVE      'S'           CONIAL
050700011127     C*
050800011127     C* RECUPERO LE CARATTERISTICHE DELLA MONETA DI CONTO
050900011127     C                   SETOFF                                       12
051000011127     C                   Z-ADD     1             K
051100011127     C     §GEDCN        LOOKUP    CV(K)                                  10
051200011127     C     *IN10         IFEQ      *ON
051300011127     C     K             OCCUR     DSCV
051400011127     C     §CVFDC        IFEQ      'S'
051500011127     C* DIVISA CHE ACCETTA DECIMALI
051600011127     C                   SETON                                        12
051700011127     C                   ENDIF
051800011127     C                   ENDIF
051900011127     C*
052000011127     C                   EXSR      CNVTAS
052100011127     C* VALORIZZO  LA DIVISA
052200011127     C                   MOVEL     §GEDCN        TASDIV
052300011127     C*
052400011127     C                   ENDIF
052500990922     C*
052600941107     C                   END
052700941107     C*
052800941107     C     RT            IFEQ      'R'
052900941107     C                   RETURN
053000941107     C                   ELSE
053100020319     C* CHIUDO PGM TRUL27R
053200020319     C                   CLEAR                   TRUL27
053300020319     C                   MOVE      'C'           I27TLA
053400020319     C                   CALL      'TRUL27R'
053500020319     C                   PARM                    TRUL27
053600941107     C                   SETON                                        LR
053700941107     C                   END
053800941107     C************       -----------       *************
053900941107     C**
054000941112     C******************************************************
054100941110     C**  CALCOLA TASSAZIONE BOLLA
054200941112     C******************************************************
054300941110     C     TARIFF        BEGSR
054400980605     C                   MOVEL     'I'           WBOEST
054500000125     C                   CLEAR                   WBODPD
054600020318     C                   CLEAR                   WBOFED
054700991014     C                   SETOFF                                       12
054800020318     C**
054900020318     C** PER VERIFICARE SE SONO BOLLA DPD FEDEX O ESTERA CHIAMO TRUL27
055000020318     C**  SENZA LA DATA
055100020319     C                   CLEAR                   TRUL27
055200020318     C                   MOVEL     TASTSP        I27TSP
055300020318     C                   MOVEL     TASLNA        I27LNA
055400020318     C                   MOVEL     TASLNP        I27LNP
055500020319     C                   MOVEL     TASKSC        I27CLI
055600021122     c                   Movel     Tasctr        I27Ctb
055700020318     C                   CALL      'TRUL27R'
055800020318     C                   PARM                    TRUL27
055900020318      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
056000020318     C     O27ERR        IFEQ      *BLANKS
056100020318     C     O27FIE        IFEQ      'E'
056200020318     C                   MOVEL     'E'           WBOEST            1
056300020318     C                   ENDIF
056400020318     C*
056500020318     C     O27FIE        IFEQ      'D'
056600020318     C                   MOVEL     'S'           WBODPD            1
056700020318     C                   MOVEL     'I'           WBOEST            1
056800020318     C                   ENDIF
056900020318     C*
057000020318     C     O27FIE        IFEQ      'F'
057100021122     C     O27FIE        orEQ      'M'
057200021122     C     O27FIE        orEQ      'N'
057300020318     C                   MOVEL     'S'           WBOFED            1
057400020318     C                   MOVEL     'E'           WBOEST            1
057500020318     C                   ENDIF
057600020318     C                   ELSE
057700020319     C                   SETON                                        94
057800020319     C                   MOVEL     O27MSG        TASMSG
057900020318     C                   GOTO      FINE
058000020318     C                   ENDIF
058100020320     C**
058200030429     C* LO PASSO IN OUTPUT
058300030131     C**!!!              MOVEL     O27FIE        §ASFIE
058400030131     c                   MOVEL     O27FIE        TASFIE
058500021122      * Per tutte le tipologie di bolla FedEx imposto sempre 'F' xchè il
058600021122      * TNSF02 testa 'F' x definire che è una bolla FedEx (descrizione varie)
058700021122     c                   If        O27Fie = 'M' or O27Fie = 'N'
058800030131     c**!!!              Movel     'F'           §AsFie
058900030131     c                   Movel     'F'           TAsFie
059000021122     c                   EndIf
059100980605     C**
059200980605     C** VEDO SE BOLLA ITALIA O ESTERA (IMPORT O EXPORT)
059300020318     C**         TASLNP    LOKUPEST                      05
059400020318     C**N05      TASLNA    LOKUPEST                      05
059500020318     C**         *IN05     IFEQ *ON
059600020318     C**                   MOVEL'E'       WBOEST  1
059700020318     C**                   ENDIF
059800020318     C**         KSCFIL    LOKUPDPD                      05
059900020318     C**N05      TASLNA    LOKUPDPD                      05
060000000828     C******* TOLTO CONTROLLO CON LA LNP BASTA LNA E LINEA KSC 27/7
060100000828     C**      28/8
060200000828     C**      BISOGNA LASCIARE IL CONTROLLO ANCHE PER LNP PER I RESI
060300000828     C**      DPD CHE SONO IN ASSEGNATO SU LNA E CLIENTE ITALIA MA
060400000828     C**      LNP DPD
060500000828     C**      NON RICORDO I PROBLEMI CHE PUO' DARE USARE ANCHE LA LNP
060600000828     C**      HO DEI DUBBI SUI DROTTAMENTI 01 PER 190 E POI 190 PER 310
060700000828     C**      FORSE E' STATO SCELTO IL "MALE MINORE"
060800020318     C**N05      TASLNP    LOKUPDPD                      05
060900020318     C**         *IN05     IFEQ *ON
061000020318     C**                   MOVEL'S'       WBODPD  1
061100020318     C**                   MOVEL'I'       WBOEST  1
061200020318     C**                   ENDIF
061300001201     C*
061400001201     C* VERIFICO SE CODICE APPARTIENE ALLA SDI
061500001201     C*
061600001201     C                   CLEAR                   KSDIT
061700001201     C     KSCFIL        CHAIN     AZORG01L                           07
061800001201     C     *IN07         IFEQ      *OFF
061900001201     C                   MOVEL     ORGDIT        KSDIT             3
062000001201     C                   ENDIF
062100980605     C**
062200980605     C**
062300020222     C* PER 8888 E 9999 --> CERCO LA TARIFFA DI CARTELLO RELATIVA AL TIPO DI SPEDIZIONE
062400980605    1C     KSCCOD        IFEQ      8888
062500980527     C     KSCCOD        OREQ      9999
062600020225      * CERCO LA TARIFFA DI CARTELLO
062700020225     C                   EXSR      CERTCA
062800020319     C                   Z-ADD     CARKSC        KSC2
062900020319     C                   Z-ADD     CARCTR        CTR2
063000020319     C                   Z-ADD     CARCTR        CODCTR
063100020319     C                   Z-ADD     CARPRG        PRG2
063200020225      *
063300980605     C     KTAM2         CHAIN     TNTAM00L                           94
063400020222      *
063500980605     C** NON TROVATA (IMPOSSIBILE!!!)
063600980605    2C     *IN94         IFEQ      *ON
063700980605     C                   MOVEL     MSG(10)       TASMSG
063800980605     C                   GOTO      FINE
063900980605    2C                   ENDIF
064000980605     C* KSCCOD NON LO IMPOSTO PERCHE'PER I 9999 VOGLIO CHE RIMANGA TALE
064100980605     C**
064200980605   X1C                   ELSE
064300941112     C** ACCESSO TNTAM
064400980605     C                   Z-ADD     TASCTR        CODCTR            3 0
064500980605     C                   Z-ADD     TASKSC        CODCLI            7 0
064600980605     C**
064700941110     C                   Z-ADD     CODCLI        TMCLI             7 0
064800980515     C                   Z-ADD     CODCTR        TMCTR             3 0
064900941110     C                   EXSR      CERTAM
065000020227     C** SE 90 /94 SONO SPENTI NON HO TROVATO LA TARIFFA DI CARTELLO RELATIVA ALLA TARIFFA
065100020319     C** DEL CLIENTE   (QUESTO CASO L'HO ELIMINATO)
065200020227     C     *IN90         IFEQ      *OFF
065300020227     C     *IN94         ANDEQ     *OFF
065400020225     C*** NON SO COSA FARE ANCHE PERCHE' E' IMPOSSIBILE MA VADO A FINE LO STESSO
065500020225     C                   SETON                                        94
065600020225     C                   GOTO      FINE
065700020225     C                   ENDIF
065800980604     C** NON C'E' TARIFFA CLIENTE
065900980605    2C     *IN94         IFEQ      *ON
066000980604     C**
066100980605     C* SE NON HO CHIAMATO PER UNA VARIA  -- > ERRORE
066200020416     C* NON DO ERRORE SOLO PER LA VARIA "R"
066300020416    3C     TASSVA        IFNE      'R'
066400980604     C                   GOTO      FINE
066500980605   X3C                   ELSE
066600980604     C**
066700020416     C** SE HO CHIAMATO PER LA VARIA 'R' --> IMPOSTO LA TAR.CARTELLO
066800980605     C* IMPOSTO ANCHE KSCCOD perche' voglio che me lo tratti come
066900980605     C*  cliente generico da tariffa di cartello per l'assicurazione
067000980605     c*  Non ho problemi per i 9999 èerche' avendoli gia' impostati
067100980605     c*  la prima volta che fa  certam trova la tariffa (di cartello)
067200980605     C                   MOVE      8888          KSCCOD
067300020225     C* CERCO LA CARTELLO IN BASE ALLE CARATTERISTICHE DELLA SPEDIZIONE EW DEL CLIENTE
067400020225     C                   EXSR      CERTCA
067500020319     C                   Z-ADD     CARKSC        KSC2
067600020319     C                   Z-ADD     CARCTR        CTR2
067700020319     C                   Z-ADD     CARCTR        CODCTR
067800020319     C                   Z-ADD     CARPRG        PRG2
067900020225     C*
068000980605     C     KTAM2         CHAIN     TNTAM00L                           94
068100980605     C** NON TROVATA (IMPOSSIBILE!!!)
068200980605    4C     *IN94         IFEQ      *ON
068300980605     C                   MOVEL     MSG(10)       TASMSG
068400980605     C                   GOTO      FINE
068500980605    4C                   ENDIF
068600020225     C* MI CARICO LA DIVISA DELLA CARTELLO
068700020225     C                   MOVEL     TAMFLO        DSTA01
068800020225     C                   MOVEL     §TADIV        DIVCAR
068900020225     C*
069000980605    3C                   ENDIF
069100980605    2C                   ENDIF
069200980605    1C                   ENDIF
069300980604     C**
069400941110     C                   Z-ADD     TAMKSC        TDCLI             7 0
069500941110     C                   Z-ADD     TAMPRG        TDPRG             3 0
069600941110     C                   Z-ADD     TAMCTR        TDCTR             3 0
069700980609     C* CAMPO DI OUTPUT
069800000125     C                   MOVEL     TAMBAP        TASBAP
069900980609     C                   MOVEL     TAMFLO        DSTA01
070000000125     C*
070100000125     C* SE BOLLA DPD E TARIFFA DI CARTELLO --> ERRORE SE NON RICHIAMO
070200020319     C*  PER UNA VARIA E SE LA CARTELLO NON E' UNA CARTELLO DPD
070300000125     C     KSCCOD        IFEQ      8888
070400000125     C     KSCCOD        OREQ      9999
070500020416     C     TASSVA        IFNE      'R'
070600000125     C     WBODPD        ANDEQ     'S'
070700020225      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
070800020225     C     §TADPD        ANDNE     'S'
070900000125     C                   SETON                                        94
071000000125     C                   MOVEL     MSG(13)       TASMSG
071100000125     C                   GOTO      FINE
071200000125     C                   ENDIF
071300020416     C     TASSVA        IFNE      'R'
071400020319     C     WBOFED        ANDEQ     'S'
071500020319      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
071600020319     C     §TAFED        ANDNE     'S'
071700020319     C                   SETON                                        94
071800020319     C                   MOVEL     MSG(16)       TASMSG
071900020319     C                   GOTO      FINE
072000020319     C                   ENDIF
072100000125     C                   ENDIF
072200000125     C*
072300000125     C* SE TARIFFA O CLIENTE TARIFFA DPD E BOLLA NON LO E'--> MANCA TAR
072400000125     C     §TADPD        IFEQ      'S'
072500000125     C     WBODPD        IFEQ      ' '
072600000125     C                   SETON                                        94
072700000125     C                   MOVEL     MSG(12)       TASMSG
072800000125     C                   GOTO      FINE
072900000125     C                   ENDIF
073000000125     C                   ELSE
073100000125     C     WBODPD        IFEQ      'S'
073200000125     C                   SETON                                        94
073300000125     C                   MOVEL     MSG(13)       TASMSG
073400000125     C                   GOTO      FINE
073500000125     C                   ENDIF
073600000125     C                   ENDIF
073700020319     C* SE TARIFFA O CLIENTE TARIFFA FED E BOLLA NON LO E'--> MANCA TAR
073800020319     C     §TAFED        IFEQ      'S'
073900020319     C     WBOFED        IFEQ      ' '
074000020319     C                   SETON                                        94
074100020319     C                   MOVEL     MSG(15)       TASMSG
074200020319     C                   GOTO      FINE
074300020319     C                   ENDIF
074400020319     C                   ELSE
074500020319     C     WBOFED        IFEQ      'S'
074600020319     C                   SETON                                        94
074700020319     C                   MOVEL     MSG(16)       TASMSG
074800020319     C                   GOTO      FINE
074900020319     C                   ENDIF
075000020319     C                   ENDIF
075100000125     C**
075200990927     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
075300990927     C     §TADIV        IFEQ      *BLANK
075400990927     C                   MOVE      'ITL'         §TADIV
075500990927     C                   ENDIF
075600011127      * VERIFICO IN BASE ALLA DATA TASSAZIONE E DATA SPEDIZIONE SE LA DIVISA DELLA TARIFFA
075700011127      * E' CORRETTA
075800011127      *
075900011127      * SE STO TASSANDO con data > del 2001 un bolla con data > del 2001 E LA DIVISA NON E'
076000011127      * UGUALE ALLA MONETA DI CONTO AZIENDALE
076100020826     C***        DATTAS    IFGT 20011231
076200020826     C     §TADIV        IFNE      §GEDCN
076300011127     C     TASDSP        ANDGT     20011231
076400011127      * MANCA TARIFFA
076500011127     C                   SETON                                        94
076600011127     C                   MOVEL     MSG(14)       TASMSG
076700011127     C                   GOTO      FINE
076800011127     C                   ENDIF
076900991014     C*
077000991014     C* RECUPERO LE CARATTERISTICHE DELLA DIVISA
077100991014     C                   Z-ADD     1             K
077200991014     C     §TADIV        LOOKUP    CV(K)                                  10
077300991014     C     *IN10         IFEQ      *ON
077400991014     C     K             OCCUR     DSCV
077500991014     C     §CVFDC        IFEQ      'S'
077600991014     C* DIVISA CHE ACCETTA DECIMALI
077700991014     C                   SETON                                        12
077800991014     C                   ENDIF
077900991014     C                   ENDIF
078000990921     C** RICERCA TNTAM
078100990921     C** VERIFICO SE LA MONETA PASSATA E'= A QUELLA  DELLA TARIFFA
078200990921     C     TASDIV        IFNE      §TADIV
078300990928     C* E LA TARIFFA PASSAT NON E' UGUALE A BLANK
078400990928     C     TASDIV        ANDNE     *BLANKS
078500990921     C** CONVERTO GLI IMPORTI NELLA MONETA DELLA TARIFFA CLIENTE
078600990921     C*  ANCHE LE VARIE
078700011127     C*
078800011127     C* PASSO LE MONETE PER LA CONVERSIONE ED IN PIÙ IL FLAG SE DEVO
078900011127     C* CONVERTIRE LA QUANTITA' DA FATTURARE OPPURE NO
079000011127     C* NEL CASO IN CUI CONVERTO NELLA MONETA DELLA TARIFFA
079100011127     C* NON E' NECESSARIO CONVERTIRE LA QTA DA FATTURARE E
079200011127     C* L'IMPORTO D'ASSICURARE CALCOLATO IN TASSAZIONE
079300011127     C                   MOVE      TASDIV        DIVINP            3
079400011127     C                   MOVE      §TADIV        DIVOUT            3
079500011127     C                   MOVE      'N'           CONQTA            1
079600011127     C                   MOVE      'N'           CONIAL            1
079700990921     C                   EXSR      CNVTAS
079800990921     C*
079900990921     C                   ENDIF
080000990929     C*------------------------------------------------------
080100990929     C* SE IMPONIBILE VALORIZZATO VADO A FINE
080200990929     C     TASIMV        IFNE      *ZEROS
080300000103     C* E NON E' STATA RICHIESTA NESSUNA VARIA
080400000103     C     TASSVA        ANDEQ     ' '
080500990929     C                   GOTO      FINE
080600990929     C                   ENDIF
080700980506     C*------------------------------------------------------
080800020826     C* VERIFICO SE APPLICARE IL PESO VDL O MENO
080900020826     C                   EXSR      CTRPES
081000020826     C*------------------------------------------------------
081100980506     C** PORTO O INOLTRO PRETASSATO
081200990721     C**  OPPURE DEVO CALCOLARE UNA VARIA SPECIFICA --> NON CERCO TITAD
081300941110     C     TASPOR        IFGT      0
081400980506     C     TASINL        ORGT      0
081500980506     C     TASSVA        ORNE      ' '
081600000516     C**                   Z-ADD0         TASPVL
081700941110     C                   GOTO      TASSAT
081800941110     C                   END
081900941110     C*
082000941112     C                   MOVEL     CODCTR        UNOCTR            1 0
082100980506     C* QUANTITA' DA FATTURARE
082200950119     C     UNOCTR        IFEQ      5
082300950210     C     TASQFT        COMP      0                                      94
082400980512     C   94              MOVEL     MSG(6)        TASMSG
082500980512     C   94              GOTO      FINE
082600950119     C                   END
082700950119     C** TARIFFA A QUANTITA' - MANCA Q.TA' DA FATTURARE
082800950119     C     UNOCTR        IFEQ      4
082900950119     C     TAMFVD        IFEQ      *BLANKS
083000950119     C     TAMFVD        OREQ      '2'
083100961007     C     TAMFVD        OREQ      '4'
083200950210     C     TASQFT        COMP      0                                      94
083300980512     C   94              MOVEL     MSG(6)        TASMSG
083400950119     C   94              GOTO      FINE
083500950120     C                   GOTO      POIQTF
083600950119     C                   END
083700950119     C** TARIFFA A VALORE FLAG 2 E BLANK
083800950119     C     TAMFVD        IFEQ      '1'
083900950210     C     TASQFT        CABGT     0             POIQTF
084000950119     C                   END
084100950119     C*
084200950210     C                   Z-ADD     TASIAS        TASQFT
084300950119     C                   Z-ADD     1             K
084400950119     C     TASVAS        LOOKUP    CV(K)                                  05
084500980527     C     *IN05         IFEQ      *OFF
084600980527     C                   SETON                                        94
084700980527     C                   MOVEL     MSG(2)        TASMSG
084800980527     C                   GOTO      FINE
084900980527     C                   ENDIF
085000990917     C**
085100990917     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA
085200990917     C                   CLEAR                   YEUR
085300990917     C                   MOVEL     TASVAS        YECDVI
085400990917     C                   MOVEL     §TADIV        YECDVO
085500990917     C                   Z-ADD     TASQFT        YECIMI
085600991014     C   12              MOVE      '3'           YECDCO
085700991014     C  N12              MOVE      '0'           YECDCO
085800990917     C*
085900990917     C                   CALL      'YEURCO'
086000990917     C                   PARM                    YEUR
086100990917     C*
086200990917     C     YECESI        IFEQ      ' '
086300990917     C                   Z-ADD     YECIMO        TASQFT
086400990917     C                   END
086500990923     C****                 ENDIF
086600990917     C****       K         OCUR DSCV
086700990917     C****       TASVAS    IFNE §CVITA
086800990917     C****       TASQFT    MULT §CVCMB    TASQFT
086900990917     C****                 END
087000950119     C* CAMBIO
087100950210     C     TASQFT        IFEQ      0
087200950119     C                   EXSR      CALVAS
087300950210     C                   Z-ADD     IMPVAS        TASQFT
087400950119     C                   END
087500950210     C     TASQFT        COMP      0                                      94
087600980527     C   94              MOVEL     MSG(6)        TASMSG
087700950119     C   94              GOTO      FINE
087800950119     C** TARIFFA A VALORE FLAG 1 E 3 - NON ESISTE VALORE ASSICURATIVO
087900950119     C                   END
088000950119     C     POIQTF        TAG
088100980518     C* MI SERVE MINTAS PER IL CARICAMENTO DELLO SCAGLIONE DEL
088200980518     C*  MINIMO TASSABILE
088300980518     C                   Z-ADD     1             A
088400980518     C                   MOVEL     UNOCTR        WTPG              1
088500980518     C     WTPG          LOOKUP    CTR(A)                                 05
088600980518     C   05A             OCCUR     DSTR
088700980518     C   05              Z-ADD     §TRATA        MINTAS
088800990722     C*
088900990722     C** RECUPERO CODICE NAZIONE MITTENTE E DESTINATARIO DAL CODIEC TASSAZIONE
089000990722     C*
089100990923     C***                  Z-ADD1         A
089200990923     C***        TASCTS    LOKUPCTS,A                    05
089300990923     C***05      A         OCUR DSCT
089400990923     C***05                MOVEL§CTNAR    TASNAD  3
089500990722     C*
089600990923     C***                  Z-ADD1         A
089700990923     C***        TASMCT    LOKUPCTS,A                    05
089800990923     C***05      A         OCUR DSCT
089900990923     C***05                MOVEL§CTNAR    TASNAM  3
090000980518     C*
090100990721     C** RICERCA TITAD
090200980518     C                   EXSR      CARTAR
090300980518     C*
090400980313     C** PESO TASSABILE E QUANTITA' DA FATTURARE
090500980429     C                   MOVEL     UNOCTR        WTPG              1
090600980312     C                   CLEAR                   WRPV
090700040603     C                   CLEAR                   RAPPV
090800980312     C                   MOVEL     'TAD'         WDET              3
090900980313     C                   Z-ADD     TAMARL        WARL
091000980313     C                   Z-ADD     TAMARF        WARF
091100980313     C                   Z-ADD     TAMARO        WARO
091200980312     C                   EXSR      CALCOL
091300980313     C                   Z-ADD     ISOPES        PESTAS
091400941112     C*--------------------------------------------------------------
091500980313     C*  PESO TASSABILE PER TARIFFE A PESO
091600980313     C                   EXSR      CALPT
091700941112     C*--------------------------------------------------------------
091800941110     C     TASSAT        TAG
091900990922     C** ESEGUE TASSAZIONE BOLLA
092000941110     C                   EXSR      TASSA
092100990922     C** MANCA TARIFFA
092200941110     C   94              GOTO      FINE
092300941110     C                   ENDSR
092400941112     C******************************************************
092500941112     C** GUIDA PER TASSAZIONE SPEDIZIONE
092600941112     C******************************************************
092700941110     C     TASSA         BEGSR
092800941110     C                   SETOFF                                       94
092900090923     C                   clear                   WvariaMag
093000090924     c                   clear                   WImpoMag
093100090923
093200970522     C** FLAG PER APPLICAZIONE INOLTRO
093300001009     C*!!* PRIMA SI FACEVA COSÌ *!!*
093400001009     C*!!* ** ASSEGNATI-MITTENTE FRANCHI-DESTINATARIO
093500001009     C*!!* ** PER 99 SEMPRE DESTINATARIO
093600001009     C*!!*           TIPBOL    COMP 'A'                      57
093700001009     C*!!*   57      KSCCOD    COMP 9999                 5757
093800001009     C*!!*   57                MOVELTASFAP    FLGINO  1
093900001009     C*!!*  N57                MOVELTASFIN    FLGINO
094000001009     C*!!*
094100001009     C*!!* ORA CONTROLLO LA TARIFFA :
094200001009     C*!!* SE E' USO LA TARIFFA REVERSIBILE (56 ON)  PRENDO COME INOLTRO L'ANTEPORTO TASFAP
094300001009     C*!!* SE USO LA TARIFFA NORMALE (56 OFF) USO L'INOLTRO TASFIN
094400001009     C*!!*
094500001009     C  N56              MOVEL     TASFIN        FLGINO            1
094600001009     C   56              MOVEL     TASFAP        FLGINO
094700001009     C*!!*
094800001009     C*!!*   FINE   *!!*
094900970522     C**
095000980506     C** SE DEVO CALCOLARE UNA VARIA VADO SUBITO DA QUELLA
095100980506     C     TASSVA        IFNE      '  '
095200990916     C     *LIKE         DEFINE    TASQFT        WQFT
095300980506     C** VARIA 'G' --> CONTRASSEGNO
095400980506     C     TASSVA        IFEQ      §$2VRG
095500980506     C                   GOTO      VARIAG
095600980506     C                   ENDIF
095700980525     C** VARIA 'R' --> ASSICURAZIONE PER CONTO
095800980526     C     TASSVA        IFEQ      §$2VRR
095900980525     C                   GOTO      VARIAR
096000980525     C                   ENDIF
096100020412     C** VARIA 'O' --> RICERCA DOCUMENTI
096200020412     C     TASSVA        IFEQ      §$2VRO
096300020412     C                   GOTO      VARIAO
096400020412     C                   ENDIF
096500020516     C** VARIA 'Y' --> RITIRI ANNULLATI
096600020516     C     TASSVA        IFEQ      §$2VRY
096700020516     C                   GOTO      VARIAY
096800020516     C                   ENDIF
096900020516     C** VARIA '*' --> DIROTTAMENTI
097000020523     C     TASSVA        IFEQ      §$2AST
097100020523     C                   GOTO      VARAST
097200020516     C                   ENDIF
097300030403     C** VARIA 'W' --> RECAPITO C/ASSEGNI
097400030403     C     TASSVA        IFEQ      §$2VRW
097500030403     C                   GOTO      VARVRW
097600030403     C                   ENDIF
097700030527     C** VARIA 'M' --> ADDEBITO INSOLUTI INTERESSI DI MORA
097800030527     C     TASSVA        IFEQ      §$2VRM
097900030527     C                   GOTO      VARVRM
098000030527     C                   ENDIF
098100030626     C** VARIA 'T' --> ORM COMMISSIONATO
098200030626     C     TASSVA        IFEQ      §$2VRT
098300030626     C                   GOTO      VARVRT
098400030626     C                   ENDIF
098500040910     C** VARIA 'a' --> P.O.D IMMAGE
098600040910     C     TASSVA        IFEQ      §$2POD
098700040910     C                   GOTO      VARPOD
098800040910     C                   ENDIF
098900030604     C** VARIA 'D' --> DIRITTO DI FATTURAZIONE
099000030604     C     TASSVA        IFEQ      §$2VRD
099100030922     C                   GOTO      FINE
099200030604     C                   ENDIF
099300040113     C** VARIA 'Z' --> ADDIZIONALE DI GESTIONE + ISTAT
099400040113     C     TASSVA        IFEQ      §$2VRZ
099500040113     C                   GOTO      FINE
099600040113     C                   ENDIF
099700980506     C                   ENDIF
099800970522     C**
099900941123     C** SE IL PORTO O L'INOLTRO SONO PRETASSATI
100000941110     C** DEBBO ESCLUDERE LA FASE DI CALCOLO TARIFFA
100100090924     C     TASPOR        CABGT     0             DOPOIN
100200090924     C     TASINL        CABGT     0             DOPOIN
100300980508     C*****
100400980508     C** CALCOLO    P O R T O     ED    I N O L T R O
100500980508     C*****
100600980506     C**
100700020218     C                   DO        200           A
100800941110     C     A             OCCUR     TARDS
100900941110     C     TSCG          IFGE      PESTAS
101000941110     C                   GOTO      ENDTAS
101100941110     C                   END
101200941110     C                   END
101300980508     C** NON TROVATO SCAGLIONE ESATTO
101400941110     C     TSCG          IFLT      PESTAS
101500941110     C                   SETON                                        94
101600980512     C                   MOVEL     MSG(5)        TASMSG
101700980512     C                   GOTO      FINE
101800941110     C                   END
101900941110     C     ENDTAS        TAG
102000980512     C** MI SALVO LA POSIZIONE DELLO SCAGLIO TROVATO CHE MI
102100980512     C*  SERVE PER IL CALCOLO TARIFFA CON GARANTITO MAX SCAGLIONE PREC
102200980512     C     *LIKE         DEFINE    A             WA
102300980512     C                   Z-ADD     A             WA
102400980508     C**
102500980508     C* CALCOLO PER QUALE VALORE MOLTIPLICARE LA TARIFFA
102600980508     C                   Z-ADD     PESTAS        WPTAS3
102700980508     C                   EXSR      CALQLI
102800980508     C**
102900980508     C* MODO APPLICAZIONE TARIFFA
103000980508     C                   MOVEL     TAMFLO        DSTA01
103100990927     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
103200990927     C     §TADIV        IFEQ      *BLANK
103300990927     C                   MOVE      'ITL'         §TADIV
103400990927     C                   ENDIF
103500980512     C**
103600980512     C* CALCOLO NORMALE ANCHE PER SCALARE AL DI SOTTO
103700980512     C*  DELL'APPLICAZIONE TARIFFA FINITA E PER MAX SCAGLIONE PREC
103800980512     C     §TAF02        IFNE      'S'
103900980512     C     §TAF02        OREQ      'S'
104000980512     C     WATA          ANDNE     ' '
104100980508     C** MOLTIPLICAZIONE TARIFFA
104200980508     C                   EXSR      MULTAR
104300980515     C* APPLICAZIONE MINIMO E MASSIMO
104400980515     C                   EXSR      MINMAX
104500980508     C                   Z-ADD     WPOR          TASPOR
104600980508     C                   Z-ADD     WINL          TASINL
104700980514     C                   ENDIF
104800980508     C**
104900980508     C** S C A L A R E
105000980512     C     §TAF02        IFEQ      'S'
105100980508     C                   EXSR      SCALAR
105200980512     C                   ENDIF
105300980508     C**
105400980508     C** M A X   S C A G L I O N E   P R E C E D E N TE
105500980512     C     §TAF02        IFEQ      'G'
105600980512     C                   EXSR      MAXSCA
105700980512     C                   ENDIF
105800990923     C**
105900990923     C** SE DIVISA DELLA TARIFFA NON ACCETTA I DECIMALI PREPARO IL
106000990923     C** DEL PORTO E DELL'INOLTRO SENZA DECIMALI
106100990923     C     *IN12         IFEQ      '0'
106200990923     C* PORTO
106300991112     C     TASPOR        ADD       0,5           CAMPOW           11 0
106400990923     C                   Z-ADD     CAMPOW        TASPOR
106500990923     C* INOLTRO
106600991112     C     TASINL        ADD       0,5           CAMPOW
106700990923     C                   Z-ADD     CAMPOW        TASINL
106800990923     C                   ENDIF
106900090924     c                   eval      WImpoMag=taspor+tasinl
107000980508     C**
107100970521     C*******************************
107200990921     C****************
107300990921     C** TIPO SERVIZIO
107400990921     C****************
107500001127     C*
107600090924     C**                 Z-ADD     1             A
107700090924     C**                 clear                   ESP
107800001127     C*
107900001127     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
108000001127     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
108100001127     C*
108200090924     C**   KSDIT         IFEQ      'SDI'
108300090924     C**   TASDSP        ANDLE     20001231
108400090924     C**   TASTSP        ANDEQ     'C'
108500090924     C**   'E'           LOOKUP    TS(A)                                  05
108600090924     C**                 ELSE
108700090924     C**   TASTSP        LOOKUP    TS(A)                                  05
108800090924     C**                 ENDIF
108900001127     C*
109000090924     C**N05              GOTO      DOPOIN
109100090924     C**   A             OCCUR     DS5E
109200090924     C**   §5EFTC        CABEQ     *BLANKS       DOPOIN
109300090922
109400980430     C** TIPO SERVIZIO SENZA TARIFFA PARTICOLARE
109500990923     C** PORTO
109600090924     C**                 Z-ADD     0             WTSP
109700090924    1C**   TASPOR        IFGT      0
109800090924     c**                 exsr      CerESPRESSO
109900090924     c**
110000060414     c* Se c'e' già la varia, non la calcolo
110100090924    2c**                 if        giaespr<>'S'
110200090924     C**                 Z-ADD     TASPOR        IMPON
110300090924     c**                 if        *in56
110400090924     c**                 eval      applicarevers='S'
110500090924     c**                 endif
110600090924     C**                 EXSR      CALTS
110700090924     c**                 clear                   applicarevers
110800090924    3C**   WTSP          IFGT      0
110900060414     C*****              ADD       WTSP          TASPOR
111000060414     c
111100090924    4c**                 if        esp<>999
111200090924     C**                 MOVEL     wVariaMag     SV(ESP)
111300090924     C**                 add       wtsp          VA(ESP)
111400090924     c**                 else
111500060414     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
111600090924     C**                 ADD       wtsp          TASPOR
111700090924    4c**                 endif
111800090924     c**
111900090924    3C**                 ENDif
112000090924    2C**                 ENDif
112100090924    1C**                 ENDif
112200990923     C** INOLTRO
112300090924     C**                 Z-ADD     0             WTSP
112400090924    1C**   TASINL        IFGT      0
112500090924     c**                 EXSR      CerESPRESSO
112600090924    2c**                 if        giaespr<>'S'
112700090924     C**                 Z-ADD     TASINL        IMPON
112800090924     c**                 if        *in56
112900090924     c**                 eval      applicarevers='S'
113000090924     c*+                 endif
113100090924     C**                 EXSR      CALTS
113200090924     c**                 clear                   applicarevers
113300090924    3C**   WTSP          IFGT      0
113400060414     C*****              ADD       WTSP          TASINL
113500090924    4c**                 if        esp<>999
113600090924     C**                 MOVEL     wVariaMag     SV(ESP)
113700090924     C**                 add       wtsp          VA(ESP)
113800090924   x4c**                 else
113900060414     C** SE NON CI SONO VARIE LIBERE AGGIUNGO ALla voce
114000090924     C**                 ADD       WTSP          TASINL
114100090924    4c**                 endif
114200090924    3C**                 END
114300090924    2C**                 END
114400090924    1C**                 END
114500090924     c
114600991012     C     DOPOIN        TAG
114700991012     C**
114800990923     C* VALORIZZO LA VARIA DELL'INOLTRO NELLA SCHIERA DELLE VARIE
114900991012     C     TASINL        IFGT      0
115000990923     C                   Z-ADD     1             X                 2 0
115100990923     C     §$2FIN        LOOKUP    SV(X)                                  30
115200990923     C* SE NON ESISTE CARICOLA SCHIERA
115300990923     C     *IN30         IFEQ      '0'
115400990923     C                   Z-ADD     1             X
115500990923     C     ' '           LOOKUP    SV(X)                                  30
115600990923     C   30              MOVEA     §$2FIN        SV(X)
115700990923     C                   ENDIF
115800990923     C* SE LO TROVO VALORIZZO TASINL
115900990923     C   30              Z-ADD     TASINL        VA(X)
116000991012     C                   END
116100990921     C****************************************
116200990921     C** ISOLA  LOCALITA' DISAGIATE C.STORICI
116300990921     C****************************************
116400970521     C** SE TROVATA TARIFFA DEL PAESE APPLICO LO STESSO L'ISOLA
116500970521     C**
116600970721    1C     FLGINO        IFEQ      'I'
116700970521     C     FLGINO        OREQ      'D'
116800110624      * sostituisco il controllo del centro storico con i nuovi flag T o Z
116900110624      * T = centro storico in città
117000110624      * Z = centro storico in provincia
117100110624     C**   FLGINO        OREQ      'S'
117200110624     C     FLGINO        OREQ      'T'
117300110624     C     FLGINO        OREQ      'Z'
117400970521     C* IMPOSTO LA TARIFFA PARTICOLARE
117500970521     C                   SELECT
117600970521     C     FLGINO        WHENEQ    'I'                                          ISOLA
117700970521     C                   MOVEL     'I'           TASTC
117800970521     C     FLGINO        WHENEQ    'D'                                          LOCALITA' DI
117900970521     C                   MOVEL     'K'           TASTC
118000110624      * sostituisco il controllo del centro storico con i nuovi flag T o Z
118100110624      * T = centro storico in città
118200110624      * Z = centro storico in provincia
118300110624     C**   FLGINO        WHENEQ    'S'                                          C.STORICO
118400110624     C     FLGINO        WHENEQ    'T'                                          C.STORICO
118500970521     C                   MOVEL     'Q'           TASTC
118600110624     C     FLGINO        WHENEQ    'Z'                                          C.STORICO
118700110624     C                   MOVEL     'Q'           TASTC
118800970521     C                   ENDSL
118900110624      ** visto che l'attivazione del calcolo del centro storico dovrebbe essere
119000110909      ** il 3/10/11  attivo il flag "Q" in tastc confrontando la dta spedizione
119100110909     c                   if        tasdsp < 20111003 and tastc = 'Q'
119200110630     c                   clear                   tastc
119300110630     c                   endif
119400110628      ** Se TASTC è valorizzato calcolo la varia. Questa specifica si può togliere
119500110628      ** quando abilitiamo il centro storico
119600110628 bis1c                   If        tastc <> ' '
119700110628     c
119800970521     C**
119900060414     c* 14/4/06-ES--> il pgm per tassare questa tar.particolare usa
120000060414     c*           sempre il cod.tassazione mittente anche se non
120100060414     c*           applica la reversibilità. Da sempre lo fa.
120200060414     c*           Abbiamo valutato che è sbagliato ma deve seguire
120300060414     c*           la regola della reversibilità. Per cui non lo devo
120400060414     c*           più fare per tipbol='A' ma per 56 ON
120500060414     C**   TIPBOL        IFEQ      'A'
120600060414     C                   IF        *in56
120700981008     C                   MOVEL     TASCTS        WCTS              2
120800941123     C                   MOVEL     TASMCT        TASCTS
120900060414     C                   ENDif
121000970521     C*
121100970521     C                   EXSR      TASPAR
121200060414     C**   TIPBOL        IFEQ      'A'
121300060414     C                   IF        *in56
121400970721     C                   MOVEL     WCTS          TASCTS
121500970721     C                   END
121600970721     C**
121700970721     C* SOLO SE L'HO CALCOLATA FACCIO LE COSE SOTTOSTANTI
121800970721    2C     WNOEST        IFEQ      'S'
121900970721     C     TASCP         ANDGT     0
122000970522     C* MEMORIZZO PORTO E POSIZIONE VARIA DOVE HO MEMORIZZATO
122100970522     C                   Z-ADD     TASCP         TASISO
122200090924     c* imponibile per maggiorazione "E" o "H"
122300090924     c                   eval      wImpoMag=wImpoMag+TASISO
122400970522     C*
122500970522     C  N96              MOVEL     'S'           WPORTO            1
122600970522     C* SALVO L'INDICE DELLA SCHIERA DELLA VARIA
122700970522     C   96              Z-ADD     A             ISO               2 0
122800970522     C   96              MOVEL     ' '           WPORTO            1
122900970521     C*
123000970521     C** SE VA IN SOSTITUZIONE CLEARO L'INOLTRO
123100970521     C** FLAG=S INOLTRO+ISOLA
123200970521     C** FLAG=N SOLO ISOLA
123300110628     c** In caso di centro storico  non considero il flag FLGISO in quanto non
123400110628     c** non deve andare in sostituzione dell'inoltro ed in tariffa non viene
123500110628     c** richiesta la valorizzazione quindi è sempre diverso da "S"
123600970721    3C     FLGISO        IFNE      'S'
123700110628     c     TASTC         ANDNE     'Q'
123800090924     c* tolgo dall'imponibile per la maggiornazione l'Inoltro
123900090924     c                   eval      wImpoMag=wImpoMag-TASINL
124000090924     c*
124100090924     C                   CLEAR                   TASINL
124200990921     C** RICHIAMO LA VECCHIA POSIZIONE DELLA VARIA E LA SOSTIUISCO CON IL
124300990921     C**  VALORE DELL'ISOLA
124400990921     C** L'INDICE "X" PUNTA ALLA VARIA DELL'INOLTRO
124500990921     C     X             IFGT      *ZERO
124600990921     C* PULISCO L'INOLTRO
124700990921     C                   CLEAR                   VA(X)
124800990921     C                   CLEAR                   SV(X)
124900990921     C* UTILIZZO LA POSIZIONE VUOTA DELLA SCHIERA PER METTERE L'IMPORTO DELLA
125000990921     C* VARIA DELL'ISOLA
125100990921     C                   MOVEL     §1PVAR        SV(X)
125200990921     C                   Z-ADD     TASCP         VA(X)
125300990921     C* PULISCO LA POSIZIONE DELLA SCHIERA PRECEDENTE
125400990921     C   96              CLEAR                   VA(A)
125500990921     C   96              CLEAR                   SV(A)
125600990921     C* SALVO L'INDICE ATTUALE DELLA SCHIERA DELLA VARIA
125700990921     C   96              Z-ADD     X             ISO
125800990922     C                   END
125900970721    3C                   END
126000990921     C***********************
126100990921     C** IS/L.D./C.S  - TIPO SERVIZIO
126200990921     C***********************
126300090924     C**                 Z-ADD     1             A                 5 0
126400001127     C*
126500001127     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
126600001127     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
126700001127     C*
126800090924     C**   KSDIT         IFEQ      'SDI'
126900090924     C**   TASDSP        ANDLE     20001231
127000090924     C**   TASTSP        ANDEQ     'C'
127100090924     C**   'E'           LOOKUP    TS(A)                                  05
127200090924     C**                 ELSE
127300090924     C**   TASTSP        LOOKUP    TS(A)                                  05
127400090924     C*+                 ENDIF
127500001127     C*
127600090924     C** 05A             OCCUR     DS5E
127700090924    3C** 05§5EFTC        IFNE      *BLANKS
127800970521     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
127900090924     c**                 EXSR      CerESPRESSO
128000090924     c**
128100090924   3ac**                 if        giaespr<>'S'
128200090924     C**                 Z-ADD     0             WTSP
128300090924     C**                 Z-ADD     TASISO        IMPON
128400060414     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
128500090924     C**                 IF        *in56
128600090924     c**                 eval      applicarevers='S'
128700090924     c**                 endif
128800090924     C**                 EXSR      CALTS
128900090924     c**                 clear                   applicarevers
129000060414     c
129100090924    4C**   WTSP          IFGT      0
129200090924    5c**                 if        esp<>999
129300090924     C**                 MOVEL     WVariaMag     SV(ESP)
129400090924     C**                 add       wtsp          VA(ESP)
129500090924   x5c**                 else
129600060414     c* Se non c'e' una varia disponibile, sommo al posto o alla
129700060414     c*  varia dell' isola
129800090924    6C**   WPORTO        IFEQ      'S'
129900090924     C**                 ADD       WTSP          TASPOR
130000090924   x6C**                 ELSE
130100090924     C**                 ADD       WTSP          TASISO
130200090924     C**                 Z-ADD     TASISO        VA(ISO)
130300090924    6C**                 END
130400090924    5C**                 END
130500090924    4C**                 END
130600060414     c
130700090924   3aC**                 ENDif
130800090924    3C**                 END
130900090924     c
131000970721    2C                   END
131100110628 bis1C                   ENDIF
131200970721    1C                   ENDIF
131300090924     c
131400090924     C***********************
131500090924     C** Maggiorazione per tipo servizio sul totale imponibile da maggiorare
131600110628     c** ovvero:  PORTO + INOLTRO + ISOLA/DISAGIATE/CENTRO STORICO
131700110628     C** Se porto o inoltro già presenti: solo su ISOLA/DISAGIATE/CENTRO STORICO
131800110628     c** Se isola/disagiate/centro storico già presente: solo su PORTO + INOLTRO
131900090924     C***********************
132000090924     C                   Z-ADD     1             A                 5 0
132100090924     C                   clear                   ESP
132200090924     c
132300090924    1c                   if        WimpoMag>0
132400090924     C*
132500090924     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
132600090924     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
132700090924     C*
132800090924    2C     KSDIT         IFEQ      'SDI'
132900090924     C     TASDSP        ANDLE     20001231
133000090924     C     TASTSP        ANDEQ     'C'
133100090924     C     'E'           LOOKUP    TS(A)                                  05
133200090924     C                   ELSE
133300090924     C     TASTSP        LOOKUP    TS(A)                                  05
133400090924    2C                   ENDIF
133500090924     C*
133600090924     C   05A             OCCUR     DS5E
133700090924    2C   05§5EFTC        IFNE      *BLANKS
133800090924     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
133900090924     c                   EXSR      CerESPRESSO
134000090924     c
134100090924    3c                   if        giaespr<>'S'
134200090924     C                   Z-ADD     0             WTSP
134300090924     c
134400090924     C                   Z-ADD     WImpoMag      IMPON
134500090924     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
134600090924     C                   IF        *in56
134700090924     c                   eval      applicarevers='S'
134800090924     c                   endif
134900090924     C                   EXSR      CALTS
135000090924     c                   clear                   applicarevers
135100090924     c
135200090924    4C     WTSP          IFGT      0
135300090924    5c                   if        esp<>999
135400090924     C                   MOVEL     WVariaMag     SV(ESP)
135500090924     C                   add       wtsp          VA(ESP)
135600090924   x5c                   else
135700090924     c* Se non c'e' una varia disponibile, sommo nel porto
135800090924     C                   ADD       WTSP          TASPOR
135900090924    5C                   END
136000090924    4C                   END
136100090924     c
136200090924    3C                   ENDif
136300090924    2C                   END
136400090924    1C                   END
136500990921     C****************
136600990921     C** DIRITTO FISSO
136700990921     C****************
136800980424     C     TASDIF        IFEQ      0
136900980424     C                   MOVEL     'H'           TASTC
137000990921     C* RICHIAMO LA ROUTINE DI CALCOLO DELLE TARIFFE PARTICOLARI
137100990921     C                   EXSR      TASPAR
137200990921     C   96              Z-ADD     TASCP         TASDIF
137300990921     C***
137400980424     C                   ENDIF
137500990921     C***********************
137600990921     C** PROVVIGIONE ASSEGNO
137700990921     C***********************
137800980506     C     VARIAG        TAG
137900980506     C     *LIKE         DEFINE    TASVA1        PROVAS
138000980506     C                   Z-ADD     0             PROVAS
138100980429     C** MANCA C/A
138200941112     C     TASCAS        CABEQ     0             POIT01
138300980429     C** PROVV. ASSEGNO GIA' PRESENTE
138400941112     C                   Z-ADD     1             A
138500941112     C     §$2VRG        LOOKUP    SV(A)                                  05
138600980429    1C  N05              DO
138700941116     C*
138800941116     C                   SETOFF                                       94
138900941116     C     *LIKE         DEFINE    TASCAS        CASLIT
139000941116     C                   Z-ADD     TASCAS        CASLIT
139100980429     C**
139200990921    2C*!*        TASCMB    IFEQ 0
139300990921     C*!*                  Z-ADD1         K
139400990921     C*!*        TASVCA    LOKUPCV,K                     05
139500990921    3C*!*        *IN05     IFEQ *OFF
139600990921     C*!*                  SETON                     94
139700990921     C*!*                  MOVELMSG,1     TASMSG
139800990921     C*!*                  GOTO FINE
139900990921    3C*!*                  ENDIF
140000941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
140100990921     C*!*        K         OCUR DSCV
140200990921    3C*!*        TASVCA    IFNE §CVITA
140300990921     C*!*        TASCAS    MULT §CVCMB    CASLIT    H
140400941116     C** CAMBIO IN BASE ALLA DIVISA STANDARD
140500990921    3C*!*                  END
140600990921   X2C*!*                  ELSE
140700990921     C*!*        TASCAS    MULT TASCMB    CASLIT    H
140800941116     C** CAMBIO IN BASE AL VALORE ESISTENTE IN BOLLA
140900990921    2C*!*                  END
141000990921     C* CALCOLO IL VALORE DEL CONTRASSEGNO  IN BASE ALLA MONETA
141100020319     C*******
141200020319     C*******
141300020319     C* SE PRESENTE IL C/A ANNULLATO VEDO IN BASE ALLA TARIFFA CLIENTE
141400020319     C*  SE FATTURARE O MENO
141500020319     C*  §TAPCA='N' NON FATTURO GLI ANNULLATI
141600030131     C**!!!§ASSTA        IFEQ      '9'
141700030131     C     TASSTA        IFEQ      '9'
141800030131     C     §TAPCA        ANDEQ     'N'
141900060922      * calcolo sempre la provvigione del C/A  annullato se si tratta di consegna anomala dirottata
142000060922     C     §ASCCA        ANDNE     '1'
142100060922      *
142200020319     C                   GOTO      POIT01
142300020319     C                   ENDIF
142400020319     C**
142500990921     C     TASVCA        IFNE      §TADIV
142600990921     C                   CLEAR                   YEUR
142700990921     C                   MOVEL     TASVCA        YECDVI
142800990921     C                   MOVEL     §TADIV        YECDVO
142900990921     C                   Z-ADD     TASCAS        YECIMI
143000991014     C   12              MOVE      '3'           YECDCO
143100991014     C  N12              MOVE      '0'           YECDCO
143200990921     C*
143300990921     C                   CALL      'YEURCO'
143400990921     C                   PARM                    YEUR
143500990921     C*
143600990921     C     YECESI        IFEQ      ' '
143700990921     C                   Z-ADD     YECIMO        CASLIT
143800990921     C                   ELSE
143900990921     C                   SETON                                        94
144000990921     C                   MOVEL     MSG(1)        TASMSG
144100990921     C                   GOTO      FINE
144200990923     C                   END
144300990921     C                   ENDIF
144400980429     C* PROVV ASSEGNO MITTENTE
144500980429    2C     TASBM         IFEQ      'M'
144600980429     C                   MOVEL     'W'           TASTC
144700980429     C                   ELSE
144800980429     C* PROVV ASSEGNO VETTORE
144900980429     C                   MOVEL     'V'           TASTC
145000980429    2C                   ENDIF
145100060712      * se spedizione EEX cerco sempre provvigione assegno mittente
145200060712     c                   If        o27fie = 'E'
145300060712     c                   movel     'W'           tastc
145400060712     c                   endif
145500980429     C**
145600980429     C                   MOVEL     TASTC         FISO
145700980506     C                   MOVEL     'S'           WCAR
145800980429     C                   EXSR      CERTPT
145900980429    2C     WEND          IFEQ      ' '
146000980429     C**
146100980429    3C     TPTTPG        IFEQ      'V'                                          VALORE
146200980429     C                   Z-ADD     CASLIT        TPDPES
146300980429     C                   ELSE                                                   ALTRO
146400980429     C                   Z-ADD     ISOPES        TPDPES
146500980429    3C                   ENDIF
146600980429     C**
146700980429     C                   EXSR      CALACD
146800941116     C*
146900980429    3C     VARIA         IFGT      0
147000941112     C                   Z-ADD     1             A
147100941112     C     ' '           LOOKUP    SV(A)                                  96
147200941112     C   96              MOVEL     §$2VRG        SV(A)
147300980429     C   96              Z-ADD     VARIA         VA(A)
147400980429     C  N96              ADD       VARIA         TASPOR
147500980429     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
147600980429    3C                   ENDIF
147700980429    2C                   ENDIF
147800980429    1C                   ENDDO
147900980506     C**
148000980506     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
148100980506     C     TASSVA        IFEQ      §$2VRG
148200980506     C                   GOTO      FINE
148300980506     C                   ENDIF
148400990921     C***********************
148500990921     C** DIRITTO ASSICURATIVO
148600990921     C***********************
148700941110     C     POIT01        TAG
148800941112     C                   Z-ADD     1             A
148900941112     C     §$2VRE        LOOKUP    SV(A)                                  05
149000941112     C  N05              DO
149100941112     C                   Z-ADD     0             DIRITT
149200941112     C     *LIKE         DEFINE    TASVA1        DIRITT
149300941110     C                   EXSR      CALVE
149400941112     C     DIRITT        IFGT      0
149500941112     C                   Z-ADD     1             A
149600941112     C     ' '           LOOKUP    SV(A)                                  96
149700941112     C   96              MOVEL     §$2VRE        SV(A)
149800941112     C   96              Z-ADD     DIRITT        VA(A)
149900941112     C  N96              ADD       DIRITT        TASPOR
150000941112     C                   END
150100941112     C                   END
150200941112     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
150300990921     C***********************
150400990921     C** ASS. X CONTO
150500990921     C***********************
150600980525     C     VARIAR        TAG
150700980525     C                   Z-ADD     0             WVARIR
150800060322     C                   Z-ADD     0             WVARIACB
150900980525     C     *LIKE         DEFINE    TASVA1        WVARIR
151000060321     C     *LIKE         DEFINE    TASVA1        WVARIACB
151100060320      * cerco la varia R sia in caso di bolla con importo d'assicurare e sia in caso di
151200060320      * ricerca di mandato
151300941112     C                   EXSR      CALVR
151400941112     C** MANCA TARIFFA:MANDATO VALORE = 0 E TASIAS=0
151500980526     C   94              GOTO      FINE
151600980526     C**
151700980525     C     WVARIR        IFGT      0
151800941112     C                   Z-ADD     1             A
151900941112     C     ' '           LOOKUP    SV(A)                                  96
152000941112     C   96              MOVEL     §$2VRR        SV(A)
152100980525     C   96              Z-ADD     WVARIR        VA(A)
152200060320      ** se non ci sono varie libere aggiungo al porto
152300980525     C  N96              ADD       WVARIR        TASPOR
152400060320
152500060320      * se varia uguale a zero e non ho trovato nessun mandato e non c'è importo d'assicurare in
152600060320      * bolla cerco la nuova AC base
152700060320
152800060320     c                   else
152900060320
153000060321     c                   If        task88 = 'X' and
153100060321      ** 88 = non si calcola
153200060322     c                             (Ksccod <> 8888 and Ksccod <> 9999) and
153300060322      ** data spedizione maggiore del 28 02 2006
153400060322     c                             tasdsp > 20060228
153500060320     c                   exsr      CALVACB
153600060320      ** carico la varia nella schiera
153700060321     c                   If        wvariacb > 0
153800060320     c                   z-add     1             a
153900060320     c     ' '           Lookup    sv(a)                                  96
154000060320     c   96              movel     §$2Acb        sv(a)
154100060321     c   96              z-add     Wvariacb      va(a)
154200060320      ** se non ci sono varie libere aggiungo al porto
154300060321     c  N96              add       Wvariacb      TASpor
154400060320     c                   endif
154500060320
154600060320     c                   endif
154700060320
154800060320     c                   END
154900980525     C**
155000980525     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
155100980525     C     TASSVA        IFEQ      §$2VRR
155200980525     C                   GOTO      FINE
155300980525     C                   ENDIF
155400990722     C***************
155500990722     C* T.C.P.
155600990722     C***************
155700990722     C*
155800990722     C                   MOVEL     §$2VRP        TASTC
155900990722     C                   EXSR      TASPAR
156000030624     C***************************************
156100030624     C* ANNULLAMENTO PORTO ASSEGNATO VARIA &
156200030624     C***************************************
156300030624     C*
156400030624     C* SE IL FLAG DI CALCOLO VARIA & È UGUALE A S
156500030624     C                   IF        TASFCAA = 'S'
156600030624     C                   MOVEL     §$2VPA        TASTC
156700030624     C                   EXSR      TASPAR
156800030624     C                   ENDIF
156900050928     C****************************
157000050928     C* LASCIATO AVVISO  VARIA 'c'
157100050928     C****************************
157200050929     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
157300050928     C                   If        tasric <> ' '
157400090305      * verifico se linea di arrivo o linea di partenza è estera non calcolo la varia "c"
157500051028     c     taslna        lookup    esttot                                 30
157600090305      * se linea di arrivo non è estero verifico la linea di partenza
157700090305     c  n30taslnp        lookup    esttot                                 30
157800090305      * se spedizione italiana calcolo
157900051028     c                   If        not *in30
158000050928     C                   Movel     §$2vla        Tastc
158100050928     C                   Exsr      TASpar
158200051005     c                   Endif
158300051005
158400050928     c                   Endif
158500110628     C****************************
158600110628     C* AVVISO AL DESTINATARIO AFFIDAMENTO SPEDIZIONE VARIA 'm'
158700110628     C****************************
158800110628     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
158900110628     C* in TASFLO §asemd
159000130920     C                   If        §asemd = 'S' or §asemd = 'X' or
159100130920     c                             §asemd = 'E'
159200110628     C                   Movel     §$2vad        Tastc
159300110628     C                   Exsr      TASpar
159400110628     c                   Endif
159500110628
159600141215     C****************************
159700141215     C* PIN CODE  CONSEGNA CON VERIFICA PIN CODE VARIA "p"
159800141215     C****************************
159900141215     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
160000141215     C* in TASFLO §aspin
160100141215     C                   If        §aspin = 'S'
160200141215     C                   Movel     §$2pin        Tastc
160300141215     C                   Exsr      TASpar
160400141215     c                   Endif
160500141215
160600020412     C***************
160700020412     C* RICERCA DOCUMENTI - SOLO SE RICHIESTA DA SOLA
160800020412     C***************
160900020412     C*
161000020412     C     VARIAO        TAG
161100020412     C     TASSVA        IFEQ      §$2VRO
161200020412     C                   MOVEL     §$2VRO        TASTC
161300020412     C                   EXSR      TASPAR
161400020412     C**
161500020412     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
161600020412     C     TASSVA        IFEQ      §$2VRO
161700020412     C                   GOTO      FINE
161800020412     C                   ENDIF
161900020412     C                   ENDIF
162000020516     C***************
162100020516     C* RITIRI ANNULLATI  - SOLO SE RICHIESTA DA SOLA
162200020516     C***************
162300020516     C*
162400020516     C     VARIAY        TAG
162500020516     C     TASSVA        IFEQ      §$2VRY
162600020516     C                   MOVEL     §$2VRY        TASTC
162700020516     C                   EXSR      TASPAR
162800020516     C**
162900020516     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
163000020516     C     TASSVA        IFEQ      §$2VRY
163100020516     C                   GOTO      FINE
163200020516     C                   ENDIF
163300020516     C                   ENDIF
163400020516     C***************
163500020516     C* DIROTTAMENTI      - SOLO SE RICHIESTA DA SOLA
163600020516     C***************
163700020516     C*
163800020523     C     VARAST        TAG
163900020523     C     TASSVA        IFEQ      §$2AST
164000020523     C                   MOVEL     §$2AST        TASTC
164100020516     C                   EXSR      TASPAR
164200020516     C**
164300020516     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
164400020523     C     TASSVA        IFEQ      §$2AST
164500020516     C                   GOTO      FINE
164600020516     C                   ENDIF
164700020516     C                   ENDIF
164800030403     C***************
164900030403     C* RECAPITO C/ASSEGNI- SOLO SE RICHIESTA DA SOLA
165000030403     C***************
165100030403     C*
165200030403     C     VARVRW        TAG
165300030403     C     TASSVA        IFEQ      §$2VRW
165400030403     C                   MOVEL     'G'           TASTC
165500030403     C                   EXSR      TASPAR
165600030403     C**
165700030403     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
165800030403     C     TASSVA        IFEQ      §$2VRW
165900030403     C                   GOTO      FINE
166000030403     C                   ENDIF
166100030403     C                   ENDIF
166200030527     C*
166300030527     C***************
166400030527     C* ADDEBITO INSOLUTI - INT.MORA  SOLO SE RICHIESTA DA SOLA
166500030527     C***************
166600030527     C     VARVRM        TAG
166700030527     C     TASSVA        IFEQ      §$2VRM
166800030527      * SE NON ESISTONO VARIE IMPOSTATE DO MANCA TARIFFA
166900030527     c                   MOVEA     SV            WRKSV            20
167000030527     C                   IF        WRKSV = *BLANKS
167100030527     C                   MOVEL     MSG(18)       TASMSG
167200030527     C                   MOVEL     'E'           TASERR            1
167300030527     C                   SETON                                        94
167400030527     C                   ENDIF
167500030527     C                   GOTO      FINE
167600030527     C                   ENDIF
167700030626     C***************
167800030626     C* ORM COMMISSIONATO  - SOLO SE RICHIESTA DA SOLA
167900030626     C***************
168000030626     C*
168100030626     C     VARVRT        TAG
168200030626     C     TASSVA        IFEQ      §$2VRT
168300030626     C                   MOVEL     §$2VRT        TASTC
168400030626     C                   EXSR      TASPAR
168500030626     C**
168600030626     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
168700030626     C     TASSVA        IFEQ      §$2VRT
168800030626     C                   GOTO      FINE
168900030626     C                   ENDIF
169000030626     C                   ENDIF
169100040910     C***************
169200040910     C* P.O.D IMMAGE      - SOLO SE RICHIESTA DA SOLA
169300040910     C***************
169400040910     C*
169500040910     C     VARPOD        TAG
169600040910     C     TASSVA        IFEQ      §$2POD
169700040910     C                   MOVEL     §$2POD        TASTC
169800040910     C                   EXSR      TASPAR
169900040910     C**
170000040910     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
170100040910     C     TASSVA        IFEQ      §$2POD
170200040910     C                   GOTO      FINE
170300040910     C                   ENDIF
170400040910     C                   ENDIF
170500990922     C************************
170600990922     C* CONSEGNE PARTICOLARI
170700990922     C************************
170800990722     C*
170900990922     C** CONSEGNE PARTICOLARI 1
171000941110     C     TASTC1        IFNE      *BLANKS
171100970521     C                   MOVEL     TASTC1        TASTC
171200970521     C                   EXSR      TASPAR
171300941112     C                   END
171400990922     C** CONSEGNE PARTICOLARI 2
171500941110     C     TASTC2        IFNE      *BLANKS
171600970521     C                   MOVEL     TASTC2        TASTC
171700970521     C                   EXSR      TASPAR
171800941110     C                   END
171900020826     C************************
172000020826     C* DITITTO DI PESATURA
172100020826     C************************
172200020826     C*
172300020826     C     §TATAP        IFEQ      'S'
172400020826     C                   MOVEL     '5'           TASTC
172500020826     C                   EXSR      TASPAR
172600020826     C                   END
172700990922     C************************
172800990922     C* RITIRO
172900990922     C************************
173000050504     C* LA VARIA "U" DI RITIRO  E'
173100050504     C** DA CALCOLARE: SUI PORTI ASSEGNATI
173200050504     C**               SUI PORTI FRANCHI NON CODIFICATI
173300050504     C**                   CON FLAG CONSEGNA A MAGAZZINO = A BLANK
173400050504     C**               SULLE BOLLE EXPORT ASSEGNATO NON DPD
173500050504     C**               SULLE BOLLE IMPORT ASSEGNATO TUTTE (ANCHE DPD)
173600990915     C* FRANCO
173700000919     C*!!*       TIPBOL    IFNE 'A'
173800000919     C*!!*       TASFDN    ANDEQ'S'
173900000919     C*!!*                 GOTO ENDRIT
174000000919     C*!!*                 ENDIF
174100990915     C*
174200990915     C* FRANCO CON CODICE CLIENTE UGUALE A 8888 OPPURE UGUALE ALLA TARIFFA DI
174300990915     C* CARTELLO
174400000919     C     TIPBOL        IFNE      'A'
174500000919     C     KSCCOD        ANDNE     8888
174600020417     C****       TASKSC    ANDNECARKSC
174700020417     C* SE NON SI TRATTA NEMMENO DI UNA TARIFFA DI CARTELLO NON TASSO
174800020417     C     TASKSC        LOOKUP    TAC                                    15
174900020417     C  N15              GOTO      ENDRIT
175000000919     C                   ENDIF
175100990930     C** DA CALCOLARE SOLO PER PORTI ASSEGNATI RITIRATI
175200000919     C*!!*       TIPBOL    CABNE'A'       ENDRIT
175300990930     C*
175400970213     C*
175500990914     C* SE BOLLA ASSEGNATA EXPORT --> NON CONSIDERO IL FLAG CONSEGNA A MAGAZZINO
175600990914     C*                               PERCHE' DEVE SEMPRE CALCOLARE LA VARIA "U"
175700970213     C     TASFDN        IFEQ      'S'
175800020319     C* PER ASSEGNATO EXPORT --> SEMPRE  (ANCHE DPD CHE FINO A OGGI
175900050504     C*                          ESCLUDEVA 4/4/02--> RIESCLUDO DPD)
176000050504     C* PER ASSEGNATO IMPORT --> SEMPRE  (ANCHE DPD 04/05/05)
176100050504     C*
176200001204     C* PER ALTRI ASSEGNATI  --> SOLO SE NON E' PORTATA A MAGAZZINO
176300001204     C     TIPBOL        IFEQ      'A'
176400001204     C*
176500980605     C     TASLNA        LOOKUP    EST                                    05
176600050504     c
176700050504     C  n05TASLNP        LOOKUP    ESTtot                                 05
176800050504     c
176900980605     C  N05              GOTO      ENDRIT
177000001204     C                   ELSE
177100001204     C* PER 8888 --> NON APPLICARE SE PORTATA A MAGAZZINO SIA PER EXP
177200001204     C*              CHE PER LE ALTRE SPEDIZIONI
177300001204     C                   GOTO      ENDRIT
177400970213     C                   ENDIF
177500001204     C                   ENDIF
177600060720      *----------------------------------------------------------------------------*
177700060720      * In caso di bolla di reso non si può NON CALCOLARE la varia U in quanto     *
177800060720      * questa serve per far pagare la tratta int.le della spedizione.             *
177900060720      * Infatti quando in Aprile 2006 è stata tolta la signora Villa ha reclamato. *
178000060720      * asterisco le seguenti specifiche                                           *
178100060720     c*** Per bolla import, non applico varia "U" se bolla di reso                 *
178200060720     C***     TASLNP        LOOKUP    ESTtot                                 05    *
178300060720     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
178400060720     C***                   GOTO      ENDRIT                                       *
178500060720     c***                   endif                                                  *
178600060720     c***                                                                          *
178700060720     c*** Per bolla EXport, non applico varia "U" se bolla di reso                 *
178800060720     C***     TASLNA        LOOKUP    EST                                    05    *
178900060720     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
179000060720     C***                   GOTO      ENDRIT                                       *
179100060720     c***                   endif                                                  *
179200060720      *----------------------------------------------------------------------------*
179300060424     c
179400970213     C**
179500970521     C* TARIFFA PARTICOLARE --> U
179600941121     C                   MOVEL     'U'           TASTC
179700970521     C**
179800110617     C** 17/06/2011 LB DN visto che sono state cambiate le tariffe della cartello
179900110617     c** per la tariffa particolare di ritiro per l'Estero e non essendo + uguale
180000110617     c** a quelle per l'italia in caso di prepagati (quindi non codificati) per la GB
180100110617     c** la tariffa di ritiro diventa di 50 euro circa rispetto i 5 euro circa di prima ?
180200110617     c** quindi risulta necessario correggere un errore che ci portiamo dietro da anni
180300110617     c** che tassiamo controllando il codice tassazione mittente solo per gli assegnati
180400110617     c** invece si deve fare anche per i poveri franchi (prepagati) per i quali viene
180500110617     c** calcolato il ritiro. Quindi sia per i franchi che assegnati il codice tassazione
180600110617     c** per la tariffa particolare di ritiro diventa il codice tassazione mittente
180700110617     c** come è giusto che venga tassata da dove viene ritirata e non dove viene
180800110617     c** consegnata la merce. A.G.
180900110617     C**   TIPBOL        IFEQ      'A'
181000970521     C                   MOVEL     TASCTS        WCTS
181100970521     C                   MOVEL     TASMCT        TASCTS
181200110617     C**                 END
181300970521     C*
181400970521     C                   EXSR      TASPAR
181500970521     C*
181600110617     C**   TIPBOL        IFEQ      'A'
181700970521     C                   MOVEL     WCTS          TASCTS
181800110617     C**                 END
181900941121     C     ENDRIT        TAG
182000990922     C************************
182100990922     C* SI DDT
182200031104      * Tutti i pgm che richiamano il TNSF20R x tassare le bolle arrivi
182300031104      * testano il flag DDT e passano 'S' nel caso di DDT SI, in questi pgm ho aggiunto anche i
182400031104      * nuovi flag 'P' e 'Q'
182500031104      * l'unico che non lo fa è FNLG20R così ho aggiunto anche qua il controllo con i 2 nuovi flag
182600031104      * 'P' è uguale a 'S' e viene impostato in arrivo quando si stampa il ddt si
182700031104      * 'Q' è uguale a 'C' e viene impostato in arrivo quando si stampa il ddt si
182800990922     C************************
182900970409     C     TASDDT        IFEQ      'S'
183000031104     C     TASDDT        orEQ      'P'
183100031104     C     TASDDT        orEQ      'Q'
183200970521     C* TARIFFA PARTICOLARE --> B
183300970521     C                   MOVEL     'B'           TASTC
183400970521     C                   EXSR      TASPAR
183500970409     C                   END
183600030203      ***************
183700030203      * BANCALI
183800030203      ***************
183900030203     C*
184000030203      * Non si tassano i bancali per le bolle in porto assegnato
184100030203     c                   If        TasBan > *Zeros and TipBol <> 'A'
184200030203     c                   Movel     §$2Vba        TASTC
184300030203     c                   ExSr      TasPar
184400030203     c                   EndIf
184500001205     C*
184600040506      ************************************************
184700040506      * SUPPLEMENTO CARBURANTE SPEDIZIONI FEDEX
184800040506      ************************************************
184900040506     C                   IF        WBOFED = 'S'
185000060905     C                   EXSR      SR_SUPCARB
185100060905     C                   ELSE
185200060905      ************************************************
185300060905      * FUEL SURCHARGE  SPEDIZIONI NO FEDEX
185400060905      ************************************************
185500060905     C                   EXSR      SR_FUEL
185600040506     C                   ENDIF
185700040506      *
185800941110     C                   SETOFF                                       49
185900941110     C                   ENDSR
186000060414     C*****************************************************************
186100090922     C** Cerco la posizione per la varia della maggiorazione
186200060414     C*****************************************************************
186300060414     c     CerESPRESSO   BEGSR
186400060414     c                   if        esp=0
186500060414     c                   clear                   giaespr           1
186600090923     c                   clear                   wVariaMag         1
186700060414     C                   Z-ADD     1             A
186800090922
186900090922     c* Maggiorazione per tipo servizio E-priority
187000090922     c                   select
187100090922     c                   when      tastsp='E'
187200090922     c                   eval      wvariaMag=§$2tsp
187300090922
187400090922     c* Maggiorazione per tipo servizio H-H1030
187500090922     c                   when      tastsp='H'
187600090922     c                   eval      wvariaMag=§$2tsh
187700090922     c                   endsl
187800090922
187900090922     C     wvariaMag     LOOKUP    SV(a)                                  96
188000090922     c* gia esiste la varia della maggioraz --> non la calcolo
188100060414     c                   if        *in96
188200060414     c                   z-add     a             ESP               3 0
188300060414     c                   eval      giaespr='S'
188400060414     c                   else
188500060414     C                   Z-ADD     1             A
188600060414     C     ' '           LOOKUP    SV(A)                                  96
188700060414     c   96              z-add     a             ESP               3 0
188800060414     C  n96              ADD       999           ESP
188900060414     c                   endif
189000060414     c                   endif
189100060414     c                   ENDSR
189200980508     C*****************************************************************
189300980508     C** VALORE COL QUALE  MOLTIPLICARE LA TARIFFA
189400980508     C*****************************************************************
189500980508     C     CALQLI        BEGSR
189600991013     C                   Z-ADD     0             PESQLI           14 6
189700980508     C** SE LO SCAGLIONE SCELTO PER TASSARE E' INFERIORE AL MINIMO
189800980508     C** TASSABILE DA TARIFFA FORZO PESO TASSABILE = MINTAS
189900980512     C**
190000980512     C* WATA MI IDENTIFICA SE L'APPLICAZIONE TARIFFA FINITA E' DOVUTA
190100980512     C*  ALLO SCAGLIONE CONFRONTATO CON MINTAR (R)
190200980512     C*  O SE E' DOVUTA DAL PESO TASSABILE CONFRONTATO COL MINIMO
190300980515     C*  TASSABILE DI QUELLA TARIFFA (S) - differenza che ora non serve
190400980515     C*  + ma dal momento che c'e' la lascio
190500980508     C                   CLEAR                   WATA
190600980508    1C                   SELECT
190700980508     C     TSCG          WHENLE    MINTAR
190800980508     C                   Z-ADD     1             PESQLI
190900980514     C                   MOVEL     'R'           WATA              1
191000980508     C**
191100980515     C** SE PESO TASSABILE E' ANCORA INFERIORE
191200980514     C*  AL MINIMO TASSABILE DA TARIFFA IMPOSTO PESO TASS = 1
191300980515     C*  NON LO CONTROLLO PER LA TARIFFA A SCALARE PERCHE' ABBIAMO
191400980515     C*  SEMPRE INSERITO UNO SCAGLIONE=A MINIMO TASSABILE E
191500980515     C*  QUINDI E' GIA' CONTROLLATO NEL WHLE PRECEDENTE
191600980515     C     WPTAS3        WHENLE    MINTAS
191700980515     C     §TAF02        ANDNE     'S'
191800980508     C                   Z-ADD     1             PESQLI
191900980508     C                   MOVEL     'S'           WATA
192000980508   X1C                   OTHER
192100980508     C** CALCOLO : PESO IN Q.LI PER TARIFFA A PESO
192200980508     C** CALCOLO   VALORE/100   PER TARIFFA A VALORE (ESSENDO UNA %)
192300980508     C** PER LE ALTRE TARIFFE PESO TASSABILE INVARIATO
192400980508    2C     UNOCTR        IFEQ      0
192500980508     C     UNOCTR        OREQ      4
192600980508     C     WPTAS3        DIV       100           PESQLI
192700980508   X2C                   ELSE
192800980508     C                   Z-ADD     WPTAS3        PESQLI
192900980508    2C                   END
193000980508    1C                   ENDSL
193100980508     C                   ENDSR
193200980508     C*****************************************************************
193300980508     C* MOLTIPLICATO TARIFFA PER VALORE TROVATO (PESO TASSABILE)
193400980508     C*****************************************************************
193500980508     C     MULTAR        BEGSR
193600980508     C     *LIKE         DEFINE    TASPOR        WPOR
193700980508     C     *LIKE         DEFINE    TASINL        WINL
193800980518     C                   CLEAR                   WPOR
193900980518     C                   CLEAR                   WINL
194000980508     C**
194100980508     C     TITR          MULT(H)   PESQLI        WPOR                           ** PORTO
194200980508     C**
194300980508     C** SE TROVATA TARIFFA DEL PAESE NON SI APPLICA MAI INOLTRO
194400110624     C*****  N49FLGINO        IFNE      'C'
194500110624     C** Sostituisco il controllo del flag inoltro diverso da C=città ma
194600110624     c** anche da "T" Città centro storico
194700110624     C                   IF        NOT *IN49  AND
194800110624     C                             FLGINO <> 'C' AND FLGINO <> 'T'
194900980508     C     TINO          MULT(H)   PESQLI        WINL                           ** INOLTRO
195000980508     C                   END
195100980508     C                   ENDSR
195200980508     C*****************************************************************
195300980508     C* CALCOLO TARIFFA A SCALARE
195400980508     C*****************************************************************
195500980508     C     SCALAR        BEGSR
195600980508     C**
195700980514     C     *LIKE         DEFINE    PESTAS        WPTAS1
195800980508     C     *LIKE         DEFINE    PESTAS        WDIFSC
195900980508     C     *LIKE         DEFINE    PESTAS        WPTAS3
196000980511     C     *LIKE         DEFINE    PESQLI        WPESQ
196100980511     C     *LIKE         DEFINE    PESQLI        WPESSV
196200980511     C     *LIKE         DEFINE    TITR          WTITR
196300980511     C     *LIKE         DEFINE    TITR          WSTITR
196400980511     C     *LIKE         DEFINE    TINO          WTINO
196500980511     C     *LIKE         DEFINE    TINO          WSTINO
196600980508     C     *LIKE         DEFINE    TSCG          WPRESC
196700980508     C**
196800980508     C                   CLEAR                   TASPOR
196900980508     C                   CLEAR                   TASINL
197000980508     C                   CLEAR                   WTROVA
197100980508     C                   CLEAR                   WPTAS3
197200980508     C                   CLEAR                   WDIFSC
197300980508     C                   CLEAR                   WPRESC
197400980511     C                   CLEAR                   WPESSV
197500980511     C                   CLEAR                   WPESQ
197600980511     C                   CLEAR                   WTITR
197700980511     C                   CLEAR                   WSTITR
197800980511     C                   CLEAR                   WTINO
197900980511     C                   CLEAR                   WSTINO
198000980508     C                   Z-ADD     PESTAS        WPTAS1
198100980508     C                   Z-ADD     1             A
198200980508     C**
198300020218    1C     A             DOWLE     200
198400980508     C     WTROVA        ANDEQ     ' '
198500980508     C     TSCG          ANDGT     0
198600980508     C     A             OCCUR     TARDS
198700980512     C**
198800980508     C* FACCIO LA DIFFERENZA TRA LO SCAGLIONE LETTO ED IL PRECEDENTE
198900980508     C     TSCG          SUB       WPRESC        WDIFSC
199000980508     C**
199100980508     C* SOTRAGGO DAL PESO TASSABILE RIMASTO LA PARTE DA TASSARE ORA
199200980508     C* IN WPTAS1 HO SEMPRE LA PARTE DI PESO TASSABILE ANCORA DA TASSAR
199300980508     C                   SUB       WDIFSC        WPTAS1
199400980508     C**
199500980508     C** SE IL VALORE E' DIVENTATO < DI 0 SIGNIFICA CHE HO TERMINATO LA
199600980508     C**  TASSAZIONE PER CUI USO SOLO LA PARTE RIMASTA DI WPTAS1
199700980508    2C     WPTAS1        IFLE      0
199800980511     C     WDIFSC        ADD       WPTAS1        WPTAS3
199900980514     C* PESO TASSABILE GLOBALE DELLA SPEDIZIONE
200000980508     C                   MOVEL     'S'           WTROVA            1
200100980508     C                   ELSE
200200980508     C**
200300980508     C* PARTE DI PESO DA TASSARE CON QUESTO SCAGLIONE
200400980508     C                   Z-ADD     WDIFSC        WPTAS3
200500980508     C* SALVO LO SCAGLIONE PRECEDENTE
200600980508     C                   Z-ADD     TSCG          WPRESC
200700980508    2C                   ENDIF
200800980508     C**
200900980508     C* IMPOSTO IL VALORE DA MOLTIPLICARE PER LA TARIFFA
201000980508     C                   EXSR      CALQLI
201100980512     C**
201200980511     C* SE LO SCAGLIONE E' APPLICAZIONE TARIFFA FINITA DEVO CONSIDERARE
201300980512     C*  SOLO L'ULTIMO SCAGLIONE COSI' E NON I PRECEDENTI
201400980514    2C     WATA          IFNE      ' '
201500980511     C     WTROVA        ANDEQ     ' '
201600980511     C* MI SALVO IL VALORE MA PER ORA NON LO MOLTIPLICO
201700980511     C                   Z-ADD     PESQLI        WPESQ
201800980511     C                   Z-ADD     TITR          WTITR
201900980511     C                   Z-ADD     TINO          WTINO
202000980511   X2C                   ELSE
202100980511     C**
202200980511     C* SE LO SCAGLIONE TROVATO E' L'ULTIMO MA COMUNQUE TARIFFA FINITA
202300980511     C* DEVO MOLTIPLICARE SOLO QUESTO --> AZZERO UN EVENTUALE PRECEDENT
202400980511     C*  CHE AVEVA LA TASSAZIONE TARIFFA FINITA
202500980511    3C     WTROVA        IFEQ      'S'
202600980514     C     WATA          ANDNE     ' '
202700980511     C                   CLEAR                   WPESQ
202800980511     C                   CLEAR                   WTITR
202900980511     C                   CLEAR                   WTINO
203000980511    3C                   ENDIF
203100980511     C*
203200980511     C* PRIMA DI MOLTIPLICARE LA TARIFFA TROVATA VEDO SE CE NE E'
203300980511     C*  UNA PRECEDENTE TARIFFA FINITA DA CONSIDERARE
203400980511    3C     WPESQ         IFGT      0
203500980511     C* SALVO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
203600980511     C*  APPENA LETTO
203700980511     C                   Z-ADD     PESQLI        WPESSV
203800980511     C                   Z-ADD     TITR          WSTITR
203900980511     C                   Z-ADD     TINO          WSTINO
204000980511     C* IMPOSTO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
204100980511     C*  PRECEDENTE PER IL CALCOLO
204200980511     C                   Z-ADD     WPESQ         PESQLI
204300980511     C                   Z-ADD     WTITR         TITR
204400980511     C                   Z-ADD     WTINO         TINO
204500980511     C                   EXSR      MULTAR
204600980511     C                   ADD       WPOR          TASPOR
204700980511     C                   ADD       WINL          TASINL
204800980511     C                   CLEAR                   WPESQ
204900980511     C                   CLEAR                   WTITR
205000980511     C                   CLEAR                   WTINO
205100980511     C* REIMPOSTO I DATI DELLO SCAGLIONE APPENA LETTO
205200980511     C                   Z-ADD     WPESSV        PESQLI
205300980511     C                   Z-ADD     WSTITR        TITR
205400980511     C                   Z-ADD     WSTINO        TINO
205500980511    3C                   ENDIF
205600980511     C**
205700980508     C* MOLTIPLICAZIONE TARIFFA
205800980508     C                   EXSR      MULTAR
205900980508     C                   ADD       WPOR          TASPOR
206000980508     C                   ADD       WINL          TASINL
206100980511    2C                   ENDIF
206200980515     C                   ADD       1             A
206300980508     C**
206400980508    1C                   ENDDO
206500980515     C**
206600980515     C* APPLICO MINIMO E MASSIMO
206700980515     C                   Z-ADD     TASPOR        WPOR
206800980515     C                   Z-ADD     TASINL        WINL
206900980515     C                   EXSR      MINMAX
207000980515     C                   Z-ADD     WPOR          TASPOR
207100980515     C                   Z-ADD     WINL          TASINL
207200980508     C                   ENDSR
207300980512     C*****************************************************************
207400980512     C* CALCOLO MAX SCAGLIONE PRECEDENTE
207500980512     C*****************************************************************
207600980512     C     MAXSCA        BEGSR
207700980513     C* SOLO SE LO SCAGLIONE APPLICATO NON ERA IL PRIMO
207800980513     C     WA            IFGT      1
207900980512     C* PER CONFRONTARLO CON IL VALORE NORMALE CALCOLATO
208000980512     C* IL CONFRONTO E' DATO DALLA SOMMA TRA PORT E INOLTRO
208100980512     C                   SUB       1             WA
208200980512     C     WA            OCCUR     TARDS
208300980512     C* CALCOLO PESO DA MOLTIPLICARE
208400980512     C                   Z-ADD     TSCG          WPTAS3
208500980512     C                   EXSR      CALQLI
208600980515     C** MOLTIPLICO LA TARIFFA
208700980512     C                   EXSR      MULTAR
208800980515     C** MINIMO E MASSIMO
208900980515     C                   EXSR      MINMAX
209000980515     C**
209100990923     C     TASPOR        ADD       TASINL        T0100            15 3
209200990923     C     WPOR          ADD       WINL          W0100            15 3
209300980515     C** PRENDO DELLE 2 LA TARIFFA PIU' ALTA
209400980512     C     W0100         IFGT      T0100
209500980512     C                   Z-ADD     WPOR          TASPOR
209600980512     C                   Z-ADD     WINL          TASINL
209700980512     C                   ENDIF
209800980513     C                   ENDIF
209900980512     C**
210000980512     C                   ENDSR
210100980515     C*****************************************************************
210200980515     C** MINIMO E MASSIMO DA APPLICARE -SE APPLICATO TUTTO NEL PORTO
210300980515     C*****************************************************************
210400980515     C     MINMAX        BEGSR
210500980515     C** NON ESISTE MINIMO E MASSIMO
210600980515     C     TMIN          IFEQ      0
210700980515     C     TMAX          ANDEQ     0
210800980515     C                   GOTO      DOPOMM
210900980515     C                   END
211000980515     C** MINIMO
211100990923     C     WPOR          ADD       WINL          WMAX             15 3
211200980515     C     TMIN          IFGT      WMAX
211300980515     C                   Z-ADD     TMIN          WPOR
211400980515     C                   Z-ADD     0             WINL
211500980515     C                   Z-ADD     TMIN          WMAX
211600980515     C                   END
211700980515     C** MASSIMO
211800980515     C     TMAX          IFGT      0
211900980515     C     WMAX          IFGT      TMAX
212000980515     C                   Z-ADD     TMAX          WPOR
212100980515     C                   Z-ADD     0             WINL
212200980515     C                   END
212300980515     C                   END
212400980515     C     DOPOMM        ENDSR
212500980429     C*****************************************************************
212600980429     C* CERCO VARIA ISOLA LOCALITA' DISAGIATA CENTRI STORICI
212700090922     c* cerco anche la maggiorazione
212800980429     C*****************************************************************
212900980429     C     CERISO        BEGSR
213000980429     C                   CLEAR                   VAISO
213100060414     C                   CLEAR                   VATSP
213200140203      * vaaggfuel varie che vengono sommate solo all'imponibile fuel e supplemento carburante
213300140203     C                   CLEAR                   VAAGGFUEL
213400980429     C     *LIKE         DEFINE    TASVA1        VAISO
213500060414     C     *LIKE         DEFINE    TASVA1        VATSP
213600140203     C     *LIKE         DEFINE    TASVA1        VAAGGFUEL
213700981020     C                   Z-ADD     1             CC                3 0
213800980429     C     §$2VRJ        LOOKUP    SV(CC)                                 96     ISOLA
213900980429     C   96              ADD       VA(CC)        VAISO
214000981008     C                   Z-ADD     1             CC
214100980429     C     §$2VRK        LOOKUP    SV(CC)                                 96     DISAGIATE
214200980429     C   96              ADD       VA(CC)        VAISO
214300981008     C                   Z-ADD     1             CC
214400980429     C     §$2VRQ        LOOKUP    SV(CC)                                 96     C.STORICI
214500980429     C   96              ADD       VA(CC)        VAISO
214600060414     C                   Z-ADD     1             CC                3 0
214700090923     c* "E" o "H"
214800090923     c                   if        WvariaMag<>' '
214900140203     C     WVariaMag     LOOKUP    SV(CC)                                 96     maggiorazione e o h
215000060414     C   96              z-add     VA(CC)        VAtsp
215100090923     c                   endif
215200140203      * CALCOLO LE VARIE  AGGIUNTIVE AL SOLO IMPONIBILE FUEL
215300140203     C                   Z-ADD     1             CC                3 0
215400140203     C     '='           LOOKUP    SV(CC)                                 96     RESO BANCALI
215500140203     C   96              ADD       VA(CC)        VAAGGFUEL
215600140203     C                   Z-ADD     1             CC                3 0
215700140203     C     'c'           LOOKUP    SV(CC)                                 96     LASCIATO AVVISO
215800140203     C   96              ADD       VA(CC)        VAAGGFUEL
215900140203     C                   Z-ADD     1             CC                3 0
216000140203     C     'A'           LOOKUP    SV(CC)                                 96     APPUNTAMENTO
216100140203     C   96              ADD       VA(CC)        VAAGGFUEL
216200140203     C                   Z-ADD     1             CC                3 0
216300140203     C     'B'           LOOKUP    SV(CC)                                 96     CONSEGNA DDT
216400140203     C   96              ADD       VA(CC)        VAAGGFUEL
216500140203     C                   Z-ADD     1             CC                3 0
216600140203     C     'C'           LOOKUP    SV(CC)                                 96     FACCH. ARR.
216700140203     C   96              ADD       VA(CC)        VAAGGFUEL
216800140203     C                   Z-ADD     1             CC                3 0
216900140203     C     'F'           LOOKUP    SV(CC)                                 96     FUORI MISURA
217000140203     C   96              ADD       VA(CC)        VAAGGFUEL
217100140203     C                   Z-ADD     1             CC                3 0
217200140203     C     'H'           LOOKUP    SV(CC)                                 96     DIRITTO FISSO
217300140203     C   96              ADD       VA(CC)        VAAGGFUEL
217400140203     C                   Z-ADD     1             CC                3 0
217500140203     C     'I'           LOOKUP    SV(CC)                                 96     SPESE DI GIACENZA
217600140203     C   96              ADD       VA(CC)        VAAGGFUEL
217700140203     C                   Z-ADD     1             CC                3 0
217800140203     C     'N'           LOOKUP    SV(CC)                                 96     ANTEPORTO
217900140203     C   96              ADD       VA(CC)        VAAGGFUEL
218000140203     C                   Z-ADD     1             CC                3 0
218100140203     C     'P'           LOOKUP    SV(CC)                                 96     AI PIANI
218200140203     C   96              ADD       VA(CC)        VAAGGFUEL
218300140203     C                   Z-ADD     1             CC                3 0
218400140203     C     'S'           LOOKUP    SV(CC)                                 96     SUPERMERCATO
218500140203     C   96              ADD       VA(CC)        VAAGGFUEL
218600140203     C                   Z-ADD     1             CC                3 0
218700140203     C     'U'           LOOKUP    SV(CC)                                 96     RITIRO
218800140203     C   96              ADD       VA(CC)        VAAGGFUEL
218900140203     C                   Z-ADD     1             CC                3 0
219000140203     C     'X'           LOOKUP    SV(CC)                                 96     CONS.DISAG.
219100140203     C   96              ADD       VA(CC)        VAAGGFUEL
219200141215     C                   Z-ADD     1             CC                3 0
219300141215     C     'p'           LOOKUP    SV(CC)                                 96     PIN CODE
219400141215     C   96              ADD       VA(CC)        VAAGGFUEL
219500090923     c
219600980429     C                   ENDSR
219700060905     C*****************************************************************
219800060905     C* Fuel Surcharge
219900060905     C*****************************************************************
220000060905     c     SR_fuel       BEGSR
220100060906     C                   CLEAR                   SCA_PB
220200060906     C                   CLEAR                   SCA_SPE
220300060906     C                   CLEAR                   SCA_SCA
220400060905      *
220500060907      * verifico se esiste già la varia non la calcolo
220600060905      *
220700060905     c     §$2VFS        lookup    sv                                     96
220800060907     c                   IF        %found
220900060907     c                   goto      Endfuel
221000060907     c                   endif
221100060905
221200060905      * verifico se cliente ha in tariffa il calcolo del FUEL Surcharge
221300060907      * altrimenti non vado a cercare nella cartello e vado a fine
221400060906     C                   MOVEL     'f'           FISO
221500060906     C                   MOVEL     'f'           WFISO
221600060905
221700060906     c                   z-add     1             a
221800060906     c     Wfiso         Lookup    Cp(a)                                  05
221900060906     C  N05              GOTO      ENDFUEL
222000060906      * aggancio la ds della tabella 1P
222100060906     c     A             Occur     DS1P
222200060906
222300060905     C     KISOT         CHAIN     TITPT01L
222400060907      * cliente non ha la tariffa  non faccio calcolo
222500060907     C                   IF        not %FOUND(TITPT01L)
222600060907     c                   goto      Endfuel
222700060907     c                   endif
222800140224      * ds x recupero % minima applicabile
222900140224     c                   eval      DTPT01 = TPTflo
223000140224      *
223100140224      * data prezzo base maggiore della data spedizone non faccio calcolo
223200060907     C                   IF        TPTDPB > TASDSP
223300060907     c                   goto      Endfuel
223400060907     c                   endif
223500140224      * Ricerco il valore minimo applicabile e lo scaglione corrispondente in vigore
223600140224      * alla data spedizione
223700150217      * solo se in tariffa non è stato specificato che non si deve applicare il VMA
223800140224
223900150217     c                   If        §TPTvma = 'N'
224000150217     c                   clear                   dpbvma
224100150217     c                   clear                   dpbsvm
224200150217     c                   else
224300150217      * si applicazione VMA
224400140224     C     TASDSP        SETLL     TIDPB01L
224500140224     C                   READ      TIDPB01L
224600140224     c                   if        not %found(tidpb01l)
224700140224     c                   clear                   dpbvma
224800140224     c                   clear                   dpbsvm
224900140224     c                   endif
225000150217     c                   endif
225100060906
225200060905      * Ricerco scaglione di appartenenza del prezzo base
225300060905
225400060911     C     TPTDPB        SETLL     TIPMG01L
225500060906     C                   READ      TIPMG01L
225600060906     C                   IF        NOT %EOF(TIPMG01L)
225700060906      *
225800060906     C                   Z-ADD     PMGSCA        SCA_PB
225900060906     C                   ENDIF
226000060906
226100060906      * Ricerco scaglione di appartenenza del prezzo del gasolio al momento della spedizione
226200060906
226300060911     C     TASDSP        SETLL     TIPMG01L
226400060906     C                   READ      TIPMG01L
226500060906     C                   IF        NOT %EOF(TIPMG01L)
226600060906      *
226700080909      * recupero il prezzo per ricercare il F.D. in tariffa
226800080909      *
226900080909     c                   z-add     pmgpmg        PMG_SGL
227000080909
227100060906     C                   Z-ADD     PMGSCA        SCA_SPE
227200060906     C                   ENDIF
227300140224      * verifico se il prezzo del gasolio al momento della spedizione è minore del valore minimo
227400140224      * applicabile sostituisco il prezzo del gasolio e il suo scaglione con il valore minimo
227500140224      * applicabile e lo scaglione corrispondente
227600140224
227700140224     c                   if        pmgpmg < dpbvma
227800140224     c                   z-add     dpbvma        PMG_SGL
227900140224     c                   z-add     dpbsvm        SCA_SPE
228000140224     c                   Endif
228100060906
228200060906      * calcolo gli scaglioni di aumento
228300060906
228400060906     C                   EVAL      SCA_SCA = SCA_SPE - SCA_PB
228500060906     C                   IF        SCA_SCA < 0
228600060906     C                   CLEAR                   SCA_SCA
228700060906     C                   ENDIF
228800140228      * calcolo dell'importo da addebitare al cliente
228900140228
229000140228     C                   CLEAR                   VARIA
229100140228     C                   CLEAR                   VARIAfuel_min
229200140228     C                   CLEAR                   IMP_SCA
229300140228
229400140228      * calcolo imponibile FUEL
229500140228      * recupero importi di isola e località disagiate ed espresso + nuove varie introdotte
229600140228      * dal 3/2/2014 dv 2473
229700140228     C                   EXSR      CERISO
229800140228      *
229900140228     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
230000140228     c                                       VAAGGFUEL
230100060906
230200060906      * se gli scaglioni scattati sono maggiore di zero cerco l'incidenza percentuale di
230300060906      * aumento nel dettaglio tariffario
230400140228    1C                   IF        SCA_SCA > 0
230500080909      * cambia con il progetto 666 aumento supplemento carburante in tariffa
230600080909      * per cercare il Fattore demoltiplicatore non usiamo più il numero di scaglioni
230700080909      * di aumento ma il prezzo medio del gasolio della settimana della data di spedizione
230800080909     C****               Z-ADD     SCA_SCA       TPDPES
230900080909     C                   Z-ADD     PMG_SGL       TPDPES
231000060906     C                   EXSR      CALACD
231100060907      * Se la percentuale di incidenza è maggiore di zero
231200140228    2C                   IF        AITR > 0
231300060905      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
231400060905      *                     per abbandonare il concetto di monete diverse dall'EURO)
231500060907     C                   EVAL(H)   VARIA = (IMP_SCA * aitr * sca_sca) / 100
231600140224
231700140228    2c                   Endif
231800140228    1c                   Endif
231900140224      * anche se il cliente ha negato la tariffa verifico
232000140224      * se nella tariffa particolare del cliente è stato espressa la % minima di apllicazione
232100140224      * calcolo l'importo del fuel utilizzando questa percentuale
232200140224
232300140224     c                   if        §tptpma > 0
232400140224     c                   eval(H)   variafuel_min =  (IMP_SCA * §TPTpma)/ 100
232500140224     c                   Endif
232600140224
232700140224      * prendo il valore della varia maggiore tra quella calcolata con la % minima applicabile
232800140224      * e quella calcolata con il FD della tariffa
232900140224
233000140224     c                   if        variafuel_min > Varia
233100140224     c                   eval      Varia =  variafuel_min
233200140224     c                   endif
233300140224
233400060905      **
233500140224    1c                   If        Varia > 0
233600060905     C                   Z-ADD     1             A
233700060905     C     ' '           LOOKUP    SV(A)                                  96
233800060907     C   96              MOVEL     §$2VFS        SV(A)
233900060905     C   96              Z-ADD     VARIA         VA(A)
234000060905     C  N96              ADD       VARIA         TASPOR
234100060905    1C                   ENDIF
234200060905      *
234300060905      *
234400060906     C     ENDFUEL       ENDSR
234500040506     C*****************************************************************
234600060905     C* SUPPLEMENTO CARBURANTE FEDEX
234700040506     C*****************************************************************
234800060905     C     SR_SUPCARB    BEGSR
234900040507      *
235000040507      * verifico se esiste già la varia
235100040507      *
235200040507     c     §$2VSC        lookup    sv                                     96
235300040507     c                   IF        not %found
235400040506
235500040506      * RICERCO IN BASE ALLA DATA SPEDIZIONE LA GIUSTA PERCENTUALE DI
235600040506      * SUPPLEMNENTO
235700040506
235800040507     C                   CLEAR                   VARIA
235900040507     C                   CLEAR                   IMP_SCA
236000040507     C                   CLEAR                   PER_SCA
236100040507     C                   Z-ADD     1             KX
236200040507    1c                   DO        *HIVAL
236300040507      * SCHIERA IN ORDINE DISCENDENTE
236400040507     C                   MOVE      DSCA(KX)      WDSPSC
236500040507      * SE DATA UGUALE A ZERO VADO A FINE
236600040507    2C                   IF        DS_DSC = *ZEROS
236700040507     C                   LEAVE
236800040507    2C                   ENDIF
236900040507      * SE DATA SPEDIZIONE MAGGIORE DI QUELLA IN SCHIERA RECUPERO LA PERCENTUALE DI CALCOLO
237000040510    2C                   IF        TASDSP >=  DS_DSC
237100040507     c                   z-add     DS_psc        PER_SCA
237200040507     C                   LEAVE
237300040507     C                   ELSE
237400040507      * SE DATA MINORE LEGGO LA DATA SUCCESSIVA
237500040507    3C                   IF        KX < 999
237600040507     C                   ADD       1             KX
237700040507     C                   ELSE
237800040507     C                   LEAVE
237900040507    3C                   ENDIF
238000040507    2C                   ENDIF
238100040507
238200040507    1C                   ENDDO
238300040507      * SE PERCENTUALE MAGGIORE DI ZERO ESEGUO IL CALCOLO
238400040507    1C                   IF        PER_SCA > 0
238500040507      * CALCOLO L'IMPONIBILE
238600060414      * recupero importi di isola e località disagiate ed espresso
238700040507     C                   EXSR      CERISO
238800040507      *
238900060414     c* Di fatto qui l'espresso non ci sarà mai!!! bolla estera
239000140203     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
239100140203     c                                       VAAGGFUEL
239200040507      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
239300040507      *                     per abbandonare il concetto di monete diverse dall'EURO)
239400040507     C                   EVAL(H)   VARIA = (IMP_SCA * PER_SCA) / 100
239500040507      **
239600040507     C                   Z-ADD     1             A
239700040507     C     ' '           LOOKUP    SV(A)                                  96
239800040507     C   96              MOVEL     §$2VSC        SV(A)
239900040507     C   96              Z-ADD     VARIA         VA(A)
240000040507     C  N96              ADD       VARIA         TASPOR
240100040507    1C                   ENDIF
240200040507      *
240300040507     C                   ENDIF
240400040507      *
240500040507     C                   ENDSR
240600970521     C*****************************************************************
240700970521     C* TASSAZIONE TARIFFE PARTICOLARI
240800970521     C*****************************************************************
240900970521     C     TASPAR        BEGSR
241000970521     C     *LIKE         DEFINE    TASVA1        TASCP
241100970522     C     *LIKE         DEFINE    TASVA1        TASISO
241200970521     C     *LIKE         DEFINE    TASTC1        TASTC
241300970721     C                   CLEAR                   TASCP
241400970721     C                   CLEAR                   WNOEST
241500970721     C* SE NON ESISTE LA SIGLA DELLA VARIA O E' GIA' IN TASSAZIONE
241600970721     C*  NON RITASSO NE APPLICO L'ESPRESSO
241700970521     C**
241800970521     C                   Z-ADD     1             A
241900970521     C     TASTC         LOOKUP    CP(A)                                  05
242000970521     C  N05              GOTO      ENDPAR
242100970521     C**
242200970521     C     A             OCCUR     DS1P
242300970521     C                   Z-ADD     1             A
242400970521     C     §1PVAR        LOOKUP    SV(A)                                  05
242500970521     C* SIGLA VARIA NON PRESENTE --> CALCOLO
242600970521     C     *IN05         IFEQ      *OFF
242700970721     C                   MOVEL     'S'           WNOEST            1
242800970521     C                   EXSR      CALCP
242900970521     C**
243000970521     C     TASCP         IFGT      0
243100130927      * se sto tassando la tariffa particolare m = avviso affidamento spedizione
243200130927      * ed il flag EMD è uguale a "E" (via sms che mail) raddoppio la varia
243300130927     c                   if        §1pvar = §$2VAD and
243400130927     c                             §asemd = 'E'
243500130927     c                   eval      tascp = tascp * 2
243600130927     c                   endif
243700130927      *
243800970521     C                   Z-ADD     1             A
243900970521     C     ' '           LOOKUP    SV(A)                                  96
244000970521     C   96              MOVEL     §1PVAR        SV(A)
244100970521     C   96              Z-ADD     TASCP         VA(A)
244200970521     C  N96              ADD       TASCP         TASPOR
244300970521     C                   END
244400970521     C                   END
244500970521     C     ENDPAR        ENDSR
244600941112     C******************************************************
244700941107     C** CALCOLO PESO TASSABILE PER TARIFFA A PESO
244800941112     C******************************************************
244900941107     C     CALPT         BEGSR
245000980313     C** MI MEMORIZZO SU BLTAS IL PESO TASSABILE SE
245100980313     C** IL PESO E' SUPERIORE AL MINIMO TASSABILE
245200980313     C** SE ESISTE IL RAPPORTO PESO VOLUME IN TARIFFA  E VOL IN BOLLA
245300980313     C** IL PESO TASSABILE SU BLTAS E' SEMPRE ARROTONDATO AL KG SUPERIO
245400980311     C                   CLEAR                   TASPVL
245500050912     C***  UNOCTR        IFEQ      0
245600050912     C**** UNOCTR        OREQ      3
245700050912      * modifico --- il peso tassabile si stampa e si calcola nei seguenti casi :
245800050912      * Tariffa a quintale "0" il peso da fatturare maggiore del  minimo tassabile
245900050912      *                        (definito nella tabella TR = 100) oppure volume da fatturare
246000050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
246100050912      *                        tariffa maggiore di zero
246200050912      * Tariffa a kg       "3" non controllo il peso da fatturare maggiore del minimo tassabile
246300050912      *                        (definito nella tabella TR = 0,1) ma solo volume da fatturare
246400050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
246500050912      *                        tariffa maggiore di zero
246600050912     C***  TARIFFA A QUINTALE
246700050912
246800050912     C     UNOCTR        IFEQ      0
246900050912
247000020826     C     USAPKF        IFGT      MINTAS
247100950901     C     PESTAS        ADD       0,9           TASPVL
247200941107     C                   ELSE
247300941107     C     TASVLF        IFNE      0
247400980311     C     RAPPV         ANDNE     0
247500950901     C     PESTAS        ADD       0,9           TASPVL
247600980313     C                   ENDIF
247700980313     C                   ENDIF
247800980313     C                   ENDIF
247900050912
248000050912     C***  TARIFFA A KG
248100050912
248200050912     C     UNOCTR        IFEQ      3
248300050912
248400050912     C     TASVLF        IFNE      0
248500050912     C     RAPPV         ANDNE     0
248600050912     C     PESTAS        ADD       0,9           TASPVL
248700050912     C                   ENDIF
248800050912     C                   ENDIF
248900050912
249000980313     C                   ENDSR
249100950119     C******************************************************
249200950119     C** CALCOLA VALORE DA ASSICURARE PER TARIFFA A VALORE
249300950119     C******************************************************
249400950119     C     CALVAS        BEGSR
249500950119     C     *LIKE         DEFINE    TASIAS        IMPVAS
249600950119     C                   Z-ADD     0             IMPVAS
249700950119     C                   MOVEL     'C'           FISO
249800990722     C                   MOVEL     'C'           WFISO             1
249900950119     C**FLAG ASSICURAZIONE X CONTO = C
250000950119     C                   Z-ADD     1             A
250100990722     C     WFISO         LOOKUP    CP(A)                                  05
250200950119     C  N05              GOTO      DOPVA1
250300950119     C**----------------------------------------
250400990721     C     KISOT         CHAIN     TITPT01L                           28
250500980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
250600980528     C     *IN28         IFEQ      *ON
250700980528     C     TPTATB        ORNE      ' '
250800980528     C                   GOTO      DOPVA1
250900980528     C                   ENDIF
251000980528     C**
251100950119     C** ESCLUDO GLI ANNULLATI
251200950119     C     TPTFVM        IFEQ      '3'
251300020826     C     TPTVLM        MULT      USAPKF        IMPVAS
251400950119     C                   END
251500950119     C*PESO
251600950119     C     TPTFVM        IFEQ      '1'
251700950119     C     TPTVLM        MULT      TASNCL        IMPVAS
251800950119     C                   END
251900950119     C* COLLI
252000950119     C     TPTFVM        IFEQ      '2'
252100950119     C                   Z-ADD     TPTVLM        IMPVAS
252200950119     C                   END
252300950119     C*SPEDIZIONE
252400950119     C     DOPVA1        ENDSR
252500941112     C******************************************************
252600941107     C** CALCOLA VARIA R  --- ASSICURAZIONE PER CONTO
252700941112     C******************************************************
252800941107     C     CALVR         BEGSR
252900050525     c* Il falg task88 cambia significato:indica se bolla con mandato o no
253000050525     c*
253100050525     c* con mandato significa: x mandato a val variabile--> tutte le bolle
253200050525     c*                        x mandato a val.indicato --> solo bolle con
253300050525     c*                        tasias=0 o con tasias <=al valore del mandat
253400050525     c*
253500050525     c* Imposto task88=N se mandato
253600050525     c* Imposto task88=S se no mandato ma tasias>0
253700050525     c* Imposto task88=X per bolle senza mandato e senza importo
253800050525     c
253900050525     C                   MOVEL     'X'           TASK88
254000980603     C** 88 = SI CALCOLA SOLO SE ESISTE IL VALORE MERCE IN BOLLA
254100941112     C     KSCCOD        IFEQ      8888
254200980527     C     KSCCOD        OREQ      9999
254300980709     C     TASIAS        IFEQ      0
254400050525     C**                 MOVEL     'S'           TASK88
254500980709     C                   GOTO      ENDVR
254600941112     C                   END
254700980709     C                   END
254800980603     C**
254900980603     C** SE TIPO BOLLA CHE NON PREVEDE L'ASSICURAZIONE, ESCO SENZA
255000980603     C**  DARE ERRORE
255100050525     C**   TASIAS        IFEQ      0
255200050525     C**   TASTBL        LOOKUP    TBN                                    05
255300050525     C** 05              MOVEL     'S'           TASK88
255400050525     C** 05              GOTO      ENDVR
255500050525     C**                 ENDIF
255600991012     C**
255700991012     C** SE IMPORTO D'ASSICURARE MAGGIORE DI ZEROS MUOVO 'S' TASK88
255800991012     C** PER STAMPA BOLLE CON ASSICURAZIONE PER TABULATONE DELLA CONSULDANNI
255900050525     C**   TASIAS        IFGT      0
256000050525     C**                 MOVEL     'S'           TASK88
256100050525     C**                 ENDIF
256200980603     C**
256300941116     C                   SETOFF                                       94
256400941116     C     *LIKE         DEFINE    TASIAS        IASLIT
256500941116     C                   Z-ADD     TASIAS        IASLIT
256600941116     C     IASLIT        IFNE      0
256700941116     C                   Z-ADD     1             K
256800941116     C     TASVAS        LOOKUP    CV(K)                                  05
256900980504     C     *IN05         IFEQ      *OFF
257000980504     C                   SETON                                        94
257100980504     C                   MOVEL     MSG(2)        TASMSG
257200980504     C                   GOTO      FINE
257300980504     C                   ENDIF
257400941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
257500990922     C**
257600990922     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA SE E' DIVERSA
257700990922     C* DALLA VALUTA DELLA TARIFFA
257800990922     C     §TADIV        IFNE      TASVAS
257900990922     C                   CLEAR                   YEUR
258000990922     C                   MOVEL     TASVAS        YECDVI
258100990922     C                   MOVEL     §TADIV        YECDVO
258200990922     C                   Z-ADD     IASLIT        YECIMI
258300011129     C   12              MOVE      '2'           YECDCO
258400991014     C  N12              MOVE      '0'           YECDCO
258500990922     C*
258600990922     C                   CALL      'YEURCO'
258700990922     C                   PARM                    YEUR
258800990922     C*
258900990922     C     YECESI        IFEQ      ' '
259000990922     C                   Z-ADD     YECIMO        IASLIT
259100990922     C                   END
259200990922     C                   ENDIF
259300990922     C                   ENDIF
259400941123     C**----------------------------------------
259500941107     C                   MOVEL     'C'           FISO
259600990722     C                   MOVEL     'C'           WFISO
259700941107     C**FLAG ASSICURAZIONE X CONTO = C
259800941123     C                   Z-ADD     1             A
259900990722     C     WFISO         LOOKUP    CP(A)                                  05
260000941123     C  N05              GOTO      ENDVR
260100020226      * AGGANCIO LA DS DELA TABELLA 1P
260200020226     C     A             OCCUR     DS1P
260300941123     C**----------------------------------------
260400990721     C     KISOT         CHAIN     TITPT01L                           28
260500980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
260600980528     C     *IN28         IFEQ      *ON
260700980528     C     TPTATB        ORNE      *BLANKS
260800980528     C                   GOTO      DOPAC1
260900980528     C                   ENDIF
261000980505     C**
261100980505     C** CONTROLLO IL TIPO APPLICAZIONE DELLA TARIFFA ASSIC X CONTO
261200980505     C**  SE INCONGRUENTE CON TIPO BOLLA PRENDO LA CARTELLO
261300980505     C     TPTAPL        IFEQ      'A'
261400980505     C     TIPBOL        ANDEQ     'F'
261500980505     C     TPTAPL        OREQ      'P'
261600980505     C     TIPBOL        ANDEQ     'A'
261700980505     C                   GOTO      DOPAC1
261800980505     C                   ENDIF
261900980505     C**
262000980504     C* VALORE MERCE = 0 --> ESCLUDO SE NON E' UN VERO MANDATO
262100980505     C                   SETOFF                                       94
262200980504    1C     TPTVLM        IFEQ      0
262300980709     C* CHE SIA UN MANDATO FITTIZIO O UN MANDATO A VALORE VARIABILE
262400980709     C*  DEVO SEMPRE SCRIVERE LA BOLLA COME 8888 PERCHE L'ASSICURAZIONE
262500980709     C*  VUOLE VEDERE BOLLA PER BOLLA QUANTO SONO ASSICURATE
262600050525     C****               MOVEL     'S'           TASK88
262700980709     C**
262800980504    2C     IASLIT        IFEQ      0
262900980504     c* MANDATO FITTIZIO
263000990922    3C     TPTTMA        IFEQ      'F'
263100980504     C                   GOTO      ENDVR
263200980504    3C                   ENDIF
263300050525     c**
263400050526     C** ES - non serve: già fatto    sopra!! Ripetizione inutile
263500050526    3C**   TPTAPL        IFEQ      'P'
263600050526     C**   TIPBOL        COMP      'F'                                    94
263700050526    3C**                 END
263800050526    3C**   TPTAPL        IFEQ      'A'
263900050526     C**   TIPBOL        COMP      'A'                                    94
264000050526    3C**                 END
264100050526    3C**   TPTAPL        IFEQ      ' '
264200050526     C**                 SETON                                            94
264300050526    3C**                 END
264400050525     c
264500050525     c
264600050525     c* anche se manca tariffa è comunque con mandato
264700050525     c                   eval      task88='N'
264800050525     c*
264900050525     c* non devo dare manca tariffa se tipo bolla che non prevede
265000050525     c* Assicurazione
265100050525     C     TASTBL        LOOKUP    TBN                                    05
265200050525     c                   if        not *in05
265300050526     c                   eval      *in94='1'
265400050525     C                   MOVEL     MSG(4)        TASMSG
265500050525     C                   GOTO      FINE
265600050525     c                   endif
265700050525     c
265800050525   x2c                   else
265900050525     c* Mandato a valore variabile con importo: se fittizio 8888 altriment
266000050525     c* con mandato
266100050525    3C     TPTTMA        IFEQ      'F'
266200050525     c                   eval      task88='S'
266300050525     c                   else
266400050525     c                   eval      task88='N'
266500050525    3C                   END
266600050525    2C                   END
266700050525     c
266800941107     C** MANCA TARIFFA: MANDATO CON VALORE = 0
266900941107     C**                E NON ESISTE VALORE IN BOLLA (SOLO COD.)
267000980504   X1C                   ELSE
267100980504    2C     IASLIT        IFGT      0
267200941112     C                   Z-ADD     0             VALSPE           15 3
267300980504    3C     TPTFVM        IFEQ      '3'
267400020826     C     TPTVLM        MULT      USAPKF        VALSPE
267500980504    3C                   END
267600941107     C*PESO
267700980504    3C     TPTFVM        IFEQ      '1'
267800941107     C     TPTVLM        MULT      TASNCL        VALSPE
267900980504    3C                   END
268000941107     C* COLLI
268100980504    3C     TPTFVM        IFEQ      '2'
268200941107     C                   Z-ADD     TPTVLM        VALSPE
268300980504    3C                   END
268400030908      *
268500030908      * ARROTONDO AL DECIMALE DELLA TARIFFA
268600030908     C                   CLEAR                   YEUR
268700030908     C                   MOVEL     §TADIV        YECDVI
268800030908     C                   MOVEL     §TADIV        YECDVO
268900030908     C                   Z-ADD     VALSPE        YECIMI
269000030908     C                   CALL      'YEURCO'
269100030908     C                   PARM                    YEUR
269200030908     C*
269300030908     C     YECESI        IFEQ      ' '
269400030908     C                   Z-ADD     YECIMO        VALSPE
269500030908     C                   END
269600941107     C*SPEDIZIONE
269700980504    3C     IASLIT        IFGT      VALSPE
269800941107     C                   GOTO      DOPAC1
269900050526     c                   else
270000050526     c
270100050526     c* Se valore rientra nel mandato, imposto che si tratta di mandato
270200050526     C                   MOVEL     'N'           TASK88
270300980504    3C                   END
270400050526     c
270500050526   x2c                   else
270600050526     c* se senza valore in bolla ma madanto con valore -->ok da mandato
270700050526     c                   eval      task88='N'
270800980504    2C                   END
270900980504    1C                   END
271000050526     c
271100980525     C** SE IL CLIENTE E' 8888 HO IMPOSTATO PRIMA LA TARIFFA DI CARTELL
271200980525     C     KSCCOD        IFEQ      8888
271300050525     C                   MOVEL     'S'           TASK88
271400980525     C                   ENDIF
271500050525     c
271600050525     c** Anche se lo considero con mandato, se non c'e' importo e tipo
271700050525     c**  bolla senza assicurazione, non calcolo varia R
271800050525     C     TASIAS        IFEQ      0
271900050525     C     TASTBL        LOOKUP    TBN                                    05
272000050525     C   05              GOTO      ENDVR
272100050525     C                   ENDIF
272200980505     C**
272300941107     C                   GOTO      DOPAC2
272400941107     C     DOPAC1        TAG
272500941107     C** NON ESISTE TARIFFA AXC DEL CLIENTE
272600941121     C*-----------------------------------------------------------------
272700980505     C* SE USATA SI TRATTA DI TARIFFA DI CARTELLO
272800050525     C***                MOVEL     'S'           TASK88
272900941112     C     IASLIT        CABEQ     0             ENDVR
273000050525     c                   eval      task88='S'
273100020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
273200020226     C     KTPTC         CHAIN     TITPT01L                           28
273300020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
273400020226     C     *IN28         IFEQ      *ON
273500020226     C     TPTATB        OREQ      'A'
273600020226     C                   GOTO      ENDVR
273700020226     C                   ENDIF
273800941107     C*-----------------
273900941107     C     DOPAC2        TAG
274000980504     C** SE NON SERVE --> ESCO DAL CALCOLO
274100980504     C** TIPO APPLICAZIONE
274200980504     C** P - SOLO PER FRANCHI
274300980504     C** A - SOLO PER ASSEGNATI
274400980504    1C     TPTAPL        IFNE      *BLANKS
274500980504     C     IASLIT        ANDEQ     0
274600980504    2C     TIPBOL        IFEQ      'F'
274700980504    3C     TPTAPL        IFEQ      'A'
274800980504     C                   GOTO      ENDVR
274900980504    3C                   ENDIF
275000980504   X2C                   ELSE
275100980504    3C     TPTAPL        IFEQ      'P'
275200980504     C                   GOTO      ENDVR
275300980504    3C                   ENDIF
275400980504    2C                   END
275500980504    1C                   END
275600980504     C**
275700941112     C     *LIKE         DEFINE    TASIAS        VALVR
275800941112     C     *LIKE         DEFINE    TASIAS        ASCPES
275900941112     C     *LIKE         DEFINE    TASIAS        ASCPE2
276000941112     C                   Z-ADD     0             VALVR
276100980526    1C     IASLIT        IFEQ      0
276200980526    2C     TPTFVM        IFEQ      '3'
276300020826     C     USAPKF        MULT      TPTVLM        VALVR
276400980526    2C                   END
276500980526    2C     TPTFVM        IFEQ      '1'
276600941107     C     TASNCL        MULT      TPTVLM        VALVR
276700980526    2C                   END
276800980526    2C     TPTFVM        IFEQ      '2'
276900941107     C                   Z-ADD     TPTVLM        VALVR
277000980526    2C                   END
277100980526   X1C                   ELSE
277200941112     C                   Z-ADD     IASLIT        VALVR
277300980526    1C                   END
277400011119     C***** OBSOLETO NON RESTITUISCO + IL VALORE IN LIRE MA NELLA DIVISA DELLA TARIFFA
277500011129     C***** PERO' ARROTONDO AL DECIMALE DELLA TARIFFA
277600011129     C                   CLEAR                   YEUR
277700011129     C                   MOVEL     §TADIV        YECDVI
277800011129     C                   MOVEL     §TADIV        YECDVO
277900011129     C                   Z-ADD     VALVR         YECIMI
278000011129     C                   CALL      'YEURCO'
278100011129     C                   PARM                    YEUR
278200011129     C*
278300011129     C     YECESI        IFEQ      ' '
278400011129     C                   Z-ADD     YECIMO        VALVR
278500011129     C                   END
278600011129     C* RESTITUISCO IN OUTPUT L'IMPORTO DA ASSICURARE
278700011129     C                   Z-ADD     VALVR         TASIAL
278800941107     C** VALORE SPEDIZIONE PER CALCOLO ASSICURAZIONE
278900980312     C**
279000980526     C** CALCOLO SOLO SE NON HO IMPOSTATO IL FLAG
279100980526    1C     PARCA         IFEQ      ' '
279200980505     C**
279300980505     C** SE C'E' GIA' LA VARIA R NON LA RICALCOLO
279400980505     C                   Z-ADD     1             A
279500980505     C     §$2VRR        LOOKUP    SV(A)                                  05
279600980526    2C     *IN05         IFEQ      *OFF
279700980312     C                   MOVEL     TPTTPG        WTPG
279800980312     C                   MOVEL     TPTRPV        WRPV
279900980313     C                   CLEAR                   WDET
280000980313     C                   Z-ADD     TPTARL        WARL
280100980313     C                   Z-ADD     TPTARF        WARF
280200980313     C                   Z-ADD     TPTARO        WARO
280300980312     C                   EXSR      CALCOL
280400980312     C**
280500980312     C** A VALORE
280600980526    3C     TPTTPG        IFEQ      'V'
280700980312     C                   Z-ADD     VALVR         ISOPES
280800980526    3C                   END
280900980312     C     *LIKE         DEFINE    TASIAS        TPDPES
281000980504     C**
281100980504     C**CALCOLO IMPORTO ASSICURAZIONE PER CONT
281200980504     C                   Z-ADD     ISOPES        TPDPES
281300941107     C                   EXSR      CALACD
281400980525     C                   Z-ADD     VARIA         WVARIR
281500980526    2C                   ENDIF
281600980526    1C                   ENDIF
281700980504     C     ENDVR         ENDSR
281800060320     C******************************************************
281900060320     C** CALCOLA VARIA d  --- A C BASE
282000060320     C******************************************************
282100060321     c     CALVACB       BEGSR
282200060320     c* Il falg task88 cambia significato:indica se bolla con mandato o no
282300060320     c* Imposto task88=N se mandato
282400060321     c* Lascio  task88=X per bolle senza mandato e senza importo
282500060321
282600060321      **----------------------------------------
282700060321     c                   movel     'd'           Fiso
282800060321     c                   movel     'd'           WFiso
282900060321      ** Flag A C BASE  = d
283000060321     c                   z-add     1             a
283100060321     c     Wfiso         Lookup    Cp(a)                                  05
283200060321     c  N05              Goto      EndVacb
283300060321      * aggancio la ds della tabella 1P
283400060321     c     A             Occur     DS1P
283500060321      **----------------------------------------
283600060321     c     Kisot         Chain     TITPT01L
283700060321      ** esiste Tariffa A C base
283800060321    1c                   If        %found(titpt01l)
283900060321      **
284000060321      ** Controllo il tipo applicazione della tariffa A C Base
284100060321      **
284200060321    2c                   If        (tptapl = 'A' and tipbol = 'A') or
284300060321     c                             (tptapl = 'P' and tipbol = 'F') or
284400060321     c                              tptapl = ' '
284500060321      *
284600060321     c                   eval      task88='N'
284700060321      ** Anche se lo considero con mandato,  tipo
284800060321      **  bolla senza assicurazione, non calcolo varia d
284900060321     c     TAStbl        Lookup    Tbn                                    05
285000060321     c   05              Goto      ENDVacb
285100060321
285200060321      * calcolo valore assicurato
285300060321
285400060321     c     *Like         Define    TASias        VALVacb
285500060321
285600060321     c                   clear                   VALVacb
285700060321
285800060321      * Peso
285900060321    3c                   If        TPTfvm = '3'
286000060321     c     TPTvlm        Mult      USAPkf        Valvacb
286100060321    3c                   Endif
286200060321      * Colli
286300060321    3c                   If        TPTfvm = '1'
286400060321     c     TPTvlm        Mult      TASncl        Valvacb
286500060321    3c                   Endif
286600060321      * Spedizione
286700060321    3c                   If        TPTfvm = '2'
286800060321     c                   Z-add     TPTvlm        Valvacb
286900060321    3c                   Endif
287000060321      *
287100060321      * Arrotondo al decimale della tariffa
287200060321     c                   Clear                   YEUR
287300060321     c                   Movel     §TAdiv        YECdvi
287400060321     c                   Movel     §TAdiv        YECdvo
287500060321     c                   Z-add     VALvacb       YECimi
287600060321     c                   call      'YEURCO'
287700060321     c                   Parm                    YEUR
287800060321      *
287900060321     c                   If        YECesi = ' '
288000060321     c                   Z-add     YECimo        VALvacb
288100060321     c                   Endif
288200060321
288300060321      ** Calcolo solo se non ho impostato il flag
288400060321    3c                   IF        Parca = ' '
288500060321
288600060321      ** Se c'è' già la varia non la ricalcolo
288700060321     c                   Z-add     1             a
288800060321     c     §$2acb        Lookup    sv(a)                                  05
288900060321    4c     *IN05         Ifeq      *off
289000060321     c                   movel     TPTtpg        Wtpg
289100060321     c                   Movel     TPTrpv        Wrpv
289200060321     c                   clear                   Wdet
289300060321     c                   Z-add     TPTarl        Warl
289400060321     c                   Z-add     TPTarf        Warf
289500060321     c                   Z-add     TPTaro        Waro
289600060321     c                   Exsr      Calcol
289700060321
289800060321      ** A Valore
289900060321    5c                   IF        tpttpg =  'V'
290000060321     c                   z-add     valvacb       ISOpes
290100060321    5c                   Endif
290200060321
290300060321     c                   Z-add     ISOpes        TPDpes
290400060321     c                   Exsr      Calacd
290500060321     c                   Z-add     varia         Wvariacb
290600060321    4c                   endif
290700060321
290800060321    3c                   endif
290900060321
291000060321    2c                   endif
291100060321    1c                   Endif
291200060321
291300060321     c     EndVacb       ENDSR
291400941112     C******************************************************
291500980312     C** CALCOLA TARIFFA PARTICOLARE
291600941112     C******************************************************
291700941107     C     CALACD        BEGSR
291800990923     C* CAMPO VARIA CON  DECIMALI
291900990923     C                   Z-ADD     0             VARIA            15 3
292000990923     C* CAMPO VARIA SENZA DECIMALI ALTRIMENTI NON FUNZIONA L'ARROTONDAMENTO
292100990923     C                   Z-ADD     0             VARIAD           15 0
292200990923     C*
292300941107     C                   DO        100           A
292400941122     C     A             OCCUR     ACDS
292500941107     C                   CLEAR                   ACDS
292600941107     C                   END
292700941112     C** PULIZIA DS TARIFFA
292800020412     C*
292900020412     C* SE ESISTE ALMENO UNA RIGA DI DETTAGLIO E NON RIESCO AD APPLICAR
293000020412     C*  QUESTA TAR.PARTICOLARE --> MANCA TARIFFA
293100980605     C* BOLLA IMPORT / ESXPORT
293200980605     C     WBOEST        IFEQ      'E'
293300020226     C     §1PCTE        IFNE      *BLANKS
293400020226     C                   MOVEL     §1PCTE        ISOCTS
293500941112     C                   GOTO      POIAC
293600941112     C                   END
293700941112     C                   ELSE
293800020226     C     §1PCTS        IFNE      *BLANKS
293900020226     C                   MOVEL     §1PCTS        ISOCTS
294000941112     C                   GOTO      POIAC
294100941112     C                   END
294200941112     C                   END
294300941112     C** CODICI TASSAZIONE FISSI DA TABELLA TARIFFE PARTICOLARI
294400941110     C                   MOVEL     TASCTS        ISOCTS            2
294500060414     c
294600060414      * Se assegnato e tariffa FEDEX metto in ISOCTS  quello mittente
294700050124     c                   If        TIPBOL = 'A' and WBOFED = 'S'
294800050124     c                   movel     tasmct        isocts
294900050124     c                   endif
295000060414     c* Se devo utilizzare quello del mittente per via della
295100060414     c*  reversibilità --> lo uso
295200060414     c                   if        applicarevers='S'
295300060414     c                   movel     tasmct        isocts
295400060414     c                   endif
295500050124      *
295600990721     C     KISOD         SETLL     TITPD02L                               28
295700941107     C   28              GOTO      DOPAC3
295800941107     C                   Z-ADD     1             A
295900941107     C     ISOCTS        LOOKUP    CTS(A)                                 05
296000941107     C   05A             OCCUR     DSCT
296100941107     C   05              MOVEL     §CTRAP        ISOCTS
296200941107     C  N05              MOVEL     *BLANKS       ISOCTS
296300941107     C** REGIONE
296400990721     C     KISOD         SETLL     TITPD02L                               28
296500941107     C   28              GOTO      DOPAC3
296600980605     C     WBOEST        IFEQ      'E'
296700941112     C                   MOVEL     'EE'          ISOCTS
296800941112     C                   ELSE
296900941112     C                   MOVEL     'IT'          ISOCTS
297000941112     C                   END
297100941112     C     POIAC         TAG
297200990721     C     KISOD         SETLL     TITPD02L                               28
297300020412     C**N28                GOTO ENDACD
297400020412     C** N 28 --> MANCA TARIFFA
297500020412     C     *IN28         IFEQ      *OFF
297600020412     C* SE NON È PRESENTE NESSUNA RIGA DI DETTAGLIO, NON DO
297700020412     C*  MANCA TARIFFA: SIGNIFICA CHE È INSERITA UNA TASSAZIONE =0
297800020412     C     KISOD2        SETLL     TITPD02L                               28
297900020412     C  N28              GOTO      ENDACD
298000020412     C*
298100020412     C                   SETON                                        94
298200020412     C                   MOVEL     MSG(17)       TASMSG
298300140224     c                   eval      %subst(tasmsg: 47: 2) = TPDFTC
298400020412     C                   GOTO      FINE
298500020412     C                   ENDIF
298600020412     C**
298700020412     C** NON TROVATA TARIFFA
298800941107     C     DOPAC3        TAG
298900941107     C                   DO        *HIVAL        A
299000990721     C     KISOD         READE     TITPD02L                               28
299100941107     C   28              GOTO      DOPAC4
299200941107     C     A             OCCUR     ACDS
299300990923     C* VERIFICO SE STO CARICANDO LA TARIFFA DI CARTELLO
299400990923     C     TPTKSC        IFEQ      CARKSC
299500990923     C     TPTCTR        ANDEQ     CARCTR
299600990923     C     TPTPRG        ANDEQ     CARPRG
299700990923     C* E LA DIVISA DELLA TARIFFA DI CARTELLO E' DIVERSA DALLA DIVISA DELLA
299800990923     C* TARIFFA DEL CLIENTE  CONVERTO I VALORI O SCAGLIONI NELLA DIVISA
299900990923     C* DELLA TARIFFA DEL CLIENTE
300000990923     C     §TADIV        ANDNE     DIVCAR
300100990923     C*
300200990923     C                   EXSR      CONTPD
300300990923     C*
300400990923     C                   ENDIF
300500990923     C*
300600941112     C                   Z-ADD     TPDSGL        ASGL
300700941112     C                   Z-ADD     TPDITR        AITR
300800941112     C                   Z-ADD     TPDMIN        AMIN
300900941112     C                   Z-ADD     TPDMAX        AMAX
301000941112     C                   MOVEL     TPDAIN        AAIN
301100941107     C                   END
301200941107     C     DOPAC4        TAG
301300941107     C                   DO        100           A
301400941107     C     A             OCCUR     ACDS
301500941112     C     ASGL          IFGE      TPDPES
301600941107     C                   GOTO      DOPAC5
301700941107     C                   END
301800941107     C                   END
301900941123     C     ASGL          IFLT      TPDPES
302000941122     C                   SETON                                        94
302100980527     C                   MOVEL     MSG(7)        TASMSG
302200140224     c                   eval      %subst(tasmsg: 47: 2) = TPDFTC
302300980527     C                   GOTO      FINE
302400980527     C                   ENDIF
302500941122     C** NON TROVATO SCAGLIONE ESATTO
302600941107     C     DOPAC5        TAG
302700941112     C*-----------
302800941122     C     ASGL          IFLE      MINTAS
302900941122     C                   Z-ADD     MINTAS        TPDPES
303000941112     C                   END
303100941122     C     TPDPES        IFLE      MINTAS
303200941122     C                   Z-ADD     MINTAS        TPDPES
303300941122     C                   END
303400991013     C                   Z-ADD     0             TPDQLI           18 6
303500980429     C* DIVIDO PER 100 TARIFFA 0 PER AVERE QUINTALI
303600980429     C*                TARIFFA 4 PERCHE' E' TARIFFA %
303700980429     C     WTPG          IFEQ      '0'
303800980429     C     WTPG          OREQ      '4'
303900941112     C     TPDPES        DIV       100           TPDQLI
304000941112     C                   ELSE
304100941112     C                   Z-ADD     TPDPES        TPDQLI
304200941112     C                   END
304300980429     C** PESO TASSABILE
304400060906      * se trovato dettaglio tariffario della tariffa "f" vado a fine routine in quanto il calcolo
304500060906      * viene eseguito nella routine FUEL
304600060906     c                   If        wfiso = 'f'
304700060906     c                   goto      endacd
304800060906     c                   endif
304900060906
305000990923     C   12AITR          MULT(H)   TPDQLI        VARIA
305100990923     C  N12AITR          MULT(H)   TPDQLI        VARIAD
305200941112     C                   MOVEL     AAIN          FLGISO
305300980429     C*
305400980430     C     VARIA         IFGT      0
305500991012     C     *IN12         ANDEQ     *ON
305600991012     C     VARIAD        ORGT      0
305700991012     C     *IN12         ANDEQ     *OFF
305800991012     C*
305900980430     C     WTPG          IFEQ      'V'
306000980430     C     WTPG          OREQ      'T'
306100990923     C   12VARIA         DIV(H)    100           VARIA
306200990923     C  N12VARIAD        DIV(H)    100           VARIAD
306300980430     C                   ENDIF
306400980430     C                   ENDIF
306500980429     C**
306600991012     C* SPOSTO L'IMPORTO DEL CAMPO VARIAD NEL CAMPO VARIA SOLO SE E' STATO USATO
306700991012     C  N12              Z-ADD     VARIAD        VARIA
306800991012     C*
306900991012     C** MINIMO
307000941112     C     AMIN          IFGT      VARIA
307100991012     C                   Z-ADD     AMIN          VARIA
307200941107     C                   END
307300991012     C** MASSIMO
307400941115     C     AMAX          IFGT      0
307500941112     C     AMAX          IFLT      VARIA
307600991012     C                   Z-ADD     AMAX          VARIA
307700941112     C                   END
307800941115     C                   END
307900000103     C** SE VARIA CON ESENZIONE IVA DEVE AVERE SOLO 2 DCEIMALI
308000000103     C* CERCO LA SIGLA VARIA CORRISPONDENTE
308100000103     C                   MOVEL     TPTFTC        WFTC              1
308200000103     C                   Z-ADD     1             A
308300000103     C     WFTC          LOOKUP    CP(A)                                  10
308400000103     C     *IN10         IFEQ      *ON
308500000103     C     A             OCCUR     DS1P
308600000103     C     §1PVAR        LOOKUP    VES                                    10
308700000103     C     *IN10         IFEQ      *ON
308800000103     C                   MOVE      VARIA         W001A             1
308900000103     C     W001A         IFNE      '0'
309000000103     C     VARIA         ADD       0,001         COMOD2           15 2
309100000103     C                   Z-ADD     COMOD2        VARIA
309200000103     C                   ENDIF
309300000103     C                   ENDIF
309400000103     C                   ENDIF
309500000103     C**
309600941107     C     ENDACD        TAG
309700990923     C*
309800941107     C                   ENDSR
309900941107     C/SPACE
310000941112     C******************************************************
310100980424     C** CALCOLO TARIFFE  PARTICOLARI
310200970521     C******************************************************
310300980428     C     CALCP         BEGSR
310400990721     C** CERO LA TARIFFA PARTICOLARE IN TITPT O CARTELLO
310500980430     C                   MOVEL     TASTC         FISO
310600980506     C                   MOVEL     'S'           WCAR
310700110908      ** se sto calcolando la varia "Q" = Centro storico o ZTL
310800110908      ** non devo cercare la tariffa nella Cartello in mancanza
310900110908      ** di quella del cliente
311000111102      ** E' stata aggiuna una tabella che pilota il recupero della
311100111102      ** tariffa di cartello se la filiale del cliente di tassazione
311200111102      ** è presente nella tabella ztl e la data spedizione è maggiore
311300111102      ** uguale alla data attivazione della tassazione
311400111102     c                   clear                   indztl            3 0
311500110908     c                   If        tastc = 'Q'
311600111102     c                   movel     'N'           WCAR
311700111102      * cerco la filiale del cliente nella tabella ZTL
311800111102     c                   eval      indztl = %lookup(kscfil:sfilztl)
311900111102      * se indice =  a zero cerco con la filiale 999
312000111102    2c                   if        indztl = *zeros
312100111102     c                   eval      indztl = %lookup(999:sfilztl)
312200111102    2c                   endif
312300111102      * imposto il flag della ricerca tariffa di cartello = 'S'  se filiale cliente
312400111102      * abilitata e la data spedizione > = data attivazione tassazione
312500111102    2c                   If        indztl > *zeros and
312600111102    3c                             tasdsp >= sdtaztl(indztl)
312700111102     c                   movel     'S'           WCAR
312800111102    3c                   endif
312900110908     c                   endif
313000110908      *
313100980428     C                   EXSR      CERTPT
313200980428     C** SOLO SE L'HO TROVATA
313300980428    1C     WEND          IFEQ      ' '
313400970521     C                   Z-ADD     ISOPES        TPDPES
313500970521     C                   EXSR      CALACD
313600970521     C                   Z-ADD     VARIA         TASCP
313700980428    1C                   ENDIF
313800970521     C*
313900970521     C                   ENDSR
314000001205     C/SPACE
314100001205     C******************************************************
314200001205     C** CALCOLO ADDIZIONALE DI GESTIONE + ISTAT
314300001205     C******************************************************
314400001205     C     ADGIST        BEGSR
314500001205     C**
314600001205     C*
314700001205     C****************
314800041129     C** ADDIZIONALE DI GESTIONE  anni precedenti       varia "Z"
314900001205     C****************
315000001205     C*
315100031121     C     *LIKE         DEFINE    TASVA1        TASADG
315200001205     C     TAMPAG        IFNE      0
315300001205     C                   Z-ADD     1             A
315400001205     C     §$2VRZ        LOOKUP    SV(A)                                  05
315500001205     C  N05              DO
315600001205     C                   Z-ADD     0             TASADG
315700001205     C* TOTALE IMPONIBILE
315800001205     C                   Z-ADD     O17IMV        IMPADG           15 3
315900001205     C**
316000001205     C                   Z-ADD     0             COMADG           15 3
316100001205     C     IMPADG        MULT      TAMPAG        COMADG
316200001205     C     COMADG        IFGT      0
316300001205     C     COMADG        DIV(H)    100           TASADG
316400001205     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
316500001205     C     *IN12         IFEQ      '0'
316600001205     C     TASADG        ADD       0,5           CAMPOW           11 0
316700001205     C                   Z-ADD     CAMPOW        TASADG
316800001205     C                   END
316900001205     C                   END
317000001205     C*
317100040128     c     tasadg        ifgt      0
317200040128      *
317300001205     C                   Z-ADD     1             A
317400001205     C     ' '           LOOKUP    SV(A)                                  96
317500001205     C   96              MOVEL     §$2VRZ        SV(A)
317600001205     C   96              Z-ADD     TASADG        VA(A)
317700001205     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
317800001205     C  N96              ADD       TASADG        TASPOR
317900001205     C*
318000001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
318100001205     C*
318200001205     C                   CLEAR                   DSLV16
318300001205     C                   MOVEL     'T'           D17FIM
318400001205     C                   Z-ADD     TASPOR        D17POR
318500001205     C                   CALL      'FNLV16R'
318600001205     C                   PARM                    DSLV16
318700001205     C                   PARM                    DTASV
318800040128      *
318900040128     c                   endif
319000001205     C*
319100001205     C                   END
319200001205     C                   END
319300041129     C*
319400041129     C****************
319500041129     C** ADDIZIONALE DI GESTIONE    anno corrente       varia "b"
319600041129     C****************
319700041129     C*
319800041129     c                   clear                   tasadg
319900041129     c                   clear                   impadg
320000041129     c                   clear                   comadg
320100041129
320200041129     C     TAMPPA        IFNE      0
320300041129     C                   Z-ADD     1             A
320400041129     C     §$2VAG        LOOKUP    SV(A)                                  05
320500041129     C  N05              DO
320600041129     C                   Z-ADD     0             TASADG
320700041129     C* TOTALE IMPONIBILE
320800041129     C                   Z-ADD     O17IMV        IMPADG
320900041129     C**
321000041129     C                   Z-ADD     0             COMADG
321100041129     C     IMPADG        MULT      TAMPPA        COMADG
321200041129     C     COMADG        IFGT      0
321300041129     C     COMADG        DIV(H)    100           TASADG
321400041129     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
321500041129     C     *IN12         IFEQ      '0'
321600041129     C     TASADG        ADD       0,5           CAMPOW           11 0
321700041129     C                   Z-ADD     CAMPOW        TASADG
321800041129     C                   END
321900041129     C                   END
322000041129     C*
322100041129     c     tasadg        ifgt      0
322200041129      *
322300041129     C                   Z-ADD     1             A
322400041129     C     ' '           LOOKUP    SV(A)                                  96
322500041129     C   96              MOVEL     §$2VAG        SV(A)
322600041129     C   96              Z-ADD     TASADG        VA(A)
322700041129     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
322800041129     C  N96              ADD       TASADG        TASPOR
322900041129     C*
323000041129     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
323100041129     C*
323200041129     C                   CLEAR                   DSLV16
323300041129     C                   MOVEL     'T'           D17FIM
323400041129     C                   Z-ADD     TASPOR        D17POR
323500041129     C                   CALL      'FNLV16R'
323600041129     C                   PARM                    DSLV16
323700041129     C                   PARM                    DTASV
323800041129      *
323900041129     c                   endif
324000041129     C*
324100041129     C                   END
324200041129     C                   END
324300001205     C**
324400001205     C***************
324500001205     C** ISTAT
324600001205     C***************
324700001205     C*
324800031121     C     *LIKE         DEFINE    TASVA1        TASIAC
324900001205     C                   Z-ADD     1             A
325000001205     C     §$2VRV        LOOKUP    SV(A)                                  05
325100101014    1C  N05TAMrct        IFGT      0
325200001205     C                   Z-ADD     0             TASIAC
325300001205     C*
325400001205     C* CALCOLO ISTAT SU TOTALE IMPONIBILE
325500001205     C*
325600001205     C                   Z-ADD     O17IMV        IMPIAC           15 3
325700101014     C                   Z-ADD     0             PUNIAC            7 4
325800101014
325900101014      * richiamo nuovo pgm per calcolo % istat da aggiungere all'imponibile
326000101014     c
326100101014     c                   clear                   trulistds
326200101014     c                   eval      IISTsca = tamrct
326300101014     c                   eval      IISTdsp = tasdsp
326400101014     c                   call      'TRULISTR'
326500101014     c                   parm                    trulistds
326600101014     c                   IF        OISTerr = *blanks
326700101014     c                   eval      puniac = OISTiac
326800101014     c                   ENDIF
326900101014     c
327000050629     C                   Z-ADD     0             PERIAC           15 6
327100050629     C                   Z-ADD     0             PERIAX           15 4
327200001205     C     PUNIAC        MULT      TAMPRI        PERIAX
327300001205    3C     PERIAX        IFNE      0
327400031121     C     PERIAX        DIV       100           PERIAC
327500001205    3C                   END
327600001205     C** PERCENTUALE DI ISTAT DA APPLICARE
327700001205     C** APPLICO I PUNTI X TRIMESTRE SOLO SE
327800001205     C** LA DATA DI SPEDIZIONE E'
327900001205     C** MAGGIORE O UGUALE ALLA DATA DI SCATTO
328000001205     C** DEI PUNTI NEL TRIMESTRE
328100001205    3C     PERIAC        IFNE      0
328200031121     C                   DIV       100           PERIAC
328300001205     C     IMPIAC        MULT(H)   PERIAC        TASIAC
328400001205    3C                   END
328500001205     C*
328600001205    3C     TASIAC        IFGT      0
328700001205     C*  SENZA DECIMALI
328800001205    4C     *IN12         IFEQ      *OFF
328900001205     C     TASIAC        ADD       0,5           CAMPOW           11 0
329000001205     C                   Z-ADD     CAMPOW        TASIAC
329100001205    4C                   END
329200001205     C*
329300001205     C                   Z-ADD     1             A
329400001205     C     ' '           LOOKUP    SV(A)                                  96
329500001205     C   96              MOVEL     §$2VRV        SV(A)
329600001205     C   96              Z-ADD     TASIAC        VA(A)
329700001205     C  N96              ADD       TASIAC        TASPOR
329800001205     C*
329900001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
330000001205     C*
330100001205     C                   CLEAR                   DSLV16
330200001205     C                   MOVEL     'T'           D17FIM
330300001205     C                   Z-ADD     TASPOR        D17POR
330400001205     C                   CALL      'FNLV16R'
330500001205     C                   PARM                    DSLV16
330600001205     C                   PARM                    DTASV
330700001205     C*
330800001205    3C                   END
330900001205    1C                   ENDIF
331000001205     C*
331100001205     C                   ENDSR
331200030922     C*******************************************************
331300030922     C** CALCOLO DIRITTO DI FATTURAZIONE  + TOTALE IMPONIBILE
331400030922     C******************************************************
331500030922     C     DIRFAT        BEGSR
331600030922     C**
331700030922      * prima verifico se esiste già
331800030922     C                   Z-ADD     1             A
331900030922     C     §$2VRD        LOOKUP    SV(A)                                  05
332000030922     C* SIGLA VARIA NON PRESENTE --> CALCOLO
332100030922     C     *IN05         IFEQ      *OFF
332200030922     C                   Z-ADD     1             A
332300030922     C     ' '           LOOKUP    SV(A)                                  96
332400030922     C   96              MOVEL     §$2VRD        SV(A)
332500030922     C   96              Z-ADD     TASIVDF       VA(A)
332600030922     C  N96              ADD       TASIVDF       TASPOR
332700030922     C*
332800030922     C* RICALCOLO L'IMPONIBILE SOMMANDO IL DIRITTO DI FATTURAZIONE
332900030922     C*
333000030922     C                   CLEAR                   DSLV16
333100030922     C                   MOVEL     'T'           D17FIM
333200030922     C                   Z-ADD     TASPOR        D17POR
333300030922     C                   CALL      'FNLV16R'
333400030922     C                   PARM                    DSLV16
333500030922     C                   PARM                    DTASV
333600030924     C*
333700030924     C                   Z-ADD     O17IMV        TASIMV
333800030924     C*
333900030922     C*
334000030922     C                   ENDIF
334100030922     C*
334200030922     C                   ENDSR
334300980428     C******************************************************
334400980428     C* CERCO TARIFFA PARTICOLARE
334500980428     C******************************************************
334600980428     C     CERTPT        BEGSR
334700980428     C                   CLEAR                   TASCP
334800980428     C                   CLEAR                   WEND
334900980428     C                   MOVEL     *BLANKS       FLGISO            1            FLAG.APPLICAZI.
335000990722     C                   MOVEL     FISO          WFISO
335100980428     C                   Z-ADD     1             A
335200990722     C     WFISO         LOOKUP    CP(A)                                  05
335300980428     C**
335400980428    1C     *IN05         IFEQ      *ON
335500020226     C     A             OCCUR     DS1P
335600980428     C*-----------------------------------------
335700990721     C     KISOT         CHAIN     TITPT01L                           28
335800980528     C** ESISTE TARIFFA DEL CLIENTE
335900980528     C     *IN28         IFEQ      *OFF
336000980528     C     TPTATB        ANDEQ     *BLANKS
336100980428     C                   GOTO      DOPIS2
336200980528     C                   ENDIF
336300980428     C*-----------------------------------------
336400980428     C** MEMORIZZO DATI DA TARIFFA DI CARTELLO
336500980506     C**  SOLO SE HO DETTO CHE LA POSSO USARE E SE C'E'
336600980428     C*-----------------------------------------
336700020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
336800020226     C     WCAR          IFEQ      'S'
336900020226     C     KTPTC         CHAIN     TITPT01L                           28
337000020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
337100020226     C     *IN28         IFEQ      *ON
337200020226     C     TPTATB        OREQ      'A'
337300020226     C                   MOVEL     'S'           WEND
337400020226     C                   GOTO      DOPIS3
337500020226     C                   ENDIF
337600020226      * E SE NON DEVO CARICARE LA CARTELLO ERRORE NON TROVATA TARIFFA PARTICOLARE
337700020226     C                   ELSE
337800020226     C                   MOVEL     'S'           WEND
337900020226     C                   GOTO      DOPIS3
338000020226     C                   ENDIF
338100980430     C     DOPIS2        TAG
338200980430     C* ARROTONDAMENTI E RPV
338300980430     C                   MOVEL     TPTTPG        WTPG
338400090310      * in caso di pod-immage varia "a" imposto come tipo pagamento a colli
338500090310
338600090310     c                   if        fiso = §$2POD
338700090310     C                   MOVEL     '1'           WTPG
338800090310     c                   endif
338900090310
339000980430     C                   MOVEL     TPTRPV        WRPV
339100980430     C                   CLEAR                   WDET
339200980430     C                   Z-ADD     TPTARL        WARL
339300980430     C                   Z-ADD     TPTARF        WARF
339400980430     C                   Z-ADD     TPTARO        WARO
339500980430     C                   EXSR      CALCOL
339600980428     C** NON TROVATA TARIFFA PARTICOLARE
339700980428   X1C                   ELSE
339800980428     C                   MOVEL     'S'           WEND              1
339900980428    1C                   ENDIF
340000020226     C     DOPIS3        ENDSR
340100980312     C******************************************************
340200980312     C** CALCOLO IL VALORE DA MOLTIPLICARE/RPV/ARROTONDAMENTI
340300980312     C******************************************************
340400980312     C     CALCOL        BEGSR
340500980313     C     *LIKE         DEFINE    TAMARL        ARR
340600980313     C     *LIKE         DEFINE    TASIAS        PESTAS
340700980313     C     *LIKE         DEFINE    TASIAS        ISOPES
340800980312     C     *LIKE         DEFINE    TASIAS        ISOPE2
340900980312     C     *LIKE         DEFINE    TPTRPV        WRPV
341000980313     C     *LIKE         DEFINE    TPTARL        WARL
341100980313     C     *LIKE         DEFINE    TPTARF        WARF
341200980313     C     *LIKE         DEFINE    TPTARO        WARO
341300980312     C                   Z-ADD     0             ISOPES
341400980316     C                   SELECT
341500030203
341600030204     c                   When      wFiso = '='
341700030203     c                   Z-Add     TasBan        Isopes
341800030203
341900980429     C     WTPG          WHENEQ    '0'
342000980429     C     WTPG          OREQ      '3'                                          KG
342100020826     C                   Z-ADD     USAPKF        ISOPES
342200980316     C*
342300980429     C     WTPG          WHENEQ    '1'                                          COLLI
342400980312     C                   Z-ADD     TASNCL        ISOPES
342500980316     C*
342600980429     C     WTPG          WHENEQ    '2'                                          SPEDIZ
342700980312     C                   Z-ADD     1             ISOPES
342800980316     C*
342900980429     C     WTPG          WHENEQ    '4'                                          VALORE
343000980429     C     WTPG          OREQ      '5'                                          Q.TA'
343100980312     C                   Z-ADD     TASQFT        ISOPES
343200980429     C*
343300980429     C     WTPG          WHENEQ    'T'                                          TRASPORTO
343400980429     C                   Z-ADD     TASPOR        ISOPES
343500980429     C                   ADD       TASINL        ISOPES
343600980429     C                   EXSR      CERISO
343700980429     C                   ADD       VAISO         ISOPES
343800060414     C                   ADD       VAtsp         ISOPES
343900980316     C                   ENDSL
344000980312     C** MINIMO TASSABILE STD
344100980312     C                   Z-ADD     0             MINTAS
344200980312     C**
344300980312     C                   Z-ADD     1             A
344400980312     C     WTPG          LOOKUP    CTR(A)                                 05
344500980312     C   05A             OCCUR     DSTR
344600980312     C   05              Z-ADD     §TRATA        MINTAS
344700980312     C**
344800980312     C** SOLO PER TAD (PER TPD NON MI SERVE)
344900980428    1C     WDET          IFEQ      'TAD'
345000980312     C                   CLEAR                   MINTAR
345100980312     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
345200980312     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
345300980312     C   05              Z-ADD     §TRATA        MINTAR
345400980428    2C     TAMATA        IFGT      0
345500980312     C                   Z-ADD     TAMATA        MINTAR
345600980428    2C                   ENDIF
345700980428    1C                   ENDIF
345800980312     C**
345900980428     C** RPV SOLO PER TARIFFA 0 E 3
346000980428    1C     §TRRPV        IFEQ      'S'
346100980428     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
346200980428    2C     WDET          IFEQ      'TAD'
346300980428     C     TASVLF        ANDGT     0
346400020218    3C                   DO        200           A
346500980428     C     A             OCCUR     TARDS
346600980428    4C     TSCG          IFGE      ISOPES
346700980428     C                   GOTO      POITAS
346800980428    4C                   END
346900980428    3C                   END
347000980428     C** RICERCO RPV CON PESO REALE
347100980428    3C     TSCG          IFLT      ISOPES
347200980428     C                   SETON                                        94
347300980527     C                   MOVEL     MSG(5)        TASMSG
347400980527     C                   GOTO      FINE
347500980428    3C                   END
347600980428     C     POITAS        TAG
347700980428     C                   Z-ADD     TRPV          WRPV
347800040603     C                   Z-ADD     TRPV          rappv
347900980428    2C                   ENDIF
348000980428     C**
348100980428    2C     TASVLF        IFNE      0
348200980428     C     WRPV          ANDNE     0
348300980428     C                   Z-ADD     0             COMPRV            9 3
348400980428     C     TASVLF        MULT      WRPV          COMPRV
348500980428     C     COMPRV        MULT      100           ISOPE2
348600980428    3C     ISOPE2        IFGT      ISOPES
348700980428     C                   Z-ADD     ISOPE2        ISOPES
348800980428    3C                   ENDIF
348900980428    2C                   ENDIF
349000980428    1C                   ENDIF
349100990923     C**
349200990923     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
349300990923    0C     §TRARR        IFEQ      'S'
349400980313     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
349500980312     C     ISOPES        IFGE      MINTAS
349600980312     C                   Z-ADD     0             ARR
349700980313     C     ISOPES        IFGT      WARL
349800980313     C                   Z-ADD     WARO          ARR
349900980312     C                   ELSE
350000980313     C                   Z-ADD     WARF          ARR
350100980312     C                   END
350200980313     C**
350300980313     C                   Z-ADD     0             RESTO            15 5
350400980312     C                   Z-ADD     0             ISOPEX           13 0
350500980312     C     ARR           IFNE      0
350600980312     C     ISOPES        ANDNE     0
350700980312     C     ISOPES        DIV       ARR           ISOPEX
350800980312     C                   MVR                     RESTO
350900980312     C     RESTO         IFGT      0
351000980312     C     ISOPEX        MULT      ARR           ISOPES
351100980312     C                   ADD       ARR           ISOPES
351200980312     C                   ENDIF
351300980312     C                   ENDIF
351400980312     C                   ENDIF
351500980312     C**
351600980312     C***        GIUARR    TAG
351700980312    0C                   ENDIF
351800980312     C                   ENDSR
351900970521     C******************************************************
352000941107     C** CALCOLA DIRITTO ASSICURATIVO - VARIA E
352100941112     C******************************************************
352200941107     C     CALVE         BEGSR
352300941107     C                   MOVEL     'R'           FISO
352400980506     C                   MOVEL     'N'           WCAR
352500980430     C                   EXSR      CERTPT
352600980430     C     WEND          IFEQ      ' '
352700990922     C** TARIFFA A VALORE TOLGO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DI CONTO
352800990922     C*  E CALCOLO VALORE AL KG (FISSO) PER LA PARTE RESTANTE
352900980428    1C     TPTTPG        IFEQ      'V'
353000980312     C                   CLEAR                   ISOPES
353100990922     C* CONVERTO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DELLA TARIFFA
353200990922     C*
353300990922     C     *LIKE         DEFINE    §GELRP        WLRP
353400990922     C*
353500990922     C     §TADIV        IFNE      §GEDCN
353600990922     C                   CLEAR                   YEUR
353700990922     C                   MOVEL     §GEDCN        YECDVI
353800990922     C                   MOVEL     §TADIV        YECDVO
353900990922     C                   Z-ADD     §GELRP        YECIMI
354000991014     C   12              MOVE      '3'           YECDCO
354100991014     C  N12              MOVE      '0'           YECDCO
354200990922     C*
354300990922     C                   CALL      'YEURCO'
354400990922     C                   PARM                    YEUR
354500990922     C*
354600990922     C     YECESI        IFEQ      ' '
354700990922     C                   Z-ADD     YECIMO        WLRP
354800990922     C                   END
354900991112     C* SE TARIFFE UGUALI VALORIZZO WLRP
355000991112     C                   ELSE
355100991118     C                   Z-ADD     §GELRP        WLRP
355200990922     C                   ENDIF
355300990922     C*
355400990922    2C     TPTVLM        IFGT      WLRP
355500990922     C     TPTVLM        SUB       WLRP          ISOPES
355600020826     C     ISOPES        MULT      USAPKF        ISOPES
355700980312    2C                   ENDIF
355800980312    1C                   ENDIF
355900980430     C**
356000980312     C                   Z-ADD     ISOPES        TPDPES
356100941112     C                   EXSR      CALACD
356200941112     C                   Z-ADD     VARIA         DIRITT
356300941112     C**
356400941112     C     TPTAPL        IFNE      *BLANKS
356500941112     C     TIPBOL        IFEQ      'F'
356600941112     C     TPTAPL        IFEQ      'A'
356700941112     C                   Z-ADD     0             DIRITT
356800941112     C                   END
356900941112     C                   ELSE
357000941112     C     TPTAPL        IFEQ      'P'
357100941112     C                   Z-ADD     0             DIRITT
357200941112     C                   END
357300941112     C                   END
357400941112     C                   END
357500941112     C** TIPO APPLICAZIONE
357600941112     C** P - SOLO PER FRANCHI
357700941112     C** A - SOLO PER ASSEGNATI
357800980430     C                   ENDIF
357900941107     C                   ENDSR
358000941107     C/SPACE
358100941112     C******************************************************
358200941112     C** CALCOLA MAGGIORAZIONE TIPO SERVIZIO
358300941112     C******************************************************
358400941112     C     CALTS         BEGSR
358500980506     C                   MOVEL     'S'           WCAR              1
358600090924     c
358700090924     C* 24/09/09 --> Ora applichiamo sempre la maggiorazine espresso
358800090924     c
358900980506     C* SE TIPO SERVIZIO DELLA TARIFFA = A TIPO SERVIZIO BOLLA
359000980506     C*  NON PRENDO LA TARIFFA DI CARTELLO
359100090922     C***  TAMTSP        IFEQ      TASTSP
359200090922     C***                MOVEL     'N'           WCAR              1
359300090922     C***                ENDIF
359400980506     C* SIGLA VARIA CORRISPONDENTE
359500980506     C                   MOVEL     §5EFTC        FISO
359600980506     C                   EXSR      CERTPT
359700980506     C     WEND          IFEQ      ' '
359800980506     C**
359900980506     C** A TRASPORTO
360000980430     C     TPTTPG        IFEQ      'T'
360100980312     C                   Z-ADD     IMPON         ISOPES
360200941112     C                   END
360300980312     C**
360400980312     C                   Z-ADD     ISOPES        TPDPES
360500941112     C                   EXSR      CALACD
360600941112     C                   Z-ADD     VARIA         WTSP
360700980506     C                   ENDIF
360800941112     C**
360900941112     C                   ENDSR
361000941112     C******************************************************
361100020225     C** CERCA TARIFFA TNTAM PIU' RELATIVA CARTELLO
361200941112     C******************************************************
361300941107     C     CERTAM        BEGSR
361400020225      *
361500941111     C     *LIKE         DEFINE    TAMPRG        PRG2
361600941111     C     *LIKE         DEFINE    TAMKSC        KSC2
361700941111     C     *LIKE         DEFINE    TAMCTR        CTR2
361800061219      * se in tasflo c'è una data riferimento per il recupero della tariffa la imposto
361900061219      * solo temporaneamente al posto della data spedizione
362000061219     c                   clear                   Sav_dsp
362100070108     c                   if        §asdrt  > *zeros
362200061219     c                   eval      Sav_dsp = Tasdsp
362300070108     c***                eval      tasdsp = §asdrt
362400070108     c                   movel     §asdrt        tasdsp
362500061219     c                   endif
362600020225      *
362700941112     C                   SETOFF                                       180717
362800020227     C                   SETOFF                                       0890
362900941111     C                   Z-ADD     0             PRG2
363000941111     C                   Z-ADD     0             KSC2
363100941111     C                   Z-ADD     0             CTR2
363200941107     C                   Z-ADD     0             TAMKSC
363300941107     C                   Z-ADD     0             TAMPRG
363400941107     C                   Z-ADD     0             TAMCTR
363500980605     C     KTAM          SETLL     TNTAM000                               08
363600980605     C* MANCA TARIFFA CLIENTE
363700980605    1C     *IN08         IFEQ      *OFF
363800980605     C                   MOVEL     MSG(3)        TASMSG
363900980527     C                   SETON                                        94
364000980527     C                   GOTO      ENDTAM
364100980605    1C                   ENDIF
364200980605     C**
364300941107     C     SUTAM         TAG
364400980605     C**
364500941107     C     KTAM          READE     TNTAM000                               07
364600980605     c**
364700941107     C** SE HO LETTO UNA TARIFFA SCADUTA (17) E IL
364800980605     C** RECORD SUCCESSIVO MI ACCENDE FINE FILE
364900980605     C** chaino vecchio progressivo per tassare con tariffa scaduTa
365000980605    1C     *IN07         IFEQ      *ON
365100980605     C   17KTAM2         CHAIN     TNTAM000                           18
365200980605     c**
365300980605     C     *IN17         IFEQ      *OFF
365400980605    2C     *IN18         OREQ      *ON
365500980605     C                   MOVEL     MSG(3)        TASMSG
365600980605     C                   SETON                                        94
365700980605    2C                   ENDIF
365800980605     C**
365900980605     C                   GOTO      ENDTAM
366000980605     C**
366100980605     C                   ELSE
366200980605     C**
366300980605     C** SE NON E' TARIFFA ADATTA PER LA BOLLA (ITALIA INVECE CHE ESTER
366400980605     C**  O VICEVERSA) --> RILEGGO COME SE NON AVESSI LETTO
366500980605     C     TAMFIE        IFNE      WBOEST
366600980605     C     TAMFIE        ANDNE     ' '
366700980605     C                   GOTO      SUTAM
366800980605     C                   ENDIF
366900980605    1C                   ENDIF
367000980605     C**
367100980605     C** TARIFFA DA DECORRERE : SEGNALO SE NON C'E' UNA TARIFFA SCADUTA
367200980605     C**    (di fatto non e' possibile perche' se c'e' una da decorrere
367300980605     C**    e ce n'e' una prima e' la tariffa valida dal momento che
367400980605     C**    non e' possibile che esista una buco di decorrenze scadenze
367500980605     C**    tra i vari progressivi)
367600980605    1C     TASDSP        IFLT      TAMDDT
367700980605     C   17KTAM2         CHAIN     TNTAM000                           18
367800980605     C* NON C'E' TARIFFA SCADUTA O NON TROVATA DI NUOVO
367900980605    2C     *IN17         IFEQ      *OFF
368000980605     C     *IN18         OREQ      *ON
368100980605     C                   MOVEL     MSG(10)       TASMSG
368200980605     C                   SETON                                        94
368300980605    2C                   ENDIF
368400980605     C**
368500941107     C                   GOTO      ENDTAM
368600980605    1C                   END
368700980605     C**
368800980605     C** TARIFFA SCADUTA: DATA SPEDIZIONE MAGGIORE DELLA DATA  SCADENZA
368900980605    1C     TASDSP        IFGT      TAMDST
369000941107     C                   SETON                                        17
369100980605     C                   Z-ADD     TAMKSC        KSC2
369200980605     C                   Z-ADD     TAMCTR        CTR2
369300980605     C                   Z-ADD     TAMPRG        PRG2
369400941107     C                   GOTO      SUTAM
369500980605    1C                   ENDIF
369600020225     C*
369700020225     C** CERCO LA CARTELLO RELATIVA ALLA TARIFFA DEL CLIENTE
369800020227     C*  SE NON HO GIA' UNA TARIFFA DI CARTELLO E SE 94 SPENTO
369900020227     C     ENDTAM        TAG
370000020227     C*
370100020225     C*
370200020227    0C     *IN94         IFEQ      *OFF
370300020227      *
370400020227     C                   SETOFF                                       90
370500020227     C                   Z-ADD     1             T
370600020227     C     TAMKSC        LOOKUP    TAC(T)                                 90
370700020227      *
370800020227      * PER 90 SPENTO VUOL DIRE CHE NON HO TROVATO NESSUNA CARTELLO
370900020227    1C     *IN90         IFEQ      *OFF
371000020319     C***                  CLEARWNET
371100020225     C                   MOVEL     TAMFLO        DSTA01
371200020225     C*
371300020319     C**                   SELEC
371400020225     C* VERIFICO IL TIPO SERVIZIO
371500020319     C**         TAMTSP    WHEQ 'P'
371600020319     C**                   MOVEL'PPT'     WNET    3
371700020225     C* VERIFICO SE TARIFFA ITALIA VERIFICO SE NETWORK DPD
371800020319     C**         TAMFIE    WHEQ 'I'
371900020319     C**         §TADPD    ANDEQ'S'
372000020319     C**                   MOVEL'DPD'     WNET
372100020225     C* VERIFICO SE TARIFFA ESTERA VERIFICO SE NETWORK FEDEX
372200020319     C**         TAMFIE    WHEQ 'E'
372300020319     C**         §TAFED    ANDEQ'S'
372400020319     C**                   MOVEL'FED'     WNET
372500020225     C* VERIFICO SE TARIFFA EUROEXPRESS
372600020319     C**         TAMFIE    WHEQ 'E'
372700020319     C**                   MOVEL'EEX'     WNET
372800020225     C* VERIFICO SE TARIFFA ITALIA
372900020319     C**         TAMFIE    WHEQ 'I'
373000020319     C**                   MOVEL'COR'     WNET
373100020225     C*
373200020319     C**                   ENDSL
373300020225     C* SE NETWORK UGUALE A BLANK CERCO LA TARIFFA DI CARTELLO CORRIERE
373400020319     C**         WNET      IFEQ *BLANK
373500020319     C**                   MOVEL'COR'     WNET
373600020319     C**                   END
373700020225     C* AGGANCIO LA TABELLA DELLE TARIFFE DI CARTELLO
373800020319     C                   EXSR      CERTCA
373900020319     C**                   SETOF                     90
374000020319     C**         WNET      LOKUPNTW,T                    90
374100020225     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
374200020319     C**         *IN90     IFEQ *OFF
374300020319     C**                   MOVELMSG,9     TASMSG
374400020319     C**                   GOTO ENDTA2
374500020319     C**                   ENDIF
374600020319    1C                   ENDIF
374700020227     C*
374800020319     C**                   Z-ADDTAC,T     CARKSC
374900020319     C**                   Z-ADDCTC,T     CARCTR
375000020319     C**                   Z-ADDPRC,T     CARPRG
375100020319     C**                   MOVE DIC,T     DIVCAR
375200020227     C*
375300020227    0C                   ENDIF
375400061220      * se impostato il campo del salvataggio della data spedizione
375500061220     c                   if        Sav_dsp <> *zeros
375600061220     c                   eval      tasdsp = Sav_dsp
375700061220     c                   clear                   Sav_dsp
375800061220     c                   endif
375900020225     C*
376000941107     C*
376100020227     C     ENDTA2        ENDSR
376200020618     C******************************************************
376300020618     C* RICERCA DETTAGLIO SU TUTAD
376400020618     C******************************************************
376500020618     C     RICTAD        BEGSR
376600020618     C     KTAD          SETLL     TITAD000                               09
376700020618     C** 49  TROVATA TARIFFA DEL CAP   DI ARRIVO
376800020618     C   09              SETON                                        49
376900020618     C**
377000020618    1C     *IN09         IFEQ      *OFF
377100020618     C                   MOVEL     *BLANKS       COMNAZ
377200020618     C                   MOVEL     *BLANKS       COMCAP
377300020618     C     KTAD          SETLL     TITAD000                               09
377400020618    2C     *IN09         IFEQ      *OFF
377500020618     C*
377600020618     C                   Z-ADD     1             A
377700020618     C     COMCTS        LOOKUP    CTS(A)                                 05
377800020618     C**
377900020618     C* SE TROVATO COD.TASSAZIONE
378000020618    3C     *IN05         IFEQ      *ON
378100020618     C     A             OCCUR     DSCT
378200020618     C                   MOVEL     §CTRAP        COMCTS
378300020618     C** CARICO REGIONE AL POSTO DEL CODICE DI TASSAZIONE
378400020618     C     KTAD          SETLL     TITAD000                               09
378500020618    3C                   ENDIF
378600020618     C**
378700020618    3C     *IN09         IFEQ      *OFF
378800020618     C*
378900020618     C     WBOEST        IFEQ      'E'
379000020618     C                   MOVEL     'EE'          COMCTS
379100020618     C                   ELSE
379200020618     C                   MOVEL     'IT'          COMCTS
379300020618     C                   END
379400020618     C     KTAD          SETLL     TITAD000                               09
379500020618    3C                   ENDIF
379600020618    2C                   ENDIF
379700020618    1C                   ENDIF
379800020618     C                   ENDSR
379900941107     C/SPACE
380000941112     C******************************************************
380100990721     C** CERCA TARIFFA TITAD DETTAGLIO E CARICA DS
380200941112     C******************************************************
380300941107     C     CARTAR        BEGSR
380400941112C    C                   SETOFF                                       495694
380500941112     C                   MOVEL     TASCAD        COMCAP            9
380600990923     C                   MOVEL     TASNZD        COMNAZ            3
380700941112     C                   MOVEL     TASCTS        COMCTS
380800020618     C                   MOVEL     TASLNP        WLNP
380900020618     C                   EXSR      RICTAD
381000020618     C   09              GOTO      LATAR
381100020618     C**
381200020618     C** SE NON TROVATO TITAD CON LA LINEA DI PARTENZA, CERCO CON LA
381300020618     C**  REGIONE TROVATA DA ORGANIGRANNA DI TASLNP
381400020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
381500020618     C     TASLNP        IFNE      SAVLNP
381600020618     C     TASLNP        CHAIN     AZORG01L                           31
381700020618     C   31              CLEAR                   ORGDE7
381800020618     C                   MOVEL     ORGDE7        OG147
381900020618     C                   MOVEL     TASLNP        SAVLNP
382000020618     C                   ENDIF
382100020618     C**
382200020618     C                   MOVEL     TASCAD        COMCAP            9
382300020618     C                   MOVEL     TASNZD        COMNAZ            3
382400020618     C                   MOVEL     TASCTS        COMCTS
382500020618     C                   MOVEL     §OGRET        WLNP
382600020618     C                   EXSR      RICTAD
382700020618     C   09              GOTO      LATAR
382800020618     C**
382900020618     C** SE NON TROVATO TITAD CON LA REGIONE DI TASLNP, CERCO CON LA
383000020618     C**  LINEA ITALIA 950
383100020618     C                   MOVEL     TASCAD        COMCAP            9
383200020618     C                   MOVEL     TASNZD        COMNAZ            3
383300020618     C                   MOVEL     TASCTS        COMCTS
383400020618     C                   MOVEL     950           WLNP
383500020618     C                   EXSR      RICTAD
383600020618     C   09              GOTO      LATAR
383700941112     C*
383800941112     C*--------------------TARIFFA REVERSIBILE----------------------------------
383900941107     C     TIPBOL        CABNE     'A'           MANTAR
384000941107     C** TARIFFA REVERSIBILE SOLO PER RITORNO(TBL=A)
384100941107     C     TASLNP        CABEQ     68            MANTAR
384200941107     C** NO TARIFFA REVERSIBILE PER 68
384300981027     C     KSCFIL        IFEQ      888
384400981027     C                   Z-ADD     TASLNA        KFIL
384500981027     C                   ELSE
384600981027     C                   MOVEL     KSCFIL        KFIL
384700981027     C                   ENDIF
384800981008     C**
384900941107     C                   SETON                                        56
385000941112     C                   MOVEL     TASCAM        COMCAP
385100990923     C                   MOVEL     TASNZM        COMNAZ
385200941123     C                   MOVEL     TASMCT        COMCTS
385300020618     C                   MOVEL     KFIL          WLNP
385400020618     C                   EXSR      RICTAD
385500020618     C   09              GOTO      LATAR
385600020618     C*
385700020618     C** SE NON TROVATO TITAD CON LA LINEA CLIENTE, CERCO CON LA
385800020618     C**  REGIONE TROVATA DA ORGANIGRANNA DELLA LIENA CLIENTE
385900020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
386000020618     C     KFIL          IFNE      SAVLNP
386100020618     C     KFIL          CHAIN     AZORG01L                           31
386200020618     C   31              CLEAR                   ORGDE7
386300020618     C                   MOVEL     ORGDE7        OG147
386400020618     C                   MOVEL     KFIL          SAVLNP
386500020618     C                   ENDIF
386600020618     C*
386700020618     C                   MOVEL     TASCAM        COMCAP
386800020618     C                   MOVEL     TASNZM        COMNAZ
386900020618     C                   MOVEL     TASMCT        COMCTS
387000020618     C                   MOVEL     §OGRET        WLNP
387100020618     C                   EXSR      RICTAD
387200020618     C   09              GOTO      LATAR
387300020618     C**
387400020618     C** SE NON TROVATO TITAD CON LA REGIONE DI KFIL, CERCO CON LA
387500020618     C**  LINEA ITALIA 950
387600020618     C                   MOVEL     TASCAM        COMCAP
387700020618     C                   MOVEL     TASNZM        COMNAZ
387800020618     C                   MOVEL     TASMCT        COMCTS
387900020618     C                   MOVEL     950           WLNP
388000020618     C                   EXSR      RICTAD
388100020618     C   09              GOTO      LATAR
388200020618     C**
388300020618     C**         KTDREV    SETLLTITAD000                 09
388400020618     C** 09                SETON                     49
388500020618     C** 09                GOTO LATAR
388600020618     C**
388700020618     C**                   MOVEL*BLANKS   COMNAZ
388800020618     C**                   MOVEL*BLANKS   COMCAP
388900020618     C**         KTDREV    SETLLTITAD000                 09
389000020618     C** 09                GOTO LATAR
389100020618     C**
389200020618     C**                   Z-ADD1         A
389300020618     C**         COMCTS    LOKUPCTS,A                    05
389400020618     C**N05                GOTO POIT2
389500020618     C**         A         OCUR DSCT
389600020618     C**                   MOVEL§CTRAP    COMCTS
389700020618     C**         KTDREV    SETLLTITAD000                 09
389800020618     C** 09                GOTO LATAR
389900020618     C**         POIT2     TAG
390000020618     C**         WBOEST    IFEQ 'E'
390100020618     C**                   MOVEL'EE'      COMCTS
390200020618     C**                   ELSE
390300020618     C**                   MOVEL'IT'      COMCTS
390400020618     C**                   END
390500020618     C**         KTDREV    SETLLTITAD000                 09
390600020618     C** 09                GOTO LATAR
390700941107     C** NON TROVATO NEMMENO LA TARIFFA REVERSIBILE
390800941107     C     MANTAR        TAG
390900980527     C     *IN09         IFEQ      *OFF
391000980527     C                   SETON                                        94
391100980527     C                   MOVEL     MSG(8)        TASMSG
391200980527     C                   GOTO      FINE
391300980527     C                   ENDIF
391400941107     C** MANCA TARIFFA
391500941107     C     LATAR         TAG
391600030902     C                   EXSR      CARDST
391700941112     C** CARICA DS DELLA TARIFFA
391800941107     C                   ENDSR
391900941107     C/SPACE
392000941112     C******************************************************
392100941107     C** CARICA DS TARIFFA
392200941112     C******************************************************
392300941107     C     CARDST        BEGSR
392400980514     C     *LIKE         DEFINE    TADRPV        RAPPV
392500980514     C**
392600020218     C                   DO        200           A
392700941112     C     A             OCCUR     TARDS
392800941112     C                   CLEAR                   TARDS
392900941112     C                   END
393000941112     C                   Z-ADD     0             RAPPV
393100980514     C                   CLEAR                   WELAMT
393200941112     C**
393300980514    1C                   DO        *HIVAL        A
393400980514     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
393500980514    2C     WELAMT        IFNE      'S'
393600020618     C** 56      KTDREV    READETITAD000                 15
393700020618     C**N56      KTAD      READETITAD000                 15
393800020618     C     KTAD          READE     TITAD000                               15
393900941107     C   15              GOTO      ENDDST
394000980514    2C                   ENDIF
394100980514     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
394200980514    2C     WELAMT        IFEQ      'S'
394300980515     C                   MOVEL     'E'           WELAMT            1
394400980514    2C                   ENDIF
394500980514     C**
394600941107     C     A             OCCUR     TARDS
394700980514     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
394800980514     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
394900980514     C**
395000941120     C                   Z-ADD     TADLNP        TLNP
395100941107     C                   MOVEL     TADCTS        TCTS
395200990721     C                   MOVEL     TADNAZ        TNAZ
395300941110     C                   MOVEL     TADCAP        TCAP
395400941107     C                   Z-ADD     TADCTR        TCTR
395500941107     C                   Z-ADD     TADRPV        TRPV
395600941122     C                   Z-ADD     TADMIN        TMIN
395700941122     C                   Z-ADD     TADMAX        TMAX
395800040603     C***!!!****         Z-ADD     TADRPV        RAPPV
395900980514     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
396000980514    2C     WELAMT        IFNE      'E'
396100980514     C**
396200980514    3C     TADSGL        IFEQ      MINTAS
396300980514     C                   MOVEL     'E'           WELAMT
396400980514   X3C                   ELSE
396500980514     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
396600980514     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
396700980514     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
396800980514     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
396900980515    4C     TADSGL        IFGT      MINTAS
397000980514     C                   MOVEL     'S'           WELAMT
397100980514     C                   Z-ADD     MINTAS        TSCG
397200980514     C**
397300980522     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
397400980522    5C     TAMATA        IFEQ      0
397500980522     C     TAMATA        ORLE      MINTAS
397600980522     C     TADSGL        ORGT      TAMATA
397700980522     C**
397800980522    6C     UNOCTR        IFEQ      4
397900980514     C     UNOCTR        OREQ      0
398000980514     C     MINTAS        DIV(H)    100           WMTAS            15 5
398100980514     C                   ELSE
398200980514     C                   Z-ADD     MINTAS        WMTAS
398300980522    6C                   ENDIF
398400980514     C* TARIFFA
398500980514     C     WMTAS         MULT(H)   TADITR        TITR
398600980514     C     WMTAS         MULT(H)   TADINO        TINO
398700980522     C                   ELSE
398800980522     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
398900980522     C                   Z-ADD     TADITR        TITR
399000980522     C                   Z-ADD     TADINO        TINO
399100980522    5C                   ENDIF
399200980522    4C                   ENDIF
399300980514    3C                   ENDIF
399400980514    2C                   ENDIF
399500980514     C*
399600980514     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
399700980514    2C     WELAMT        IFNE      'S'
399800980514     C                   Z-ADD     TADSGL        TSCG
399900980514     C                   Z-ADD     TADITR        TITR
400000980514     C                   Z-ADD     TADINO        TINO
400100980514    2C                   ENDIF
400200980514    1C                   ENDDO
400300941107     C     ENDDST        ENDSR
400400941107     C/SPACE
400500941112     C******************************************************
400600941110     C** CARICAMENTO TABELLE SOLO LA 1 VOLTA
400700941112     C******************************************************
400800941110     C     CARTAB        BEGSR
400900980605     C     *LIKE         DEFINE    TAMATA        MINTAS
401000980605     C     *LIKE         DEFINE    TAMATA        MINTAR
401100990923     C     *LIKE         DEFINE    §TADIV        DIVCAR
401200020618     C                   CLEAR                   SAVLNP
401300980605     C**
401400980605     C                   OPEN      TABEL00F
401500980605     C**
401600941110     C                   EXSR      CARQT
401700941110     C** CARICAMENTO IAC
401800941110     C                   EXSR      CARTR
401900941110     C** CARICAMENTO TIPI TARIFFA
402000941110     C                   EXSR      CAR$2
402100941110     C** CARICAMENTO SIGLE VARIE
402200000103     C                   EXSR      CARCC
402300000103     C** CARICAMENTO SIGLE VARIE ESENTI
402400941112     C                   EXSR      CARCV
402500941112     C** CARICAMENTO CAMBI
402600941110     C                   EXSR      CARCT
402700941110     C** CARICAMENTO CODIFICI TASSAZIONE
402800941112     C                   EXSR      CAR5E
402900941112     C** CARICA TIPI SERVIZIO
403000980429     C                   EXSR      CAR1A
403100980429     C** CARICA TIPI INCASSO C/A MITTENTI
403200980603     C                   EXSR      CARTB
403300980603     C** CARICA DS TIPI BOLLA CHE NON PREVEDONO ASSICURAZIONE
403400020319     C                   EXSR      CAREST
403500980605     C** CARICA P.O. ESTERI
403600990917     C                   EXSR      CARDIV
403700990917     C** CARICAMENTO MONETA DI CONTO
403800020320     C                   EXSR      CARPAR
403900020320     C** CARICA TARIFFE DI CARTELLO
404000031120     C                   EXSR      CAR3A
404100031120     C** CARICO VARIE DELLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
404200980605     C                   CLOSE     TABEL00F
404300040505     C**
404400040505     C                   OPEN      TNTBE01L
404500040505     C** caricamento percentuale carburante
404600040505     C                   EXSR      CARPSC
404700080617     C** caricamento filiali addebito lasciato avviso
404800080617     C                   EXSR      CARLAV
404900111102     C** caricamento filiali addebito centro storico da cartello
405000111102     C                   EXSR      CARZTL
405100040505     C**
405200040505     C                   CLOSE     TNTBE01L
405300040505     C**
405400941110     C                   ENDSR
405500941110     C**********************************************************
405600941110     C** CARICAMENTO DS MULTIPLA  CODICI TASSAZIONE
405700941110     C**********************************************************
405800941110     C     CARCT         BEGSR
405900041220     C                   DO        600           A
406000941110     C     A             OCCUR     DSCT
406100941110     C                   CLEAR                   DSCT
406200941110     C                   END
406300941110     C** PULIZIA DS
406400941110     C                   MOVEL     'CT'          COD
406500941110     C     KTAB          SETLL     TABEL                                  07
406600941110     C  N07              SETON                                          H7
406700941110     C   H7              GOTO      FINE
406800941110     C                   Z-ADD     0             K
406900941110     C                   DO        *HIVAL        A
407000941110     C     KTAB          READE     TABEL                                  07
407100941110     C   07              GOTO      ENDCTS
407200941110     C     TBLKEY        IFNE      *BLANKS
407300941110     C                   ADD       1             K
407400941110     C                   MOVEL     TBLKEY        CTS(K)
407500941110     C     K             OCCUR     DSCT
407600941110     C                   MOVEL     TBLUNI        DSCT
407700941110     C                   END
407800941110     C                   END
407900941110     C     ENDCTS        TAG
408000941110     C                   ENDSR
408100941110     C/SPACE
408200941112     C**********************************************************
408300941112     C** CARICAMENTO TIPI SERVIZIO
408400941112     C**********************************************************
408500941112     C     CAR5E         BEGSR
408600941112     C                   DO        10            A
408700941112     C     A             OCCUR     DS5E
408800941112     C                   CLEAR                   DS5E
408900941112     C                   END
409000941112     C** PULIZIA DS
409100941112     C                   MOVEL     '5E'          COD
409200941112     C     KTAB          SETLL     TABEL                                  07
409300941112     C  N07              SETON                                          H7
409400941112     C   H7              GOTO      FINE
409500941112     C                   Z-ADD     0             K
409600941112     C                   DO        *HIVAL        A
409700941112     C     KTAB          READE     TABEL                                  07
409800941112     C   07              GOTO      END5E
409900941112     C     TBLKEY        IFNE      *BLANKS
410000941112     C                   ADD       1             K
410100941112     C                   MOVEL     TBLKEY        TS(K)
410200941112     C     K             OCCUR     DS5E
410300941112     C                   MOVEL     TBLUNI        DS5E
410400941112     C                   END
410500941112     C                   END
410600941112     C     END5E         TAG
410700941112     C                   ENDSR
410800980429     C**********************************************************
410900980429     C** CARICAMENTO TIPI INCASSO C/A
411000980429     C**********************************************************
411100980429     C     CAR1A         BEGSR
411200980430     C                   MOVEA     *ALL'9'       TIC
411300980429     C                   MOVEL     '1A'          COD
411400980429     C                   Z-ADD     0             K
411500980429     C     KTAB          SETLL     TABEL                                  07
411600980429     C     KTAB          READE     TABEL                                  07
411700980430    1C     *IN07         DOWEQ     *OFF
411800980430    2C     TBLKEY        IFNE      *BLANKS
411900980430     C     TBLFLG        ANDEQ     ' '
412000980429     C                   MOVEL     TBLUNI        DS1A
412100980430    3C     §1AFMB        IFEQ      'M'
412200980429     C                   ADD       1             K
412300980429     C                   MOVEL     TBLKEY        TIC(K)
412400980430    3C                   ENDIF
412500980430    2C                   ENDIF
412600980429     C     KTAB          READE     TABEL                                  07
412700980430    1C                   ENDDO
412800980429     C                   ENDSR
412900980603     C**********************************************************
413000980603     C** CARICAMENTO TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
413100980603     C**********************************************************
413200980603     C     CARTB         BEGSR
413300980603     C                   MOVEL     'TB'          COD
413400980603     C                   Z-ADD     0             K
413500980603     C     KTAB          SETLL     TABEL                                  07
413600980603     C     KTAB          READE     TABEL                                  07
413700980603    1C     *IN07         DOWEQ     *OFF
413800980603    2C     TBLKEY        IFNE      *BLANKS
413900980603     C     TBLFLG        ANDEQ     ' '
414000980603     C                   MOVEL     TBLUNI        DSTB
414100980603    3C     §TBFAS        IFEQ      '0'
414200980603     C                   ADD       1             K
414300980603     C                   MOVEL     TBLKEY        TBN(K)
414400980603    3C                   ENDIF
414500980603    2C                   ENDIF
414600980603     C     KTAB          READE     TABEL                                  07
414700980603    1C                   ENDDO
414800980603     C                   ENDSR
414900031120     C**********************************************************
415000031120     C** CARICAMENTO VARIE PER TIPI BOLLE CHE ACCETTANO UNA SINGOLA VARIA
415100031120     C**********************************************************
415200031120     C     CAR3A         BEGSR
415300031120     C                   MOVEL     '3A'          COD
415400031120     C                   Z-ADD     0             K
415500031120     C     KTAB          SETLL     TABEL                                  07
415600031120     C     KTAB          READE     TABEL                                  07
415700031120    1C     *IN07         DOWEQ     *OFF
415800031120    2C     TBLKEY        IFNE      *BLANKS
415900031120     C     TBLFLG        ANDEQ     ' '
416000031120     C                   MOVEL     TBLUNI        DS3A
416100031120    3C     §3ASVA        IFNE      ' '
416200031120     C                   ADD       1             K
416300031120     C                   MOVEL     §3ASVA        T3A(K)
416400031120    3C                   ENDIF
416500031120    2C                   ENDIF
416600031120     C     KTAB          READE     TABEL                                  07
416700031120    1C                   ENDDO
416800031120     C                   ENDSR
416900020319     C**********************************************************
417000020319     C** CARICAMENTO PO ESTERI
417100020319     C**********************************************************
417200020319     C     CAREST        BEGSR
417300020319     C*
417400020319     C                   CLEAR                   K
417500050504     C                   CLEAR                   KK                4 0
417600020319     C**
417700020319     C                   CLEAR                   CC
417800020319     C     *LOVAL        SETLL     AZORG01L
417900020319     C                   READ      AZORG01L                               07
418000020319    1C     *IN07         DOWEQ     *OFF
418100020319     C** FIL O AGENZIA
418200020319    2C     ORGFAG        IFEQ      'F'
418300020319     C     ORGFAG        OREQ      'A'
418400020319     C* NON ANNULLATO
418500020319    3C     ORGFVA        IFEQ      ' '
418600020319    4C                   MOVEL     ORGDE3        OG143
418700050504     c* p.o. esteri senza dpd
418800020319     C     §OGNTW        IFEQ      'EEX'
418900020319     C     §OGNTW        OREQ      'EUP'
419000020404     C*******    §OGNTW    OREQ 'DPD'
419100020319     C     §OGNTW        OREQ      'FED'
419200020319     C                   ADD       1             K
419300020319     C                   MOVEL     ORGFIL        EST(K)
419400020319    4C                   ENDIF
419500050504     c* p.o. estero totale, anche con DPD
419600050504    4C     §OGNTW        IFEQ      'EEX'
419700050504     C     §OGNTW        OREQ      'EUP'
419800050504     C     §OGNTW        OREQ      'DPD'
419900050504     C     §OGNTW        OREQ      'FED'
420000050504     C                   ADD       1             KK
420100050504     C                   MOVEL     ORGFIL        ESTtot(KK)
420200050504    4C                   ENDIF
420300020319    3C                   ENDIF
420400020319    2C                   ENDIF
420500020319     C**
420600020319     C                   READ      AZORG01L                               07
420700020319    1C                   ENDDO
420800020319     C                   ENDSR
420900941110     C********************************************************
421000941110     C** CARICAMENTO SIGLE VARIE
421100941110     C********************************************************
421200941110     C     CAR$2         BEGSR
421300941110     C                   CLEAR                   DS$2
421400941110     C                   MOVEL     '$2'          COD
421500941110     C                   MOVEL     *BLANKS       KEY
421600941110     C                   MOVEL     '1'           KEY
421700941110     C     KTABC         CHAIN     TABEL                              07
421800941110     C   07              SETON                                          H7
421900941110     C   H7              GOTO      FINE
422000941110     C                   MOVEL     TBLUNI        DS$2
422100941110     C                   ENDSR
422200000103     C********************************************************
422300000103     C** CARICAMENTO SIGLE VARIE  ESENTI IVA
422400000103     C********************************************************
422500000103     C     CARCC         BEGSR
422600000103     C                   CLEAR                   DSCC
422700020416     C                   CLEAR                   KK
422800000103     C                   MOVEL     'CC'          COD
422900000103     C                   MOVEL     *BLANKS       KEY
423000000103     C                   MOVEL     'VARIE'       KEY
423100000103     C     KTABC         SETLL     TABEL                                  07
423200020416     C**N07                GOTO ENDCC
423300000103     C                   DO        *HIVAL
423400000103     C     KTAB          READE     TABEL                                  07
423500000103     C   07              LEAVE
423600000103     C                   MOVEL     TBLUNI        DSCC
423700000103     C* CONTROLLO SE VARIA ESENTE CARICO LA SIGLA IN TABELLA
423800000103     C     §CCJEI        IFNE      *BLANKS
423900000103     C                   ADD       1             K
424000000103     C                   MOVE      TBLKEY        VES(K)
424100000103     C                   ENDIF
424200020416     C* CONTROLLO SE DA RESTITUIRE CON SIGAL VARIA E IMPORTO =0
424300020416     C     §CCSV0        IFEQ      'S'
424400050504     C                   ADD       1             KK                4 0
424500020416     C                   MOVE      TBLKEY        SV0(KK)
424600020416     C                   ENDIF
424700000103     C                   ENDDO
424800000103     C                   Z-ADD     0             K
424900020416     C                   Z-ADD     0             KK
425000000103     C     ENDCC         ENDSR
425100941112     C********************************************************
425200941112     C** CARICAMENTO CAMBI
425300941112     C********************************************************
425400941112     C     CARCV         BEGSR
425500941116     C                   Z-ADD     0             K
425600941116     C                   DO        50            A
425700941116     C     A             OCCUR     DSCV
425800941112     C                   CLEAR                   DSCV
425900941116     C                   END
426000941116     C                   MOVEL     *BLANKS       CV
426100941116     C*
426200941112     C                   MOVEL     'CV'          COD
426300941116     C     KTAB          SETLL     TABEL                                  07
426400941116     C  N07              GOTO      ENDCV
426500941116     C                   DO        *HIVAL        A
426600941116     C     KTAB          READE     TABEL                                  07
426700941116     C   07              GOTO      ENDCV
426800941116     C     TBLKEY        IFNE      *BLANKS
426900941116     C                   ADD       1             K
427000941116     C                   MOVEL     TBLKEY        CV(K)
427100941116     C     K             OCCUR     DSCV
427200941116     C                   MOVEL     TBLUNI        DSCV
427300941116     C                   END
427400941116     C                   END
427500941112     C     ENDCV         ENDSR
427600941110     C******************************************************
427700941110     C** CARICAMENTO TIPI TARIFFA
427800941110     C******************************************************
427900941110     C     CARTR         BEGSR
428000030205     C                   DO        50            A
428100941110     C     A             OCCUR     DSTR
428200941110     C                   CLEAR                   DSTR
428300941110     C                   END
428400941110     C**
428500941110     C                   MOVEL     'TR'          COD
428600941112     C                   Z-ADD     0             K
428700941110     C     KTAB          SETLL     TABEL                                  07
428800941110     C  N07              SETON                                          H7
428900941110     C   H7              GOTO      FINE
429000941110     C                   DO        *HIVAL        A
429100941110     C     KTAB          READE     TABEL                                  07
429200941110     C   07              GOTO      TAB1
429300941110     C     TBLKEY        IFNE      *BLANKS
429400941110     C                   ADD       1             K
429500941110     C                   MOVEL     TBLKEY        CTR(K)
429600941110     C     K             OCCUR     DSTR
429700941110     C                   MOVEL     TBLUNI        DSTR
429800941110     C                   END
429900941110     C                   END
430000941110     C     TAB1          ENDSR
430100941110     C**********************************************************
430200020221     C** MEMORIZZA
430300941110     C** TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
430400941110     C**********************************************************
430500941110     C     CARPAR        BEGSR
430600020225      *
430700020225     C     *LIKE         DEFINE    TAMKSC        CARKSC
430800020225     C     *LIKE         DEFINE    TAMCTR        CARCTR
430900020225     C     *LIKE         DEFINE    TAMPRG        CARPRG
431000020221      *
431100020222      * RICHIAMO IL TRUL27R PER OGNI NETWORK PER CARICARMI LE TARIFFE DI CARTELLO IN SCHIERA
431200020221      *
431300020221     C* CORRIERE BARTOLINI
431400020221     C                   CLEAR                   TRUL27
431500020221     C                   MOVEL     'COR'         I27NTW
431600020222     C                   EXSR      SRTRUL
431700020221      * POSTE
431800020222     C                   CLEAR                   TRUL27
431900020222     C                   MOVEL     'PPT'         I27NTW
432000020222     C                   EXSR      SRTRUL
432100020222      * DPD
432200020222     C                   CLEAR                   TRUL27
432300020222     C                   MOVEL     'DPD'         I27NTW
432400020222     C                   EXSR      SRTRUL
432500020222      * FEDEX
432600020222     C                   CLEAR                   TRUL27
432700020222     C                   MOVEL     'FED'         I27NTW
432800020222     C                   EXSR      SRTRUL
432900020222      * EUROEXPRESS
433000020222     C                   CLEAR                   TRUL27
433100020226     C                   MOVE      'L'           I27TLA
433200020222     C                   MOVEL     'EEX'         I27NTW
433300020222     C                   EXSR      SRTRUL
433400020226      *
433500020226     C                   CLEAR                   CARKSC
433600020226     C                   CLEAR                   CARCTR
433700020226     C                   CLEAR                   CARPRG
433800020222      *
433900020222      ** CARICO LE TARIFFE PARTICOLARI
434000941111     C                   EXSR      CAR1P
434100020222      *
434200941110     C                   ENDSR
434300040505     C**********************************************************
434400040505     C** CARICAMENTO SCHIERA SUPPLEMEMNTO CARBURANTE
434500040505     C**********************************************************
434600040505     C     CARPSC        BEGSR
434700040505
434800040507     C                   CLEAR                   KX
434900040505
435000040505     C                   MOVEL     'PSC'         KEYTBE
435100040505     C     KEYTBE        SETLL     TNTBE01L
435200040505     C                   DO        *HIVAL
435300040505     C     KEYTBE        READE     TNTBE01L
435400040505
435500040505     C                   IF        %EOF(TNTBE01L)
435600040505     C                   LEAVE
435700040505     C                   ENDIF
435800040505
435900040505     C                   IF        TBEATB = ' '
436000040507     C                   ADD       1             KX
436100040507     C                   IF        KX <= 999
436200040505     C                   MOVEL     TBEUNI        DPSC
436300040507     C                   Z-ADD     §PSCDAD       DS_DSC
436400040507     C                   Z-ADD     §PSCPER       DS_PSC
436500040507     C                   MOVE      WDSPSC        DSCA(KX)
436600040507     C                   ENDIF
436700040505     C                   ENDIF
436800040505
436900040505     C                   ENDDO
437000040507
437100040507     C                   SORTA     DSCA
437200040505
437300040505     C                   ENDSR
437400080617      *****************************************************************
437500080617      ** CARICAMENTO SCHIERA FILIALI ABILITATE ADDEBITO LASCIATO AVVISO
437600080617      *****************************************************************
437700080617     C     CARLAV        BEGSR
437800080617
437900080617     C                   CLEAR                   KX
438000080617
438100080617     C                   MOVEL     'LAV'         KEYTBE
438200080617     C     KEYTBE        SETLL     TNTBE01L
438300080617     C                   DO        *HIVAL
438400080617     C     KEYTBE        READE     TNTBE01L
438500080617
438600080617     C                   IF        %EOF(TNTBE01L)
438700080617     C                   LEAVE
438800080617     C                   ENDIF
438900080617
439000080617     C                   IF        TBEATB = ' '
439100080617     C                   ADD       1             KX
439200080617     C                   IF        KX <= 999
439300080617     C                   MOVEL     TBEUNI        DLAV
439400080617     C                   EVAL      SFILLAV(KX) = %dec(tbeke1:3:0)
439500080618     C                   EVAL      SDTALAV(KX) = D§LAVTAS
439600080617     C                   ENDIF
439700080617     C                   ENDIF
439800080617
439900080617     C                   ENDDO
440000080617
440100080617
440200080617     C                   ENDSR
440300111102      ******************************************************************
440400111102      ** CARICAMENTO SCHIERA FILIALI ADDEBITO DA CARTELLO CENTRO STORICO
440500111102      ******************************************************************
440600111102     C     CARZTL        BEGSR
440700111102
440800111102     C                   CLEAR                   KX
440900111102
441000111102     C                   MOVEL     'ZTL'         KEYTBE
441100111102     C     KEYTBE        SETLL     TNTBE01L
441200111102     C                   DO        *HIVAL
441300111102     C     KEYTBE        READE     TNTBE01L
441400111102
441500111102     C                   IF        %EOF(TNTBE01L)
441600111102     C                   LEAVE
441700111102     C                   ENDIF
441800111102
441900111102     C                   IF        TBEATB = ' '
442000111102     C                   ADD       1             KX
442100111102     C                   IF        KX <= 999
442200111102     C                   MOVEL     TBEUNI        DZTL
442300111102     C                   EVAL      SFILZTL(KX) = %dec(tbeke1:3:0)
442400111102     C                   EVAL      SDTAZTL(KX) = D§ZTLTAS
442500111102     C                   ENDIF
442600111102     C                   ENDIF
442700111102
442800111102     C                   ENDDO
442900111102
443000111102
443100111102     C                   ENDSR
443200020222     C*************************************************************
443300020222     C** RICHIAMO DEL TRUL27R
443400020222     C*************************************************************
443500020222     C     SRTRUL        BEGSR
443600020222      *
443700020222     C                   Z-ADD     TASDCT        I27DTA
443800021122     c                   Movel     TasCtr        I27Ctb
443900020222     C                   CALL      'TRUL27R'
444000020222     C                   PARM                    TRUL27
444100020222      *
444200020222     C     O27ERR        IFEQ      *BLANKS
444300020319     C     O27PRG        ANDNE     *BLANKS
444400020222     C                   ADD       1             T                 2 0
444500020222     C                   MOVE      I27NTW        NTW(T)
444600020222     C                   Z-ADD     O27KSC        TAC(T)
444700020222     C                   Z-ADD     O27CTR        CTC(T)
444800020226     C                   MOVE      O27PRG        PRC(T)
444900020222      *
445000020222      * AGGANCIO LA TARIFFA PER RECUPERARE LA DIVISA
445100020222      *
445200020226     C                   Z-ADD     O27KSC        CARKSC
445300020226     C                   Z-ADD     O27CTR        CARCTR
445400020226     C                   MOVE      O27PRG        CARPRG
445500020222     C     KTAMC         CHAIN     TNTAM00L                           94
445600020222      *
445700020222     C     *IN94         IFEQ      *OFF
445800020222     C                   MOVEL     TAMFLO        DSTA01
445900020222     C                   MOVE      §TADIV        DIC(T)
446000020226      * SE C'E ERRORE VADO A FINE PROGRAMMA
446100020226     C                   ELSE
446200020226     C                   MOVEL     MSG(9)        TASMSG
446300020226     C                   ENDIF
446400020226      *
446500020222      * SE C'E ERRORE VADO A FINE PROGRAMMA
446600020222     C                   ELSE
446700020222     C                   SETON                                        94
446800020222     C                   MOVEL     MSG(9)        TASMSG
446900020222     C                   ENDIF
447000020222      *
447100020222     C                   ENDSR
447200020225     C*************************************************************
447300020225     C** CERCO LA TARIFFA DI CARTELLO
447400020225     C*************************************************************
447500020225     C     CERTCA        BEGSR
447600020225      *
447700020225      * RICHIAMO IL TRUL27R PASANDOGLI LE CARATTERISTICHE DELLA SPEDIZIONE
447800020225      *
447900020318     C**                   CLEARTRUL27
448000020318     C**                   MOVE 'L'       I27TLA
448100020318     C**                   MOVE TASTSP    I27TSP
448200020318     C**                   MOVE TASLNA    I27LNA
448300020318     C**                   MOVE TASLNP    I27LNP
448400020318     C**                   MOVE TASKSC    I27CLI
448500020318     C**                   MOVE TASDCT    I27DTA
448600020318     C**                   CALL 'TRUL27R'
448700020318     C**                   PARM           TRUL27
448800020225      *
448900020318     C**         O27ERR    IFEQ ' '
449000020318     C**                   Z-ADDO27KSC    KSC2
449100020318     C**                   Z-ADDO27CTR    CTR2
449200020318     C**                   Z-ADDO27CTR    CODCTR
449300020318     C**                   MOVE O27PRG    PRG2
449400020225      * CARICO ANCHE I CAMPI  RELATIVI ALLA TARIFFA DI CARTELLO
449500020318     C**                   Z-ADDO27KSC    CARKSC
449600020318     C**                   Z-ADDO27CTR    CARCTR
449700020318     C**                   MOVE O27PRG    CARPRG
449800020225      *
449900020318     C**                   ENDIF
450000020318     C** DALLE SCHIERE CARICO LA TARIFFA DI CARTELLO
450100020318     C                   SETOFF                                       90
450200020319     C                   Z-ADD     1             T
450300020318     C     O27NTW        LOOKUP    NTW(T)                                 90
450400020318     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
450500020318     C     *IN90         IFEQ      *OFF
450600020318     C                   MOVEL     MSG(9)        TASMSG
450700020319     C                   SETON                                        94
450800020318     C                   GOTO      FINE
450900020318     C                   ENDIF
451000020318     C*
451100020318     C                   Z-ADD     TAC(T)        CARKSC
451200020318     C                   Z-ADD     CTC(T)        CARCTR
451300020318     C                   Z-ADD     PRC(T)        CARPRG
451400020318     C                   MOVE      DIC(T)        DIVCAR
451500020225      *
451600020225     C                   ENDSR
451700941114     C*************************************************************
451800941121     C** CARICAMENTO CODICI E TARIFFE PARTICOLARI DI CARTELLO
451900941114     C*************************************************************
452000941110     C     CAR1P         BEGSR
452100941111     C*
452200020114     C                   DO        50            A
452300941112     C     A             OCCUR     DS1P
452400941111     C                   CLEAR                   DS1P
452500941112     C                   END
452600941111     C                   MOVEL     *BLANKS       CP
452700941110     C**
452800941110     C                   MOVEL     '1P'          COD
452900941111     C                   Z-ADD     0             K                 5 0
453000941110     C     KTAB          SETLL     TABEL                                  07
453100941110     C  N07              SETON                                          H7
453200941110     C   H7              GOTO      FINE
453300941110     C                   DO        *HIVAL        A
453400941110     C     KTAB          READE     TABEL                                  07
453500941110     C   07              GOTO      TABCP
453600941110     C     TBLKEY        IFNE      *BLANKS
453700941115     C                   MOVEL     TBLUNI        CAMPO            26
453800941115     C                   MOVE      CAMPO         WFCP              1
453900941115     C     WFCP          IFEQ      'S'
454000941110     C                   ADD       1             K
454100941110     C                   MOVEL     TBLKEY        CP(K)
454200941112     C     K             OCCUR     DS1P
454300941112     C                   MOVEL     TBLUNI        DS1P
454400941112     C**
454500941110     C                   END
454600941110     C                   END
454700941111     C                   ENDDO
454800020222     C*
454900941110     C     TABCP         ENDSR
455000941110     C****************************************************
455100941110     C** CARICAMENTO DATI VARI TASSAZIONE
455200941110     C****************************************************
455300941110     C     CARQT         BEGSR
455400941110     C                   MOVEL     'QT'          COD
455500941110     C                   MOVEL     *BLANKS       KEY
455600941110     C                   MOVEL     '1'           KEY
455700941110     C     KTABC         CHAIN     TABEL                              07
455800941113     C   07              SETON                                        H7
455900941113     C   H7              GOTO      FINE
456000941110     C  N07              MOVEL     TBLUNI        DSQT1
456100941110     C*
456200941110     C                   ENDSR
456300990917     C****************************************************
456400990922     C** CARICAMENTO MONETE DI CONTO / IMPORTI STANDARD
456500990917     C****************************************************
456600990917     C     CARDIV        BEGSR
456700990917     C*****
456800990922      *  RICERCA DELLA MONETA DI CONTO
456900990917     C                   CLEAR                   DSBS02
457000990917     C                   CLEAR                   DGED
457100990922     C*
457200990917     C                   MOVEL     'C'           T02MOD
457300990917     C                   MOVEL     KNSIF         T02SIF
457400990917     C                   MOVEL     'GED'         T02COD
457500991112     C                   MOVEL     '1'           T02KE1
457600990917      *
457700990917     C                   CALL      'TIBS02R'
457800990917     C                   PARM                    KPJBA
457900990917     C                   PARM                    DSBS02
458000990917      *
458100990917     C     T02ERR        IFEQ      *BLANKS
458200990917     C                   MOVEL     T02UNI        DGED
458300990917     C                   ENDIF
458400990922      * RICERCA DEGLI IMPORTI STANDARD NELLA MONETA DI CONTO
458500990922     C                   CLEAR                   DSBS02
458600990922     C                   CLEAR                   DGEI
458700990922     C                   MOVEL     'L'           T02TLA
458800990922     C                   MOVEL     'C'           T02MOD
458900990922     C                   MOVEL     KNSIF         T02SIF
459000990922     C                   MOVEL     'GEI'         T02COD
459100991112     C                   MOVEL     §GEDCN        T02KE1
459200990922      *
459300990922     C                   CALL      'TIBS02R'
459400990922     C                   PARM                    KPJBA
459500990922     C                   PARM                    DSBS02
459600990922      *
459700990922     C     T02ERR        IFEQ      *BLANKS
459800990922     C                   MOVEL     T02UNI        DGEI
459900990922     C                   ENDIF
460000990922      *
460100990917      *
460200990917     C                   ENDSR
460300011127     C**********************************************************
460400011127     C** RECUPERO LA DATA DEL GIORNO
460500011127     C**********************************************************
460600011127     C     RUDATE        BEGSR
460700011127      *
460800011127     C                   TIME                    W0140            14 0
460900011127     C* UDATE IN GGMMAAAA
461000011127     C                   MOVE      W0140         WDTGIO            8 0
461100011127     C*
461200011127     C                   Z-ADD     WDTGIO        G02DAT
461300011127     C                   MOVEL     *BLANK        G02ERR
461400011127     C                   CALL      'XSRDA8'
461500011127     C                   PARM                    WLBDAT
461600011127     C*
461700011127     C* UDATE IN AAAAMMGG
461800011127     C                   Z-ADD     G02INV        WDTGIO
461900011127      *
462000011127     C                   ENDSR
462100011127     C**********************************************************
462200011127     C** RECUPERO LA DATA TASSAZIONE DA USARE PER QUESTA BOLLA
462300011127     C**********************************************************
462400030131     C**!!!RECDAT        BEGSR
462500011127      *
462600030131     C**!!!              SETOFF                                       31
462700011127      * VERIFICO SE DATA VALORIZZATA E SOPRATTUTTO NUMERICA
462800020826     C**         §ASDAT    IFNE *BLANK
462900020826     C**         §ASDAT    ANDNE*ZEROS
463000011127      *
463100020826     C**                   TESTN          §ASDAT     31
463200011127      * SE 31 ACCESO VUOL DIRE CHE È NUMERICO MA VERIFICO ANCHE L'ULTIMO CARATTERE
463300020826     C**         *IN31     IFEQ *ON
463400020826     C**                   MOVE §ASDAT    W01A    1
463500020826     C**         W01A      COMP '0'                  31  31
463600020826     C**                   ENDIF
463700011127      *
463800020826     C**                   ENDIF
463900011127      * SE 31 ACCESO DATA VALIDA ALTRIMENTI PRENDO LA DATA DEL GIORNO
464000020826     C**         *IN31     IFEQ *ON
464100020826     C**                   MOVE §ASDAT    DATTAS  80
464200020826     C**                   ELSE
464300020826     C**                   Z-ADDWDTGIO    DATTAS
464400020826     C**                   ENDIF
464500020618     C*
464600011127      *
464700030203     C**!!!              ENDSR
464800990916     C****************************************************
464900990916     C** CARICAMENTO IMPORTI PASSATI DALLE DS
465000990916     C****************************************************
465100990916     C     SAVDS         BEGSR
465200990916     C*
465300990922     C     *LIKE         DEFINE    TASPOR        TASINL
465400990916     C*
465500990916     C* SALVO LE SCHIERE DELLE VARIE (SIGLE VARIE+IMPORTI)
465600990916     C                   MOVEA     SV            SVW
465700990916     C                   MOVEA     VA            VAW
465800991012     C                   CLEAR                   TASINL
465900991012     C                   CLEAR                   TASDIF
466000990916     C*
466100990916     C* RECUPERO L'IMPORTO DELL'INOLTRO SE C'E' NELLA SCHIERA DELLE VARIE
466200990916     C                   Z-ADD     1             X                 2 0
466300990923     C     §$2FIN        LOOKUP    SV(X)                                  30
466400990916     C* SE LO TROVO VALORIZZO TASINL
466500990923     C   30              Z-ADD     VA(X)         TASINL
466600991111     C  N30              Z-ADD     0             X
466700990921     C*
466800990921     C* RECUPERO L'IMPORTO DEL DIRITTO FISSO  NELLA SCHIERA DELLE VARIE
466900990921     C                   Z-ADD     1             Y                 2 0
467000990921     C     §$2VRH        LOOKUP    SV(Y)                                  30
467100990921     C* SE LO TROVO VALORIZZO TASINL
467200990923     C   30              Z-ADD     VA(Y)         TASDIF           11 3
467300990916     C*
467400990916     C                   ENDSR
467500990921     C****************************************************
467600990921     C** CONVERSIONE DELLE VARIE IN MONETA DELLA TARIFFA
467700990921     C****************************************************
467800990921     C     CNVTAS        BEGSR
467900990921     C*
468000990921     C* SE DIVISA PASSATA (DELLA BOLLA) E' DIVERSA DALLA DIVISA DELLA
468100990921     C* TARIFFA CONVERTO IN DIVISA TARIFFA
468200990921     C                   Z-ADD     1             A
468300990923    1C                   DO        20            A
468400990923    2C     VA(A)         IFNE      *ZEROS
468500990921     C                   CLEAR                   YEUR
468600011127     C                   MOVEL     DIVINP        YECDVI
468700011127     C                   MOVEL     DIVOUT        YECDVO
468800990921     C                   Z-ADD     VA(A)         YECIMI
468900991014     C   12              MOVE      '3'           YECDCO
469000991014     C  N12              MOVE      '0'           YECDCO
469100990921     C*
469200990921     C                   CALL      'YEURCO'
469300990921     C                   PARM                    YEUR
469400990921     C*
469500990923    3C     YECESI        IFEQ      ' '
469600990921     C                   Z-ADD     YECIMO        VA(A)
469700990923    3C                   END
469800990923    2C                   END
469900990923    1C                   END
470000990921     C* CONVERTO L'IMPORTO DEL PORTO
470100990921     C* SE PORTO DIVERSO DA 0
470200990923    1C     TASPOR        IFGT      0
470300990921     C                   CLEAR                   YEUR
470400011127     C                   MOVEL     DIVINP        YECDVI
470500011127     C                   MOVEL     DIVOUT        YECDVO
470600990921     C                   Z-ADD     TASPOR        YECIMI
470700991014     C   12              MOVE      '3'           YECDCO
470800991014     C  N12              MOVE      '0'           YECDCO
470900990921     C*
471000990921     C                   CALL      'YEURCO'
471100990921     C                   PARM                    YEUR
471200990921     C*
471300990923    2C     YECESI        IFEQ      ' '
471400990921     C                   Z-ADD     YECIMO        TASPOR
471500990921     C                   ELSE
471600990921     C                   MOVEL     MSG(10)       TASMSG
471700990921     C                   GOTO      FINE
471800990923    2C                   END
471900990923    1C                   ENDIF
472000990921     C* SE INOLTRO DIVERSO DA 0
472100990923    1C     TASINL        IFGT      0
472200990921     C                   CLEAR                   YEUR
472300011127     C                   MOVEL     DIVINP        YECDVI
472400011127     C                   MOVEL     DIVOUT        YECDVO
472500990921     C                   Z-ADD     TASINL        YECIMI
472600991014     C   12              MOVE      '3'           YECDCO
472700991014     C  N12              MOVE      '0'           YECDCO
472800990921     C*
472900990921     C                   CALL      'YEURCO'
473000990921     C                   PARM                    YEUR
473100990921     C*
473200990923    2C     YECESI        IFEQ      ' '
473300990921     C                   Z-ADD     YECIMO        TASINL
473400990921     C                   ELSE
473500990921     C                   MOVEL     MSG(10)       TASMSG
473600990921     C                   GOTO      FINE
473700990923    2C                   END
473800990923    1C                   ENDIF
473900990921     C* SE DIRITTO FISSO DIVERSO DA 0
474000990923    1C     TASDIF        IFGT      0
474100990921     C                   CLEAR                   YEUR
474200011127     C                   MOVEL     DIVINP        YECDVI
474300011127     C                   MOVEL     DIVOUT        YECDVO
474400990921     C                   Z-ADD     TASDIF        YECIMI
474500991014     C   12              MOVE      '3'           YECDCO
474600991014     C  N12              MOVE      '0'           YECDCO
474700990921     C*
474800990921     C                   CALL      'YEURCO'
474900990921     C                   PARM                    YEUR
475000990921     C*
475100990923    2C     YECESI        IFEQ      ' '
475200990921     C                   Z-ADD     YECIMO        TASDIF
475300990923    2C                   END
475400990923    1C                   ENDIF
475500990929     C* SE IMPORTO IMPONIBILE DIVERSO DA ZEROS
475600991014     C* LO RICALCOLO PER SICUREZZA
475700990929    1C     TASIMV        IFGT      0
475800991014     C                   CLEAR                   DSLV16
475900991014     C                   MOVEL     'T'           D17FIM
476000991014     C                   Z-ADD     TASPOR        D17POR
476100991014     C                   CALL      'FNLV16R'
476200991014     C                   PARM                    DSLV16
476300991014     C                   PARM                    DTASV
476400991014     C                   Z-ADD     O17IMV        TASIMV
476500990929    1C                   ENDIF
476600990921     C* SE QUANTITA' DA FATTURARE E' DIVERSA DA 0 E LA TARIFFA E' UNA TARIFFA
476700011127     C* A VALORE SPECIFICO DEVO CONVERTIRE IL CAMPO SE RICHIESTO
476800990921     C*
476900011127    1C     TASQFT        IFGT      0
477000011127     C     TAMFVD        ANDEQ     '4'
477100011127     C     CONQTA        ANDNE     'N'
477200011127     C                   CLEAR                   YEUR
477300011127     C                   MOVEL     DIVINP        YECDVI
477400011127     C                   MOVEL     DIVOUT        YECDVO
477500011127     C                   Z-ADD     TASQFT        YECIMI
477600011127     C   12              MOVE      '3'           YECDCO
477700011127     C  N12              MOVE      '0'           YECDCO
477800011127     C*
477900011127     C                   CALL      'YEURCO'
478000011127     C                   PARM                    YEUR
478100011127     C*
478200011127    2C     YECESI        IFEQ      ' '
478300011127     C                   Z-ADD     YECIMO        TASQFT
478400011127    2C                   END
478500011127    1C                   ENDIF
478600011127     C* SE TASIAL VALORIZZATO DEVO CONVERTIRLO SOLO SE RICHIESTO
478700011127     C*
478800011127    1C     TASIAL        IFGT      0
478900011127     C     CONIAL        ANDNE     'N'
479000011127     C                   CLEAR                   YEUR
479100011127     C                   MOVEL     DIVINP        YECDVI
479200011127     C                   MOVEL     DIVOUT        YECDVO
479300011127     C                   Z-ADD     TASIAL        YECIMI
479400011129     C   12              MOVE      '2'           YECDCO
479500011127     C  N12              MOVE      '0'           YECDCO
479600011127     C*
479700011127     C                   CALL      'YEURCO'
479800011127     C                   PARM                    YEUR
479900011127     C*
480000011127    2C     YECESI        IFEQ      ' '
480100011127     C                   Z-ADD     YECIMO        TASIAL
480200011127    2C                   END
480300011127    1C                   ENDIF
480400990921     C*
480500990921     C                   ENDSR
480600990923     C************************************************************
480700990923     C** CONVERSIONE DEL DETTAGLIO TARIFFA PARTICOLARE DI CARTELLO
480800990923     C************************************************************
480900990923     C     CONTPD        BEGSR
481000990923     C*
481100990923     C                   CLEAR                   YEUR
481200990923     C                   MOVEL     DIVCAR        YECDVI
481300990923     C                   MOVEL     §TADIV        YECDVO
481400991014     C   12              MOVE      '3'           YECDCO
481500991014     C  N12              MOVE      '0'           YECDCO
481600990923     C*
481700990923     C* CONTROLLLO IL TIPO TARIFFA SE E' VALORE OPPURE NO PER CONVERTIRE
481800990923     C* LO SCAGLIONE OPPURE NO
481900990923     C*
482000990923    1C     WTPG          IFEQ      'T'
482100990923     C     WTPG          OREQ      '4'
482200990923     C     WTPG          OREQ      'V'
482300990923     C*
482400990923     C                   Z-ADD     TPDSGL        YECIMI
482500990923     C                   CALL      'YEURCO'
482600990923     C                   PARM                    YEUR
482700990923    2C     YECESI        IFEQ      ' '
482800990923     C                   Z-ADD     YECIMO        TPDSGL
482900990923    2C                   END
483000990923     C* NON CONVERTO L'IMPORTO TARIFFA PERCHE' E' UNA PERCENTUALE
483100990923    1C                   ELSE
483200990923     C*
483300990923     C                   Z-ADD     TPDITR        YECIMI
483400990923     C                   CALL      'YEURCO'
483500990923     C                   PARM                    YEUR
483600990923    2C     YECESI        IFEQ      ' '
483700990923     C                   Z-ADD     YECIMO        TPDITR
483800990923    2C                   END
483900990923    1C                   ENDIF
484000990923     C*
484100990923     C* MINIMO
484200990923     C                   Z-ADD     TPDMIN        YECIMI
484300990923     C                   CALL      'YEURCO'
484400990923     C                   PARM                    YEUR
484500990923    1C     YECESI        IFEQ      ' '
484600990923     C                   Z-ADD     YECIMO        TPDMIN
484700990923    1C                   END
484800990923     C* MASSIMO
484900990923     C                   Z-ADD     TPDMAX        YECIMI
485000990923     C                   CALL      'YEURCO'
485100990923     C                   PARM                    YEUR
485200990923    1C     YECESI        IFEQ      ' '
485300990923     C                   Z-ADD     YECIMO        TPDMAX
485400990923    1C                   END
485500990923     C*
485600990923     C                   ENDSR
485700020826     C************************************************************
485800020826     C** CONTROLLO SE APPLICARE PESO NORMALE O VDL
485900020826     C************************************************************
486000020826     C     CTRPES        BEGSR
486100020826     C                   CLEAR                   USAPKF
486200030131     C**!!!              CLEAR                   §ASSPC
486300030131     C                   CLEAR                   TASSPC
486400030429     C                   CLEAR                   TASPKCN
486500020826     C*
486600020826     C* SE NON C'E' IL PESO VDL, PRENDO QUELLO DA FATTURARE
486700030131     C**!!!§ASPKC        IFEQ      0
486800030131     C     TASPKC        IFEQ      0
486900020826     C                   Z-ADD     TASPKF        USAPKF
487000020826     C                   GOTO      ENDPES
487100020826     C                   ELSE
487200020826     C* SE IL VDL NON + MAI DA APPLICARE --> USO IL DA FATTURARE
487300020826     C     §TATAP        IFEQ      'M'
487400020826     C                   Z-ADD     TASPKF        USAPKF
487500020826     C                   GOTO      ENDPES
487600020826     C                   ENDIF
487700020826     C                   ENDIF
487800020826     C* PESO VDL - TARA
487900030131     C**!!!§QTTPC        MULT      §ASNCP        WTARA
488000030131     C     §QTTPC        MULT      TASNCP        WTARA
488100030131     C**!!!§ASPKC        SUB(h)    WTARA         WNTARA
488200030131     C     TASPKC        SUB(h)    WTARA         WNTARA
488300020826     C* SE PESO DA FATTURARE COMPRESO DA VDL E VDL-TARA
488400020826     C*  APPLICO SEMPRE IL PESO DA FATTURARE
488500020826     C*
488600020827     C     TASPKF        IFGE      WNTARA
488700030131     C**!!!TASPKF        ANDLE     §ASPKC
488800030131     C     TASPKF        ANDLE     TASPKC
488900020826     C                   Z-ADD     TASPKF        USAPKF
489000020826     C                   GOTO      ENDPES
489100020826     C                   ENDIF
489200020826     C* SE VDL-TARA > PESO FATTURARE LO APPLICO SIA PER " " CHE PER "S"
489300020827     C     WNTARA        IFGT      TASPKF
489400020827     C                   Z-ADD     WNTARA        USAPKF
489500030131     C**!!!              MOVEL     'S'           §ASSPC
489600030131     C**!!!              z-add     wntara        §ASpkc
489700030131     C                   MOVEL     'S'           TASSPC
489800030429     C                   z-add     wntara        TASpkcN
489900020826     C                   GOTO      ENDPES
490000020826     C                   ENDIF
490100020826     C* SE VDL-TARA < PESO FATTURARE LO APPLICO SOLO SE TOTALE E SE
490200020826     C* "S"
490300020827     C     WNTARA        IFLT      TASPKF
490400030131     C**!!!§ASNCP        ANDEQ     TASNCL
490500030131     C     TASNCP        ANDEQ     TASNCL
490600020826     C     §TATAP        ANDEQ     'S'
490700020827     C                   Z-ADD     WNTARA        USAPKF
490800030131     C**!!!              MOVEL     'S'           §ASSPC
490900030131     C**!!!              z-add     wntara        §ASpkc
491000030131     C                   MOVEL     'S'           TASSPC
491100030429     C                   z-add     wntara        TASpkcN
491200020826     C                   GOTO      ENDPES
491300020826     C                   ENDIF
491400020826     C* SE ANCORA A 0 (IMPOSSIBILE) USO IL PESO DA FATTURARE
491500020826     C     USAPKF        IFEQ      0
491600020826     C                   Z-ADD     TASPKF        USAPKF
491700020826     C                   ENDIF
491800020827     C**
491900020829     c     endpes        tag
492000020829     C                   ENDSR
492100020412**
492200980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo del CONTRASSEGNO  1
492300980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo da ASSICURARE     2
492400980529Non trovata tariffa cliente                                                    3
492500980512Mancano importo da assicurare in bolla e valore merce in tariffa               4
492600980603Non trovato SCAGLIONE in tariffa                                               5
492700980512Manca la quantita' da fatturare                                                6
492800980603Non trovato SCAGLIONE in tariffa particolare
492900980603Non trovato CODICE TASSAZIONE in tariffa                                       8
493000980529Non trovata tariffa di CARTELLO                                                9
493100000125Tariffa cliente con decorrenza maggiore della data spedizione                 10
493200990917Divisa errata                                                                 11
493300000125Tariffa DPD ma bolla Italia                                                   12
493400000125Bolla import/export DPD  ma  tariffa NON DPD                                  13
493500011127Divisa tariffa del cliente non più utilizzabile                               14
493600020318Tariffa FEDEX ma bolla Italia                                                 15
493700020318Bolla import/export FEDEX ma  tariffa NON FEDEX                               16
493800140224Non trovato COD.TASSAZIONE in tar.PARTICOLARE                                 17
493900030527Non presente tassazione per addebito insoluti                                 18
494000080221Tariffa valida ma imponibile minore di 0,000                                  19
