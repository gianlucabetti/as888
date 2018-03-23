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
002200020319     FAZORG01L  IF   E           K DISK
002300030205     D CTR             S              1    DIM(50)                              CODICI TARIFFA
002400020114     D CP              S              1    DIM(50)                              TARIFFE PARTICOLARI
002500941112     D TS              S              1    DIM(10)                              TIPI SERVIZIO
002600041220     D CTS             S              2    DIM(600)                             CODICI TASSAZIONE
002700990916     D SVW             S              1    DIM(20)                              SALVO SIGLE VARIE
002800990916     D VAW             S             11  3 DIM(20)                              SALVO IMPORTI VARIE
002900941116     D CV              S              3    DIM(50)                              CAMBI
003000980429     D TIC             S              2    DIM(20)                              TIPI INCASSO
003100031120     D* VARIE LEGATE ALLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
003200031120     D T3A             S              1    DIM(20)                              VARIE X BOLLE SING.V
003300980605     D* TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
003400980605     D TBN             S              2    DIM(20)                              TIPI BOLLA NO ASSIC
003500050504     D* P.O. ESTERI senza DPD
003600020319     D EST             S              3  0 DIM(300)                             P.O. ESTERI
003700050504     D* P.O. ESTERI tutti con DPD
003800050504     D ESTtot          S              3  0 DIM(300)                             P.O. ESTERI
003900000103     D* SIGLE VARIE ESENTI IVA
004000000103     D VES             S              1    DIM(20)                              SIGLE VARIE ESENTI D
004100020416     D* SIGLE VARIE DA RESTITUIRE CON IMPORTO =0
004200020416     D SV0             S              1    DIM(20)                              SIGLE VARIE IMP0 I D
004300020221     D* TARIFFE DI CARTELLO PER OGNI NETWORK
004400020221     D NTW             S              3    DIM(10)                              NETWORK
004500020221     D TAC             S              7  0 DIM(10)                              TARIFFA CARTELLO
004600020221     D CTC             S              3  0 DIM(10)                              CODICE TARIFFA CARTE
004700020221     D PRC             S              3  0 DIM(10)                              PROGRESSIVO TARIFFA
004800020222     D DIC             S              3    DIM(10)                              DIVISA      TARIFFA
004900040506      * date decorrenza + supplemento carburante
005000040506     D DSCA            S             13  0 DIM(999) DESCEND                     DTA DEC. + % carbur
005100080617      * filiali abilitate addebito lasciato avviso  + date inizio tassazione
005200080617     D SFilLav         S              3  0 DIM(999)                             Filiale
005300080617     D SDtaLav         S              8  0 DIM(999)                             Data tassazione
005400111102      * filiali abilitate addebito centro storico   + date inizio tassazione
005500111102     D SFilZtl         S              3  0 DIM(999)                             Filiale
005600111102     D SDtaZtl         S              8  0 DIM(999)                             Data tassazione
005700040506
005800040505
005900980605     D* MESSAGGI DI ERRORE DEI MANCA TARIFFA
006000080220     D MSG             S             78    DIM(19) CTDATA PERRCD(1)             MSG DI ERRORE
006100040505
006200040505
006300941112     D WLBDAT          DS                  INZ
006400941112     D  G02DAT                 1      8  0
006500941112     D  G02INV                 9     16  0
006600941112     D  G02ERR                17     17
006700941115     D  G02TGI                18     22  0
006800941107     D ACDS            DS                  OCCURS(100)
006900941112     D  ASGL                   1     13  3
007000941112     D  AITR                  14     24  3
007100990921     D  AMIN                  25     35  3
007200990921     D  AMAX                  36     46  3
007300990921     D  AAIN                  47     47
007400990721     D** VALORI TITPT DELLE TARIFFE PARTICOLARI
007500020218     D TARDS           DS                  OCCURS(200)
007600941107     D  TLNP                   1      3  0
007700941113     D  TCTS                   4      5
007800990721     D  TNAZ                   6      8
007900990721     D  TCAP                   9     17
008000990721     D  TCTR                  18     20  0
008100990721     D  TSCG                  21     33  3
008200990721     D  TITR                  34     44  3
008300990721     D  TINO                  45     55  3
008400990721     D  TRPV                  56     58  1
008500990921     D  TMIN                  59     69  3
008600990921     D  TMAX                  70     80  3
008700040507      * DS SUPPLEMMENTO CARBURANTE
008800040507     D WDSPSC          DS
008900040507     D  DS_DSC                 1      8  0
009000040507     D  DS_PSC                 9     13  2
009100941111     D** CODICI DI TASSAZIONE
009200041220     D DSCT          E DS                  OCCURS(600) INZ
009300941112     D*  TARIFFE PARTICOLARI
009400020114     D DS1P          E DS                  OCCURS(50) INZ
009500000103     D** SIGLE VARIE
009600000103     D DS$2          E DS
009700000103     D** VARIA ESENTE
009800000103     D DSCC          E DS
009900031120     D** 3A TIPIO BOLLE
010000031120     D DS3A          E DS
010100941112     D** CAMBI
010200980504     D DSCV          E DS                  OCCURS(50)
010300980504     D** DS DEL CAMPO TAMFLO DI TNTAM
010400980504     D DSTA01        E DS
010500020618     D OG147         E DS
010502140115     D** DS DEL CAMPO TPTFLO DI TITPT
010503140115     D DTPT01        E DS
010600980504     D** DS PASSATE DAI PGM RICHIAMANTI
010700990916     D DTAS          E DS
010800050928
010900060922     D DTAS01        E DS
011000060922
011100990916     D DTASV         E DS
011200941112     D  SV                     1     20
011300941112     D                                     DIM(20)                              SIGLE VARIE
011400990916     D  VA                    21    140P 3
011500990916     D                                     DIM(20)                              IMPORTI VARIE
011600990916     D  ES                   141    160
011700011127     D*
011800941110     D TAMDS         E DS                  EXTNAME(TNTAM00F) INZ
011900000000     D**  FILE TARIFFE  - MASTER
012000000000     D KPJBA         E DS
012100000125     D OG143         E DS                  INZ
012200000125     D DSQT1         E DS                  INZ
012300941107     D* DATI VARI TASSAZIONE 1
012400980429     D DS1A          E DS                  INZ
012500980429     D** TIPI INCASSO C/A
012600030205     D DSTR          E DS                  OCCURS(50) INZ
012700941107     D** TIPI TARIFFA
012800980603     D DSTB          E DS
012900980603     D** TIPI BOLLA
013000980429     D DS5E          E DS                  OCCURS(10) INZ
013100990917     D** MONETA DI CONTO/MONETA CORRENTE
013200990917     D DGED          E DS
013300990922     D** IMPORTI STANDARD NELLE MONETE DI CONTO
013400990922     D DGEI          E DS
013500040505     D** PERCENTUALE SUPPLEMENTO CARBURANTE
013600040505     D DPSC          E DS
013700080617     D** FILIALI ADDEBITO LASCIATO AVVISO
013800080617     D DLAV          E DS
013900111102     D** FILIALI ADDEBITO CENTRO STORICO
014000111102     D DZTL          E DS
014100990917     D** CALL PGM RICERCA TABELLA
014200990917     D DSBS02        E DS                  EXTNAME(TIBS02DS)
014300990917     D** CAMBIO DIVISA
014400990917     D YEUR          E DS                  EXTNAME(YEURCODS)
014500990922     D** CALL PGM CALCOLO TOTALE IMPONIBILE
014600990922     D DSLV16        E DS                  EXTNAME(FNLV16DS)
014700020221     D** CALL PGM RECUPERO TARIFFE DI CARTELLO
014800020221     D TRUL27        E DS                  EXTNAME(TRUL27DS)
014900980526     D PARAMT          DS
015000980526     D  PARCA                256    256
015100101014
015200101014     d TrulISTds     e ds
015300040505
015400040505      * -------------- variabili ------------------------------------
015500040505
015600040507     D Keytbe          S              3                                         chiave TBE
015700080617     D indx            S              3  0                                      Indice schiera
015800040507     D KX              S              3  0                                      Indice schiera
015900040507     D IMP_SCA         S             15  3                                      Impon.sup.carb.
016000040507     D PER_SCA         S              5  2                                      Perc.sup.carb.
016100060414     d applicarevers   s              1
016200060906
016300060906     d Sca_pb          s              3  0                                      scag.prez.base gasol
016400060906     d Sca_spe         s              3  0                                      scag.prez.gasol sped
016500060906     d Sca_sca         s              3  0                                      scaglioni scattati
016600080909     d Pmg_sgl         s             13  3                                      prezzo medio gasolio
016601140115     d Per_fuel        s              7  3                                      % calcolo fuel
016700040505
016800061219     d Sav_dsp         s              8  0                                      Salv. dta spedizione
016900080220
017000080220     d Wrksv           s             20                                         salvataggio sigle va
017100090922     d WvariaMag       s              1                                         salvataggio sigle va
017200090924     d WTSP            s                   like(taditr)
017300090924     d IMPON           s                   like(taditr)
017400090924     d WimpoMag        s                   like(taditr)
017500990916     C****************************************************************
017600990916     C*  RIEPILOGO INDICATORI
017700990916     C***************************************************************
017800000125     C* 05    - DI COMODO
017900990916     C* 07    - LETTURA GENERICO
018000990916     C* 08    - OFF MANCA TARIFFA CLIENTE
018100990916     C* 09    - LETTURA TITAD
018200000103     C* 10    - LOKUP
018300990923     C* 12    - MONETA CHE ACCETTA IMPORTI CON DECIMALI
018400990916     C* 15    - GENERICO
018500990916     C* 17    - TARIFFA SCADUTA
018600990916     C* 18    - TARIFFA NON TROVATA
018700990916     C* 28    - LETTURA TARIFFE PARTICOLARI
018800990916     C* 49    - TROVATA TARIFFA DEL CAP DI ARRIVO
018900001009     C* 56    - TROVATA TARIFFA  (TARIFFA REVERSIBILE)
019000001009     C* 57    - ASSEGNATO      (NON + IN USO *!!*)
019100020225     C* 90    - MANCA TARIFFA DI CARTELLO
019200990916     C* 94    - MANCA TARIFFA
019300990916     C* 96    - SCHIERE VARIE
019400990916     C*****************************************************************
019500000000     C     *ENTRY        PLIST
019600941111     C                   PARM                    KPJBA
019700990922     C                   PARM                    DTAS
019800990922     C                   PARM                    DTASV
019900060922
020000060922     C                   MOVEL     TASFLO        DTAS01
020100110624      * verifico data spedizione con data attivazione varia email avviso destinatario
020200130920     c***                if        §asemd = 'S' and tasdsp < 20110901
020300130920     c***                clear                   §asemd
020400130920     c***                endif
020500110624
020600060922
020700920225     C                   Z-ADD     1             CODUT             1 0
020800000000     C*---------------------------------------------------------------*
020900020226     C*
021000020226     C** ACCESSO TITPT01L
021100941107     C     KISOT         KLIST
021200941107     C                   KFLD                    TAMKSC
021300941107     C                   KFLD                    TAMCTR
021400941107     C                   KFLD                    TAMPRG
021500990722     C                   KFLD                    FISO              2
021600020226     C     KTPTC         KLIST
021700020226     C                   KFLD                    CARKSC
021800020226     C                   KFLD                    CARCTR
021900020226     C                   KFLD                    CARPRG
022000020226     C                   KFLD                    FISO
022100930419     C*
022200020222     C** ACCESSO TFTPD02L
022300941107     C     KISOD         KLIST
022400941107     C                   KFLD                    TPTKSC
022500941107     C                   KFLD                    TPTCTR
022600941107     C                   KFLD                    TPTPRG
022700941107     C                   KFLD                    TPTFTC
022800941107     C                   KFLD                    ISOCTS
022900020412     C** ACCESSO TFTPD02L
023000020412     C     KISOD2        KLIST
023100020412     C                   KFLD                    TPTKSC
023200020412     C                   KFLD                    TPTCTR
023300020412     C                   KFLD                    TPTPRG
023400020412     C                   KFLD                    TPTFTC
023500020222     C** ACCESSO TABEL X CODICE
023600941107     C     KTAB          KLIST
023700941107     C                   KFLD                    CODUT
023800941107     C                   KFLD                    COD               2
023900020222     C** ACCESSO TABEL X CODICE + CHIAVE
024000941107     C     KTABC         KLIST
024100941107     C                   KFLD                    CODUT
024200941107     C                   KFLD                    COD
024300941107     C                   KFLD                    KEY               8
024400020222     C** ACCESSO TNTAM  TARIFFA DI CARTELLO
024500020222     C     KTAMC         KLIST
024600020226     C                   KFLD                    CARKSC
024700020226     C                   KFLD                    CARCTR
024800020226     C                   KFLD                    CARPRG
024900020222     C** ACCESSO TNTAM TARIFFE SCADUTA
025000941107     C     KTAM2         KLIST
025100941107     C                   KFLD                    KSC2
025200941111     C                   KFLD                    CTR2
025300941107     C                   KFLD                    PRG2
025400020222     C** ACCESSO TNTAM X CLIENTE
025500941107     C     KTAM          KLIST
025600941107     C                   KFLD                    TMCLI
025700941107     C                   KFLD                    TMCTR
025800020222     C** ACCESO TITAD
025900941107     C     KTAD          KLIST
026000941107     C                   KFLD                    TDCLI
026100941107     C                   KFLD                    TDPRG
026200020618     C                   KFLD                    WLNP
026300941112     C                   KFLD                    COMCTS
026400990722     C                   KFLD                    COMNAZ
026500990722     C                   KFLD                    COMCAP
026600941107     C                   KFLD                    TDCTR
026700020222     C** ACCESSO TITAD PER TARIFFA REVERSIBILE
026800941112     C     KTDREV        KLIST
026900941112     C                   KFLD                    TDCLI
027000941112     C                   KFLD                    TDPRG
027100981027     C                   KFLD                    KFIL
027200941112     C                   KFLD                    COMCTS
027300990722     C                   KFLD                    COMNAZ
027400941112     C                   KFLD                    COMCAP
027500941112     C                   KFLD                    TDCTR
027600020826     C     *LIKE         DEFINE    TASPKF        USAPKF
027700020827     C     *LIKE         DEFINE    §qttpc        WTARA
027800020827     C     *LIKE         DEFINE    TASPKF        WNTARA
027900020826     C     *LIKE         DEFINE    KSCFIL        KFIL
028000020618     C     *LIKE         DEFINE    TASLNP        SAVLNP
028100020618     C     *LIKE         DEFINE    TASLNP        WLNP
028200020618     C     *LIKE         DEFINE    TASCTS        COMCTS
028300941107     C***********     -------------       ***********
028400930419     C                   MOVEL     ' '           RT
028500930419     C*
028600941125     C* TASTLA = ' ' ELABORO E CHIUDO PGM  CON RETRN
028700941125     C* TASTLA = 'L' ELABORO E CHIUDO PGM  CON LR
028800941125     C* TASTLA = 'C' NO ELAB.  CHIUDO PGM  CON LR PER CHIUSURA FILE
028900941116     C     TASTLA        IFEQ      ' '
029000941116     C     TASTLA        OREQ      'L'
029100941107     C*
029200941116     C     TASTLA        IFEQ      ' '
029300930419     C                   MOVEL     'R'           RT                1
029400930419     C                   END
029500980505     C* C A M P I   D I   O U T P U T
029600980505     C                   CLEAR                   TASERR
029700980505     C                   CLEAR                   TASMSG
029800980505     C                   CLEAR                   TASIAL
029900980526     C                   MOVEL     KPJBU         PARAMT
030000030902      * clearizzo indicatiori x manca tariffa e inoltro
030100030902     C                   SETOFF                                       9456
030200030211
030300030211     c                   Clear                   wFiso
030400990930     C** CONTROLLO LA DIVISA CHE MI VIENE PASSATA SE DIVERSA DA BLANK VERIFICO
030500990930     C** CHE SIA CORRETTA
030600990930     C     TASDIV        IFNE      *BLANKS
030700990930     C                   CLEAR                   DSLV16
030800990930     C                   MOVEL     'D'           D17FIM
030900990930     C                   MOVE      TASDIV        D17DIV
031000991012     C****                 MOVE TASDSP    D17DSP
031100990930     C                   CALL      'FNLV16R'
031200990930     C                   PARM                    DSLV16
031300990930     C                   PARM                    DTASV
031400990930     C* CONTROLLO IL CODICE DI RITORNO
031500990930     C     O17ERR        IFNE      *BLANKS
031600990930     C                   MOVEL     'E'           TASERR
031700990930     C                   MOVEL     O17MSG        TASMSG
031800990930     C                   SETON                                        94
031900990930     C                   GOTO      FINE
032000990930     C                   ENDIF
032100990930     C                   ENDIF
032200941125     C*
032300980605     C** CARICA TABELLE SOLO LA 1° VOLTA
032400941125     C     CARTBL        IFEQ      *BLANK
032500941125     C                   MOVE      '1'           CARTBL            1
032600941125     C                   EXSR      CARTAB
032700011127      * RECUPERO UDATE SOLO LA PRIMA VOLTA
032800011127     C                   EXSR      RUDATE
032900020320     C     TASMSG        IFNE      *BLANKS
033000020320     C                   SETON                                        94
033100020320     C                   GOTO      FINE
033200020320     C                   ENDIF
033300941125     C                   END
033400011127     C* R E C U P E R O   D A T A   D I   T A S S A Z I O N E
033500030131     C**!!!              EXSR      RECDAT
033600991012     C*
033700991012     C** SALVO GLI IMPORTI CHE MI VENGONO PASSATI
033800991012     C                   EXSR      SAVDS
033900941125     C*
034000941110     C                   SETOFF                                       94
034100030925      *
034200030925      * se richiamato per il solo calcolo della varia D  VADO A FINE
034300030925     C                   IF        TASSVA = 'D'
034400030925     C                   GOTO      FINE
034500030925     C                   ENDIF
034600030925      *+
034700941124     C                   MOVEL     TASTBL        TIPBOL            1
034800941124     C                   MOVEL     TASKSC        KSCFIL            3 0
034900941124     C                   MOVE      TASKSC        KSCCOD            4 0
035000080617      * da giugno 2008 viene riattivato per alcune filiali l'addebito del lasciato avviso
035100080617      * se tasric diverso da blank .. quindi la spedizione ha un evento di lasciato avviso
035200080617      * verifico se la filiale del cliente è presente nella tabella LAV oppure nella tabella
035300080617      * LAV esiste la filiale 999
035400080617    1c                   If        TasRic <> ' '
035500080617     c                   clear                   indx
035600080617     c                   eval      indx = %lookup(kscfil:sfillav)
035700080617      * se indice =  a zero cerco con la filiale 999
035800080617    2c                   if        indx = *zeros
035900080617     c                   eval      indx = %lookup(999:sfillav)
036000080617    2c                   endif
036100080617      * pulisco il flag del calcolo dell'addebito del lasciato avviso perchè filiale cliente
036200080617      * non abilitato all'addebito
036300080617    2c                   If        indx = *zeros
036400080617     c                   clear                   tasric
036500080617      * altrimenti verifico la data attivazione tassazione
036600080617   x2c                   else
036700080617      * se data spedizione inferiore all'attivazione pulisco il flag
036800080618    3c                   if        tasdsp < sdtalav(indx)
036900080617     c                   clear                   tasric
037000080617    3c                   endif
037100080617    2c                   endif
037200080617    1c                   endif
037300080617     c
037400980429     C* VEDO SE TIPO INCASSO E' INTESTATO AL MITTENTE
037500980430     C                   CLEAR                   TASBM
037600061016      * se il primo carattere del campo TASTIC è uguale a blank
037700061016      * ci troviamo a tassare una spedizione in sede in quanto
037800061016      * in TNCSB (file contrassegni ) la combinazione di tpa (tipo assegno)
037900061018      * e tpi (tipo intestazione) o fus (tipo intestazione in bolla)
038000061018      * potrebbe non esistere in tabella  1A tipo incasso c/assegno
038100061016      * a questo punto prendo solo il tipo intestazione assegno che
038200061016      * è uguale a  M per mittente  e "B" o " " per Vettore
038300061016      * In Tasbm passo solo se è uguale a M
038400061016
038500061016     C                   If        %subst(tastic:1:1) = ' '
038600061016     C                   If        %subst(tastic:2:1) = 'M'
038700061016     c                   eval      TASBM = 'M'
038800061016     c                   endif
038900061016
039000061016     c                   else
039100980429     C     TASTIC        LOOKUP    TIC                                    96
039200980429     C   96              MOVEL     'M'           TASBM             1
039300061016     c                   endif
039400980605     C** TASSAZIONE
039500941107     C                   EXSR      TARIFF
039600941110     C     FINE          TAG
039700980605     C** 94 ON - MANCA TARIFFA
039800990922     C     *IN94         IFEQ      '1'
039900990929     C* E NON C'E' TOTALE IMPONIBILE  DO ERRORE
040000990929     C     TASIMV        IFEQ      *ZEROS
040100990922     C                   MOVEL     'E'           TASERR            1
040200990929     C                   ENDIF
040300990922     C* REIMPOSTO LA DS COME PRIMA SENZA CONVERSIONI IN MONETA
040400990922     C*    MI SEMBRA INUTILE ADESSO EVENTUALMENTE LO FACCIO DOPO
040500990922     C*
040600990922     C                   ELSE
040700990929     C* CALCOLO IL TOTALE IMPONIBILE SE E' UGUALE A ZEROS
040800990929     C*
040900990929     C     TASIMV        IFEQ      *ZEROS
041000990923     C                   CLEAR                   DSLV16
041100990923     C                   MOVEL     'T'           D17FIM
041200990923     C                   Z-ADD     TASPOR        D17POR
041300990923     C                   CALL      'FNLV16R'
041400990923     C                   PARM                    DSLV16
041500990923     C                   PARM                    DTASV
041600001205     C*
041700001205     C* SE SPEDIZIONI CON DATA MAGGIORE O UGUALE AL 01012001 CALCOLO L'ADDIZIONALE DI GESTIONE
041800001211     C* SUL TOTALE IMPONIBILE E DOPO CALCOLO SUL TOTALE IMPONIBILE + ADDIZ. GESTIONE L'ISTAT
041900001205     C*
042000001221     C     TASDSP        IFGE      20010101
042100020416     C* ESEGUO SOLO SE NON HO RICHIAMATO LA TASSAZIONE PER UNA VARIA
042200031120     C*  SOLA SE VARIA NON PRESENTE NELLA TABELLA T3A (TABELLA DELLE VARIE DELLE BOLLE CHE
042300031121     C* ACCETTANO UNA SINGOLA  VARIA ESEMPIO "O" "Y" "*".....)
042400031120     C*    TASSVA        ANDEQ     ' '
042500031120      *
042600031120     C                   IF        TASSVA <> ' '
042700031120     C                   SETOFF                                       11
042800031120     C     TASSVA        LOOKUP    T3A(1)                                 11
042900031120     C                   ENDIF
043000040113      *
043100040113      *
043200040113      * SE VARIA UGUALE A BLANK                                             OPPURE
043300040113      * VARIA PRESENTE NELLA TABELLA T3A                                    OPPURE
043400040113      * TASSAZIONE 2° BOLLA     TASSVA = 'G' E TASCVAG = 'S'                OPPURE
043500040113      * VARIA UGUALE A "Z"                                                  OPPURE
043600040113      * CALCOLO A.G.+ ISTAT
043700040113      *
043800040113     C                   IF        TASSVA = ' ' OR
043900040113     C                             %FOUND       OR
044000040113     C                             (TASSVA = 'G' AND TASCVAG = 'S') OR
044100040113     C                             TASSVA = 'Z'
044200001205     C                   EXSR      ADGIST
044300001205     C                   ENDIF
044400031120      *
044500031120     C                   ENDIF
044600001205     C*
044700990923     C                   Z-ADD     O17IMV        TASIMV
044800001205     C*
044900990929     C                   ENDIF
045000030924      *
045100030924      * DIRITTO DI FATTURAZIONE lo calcolo solo alla fine della TASSAZIONE senza aggiunte dell'addi-
045200030924      *                         zionale di gestione
045300030924      *                         se totale imponibile maggiore di zero
045400030924     C                   IF        TASIMV > 0 AND
045500030924     C                             (TASSVA = §$2VRD OR TASSVDF= §$2VRD)
045600030924      *
045700030924     C                   EXSR      DIRFAT
045800030924      *
045900030924     C                   ENDIF
046000990922     C* MUOVO LA DIVISA DELLA TARIFFA
046100020416     C*  SE NON PASSATA LA VARIA O SE PASSATA E IMPORTO >0
046200020416     C*
046300020416     C     TASSVA        IFEQ      *BLANKS
046400020416     C     O17IMV        ORGT      0
046500990929     C                   MOVE      §TADIV        TASDIV
046600020416     C                   ENDIF
046700020416     C*
046800020416     C* SE SI TRATTA DI UNA VARIA DA PASSARE ANCHE A 0 CONTROLLO
046900020416     C     TASSVA        IFNE      *BLANKS
047000020416     C     O17IMV        ANDEQ     0
047100020416     C     TASSVA        LOOKUP    SV0                                    05
047200020416     C     *IN05         IFEQ      *ON
047300020416     C                   MOVEL     TASSVA        SV(1)
047400020416     C                   ENDIF
047500020416     C                   ENDIF
047600080221
047700080221      * VERIFICO SE IMPONIBILE A ZERO SENZA SIGLE DI VARIE PERCHÈ QUESTO TIPO DI CONTROLLO
047800080221      * VIENE ESEGUITO DAI PGM CHIAMANTI
047900080221     c                   clear                   wrksv
048000080221     c                   movea     sv            wrksv
048100080221     C                   IF        TASIMV = 0 AND wrkSV = *BLANKS
048200080221     C                   MOVEL     'E'           TASERR            1
048300080221     C                   MOVEL     MSG(19)       TASMSG
048400080221     C                   SETON                                        94
048500080221     C                   GOTO      FINE
048600080221     C                   ENDIF
048700990922     C*
048800990922     C                   ENDIF
048900011127     C* CONTROLLO LA DIVISA IN BASE ALLA DATA TASSAZIONE SE DA CONVERTIRE OPPURE NO
049000011127     C* SE STO TASSANDO CON DATA > DEL 2001 UNA BOLLA CON DATA < DEL 2002 E LA DIVISA NON E'
049100011127     C* UGUALE ALLA MONETA DI CONTO AZIENDALE CONVERTO TUTTI GLI IMPORTI ANCHE LA QTA DA
049200011127     C* FATTURARE NELLA DIVISA DI CONTO
049300011127     C*
049400020826     C**         DATTAS    IFGE 20020101
049500020826     C     TASDIV        IFNE      §GEDCN
049600011127     C     TASDSP        ANDLT     20020101
049700011127     C* CONVERTO
049800011127     C                   MOVE      TASDIV        DIVINP
049900011127     C                   MOVE      §GEDCN        DIVOUT
050000011127     C                   MOVE      'S'           CONQTA
050100011127     C                   MOVE      'S'           CONIAL
050200011127     C*
050300011127     C* RECUPERO LE CARATTERISTICHE DELLA MONETA DI CONTO
050400011127     C                   SETOFF                                       12
050500011127     C                   Z-ADD     1             K
050600011127     C     §GEDCN        LOOKUP    CV(K)                                  10
050700011127     C     *IN10         IFEQ      *ON
050800011127     C     K             OCCUR     DSCV
050900011127     C     §CVFDC        IFEQ      'S'
051000011127     C* DIVISA CHE ACCETTA DECIMALI
051100011127     C                   SETON                                        12
051200011127     C                   ENDIF
051300011127     C                   ENDIF
051400011127     C*
051500011127     C                   EXSR      CNVTAS
051600011127     C* VALORIZZO  LA DIVISA
051700011127     C                   MOVEL     §GEDCN        TASDIV
051800011127     C*
051900011127     C                   ENDIF
052000990922     C*
052100941107     C                   END
052200941107     C*
052300941107     C     RT            IFEQ      'R'
052400941107     C                   RETURN
052500941107     C                   ELSE
052600020319     C* CHIUDO PGM TRUL27R
052700020319     C                   CLEAR                   TRUL27
052800020319     C                   MOVE      'C'           I27TLA
052900020319     C                   CALL      'TRUL27R'
053000020319     C                   PARM                    TRUL27
053100941107     C                   SETON                                        LR
053200941107     C                   END
053300941107     C************       -----------       *************
053400941107     C**
053500941112     C******************************************************
053600941110     C**  CALCOLA TASSAZIONE BOLLA
053700941112     C******************************************************
053800941110     C     TARIFF        BEGSR
053900980605     C                   MOVEL     'I'           WBOEST
054000000125     C                   CLEAR                   WBODPD
054100020318     C                   CLEAR                   WBOFED
054200991014     C                   SETOFF                                       12
054300020318     C**
054400020318     C** PER VERIFICARE SE SONO BOLLA DPD FEDEX O ESTERA CHIAMO TRUL27
054500020318     C**  SENZA LA DATA
054600020319     C                   CLEAR                   TRUL27
054700020318     C                   MOVEL     TASTSP        I27TSP
054800020318     C                   MOVEL     TASLNA        I27LNA
054900020318     C                   MOVEL     TASLNP        I27LNP
055000020319     C                   MOVEL     TASKSC        I27CLI
055100021122     c                   Movel     Tasctr        I27Ctb
055200020318     C                   CALL      'TRUL27R'
055300020318     C                   PARM                    TRUL27
055400020318      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
055500020318     C     O27ERR        IFEQ      *BLANKS
055600020318     C     O27FIE        IFEQ      'E'
055700020318     C                   MOVEL     'E'           WBOEST            1
055800020318     C                   ENDIF
055900020318     C*
056000020318     C     O27FIE        IFEQ      'D'
056100020318     C                   MOVEL     'S'           WBODPD            1
056200020318     C                   MOVEL     'I'           WBOEST            1
056300020318     C                   ENDIF
056400020318     C*
056500020318     C     O27FIE        IFEQ      'F'
056600021122     C     O27FIE        orEQ      'M'
056700021122     C     O27FIE        orEQ      'N'
056800020318     C                   MOVEL     'S'           WBOFED            1
056900020318     C                   MOVEL     'E'           WBOEST            1
057000020318     C                   ENDIF
057100020318     C                   ELSE
057200020319     C                   SETON                                        94
057300020319     C                   MOVEL     O27MSG        TASMSG
057400020318     C                   GOTO      FINE
057500020318     C                   ENDIF
057600020320     C**
057700030429     C* LO PASSO IN OUTPUT
057800030131     C**!!!              MOVEL     O27FIE        §ASFIE
057900030131     c                   MOVEL     O27FIE        TASFIE
058000021122      * Per tutte le tipologie di bolla FedEx imposto sempre 'F' xchè il
058100021122      * TNSF02 testa 'F' x definire che è una bolla FedEx (descrizione varie)
058200021122     c                   If        O27Fie = 'M' or O27Fie = 'N'
058300030131     c**!!!              Movel     'F'           §AsFie
058400030131     c                   Movel     'F'           TAsFie
058500021122     c                   EndIf
058600980605     C**
058700980605     C** VEDO SE BOLLA ITALIA O ESTERA (IMPORT O EXPORT)
058800020318     C**         TASLNP    LOKUPEST                      05
058900020318     C**N05      TASLNA    LOKUPEST                      05
059000020318     C**         *IN05     IFEQ *ON
059100020318     C**                   MOVEL'E'       WBOEST  1
059200020318     C**                   ENDIF
059300020318     C**         KSCFIL    LOKUPDPD                      05
059400020318     C**N05      TASLNA    LOKUPDPD                      05
059500000828     C******* TOLTO CONTROLLO CON LA LNP BASTA LNA E LINEA KSC 27/7
059600000828     C**      28/8
059700000828     C**      BISOGNA LASCIARE IL CONTROLLO ANCHE PER LNP PER I RESI
059800000828     C**      DPD CHE SONO IN ASSEGNATO SU LNA E CLIENTE ITALIA MA
059900000828     C**      LNP DPD
060000000828     C**      NON RICORDO I PROBLEMI CHE PUO' DARE USARE ANCHE LA LNP
060100000828     C**      HO DEI DUBBI SUI DROTTAMENTI 01 PER 190 E POI 190 PER 310
060200000828     C**      FORSE E' STATO SCELTO IL "MALE MINORE"
060300020318     C**N05      TASLNP    LOKUPDPD                      05
060400020318     C**         *IN05     IFEQ *ON
060500020318     C**                   MOVEL'S'       WBODPD  1
060600020318     C**                   MOVEL'I'       WBOEST  1
060700020318     C**                   ENDIF
060800001201     C*
060900001201     C* VERIFICO SE CODICE APPARTIENE ALLA SDI
061000001201     C*
061100001201     C                   CLEAR                   KSDIT
061200001201     C     KSCFIL        CHAIN     AZORG01L                           07
061300001201     C     *IN07         IFEQ      *OFF
061400001201     C                   MOVEL     ORGDIT        KSDIT             3
061500001201     C                   ENDIF
061600980605     C**
061700980605     C**
061800020222     C* PER 8888 E 9999 --> CERCO LA TARIFFA DI CARTELLO RELATIVA AL TIPO DI SPEDIZIONE
061900980605    1C     KSCCOD        IFEQ      8888
062000980527     C     KSCCOD        OREQ      9999
062100020225      * CERCO LA TARIFFA DI CARTELLO
062200020225     C                   EXSR      CERTCA
062300020319     C                   Z-ADD     CARKSC        KSC2
062400020319     C                   Z-ADD     CARCTR        CTR2
062500020319     C                   Z-ADD     CARCTR        CODCTR
062600020319     C                   Z-ADD     CARPRG        PRG2
062700020225      *
062800980605     C     KTAM2         CHAIN     TNTAM00L                           94
062900020222      *
063000980605     C** NON TROVATA (IMPOSSIBILE!!!)
063100980605    2C     *IN94         IFEQ      *ON
063200980605     C                   MOVEL     MSG(10)       TASMSG
063300980605     C                   GOTO      FINE
063400980605    2C                   ENDIF
063500980605     C* KSCCOD NON LO IMPOSTO PERCHE'PER I 9999 VOGLIO CHE RIMANGA TALE
063600980605     C**
063700980605   X1C                   ELSE
063800941112     C** ACCESSO TNTAM
063900980605     C                   Z-ADD     TASCTR        CODCTR            3 0
064000980605     C                   Z-ADD     TASKSC        CODCLI            7 0
064100980605     C**
064200941110     C                   Z-ADD     CODCLI        TMCLI             7 0
064300980515     C                   Z-ADD     CODCTR        TMCTR             3 0
064400941110     C                   EXSR      CERTAM
064500020227     C** SE 90 /94 SONO SPENTI NON HO TROVATO LA TARIFFA DI CARTELLO RELATIVA ALLA TARIFFA
064600020319     C** DEL CLIENTE   (QUESTO CASO L'HO ELIMINATO)
064700020227     C     *IN90         IFEQ      *OFF
064800020227     C     *IN94         ANDEQ     *OFF
064900020225     C*** NON SO COSA FARE ANCHE PERCHE' E' IMPOSSIBILE MA VADO A FINE LO STESSO
065000020225     C                   SETON                                        94
065100020225     C                   GOTO      FINE
065200020225     C                   ENDIF
065300980604     C** NON C'E' TARIFFA CLIENTE
065400980605    2C     *IN94         IFEQ      *ON
065500980604     C**
065600980605     C* SE NON HO CHIAMATO PER UNA VARIA  -- > ERRORE
065700020416     C* NON DO ERRORE SOLO PER LA VARIA "R"
065800020416    3C     TASSVA        IFNE      'R'
065900980604     C                   GOTO      FINE
066000980605   X3C                   ELSE
066100980604     C**
066200020416     C** SE HO CHIAMATO PER LA VARIA 'R' --> IMPOSTO LA TAR.CARTELLO
066300980605     C* IMPOSTO ANCHE KSCCOD perche' voglio che me lo tratti come
066400980605     C*  cliente generico da tariffa di cartello per l'assicurazione
066500980605     c*  Non ho problemi per i 9999 èerche' avendoli gia' impostati
066600980605     c*  la prima volta che fa  certam trova la tariffa (di cartello)
066700980605     C                   MOVE      8888          KSCCOD
066800020225     C* CERCO LA CARTELLO IN BASE ALLE CARATTERISTICHE DELLA SPEDIZIONE EW DEL CLIENTE
066900020225     C                   EXSR      CERTCA
067000020319     C                   Z-ADD     CARKSC        KSC2
067100020319     C                   Z-ADD     CARCTR        CTR2
067200020319     C                   Z-ADD     CARCTR        CODCTR
067300020319     C                   Z-ADD     CARPRG        PRG2
067400020225     C*
067500980605     C     KTAM2         CHAIN     TNTAM00L                           94
067600980605     C** NON TROVATA (IMPOSSIBILE!!!)
067700980605    4C     *IN94         IFEQ      *ON
067800980605     C                   MOVEL     MSG(10)       TASMSG
067900980605     C                   GOTO      FINE
068000980605    4C                   ENDIF
068100020225     C* MI CARICO LA DIVISA DELLA CARTELLO
068200020225     C                   MOVEL     TAMFLO        DSTA01
068300020225     C                   MOVEL     §TADIV        DIVCAR
068400020225     C*
068500980605    3C                   ENDIF
068600980605    2C                   ENDIF
068700980605    1C                   ENDIF
068800980604     C**
068900941110     C                   Z-ADD     TAMKSC        TDCLI             7 0
069000941110     C                   Z-ADD     TAMPRG        TDPRG             3 0
069100941110     C                   Z-ADD     TAMCTR        TDCTR             3 0
069200980609     C* CAMPO DI OUTPUT
069300000125     C                   MOVEL     TAMBAP        TASBAP
069400980609     C                   MOVEL     TAMFLO        DSTA01
069500000125     C*
069600000125     C* SE BOLLA DPD E TARIFFA DI CARTELLO --> ERRORE SE NON RICHIAMO
069700020319     C*  PER UNA VARIA E SE LA CARTELLO NON E' UNA CARTELLO DPD
069800000125     C     KSCCOD        IFEQ      8888
069900000125     C     KSCCOD        OREQ      9999
070000020416     C     TASSVA        IFNE      'R'
070100000125     C     WBODPD        ANDEQ     'S'
070200020225      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
070300020225     C     §TADPD        ANDNE     'S'
070400000125     C                   SETON                                        94
070500000125     C                   MOVEL     MSG(13)       TASMSG
070600000125     C                   GOTO      FINE
070700000125     C                   ENDIF
070800020416     C     TASSVA        IFNE      'R'
070900020319     C     WBOFED        ANDEQ     'S'
071000020319      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
071100020319     C     §TAFED        ANDNE     'S'
071200020319     C                   SETON                                        94
071300020319     C                   MOVEL     MSG(16)       TASMSG
071400020319     C                   GOTO      FINE
071500020319     C                   ENDIF
071600000125     C                   ENDIF
071700000125     C*
071800000125     C* SE TARIFFA O CLIENTE TARIFFA DPD E BOLLA NON LO E'--> MANCA TAR
071900000125     C     §TADPD        IFEQ      'S'
072000000125     C     WBODPD        IFEQ      ' '
072100000125     C                   SETON                                        94
072200000125     C                   MOVEL     MSG(12)       TASMSG
072300000125     C                   GOTO      FINE
072400000125     C                   ENDIF
072500000125     C                   ELSE
072600000125     C     WBODPD        IFEQ      'S'
072700000125     C                   SETON                                        94
072800000125     C                   MOVEL     MSG(13)       TASMSG
072900000125     C                   GOTO      FINE
073000000125     C                   ENDIF
073100000125     C                   ENDIF
073200020319     C* SE TARIFFA O CLIENTE TARIFFA FED E BOLLA NON LO E'--> MANCA TAR
073300020319     C     §TAFED        IFEQ      'S'
073400020319     C     WBOFED        IFEQ      ' '
073500020319     C                   SETON                                        94
073600020319     C                   MOVEL     MSG(15)       TASMSG
073700020319     C                   GOTO      FINE
073800020319     C                   ENDIF
073900020319     C                   ELSE
074000020319     C     WBOFED        IFEQ      'S'
074100020319     C                   SETON                                        94
074200020319     C                   MOVEL     MSG(16)       TASMSG
074300020319     C                   GOTO      FINE
074400020319     C                   ENDIF
074500020319     C                   ENDIF
074600000125     C**
074700990927     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
074800990927     C     §TADIV        IFEQ      *BLANK
074900990927     C                   MOVE      'ITL'         §TADIV
075000990927     C                   ENDIF
075100011127      * VERIFICO IN BASE ALLA DATA TASSAZIONE E DATA SPEDIZIONE SE LA DIVISA DELLA TARIFFA
075200011127      * E' CORRETTA
075300011127      *
075400011127      * SE STO TASSANDO con data > del 2001 un bolla con data > del 2001 E LA DIVISA NON E'
075500011127      * UGUALE ALLA MONETA DI CONTO AZIENDALE
075600020826     C***        DATTAS    IFGT 20011231
075700020826     C     §TADIV        IFNE      §GEDCN
075800011127     C     TASDSP        ANDGT     20011231
075900011127      * MANCA TARIFFA
076000011127     C                   SETON                                        94
076100011127     C                   MOVEL     MSG(14)       TASMSG
076200011127     C                   GOTO      FINE
076300011127     C                   ENDIF
076400991014     C*
076500991014     C* RECUPERO LE CARATTERISTICHE DELLA DIVISA
076600991014     C                   Z-ADD     1             K
076700991014     C     §TADIV        LOOKUP    CV(K)                                  10
076800991014     C     *IN10         IFEQ      *ON
076900991014     C     K             OCCUR     DSCV
077000991014     C     §CVFDC        IFEQ      'S'
077100991014     C* DIVISA CHE ACCETTA DECIMALI
077200991014     C                   SETON                                        12
077300991014     C                   ENDIF
077400991014     C                   ENDIF
077500990921     C** RICERCA TNTAM
077600990921     C** VERIFICO SE LA MONETA PASSATA E'= A QUELLA  DELLA TARIFFA
077700990921     C     TASDIV        IFNE      §TADIV
077800990928     C* E LA TARIFFA PASSAT NON E' UGUALE A BLANK
077900990928     C     TASDIV        ANDNE     *BLANKS
078000990921     C** CONVERTO GLI IMPORTI NELLA MONETA DELLA TARIFFA CLIENTE
078100990921     C*  ANCHE LE VARIE
078200011127     C*
078300011127     C* PASSO LE MONETE PER LA CONVERSIONE ED IN PIÙ IL FLAG SE DEVO
078400011127     C* CONVERTIRE LA QUANTITA' DA FATTURARE OPPURE NO
078500011127     C* NEL CASO IN CUI CONVERTO NELLA MONETA DELLA TARIFFA
078600011127     C* NON E' NECESSARIO CONVERTIRE LA QTA DA FATTURARE E
078700011127     C* L'IMPORTO D'ASSICURARE CALCOLATO IN TASSAZIONE
078800011127     C                   MOVE      TASDIV        DIVINP            3
078900011127     C                   MOVE      §TADIV        DIVOUT            3
079000011127     C                   MOVE      'N'           CONQTA            1
079100011127     C                   MOVE      'N'           CONIAL            1
079200990921     C                   EXSR      CNVTAS
079300990921     C*
079400990921     C                   ENDIF
079500990929     C*------------------------------------------------------
079600990929     C* SE IMPONIBILE VALORIZZATO VADO A FINE
079700990929     C     TASIMV        IFNE      *ZEROS
079800000103     C* E NON E' STATA RICHIESTA NESSUNA VARIA
079900000103     C     TASSVA        ANDEQ     ' '
080000990929     C                   GOTO      FINE
080100990929     C                   ENDIF
080200980506     C*------------------------------------------------------
080300020826     C* VERIFICO SE APPLICARE IL PESO VDL O MENO
080400020826     C                   EXSR      CTRPES
080500020826     C*------------------------------------------------------
080600980506     C** PORTO O INOLTRO PRETASSATO
080700990721     C**  OPPURE DEVO CALCOLARE UNA VARIA SPECIFICA --> NON CERCO TITAD
080800941110     C     TASPOR        IFGT      0
080900980506     C     TASINL        ORGT      0
081000980506     C     TASSVA        ORNE      ' '
081100000516     C**                   Z-ADD0         TASPVL
081200941110     C                   GOTO      TASSAT
081300941110     C                   END
081400941110     C*
081500941112     C                   MOVEL     CODCTR        UNOCTR            1 0
081600980506     C* QUANTITA' DA FATTURARE
081700950119     C     UNOCTR        IFEQ      5
081800950210     C     TASQFT        COMP      0                                      94
081900980512     C   94              MOVEL     MSG(6)        TASMSG
082000980512     C   94              GOTO      FINE
082100950119     C                   END
082200950119     C** TARIFFA A QUANTITA' - MANCA Q.TA' DA FATTURARE
082300950119     C     UNOCTR        IFEQ      4
082400950119     C     TAMFVD        IFEQ      *BLANKS
082500950119     C     TAMFVD        OREQ      '2'
082600961007     C     TAMFVD        OREQ      '4'
082700950210     C     TASQFT        COMP      0                                      94
082800980512     C   94              MOVEL     MSG(6)        TASMSG
082900950119     C   94              GOTO      FINE
083000950120     C                   GOTO      POIQTF
083100950119     C                   END
083200950119     C** TARIFFA A VALORE FLAG 2 E BLANK
083300950119     C     TAMFVD        IFEQ      '1'
083400950210     C     TASQFT        CABGT     0             POIQTF
083500950119     C                   END
083600950119     C*
083700950210     C                   Z-ADD     TASIAS        TASQFT
083800950119     C                   Z-ADD     1             K
083900950119     C     TASVAS        LOOKUP    CV(K)                                  05
084000980527     C     *IN05         IFEQ      *OFF
084100980527     C                   SETON                                        94
084200980527     C                   MOVEL     MSG(2)        TASMSG
084300980527     C                   GOTO      FINE
084400980527     C                   ENDIF
084500990917     C**
084600990917     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA
084700990917     C                   CLEAR                   YEUR
084800990917     C                   MOVEL     TASVAS        YECDVI
084900990917     C                   MOVEL     §TADIV        YECDVO
085000990917     C                   Z-ADD     TASQFT        YECIMI
085100991014     C   12              MOVE      '3'           YECDCO
085200991014     C  N12              MOVE      '0'           YECDCO
085300990917     C*
085400990917     C                   CALL      'YEURCO'
085500990917     C                   PARM                    YEUR
085600990917     C*
085700990917     C     YECESI        IFEQ      ' '
085800990917     C                   Z-ADD     YECIMO        TASQFT
085900990917     C                   END
086000990923     C****                 ENDIF
086100990917     C****       K         OCUR DSCV
086200990917     C****       TASVAS    IFNE §CVITA
086300990917     C****       TASQFT    MULT §CVCMB    TASQFT
086400990917     C****                 END
086500950119     C* CAMBIO
086600950210     C     TASQFT        IFEQ      0
086700950119     C                   EXSR      CALVAS
086800950210     C                   Z-ADD     IMPVAS        TASQFT
086900950119     C                   END
087000950210     C     TASQFT        COMP      0                                      94
087100980527     C   94              MOVEL     MSG(6)        TASMSG
087200950119     C   94              GOTO      FINE
087300950119     C** TARIFFA A VALORE FLAG 1 E 3 - NON ESISTE VALORE ASSICURATIVO
087400950119     C                   END
087500950119     C     POIQTF        TAG
087600980518     C* MI SERVE MINTAS PER IL CARICAMENTO DELLO SCAGLIONE DEL
087700980518     C*  MINIMO TASSABILE
087800980518     C                   Z-ADD     1             A
087900980518     C                   MOVEL     UNOCTR        WTPG              1
088000980518     C     WTPG          LOOKUP    CTR(A)                                 05
088100980518     C   05A             OCCUR     DSTR
088200980518     C   05              Z-ADD     §TRATA        MINTAS
088300990722     C*
088400990722     C** RECUPERO CODICE NAZIONE MITTENTE E DESTINATARIO DAL CODIEC TASSAZIONE
088500990722     C*
088600990923     C***                  Z-ADD1         A
088700990923     C***        TASCTS    LOKUPCTS,A                    05
088800990923     C***05      A         OCUR DSCT
088900990923     C***05                MOVEL§CTNAR    TASNAD  3
089000990722     C*
089100990923     C***                  Z-ADD1         A
089200990923     C***        TASMCT    LOKUPCTS,A                    05
089300990923     C***05      A         OCUR DSCT
089400990923     C***05                MOVEL§CTNAR    TASNAM  3
089500980518     C*
089600990721     C** RICERCA TITAD
089700980518     C                   EXSR      CARTAR
089800980518     C*
089900980313     C** PESO TASSABILE E QUANTITA' DA FATTURARE
090000980429     C                   MOVEL     UNOCTR        WTPG              1
090100980312     C                   CLEAR                   WRPV
090200040603     C                   CLEAR                   RAPPV
090300980312     C                   MOVEL     'TAD'         WDET              3
090400980313     C                   Z-ADD     TAMARL        WARL
090500980313     C                   Z-ADD     TAMARF        WARF
090600980313     C                   Z-ADD     TAMARO        WARO
090700980312     C                   EXSR      CALCOL
090800980313     C                   Z-ADD     ISOPES        PESTAS
090900941112     C*--------------------------------------------------------------
091000980313     C*  PESO TASSABILE PER TARIFFE A PESO
091100980313     C                   EXSR      CALPT
091200941112     C*--------------------------------------------------------------
091300941110     C     TASSAT        TAG
091400990922     C** ESEGUE TASSAZIONE BOLLA
091500941110     C                   EXSR      TASSA
091600990922     C** MANCA TARIFFA
091700941110     C   94              GOTO      FINE
091800941110     C                   ENDSR
091900941112     C******************************************************
092000941112     C** GUIDA PER TASSAZIONE SPEDIZIONE
092100941112     C******************************************************
092200941110     C     TASSA         BEGSR
092300941110     C                   SETOFF                                       94
092400090923     C                   clear                   WvariaMag
092500090924     c                   clear                   WImpoMag
092600090923
092700970522     C** FLAG PER APPLICAZIONE INOLTRO
092800001009     C*!!* PRIMA SI FACEVA COSÌ *!!*
092900001009     C*!!* ** ASSEGNATI-MITTENTE FRANCHI-DESTINATARIO
093000001009     C*!!* ** PER 99 SEMPRE DESTINATARIO
093100001009     C*!!*           TIPBOL    COMP 'A'                      57
093200001009     C*!!*   57      KSCCOD    COMP 9999                 5757
093300001009     C*!!*   57                MOVELTASFAP    FLGINO  1
093400001009     C*!!*  N57                MOVELTASFIN    FLGINO
093500001009     C*!!*
093600001009     C*!!* ORA CONTROLLO LA TARIFFA :
093700001009     C*!!* SE E' USO LA TARIFFA REVERSIBILE (56 ON)  PRENDO COME INOLTRO L'ANTEPORTO TASFAP
093800001009     C*!!* SE USO LA TARIFFA NORMALE (56 OFF) USO L'INOLTRO TASFIN
093900001009     C*!!*
094000001009     C  N56              MOVEL     TASFIN        FLGINO            1
094100001009     C   56              MOVEL     TASFAP        FLGINO
094200001009     C*!!*
094300001009     C*!!*   FINE   *!!*
094400970522     C**
094500980506     C** SE DEVO CALCOLARE UNA VARIA VADO SUBITO DA QUELLA
094600980506     C     TASSVA        IFNE      '  '
094700990916     C     *LIKE         DEFINE    TASQFT        WQFT
094800980506     C** VARIA 'G' --> CONTRASSEGNO
094900980506     C     TASSVA        IFEQ      §$2VRG
095000980506     C                   GOTO      VARIAG
095100980506     C                   ENDIF
095200980525     C** VARIA 'R' --> ASSICURAZIONE PER CONTO
095300980526     C     TASSVA        IFEQ      §$2VRR
095400980525     C                   GOTO      VARIAR
095500980525     C                   ENDIF
095600020412     C** VARIA 'O' --> RICERCA DOCUMENTI
095700020412     C     TASSVA        IFEQ      §$2VRO
095800020412     C                   GOTO      VARIAO
095900020412     C                   ENDIF
096000020516     C** VARIA 'Y' --> RITIRI ANNULLATI
096100020516     C     TASSVA        IFEQ      §$2VRY
096200020516     C                   GOTO      VARIAY
096300020516     C                   ENDIF
096400020516     C** VARIA '*' --> DIROTTAMENTI
096500020523     C     TASSVA        IFEQ      §$2AST
096600020523     C                   GOTO      VARAST
096700020516     C                   ENDIF
096800030403     C** VARIA 'W' --> RECAPITO C/ASSEGNI
096900030403     C     TASSVA        IFEQ      §$2VRW
097000030403     C                   GOTO      VARVRW
097100030403     C                   ENDIF
097200030527     C** VARIA 'M' --> ADDEBITO INSOLUTI INTERESSI DI MORA
097300030527     C     TASSVA        IFEQ      §$2VRM
097400030527     C                   GOTO      VARVRM
097500030527     C                   ENDIF
097600030626     C** VARIA 'T' --> ORM COMMISSIONATO
097700030626     C     TASSVA        IFEQ      §$2VRT
097800030626     C                   GOTO      VARVRT
097900030626     C                   ENDIF
098000040910     C** VARIA 'a' --> P.O.D IMMAGE
098100040910     C     TASSVA        IFEQ      §$2POD
098200040910     C                   GOTO      VARPOD
098300040910     C                   ENDIF
098400030604     C** VARIA 'D' --> DIRITTO DI FATTURAZIONE
098500030604     C     TASSVA        IFEQ      §$2VRD
098600030922     C                   GOTO      FINE
098700030604     C                   ENDIF
098800040113     C** VARIA 'Z' --> ADDIZIONALE DI GESTIONE + ISTAT
098900040113     C     TASSVA        IFEQ      §$2VRZ
099000040113     C                   GOTO      FINE
099100040113     C                   ENDIF
099200980506     C                   ENDIF
099300970522     C**
099400941123     C** SE IL PORTO O L'INOLTRO SONO PRETASSATI
099500941110     C** DEBBO ESCLUDERE LA FASE DI CALCOLO TARIFFA
099600090924     C     TASPOR        CABGT     0             DOPOIN
099700090924     C     TASINL        CABGT     0             DOPOIN
099800980508     C*****
099900980508     C** CALCOLO    P O R T O     ED    I N O L T R O
100000980508     C*****
100100980506     C**
100200020218     C                   DO        200           A
100300941110     C     A             OCCUR     TARDS
100400941110     C     TSCG          IFGE      PESTAS
100500941110     C                   GOTO      ENDTAS
100600941110     C                   END
100700941110     C                   END
100800980508     C** NON TROVATO SCAGLIONE ESATTO
100900941110     C     TSCG          IFLT      PESTAS
101000941110     C                   SETON                                        94
101100980512     C                   MOVEL     MSG(5)        TASMSG
101200980512     C                   GOTO      FINE
101300941110     C                   END
101400941110     C     ENDTAS        TAG
101500980512     C** MI SALVO LA POSIZIONE DELLO SCAGLIO TROVATO CHE MI
101600980512     C*  SERVE PER IL CALCOLO TARIFFA CON GARANTITO MAX SCAGLIONE PREC
101700980512     C     *LIKE         DEFINE    A             WA
101800980512     C                   Z-ADD     A             WA
101900980508     C**
102000980508     C* CALCOLO PER QUALE VALORE MOLTIPLICARE LA TARIFFA
102100980508     C                   Z-ADD     PESTAS        WPTAS3
102200980508     C                   EXSR      CALQLI
102300980508     C**
102400980508     C* MODO APPLICAZIONE TARIFFA
102500980508     C                   MOVEL     TAMFLO        DSTA01
102600990927     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
102700990927     C     §TADIV        IFEQ      *BLANK
102800990927     C                   MOVE      'ITL'         §TADIV
102900990927     C                   ENDIF
103000980512     C**
103100980512     C* CALCOLO NORMALE ANCHE PER SCALARE AL DI SOTTO
103200980512     C*  DELL'APPLICAZIONE TARIFFA FINITA E PER MAX SCAGLIONE PREC
103300980512     C     §TAF02        IFNE      'S'
103400980512     C     §TAF02        OREQ      'S'
103500980512     C     WATA          ANDNE     ' '
103600980508     C** MOLTIPLICAZIONE TARIFFA
103700980508     C                   EXSR      MULTAR
103800980515     C* APPLICAZIONE MINIMO E MASSIMO
103900980515     C                   EXSR      MINMAX
104000980508     C                   Z-ADD     WPOR          TASPOR
104100980508     C                   Z-ADD     WINL          TASINL
104200980514     C                   ENDIF
104300980508     C**
104400980508     C** S C A L A R E
104500980512     C     §TAF02        IFEQ      'S'
104600980508     C                   EXSR      SCALAR
104700980512     C                   ENDIF
104800980508     C**
104900980508     C** M A X   S C A G L I O N E   P R E C E D E N TE
105000980512     C     §TAF02        IFEQ      'G'
105100980512     C                   EXSR      MAXSCA
105200980512     C                   ENDIF
105300990923     C**
105400990923     C** SE DIVISA DELLA TARIFFA NON ACCETTA I DECIMALI PREPARO IL
105500990923     C** DEL PORTO E DELL'INOLTRO SENZA DECIMALI
105600990923     C     *IN12         IFEQ      '0'
105700990923     C* PORTO
105800991112     C     TASPOR        ADD       0,5           CAMPOW           11 0
105900990923     C                   Z-ADD     CAMPOW        TASPOR
106000990923     C* INOLTRO
106100991112     C     TASINL        ADD       0,5           CAMPOW
106200990923     C                   Z-ADD     CAMPOW        TASINL
106300990923     C                   ENDIF
106400090924     c                   eval      WImpoMag=taspor+tasinl
106500980508     C**
106600970521     C*******************************
106700990921     C****************
106800990921     C** TIPO SERVIZIO
106900990921     C****************
107000001127     C*
107100090924     C**                 Z-ADD     1             A
107200090924     C**                 clear                   ESP
107300001127     C*
107400001127     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
107500001127     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
107600001127     C*
107700090924     C**   KSDIT         IFEQ      'SDI'
107800090924     C**   TASDSP        ANDLE     20001231
107900090924     C**   TASTSP        ANDEQ     'C'
108000090924     C**   'E'           LOOKUP    TS(A)                                  05
108100090924     C**                 ELSE
108200090924     C**   TASTSP        LOOKUP    TS(A)                                  05
108300090924     C**                 ENDIF
108400001127     C*
108500090924     C**N05              GOTO      DOPOIN
108600090924     C**   A             OCCUR     DS5E
108700090924     C**   §5EFTC        CABEQ     *BLANKS       DOPOIN
108800090922
108900980430     C** TIPO SERVIZIO SENZA TARIFFA PARTICOLARE
109000990923     C** PORTO
109100090924     C**                 Z-ADD     0             WTSP
109200090924    1C**   TASPOR        IFGT      0
109300090924     c**                 exsr      CerESPRESSO
109400090924     c**
109500060414     c* Se c'e' già la varia, non la calcolo
109600090924    2c**                 if        giaespr<>'S'
109700090924     C**                 Z-ADD     TASPOR        IMPON
109800090924     c**                 if        *in56
109900090924     c**                 eval      applicarevers='S'
110000090924     c**                 endif
110100090924     C**                 EXSR      CALTS
110200090924     c**                 clear                   applicarevers
110300090924    3C**   WTSP          IFGT      0
110400060414     C*****              ADD       WTSP          TASPOR
110500060414     c
110600090924    4c**                 if        esp<>999
110700090924     C**                 MOVEL     wVariaMag     SV(ESP)
110800090924     C**                 add       wtsp          VA(ESP)
110900090924     c**                 else
111000060414     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
111100090924     C**                 ADD       wtsp          TASPOR
111200090924    4c**                 endif
111300090924     c**
111400090924    3C**                 ENDif
111500090924    2C**                 ENDif
111600090924    1C**                 ENDif
111700990923     C** INOLTRO
111800090924     C**                 Z-ADD     0             WTSP
111900090924    1C**   TASINL        IFGT      0
112000090924     c**                 EXSR      CerESPRESSO
112100090924    2c**                 if        giaespr<>'S'
112200090924     C**                 Z-ADD     TASINL        IMPON
112300090924     c**                 if        *in56
112400090924     c**                 eval      applicarevers='S'
112500090924     c*+                 endif
112600090924     C**                 EXSR      CALTS
112700090924     c**                 clear                   applicarevers
112800090924    3C**   WTSP          IFGT      0
112900060414     C*****              ADD       WTSP          TASINL
113000090924    4c**                 if        esp<>999
113100090924     C**                 MOVEL     wVariaMag     SV(ESP)
113200090924     C**                 add       wtsp          VA(ESP)
113300090924   x4c**                 else
113400060414     C** SE NON CI SONO VARIE LIBERE AGGIUNGO ALla voce
113500090924     C**                 ADD       WTSP          TASINL
113600090924    4c**                 endif
113700090924    3C**                 END
113800090924    2C**                 END
113900090924    1C**                 END
114000090924     c
114100991012     C     DOPOIN        TAG
114200991012     C**
114300990923     C* VALORIZZO LA VARIA DELL'INOLTRO NELLA SCHIERA DELLE VARIE
114400991012     C     TASINL        IFGT      0
114500990923     C                   Z-ADD     1             X                 2 0
114600990923     C     §$2FIN        LOOKUP    SV(X)                                  30
114700990923     C* SE NON ESISTE CARICOLA SCHIERA
114800990923     C     *IN30         IFEQ      '0'
114900990923     C                   Z-ADD     1             X
115000990923     C     ' '           LOOKUP    SV(X)                                  30
115100990923     C   30              MOVEA     §$2FIN        SV(X)
115200990923     C                   ENDIF
115300990923     C* SE LO TROVO VALORIZZO TASINL
115400990923     C   30              Z-ADD     TASINL        VA(X)
115500991012     C                   END
115600990921     C****************************************
115700990921     C** ISOLA  LOCALITA' DISAGIATE C.STORICI
115800990921     C****************************************
115900970521     C** SE TROVATA TARIFFA DEL PAESE APPLICO LO STESSO L'ISOLA
116000970521     C**
116100970721    1C     FLGINO        IFEQ      'I'
116200970521     C     FLGINO        OREQ      'D'
116300110624      * sostituisco il controllo del centro storico con i nuovi flag T o Z
116400110624      * T = centro storico in città
116500110624      * Z = centro storico in provincia
116600110624     C**   FLGINO        OREQ      'S'
116700110624     C     FLGINO        OREQ      'T'
116800110624     C     FLGINO        OREQ      'Z'
116900970521     C* IMPOSTO LA TARIFFA PARTICOLARE
117000970521     C                   SELECT
117100970521     C     FLGINO        WHENEQ    'I'                                          ISOLA
117200970521     C                   MOVEL     'I'           TASTC
117300970521     C     FLGINO        WHENEQ    'D'                                          LOCALITA' DI
117400970521     C                   MOVEL     'K'           TASTC
117500110624      * sostituisco il controllo del centro storico con i nuovi flag T o Z
117600110624      * T = centro storico in città
117700110624      * Z = centro storico in provincia
117800110624     C**   FLGINO        WHENEQ    'S'                                          C.STORICO
117900110624     C     FLGINO        WHENEQ    'T'                                          C.STORICO
118000970521     C                   MOVEL     'Q'           TASTC
118100110624     C     FLGINO        WHENEQ    'Z'                                          C.STORICO
118200110624     C                   MOVEL     'Q'           TASTC
118300970521     C                   ENDSL
118400110624      ** visto che l'attivazione del calcolo del centro storico dovrebbe essere
118500110909      ** il 3/10/11  attivo il flag "Q" in tastc confrontando la dta spedizione
118600110909     c                   if        tasdsp < 20111003 and tastc = 'Q'
118700110630     c                   clear                   tastc
118800110630     c                   endif
118900110628      ** Se TASTC è valorizzato calcolo la varia. Questa specifica si può togliere
119000110628      ** quando abilitiamo il centro storico
119100110628 bis1c                   If        tastc <> ' '
119200110628     c
119300970521     C**
119400060414     c* 14/4/06-ES--> il pgm per tassare questa tar.particolare usa
119500060414     c*           sempre il cod.tassazione mittente anche se non
119600060414     c*           applica la reversibilità. Da sempre lo fa.
119700060414     c*           Abbiamo valutato che è sbagliato ma deve seguire
119800060414     c*           la regola della reversibilità. Per cui non lo devo
119900060414     c*           più fare per tipbol='A' ma per 56 ON
120000060414     C**   TIPBOL        IFEQ      'A'
120100060414     C                   IF        *in56
120200981008     C                   MOVEL     TASCTS        WCTS              2
120300941123     C                   MOVEL     TASMCT        TASCTS
120400060414     C                   ENDif
120500970521     C*
120600970521     C                   EXSR      TASPAR
120700060414     C**   TIPBOL        IFEQ      'A'
120800060414     C                   IF        *in56
120900970721     C                   MOVEL     WCTS          TASCTS
121000970721     C                   END
121100970721     C**
121200970721     C* SOLO SE L'HO CALCOLATA FACCIO LE COSE SOTTOSTANTI
121300970721    2C     WNOEST        IFEQ      'S'
121400970721     C     TASCP         ANDGT     0
121500970522     C* MEMORIZZO PORTO E POSIZIONE VARIA DOVE HO MEMORIZZATO
121600970522     C                   Z-ADD     TASCP         TASISO
121700090924     c* imponibile per maggiorazione "E" o "H"
121800090924     c                   eval      wImpoMag=wImpoMag+TASISO
121900970522     C*
122000970522     C  N96              MOVEL     'S'           WPORTO            1
122100970522     C* SALVO L'INDICE DELLA SCHIERA DELLA VARIA
122200970522     C   96              Z-ADD     A             ISO               2 0
122300970522     C   96              MOVEL     ' '           WPORTO            1
122400970521     C*
122500970521     C** SE VA IN SOSTITUZIONE CLEARO L'INOLTRO
122600970521     C** FLAG=S INOLTRO+ISOLA
122700970521     C** FLAG=N SOLO ISOLA
122800110628     c** In caso di centro storico  non considero il flag FLGISO in quanto non
122900110628     c** non deve andare in sostituzione dell'inoltro ed in tariffa non viene
123000110628     c** richiesta la valorizzazione quindi è sempre diverso da "S"
123100970721    3C     FLGISO        IFNE      'S'
123200110628     c     TASTC         ANDNE     'Q'
123300090924     c* tolgo dall'imponibile per la maggiornazione l'Inoltro
123400090924     c                   eval      wImpoMag=wImpoMag-TASINL
123500090924     c*
123600090924     C                   CLEAR                   TASINL
123700990921     C** RICHIAMO LA VECCHIA POSIZIONE DELLA VARIA E LA SOSTIUISCO CON IL
123800990921     C**  VALORE DELL'ISOLA
123900990921     C** L'INDICE "X" PUNTA ALLA VARIA DELL'INOLTRO
124000990921     C     X             IFGT      *ZERO
124100990921     C* PULISCO L'INOLTRO
124200990921     C                   CLEAR                   VA(X)
124300990921     C                   CLEAR                   SV(X)
124400990921     C* UTILIZZO LA POSIZIONE VUOTA DELLA SCHIERA PER METTERE L'IMPORTO DELLA
124500990921     C* VARIA DELL'ISOLA
124600990921     C                   MOVEL     §1PVAR        SV(X)
124700990921     C                   Z-ADD     TASCP         VA(X)
124800990921     C* PULISCO LA POSIZIONE DELLA SCHIERA PRECEDENTE
124900990921     C   96              CLEAR                   VA(A)
125000990921     C   96              CLEAR                   SV(A)
125100990921     C* SALVO L'INDICE ATTUALE DELLA SCHIERA DELLA VARIA
125200990921     C   96              Z-ADD     X             ISO
125300990922     C                   END
125400970721    3C                   END
125500990921     C***********************
125600990921     C** IS/L.D./C.S  - TIPO SERVIZIO
125700990921     C***********************
125800090924     C**                 Z-ADD     1             A                 5 0
125900001127     C*
126000001127     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
126100001127     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
126200001127     C*
126300090924     C**   KSDIT         IFEQ      'SDI'
126400090924     C**   TASDSP        ANDLE     20001231
126500090924     C**   TASTSP        ANDEQ     'C'
126600090924     C**   'E'           LOOKUP    TS(A)                                  05
126700090924     C**                 ELSE
126800090924     C**   TASTSP        LOOKUP    TS(A)                                  05
126900090924     C*+                 ENDIF
127000001127     C*
127100090924     C** 05A             OCCUR     DS5E
127200090924    3C** 05§5EFTC        IFNE      *BLANKS
127300970521     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
127400090924     c**                 EXSR      CerESPRESSO
127500090924     c**
127600090924   3ac**                 if        giaespr<>'S'
127700090924     C**                 Z-ADD     0             WTSP
127800090924     C**                 Z-ADD     TASISO        IMPON
127900060414     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
128000090924     C**                 IF        *in56
128100090924     c**                 eval      applicarevers='S'
128200090924     c**                 endif
128300090924     C**                 EXSR      CALTS
128400090924     c**                 clear                   applicarevers
128500060414     c
128600090924    4C**   WTSP          IFGT      0
128700090924    5c**                 if        esp<>999
128800090924     C**                 MOVEL     WVariaMag     SV(ESP)
128900090924     C**                 add       wtsp          VA(ESP)
129000090924   x5c**                 else
129100060414     c* Se non c'e' una varia disponibile, sommo al posto o alla
129200060414     c*  varia dell' isola
129300090924    6C**   WPORTO        IFEQ      'S'
129400090924     C**                 ADD       WTSP          TASPOR
129500090924   x6C**                 ELSE
129600090924     C**                 ADD       WTSP          TASISO
129700090924     C**                 Z-ADD     TASISO        VA(ISO)
129800090924    6C**                 END
129900090924    5C**                 END
130000090924    4C**                 END
130100060414     c
130200090924   3aC**                 ENDif
130300090924    3C**                 END
130400090924     c
130500970721    2C                   END
130600110628 bis1C                   ENDIF
130700970721    1C                   ENDIF
130800090924     c
130900090924     C***********************
131000090924     C** Maggiorazione per tipo servizio sul totale imponibile da maggiorare
131100110628     c** ovvero:  PORTO + INOLTRO + ISOLA/DISAGIATE/CENTRO STORICO
131200110628     C** Se porto o inoltro già presenti: solo su ISOLA/DISAGIATE/CENTRO STORICO
131300110628     c** Se isola/disagiate/centro storico già presente: solo su PORTO + INOLTRO
131400090924     C***********************
131500090924     C                   Z-ADD     1             A                 5 0
131600090924     C                   clear                   ESP
131700090924     c
131800090924    1c                   if        WimpoMag>0
131900090924     C*
132000090924     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
132100090924     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
132200090924     C*
132300090924    2C     KSDIT         IFEQ      'SDI'
132400090924     C     TASDSP        ANDLE     20001231
132500090924     C     TASTSP        ANDEQ     'C'
132600090924     C     'E'           LOOKUP    TS(A)                                  05
132700090924     C                   ELSE
132800090924     C     TASTSP        LOOKUP    TS(A)                                  05
132900090924    2C                   ENDIF
133000090924     C*
133100090924     C   05A             OCCUR     DS5E
133200090924    2C   05§5EFTC        IFNE      *BLANKS
133300090924     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
133400090924     c                   EXSR      CerESPRESSO
133500090924     c
133600090924    3c                   if        giaespr<>'S'
133700090924     C                   Z-ADD     0             WTSP
133800090924     c
133900090924     C                   Z-ADD     WImpoMag      IMPON
134000090924     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
134100090924     C                   IF        *in56
134200090924     c                   eval      applicarevers='S'
134300090924     c                   endif
134400090924     C                   EXSR      CALTS
134500090924     c                   clear                   applicarevers
134600090924     c
134700090924    4C     WTSP          IFGT      0
134800090924    5c                   if        esp<>999
134900090924     C                   MOVEL     WVariaMag     SV(ESP)
135000090924     C                   add       wtsp          VA(ESP)
135100090924   x5c                   else
135200090924     c* Se non c'e' una varia disponibile, sommo nel porto
135300090924     C                   ADD       WTSP          TASPOR
135400090924    5C                   END
135500090924    4C                   END
135600090924     c
135700090924    3C                   ENDif
135800090924    2C                   END
135900090924    1C                   END
136000990921     C****************
136100990921     C** DIRITTO FISSO
136200990921     C****************
136300980424     C     TASDIF        IFEQ      0
136400980424     C                   MOVEL     'H'           TASTC
136500990921     C* RICHIAMO LA ROUTINE DI CALCOLO DELLE TARIFFE PARTICOLARI
136600990921     C                   EXSR      TASPAR
136700990921     C   96              Z-ADD     TASCP         TASDIF
136800990921     C***
136900980424     C                   ENDIF
137000990921     C***********************
137100990921     C** PROVVIGIONE ASSEGNO
137200990921     C***********************
137300980506     C     VARIAG        TAG
137400980506     C     *LIKE         DEFINE    TASVA1        PROVAS
137500980506     C                   Z-ADD     0             PROVAS
137600980429     C** MANCA C/A
137700941112     C     TASCAS        CABEQ     0             POIT01
137800980429     C** PROVV. ASSEGNO GIA' PRESENTE
137900941112     C                   Z-ADD     1             A
138000941112     C     §$2VRG        LOOKUP    SV(A)                                  05
138100980429    1C  N05              DO
138200941116     C*
138300941116     C                   SETOFF                                       94
138400941116     C     *LIKE         DEFINE    TASCAS        CASLIT
138500941116     C                   Z-ADD     TASCAS        CASLIT
138600980429     C**
138700990921    2C*!*        TASCMB    IFEQ 0
138800990921     C*!*                  Z-ADD1         K
138900990921     C*!*        TASVCA    LOKUPCV,K                     05
139000990921    3C*!*        *IN05     IFEQ *OFF
139100990921     C*!*                  SETON                     94
139200990921     C*!*                  MOVELMSG,1     TASMSG
139300990921     C*!*                  GOTO FINE
139400990921    3C*!*                  ENDIF
139500941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
139600990921     C*!*        K         OCUR DSCV
139700990921    3C*!*        TASVCA    IFNE §CVITA
139800990921     C*!*        TASCAS    MULT §CVCMB    CASLIT    H
139900941116     C** CAMBIO IN BASE ALLA DIVISA STANDARD
140000990921    3C*!*                  END
140100990921   X2C*!*                  ELSE
140200990921     C*!*        TASCAS    MULT TASCMB    CASLIT    H
140300941116     C** CAMBIO IN BASE AL VALORE ESISTENTE IN BOLLA
140400990921    2C*!*                  END
140500990921     C* CALCOLO IL VALORE DEL CONTRASSEGNO  IN BASE ALLA MONETA
140600020319     C*******
140700020319     C*******
140800020319     C* SE PRESENTE IL C/A ANNULLATO VEDO IN BASE ALLA TARIFFA CLIENTE
140900020319     C*  SE FATTURARE O MENO
141000020319     C*  §TAPCA='N' NON FATTURO GLI ANNULLATI
141100030131     C**!!!§ASSTA        IFEQ      '9'
141200030131     C     TASSTA        IFEQ      '9'
141300030131     C     §TAPCA        ANDEQ     'N'
141400060922      * calcolo sempre la provvigione del C/A  annullato se si tratta di consegna anomala dirottata
141500060922     C     §ASCCA        ANDNE     '1'
141600060922      *
141700020319     C                   GOTO      POIT01
141800020319     C                   ENDIF
141900020319     C**
142000990921     C     TASVCA        IFNE      §TADIV
142100990921     C                   CLEAR                   YEUR
142200990921     C                   MOVEL     TASVCA        YECDVI
142300990921     C                   MOVEL     §TADIV        YECDVO
142400990921     C                   Z-ADD     TASCAS        YECIMI
142500991014     C   12              MOVE      '3'           YECDCO
142600991014     C  N12              MOVE      '0'           YECDCO
142700990921     C*
142800990921     C                   CALL      'YEURCO'
142900990921     C                   PARM                    YEUR
143000990921     C*
143100990921     C     YECESI        IFEQ      ' '
143200990921     C                   Z-ADD     YECIMO        CASLIT
143300990921     C                   ELSE
143400990921     C                   SETON                                        94
143500990921     C                   MOVEL     MSG(1)        TASMSG
143600990921     C                   GOTO      FINE
143700990923     C                   END
143800990921     C                   ENDIF
143900980429     C* PROVV ASSEGNO MITTENTE
144000980429    2C     TASBM         IFEQ      'M'
144100980429     C                   MOVEL     'W'           TASTC
144200980429     C                   ELSE
144300980429     C* PROVV ASSEGNO VETTORE
144400980429     C                   MOVEL     'V'           TASTC
144500980429    2C                   ENDIF
144600060712      * se spedizione EEX cerco sempre provvigione assegno mittente
144700060712     c                   If        o27fie = 'E'
144800060712     c                   movel     'W'           tastc
144900060712     c                   endif
145000980429     C**
145100980429     C                   MOVEL     TASTC         FISO
145200980506     C                   MOVEL     'S'           WCAR
145300980429     C                   EXSR      CERTPT
145400980429    2C     WEND          IFEQ      ' '
145500980429     C**
145600980429    3C     TPTTPG        IFEQ      'V'                                          VALORE
145700980429     C                   Z-ADD     CASLIT        TPDPES
145800980429     C                   ELSE                                                   ALTRO
145900980429     C                   Z-ADD     ISOPES        TPDPES
146000980429    3C                   ENDIF
146100980429     C**
146200980429     C                   EXSR      CALACD
146300941116     C*
146400980429    3C     VARIA         IFGT      0
146500941112     C                   Z-ADD     1             A
146600941112     C     ' '           LOOKUP    SV(A)                                  96
146700941112     C   96              MOVEL     §$2VRG        SV(A)
146800980429     C   96              Z-ADD     VARIA         VA(A)
146900980429     C  N96              ADD       VARIA         TASPOR
147000980429     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
147100980429    3C                   ENDIF
147200980429    2C                   ENDIF
147300980429    1C                   ENDDO
147400980506     C**
147500980506     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
147600980506     C     TASSVA        IFEQ      §$2VRG
147700980506     C                   GOTO      FINE
147800980506     C                   ENDIF
147900990921     C***********************
148000990921     C** DIRITTO ASSICURATIVO
148100990921     C***********************
148200941110     C     POIT01        TAG
148300941112     C                   Z-ADD     1             A
148400941112     C     §$2VRE        LOOKUP    SV(A)                                  05
148500941112     C  N05              DO
148600941112     C                   Z-ADD     0             DIRITT
148700941112     C     *LIKE         DEFINE    TASVA1        DIRITT
148800941110     C                   EXSR      CALVE
148900941112     C     DIRITT        IFGT      0
149000941112     C                   Z-ADD     1             A
149100941112     C     ' '           LOOKUP    SV(A)                                  96
149200941112     C   96              MOVEL     §$2VRE        SV(A)
149300941112     C   96              Z-ADD     DIRITT        VA(A)
149400941112     C  N96              ADD       DIRITT        TASPOR
149500941112     C                   END
149600941112     C                   END
149700941112     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
149800990921     C***********************
149900990921     C** ASS. X CONTO
150000990921     C***********************
150100980525     C     VARIAR        TAG
150200980525     C                   Z-ADD     0             WVARIR
150300060322     C                   Z-ADD     0             WVARIACB
150400980525     C     *LIKE         DEFINE    TASVA1        WVARIR
150500060321     C     *LIKE         DEFINE    TASVA1        WVARIACB
150600060320      * cerco la varia R sia in caso di bolla con importo d'assicurare e sia in caso di
150700060320      * ricerca di mandato
150800941112     C                   EXSR      CALVR
150900941112     C** MANCA TARIFFA:MANDATO VALORE = 0 E TASIAS=0
151000980526     C   94              GOTO      FINE
151100980526     C**
151200980525     C     WVARIR        IFGT      0
151300941112     C                   Z-ADD     1             A
151400941112     C     ' '           LOOKUP    SV(A)                                  96
151500941112     C   96              MOVEL     §$2VRR        SV(A)
151600980525     C   96              Z-ADD     WVARIR        VA(A)
151700060320      ** se non ci sono varie libere aggiungo al porto
151800980525     C  N96              ADD       WVARIR        TASPOR
151900060320
152000060320      * se varia uguale a zero e non ho trovato nessun mandato e non c'è importo d'assicurare in
152100060320      * bolla cerco la nuova AC base
152200060320
152300060320     c                   else
152400060320
152500060321     c                   If        task88 = 'X' and
152600060321      ** 88 = non si calcola
152700060322     c                             (Ksccod <> 8888 and Ksccod <> 9999) and
152800060322      ** data spedizione maggiore del 28 02 2006
152900060322     c                             tasdsp > 20060228
153000060320     c                   exsr      CALVACB
153100060320      ** carico la varia nella schiera
153200060321     c                   If        wvariacb > 0
153300060320     c                   z-add     1             a
153400060320     c     ' '           Lookup    sv(a)                                  96
153500060320     c   96              movel     §$2Acb        sv(a)
153600060321     c   96              z-add     Wvariacb      va(a)
153700060320      ** se non ci sono varie libere aggiungo al porto
153800060321     c  N96              add       Wvariacb      TASpor
153900060320     c                   endif
154000060320
154100060320     c                   endif
154200060320
154300060320     c                   END
154400980525     C**
154500980525     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
154600980525     C     TASSVA        IFEQ      §$2VRR
154700980525     C                   GOTO      FINE
154800980525     C                   ENDIF
154900990722     C***************
155000990722     C* T.C.P.
155100990722     C***************
155200990722     C*
155300990722     C                   MOVEL     §$2VRP        TASTC
155400990722     C                   EXSR      TASPAR
155500030624     C***************************************
155600030624     C* ANNULLAMENTO PORTO ASSEGNATO VARIA &
155700030624     C***************************************
155800030624     C*
155900030624     C* SE IL FLAG DI CALCOLO VARIA & È UGUALE A S
156000030624     C                   IF        TASFCAA = 'S'
156100030624     C                   MOVEL     §$2VPA        TASTC
156200030624     C                   EXSR      TASPAR
156300030624     C                   ENDIF
156400050928     C****************************
156500050928     C* LASCIATO AVVISO  VARIA 'c'
156600050928     C****************************
156700050929     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
156800050928     C                   If        tasric <> ' '
156900090305      * verifico se linea di arrivo o linea di partenza è estera non calcolo la varia "c"
157000051028     c     taslna        lookup    esttot                                 30
157100090305      * se linea di arrivo non è estero verifico la linea di partenza
157200090305     c  n30taslnp        lookup    esttot                                 30
157300090305      * se spedizione italiana calcolo
157400051028     c                   If        not *in30
157500050928     C                   Movel     §$2vla        Tastc
157600050928     C                   Exsr      TASpar
157700051005     c                   Endif
157800051005
157900050928     c                   Endif
158000110628     C****************************
158100110628     C* AVVISO AL DESTINATARIO AFFIDAMENTO SPEDIZIONE VARIA 'm'
158200110628     C****************************
158300110628     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
158400110628     C* in TASFLO §asemd
158500130920     C                   If        §asemd = 'S' or §asemd = 'X' or
158600130920     c                             §asemd = 'E'
158700110628     C                   Movel     §$2vad        Tastc
158800110628     C                   Exsr      TASpar
158900110628     c                   Endif
159000110628
159100020412     C***************
159200020412     C* RICERCA DOCUMENTI - SOLO SE RICHIESTA DA SOLA
159300020412     C***************
159400020412     C*
159500020412     C     VARIAO        TAG
159600020412     C     TASSVA        IFEQ      §$2VRO
159700020412     C                   MOVEL     §$2VRO        TASTC
159800020412     C                   EXSR      TASPAR
159900020412     C**
160000020412     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
160100020412     C     TASSVA        IFEQ      §$2VRO
160200020412     C                   GOTO      FINE
160300020412     C                   ENDIF
160400020412     C                   ENDIF
160500020516     C***************
160600020516     C* RITIRI ANNULLATI  - SOLO SE RICHIESTA DA SOLA
160700020516     C***************
160800020516     C*
160900020516     C     VARIAY        TAG
161000020516     C     TASSVA        IFEQ      §$2VRY
161100020516     C                   MOVEL     §$2VRY        TASTC
161200020516     C                   EXSR      TASPAR
161300020516     C**
161400020516     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
161500020516     C     TASSVA        IFEQ      §$2VRY
161600020516     C                   GOTO      FINE
161700020516     C                   ENDIF
161800020516     C                   ENDIF
161900020516     C***************
162000020516     C* DIROTTAMENTI      - SOLO SE RICHIESTA DA SOLA
162100020516     C***************
162200020516     C*
162300020523     C     VARAST        TAG
162400020523     C     TASSVA        IFEQ      §$2AST
162500020523     C                   MOVEL     §$2AST        TASTC
162600020516     C                   EXSR      TASPAR
162700020516     C**
162800020516     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
162900020523     C     TASSVA        IFEQ      §$2AST
163000020516     C                   GOTO      FINE
163100020516     C                   ENDIF
163200020516     C                   ENDIF
163300030403     C***************
163400030403     C* RECAPITO C/ASSEGNI- SOLO SE RICHIESTA DA SOLA
163500030403     C***************
163600030403     C*
163700030403     C     VARVRW        TAG
163800030403     C     TASSVA        IFEQ      §$2VRW
163900030403     C                   MOVEL     'G'           TASTC
164000030403     C                   EXSR      TASPAR
164100030403     C**
164200030403     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
164300030403     C     TASSVA        IFEQ      §$2VRW
164400030403     C                   GOTO      FINE
164500030403     C                   ENDIF
164600030403     C                   ENDIF
164700030527     C*
164800030527     C***************
164900030527     C* ADDEBITO INSOLUTI - INT.MORA  SOLO SE RICHIESTA DA SOLA
165000030527     C***************
165100030527     C     VARVRM        TAG
165200030527     C     TASSVA        IFEQ      §$2VRM
165300030527      * SE NON ESISTONO VARIE IMPOSTATE DO MANCA TARIFFA
165400030527     c                   MOVEA     SV            WRKSV            20
165500030527     C                   IF        WRKSV = *BLANKS
165600030527     C                   MOVEL     MSG(18)       TASMSG
165700030527     C                   MOVEL     'E'           TASERR            1
165800030527     C                   SETON                                        94
165900030527     C                   ENDIF
166000030527     C                   GOTO      FINE
166100030527     C                   ENDIF
166200030626     C***************
166300030626     C* ORM COMMISSIONATO  - SOLO SE RICHIESTA DA SOLA
166400030626     C***************
166500030626     C*
166600030626     C     VARVRT        TAG
166700030626     C     TASSVA        IFEQ      §$2VRT
166800030626     C                   MOVEL     §$2VRT        TASTC
166900030626     C                   EXSR      TASPAR
167000030626     C**
167100030626     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
167200030626     C     TASSVA        IFEQ      §$2VRT
167300030626     C                   GOTO      FINE
167400030626     C                   ENDIF
167500030626     C                   ENDIF
167600040910     C***************
167700040910     C* P.O.D IMMAGE      - SOLO SE RICHIESTA DA SOLA
167800040910     C***************
167900040910     C*
168000040910     C     VARPOD        TAG
168100040910     C     TASSVA        IFEQ      §$2POD
168200040910     C                   MOVEL     §$2POD        TASTC
168300040910     C                   EXSR      TASPAR
168400040910     C**
168500040910     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
168600040910     C     TASSVA        IFEQ      §$2POD
168700040910     C                   GOTO      FINE
168800040910     C                   ENDIF
168900040910     C                   ENDIF
169000990922     C************************
169100990922     C* CONSEGNE PARTICOLARI
169200990922     C************************
169300990722     C*
169400990922     C** CONSEGNE PARTICOLARI 1
169500941110     C     TASTC1        IFNE      *BLANKS
169600970521     C                   MOVEL     TASTC1        TASTC
169700970521     C                   EXSR      TASPAR
169800941112     C                   END
169900990922     C** CONSEGNE PARTICOLARI 2
170000941110     C     TASTC2        IFNE      *BLANKS
170100970521     C                   MOVEL     TASTC2        TASTC
170200970521     C                   EXSR      TASPAR
170300941110     C                   END
170400020826     C************************
170500020826     C* DITITTO DI PESATURA
170600020826     C************************
170700020826     C*
170800020826     C     §TATAP        IFEQ      'S'
170900020826     C                   MOVEL     '5'           TASTC
171000020826     C                   EXSR      TASPAR
171100020826     C                   END
171200990922     C************************
171300990922     C* RITIRO
171400990922     C************************
171500050504     C* LA VARIA "U" DI RITIRO  E'
171600050504     C** DA CALCOLARE: SUI PORTI ASSEGNATI
171700050504     C**               SUI PORTI FRANCHI NON CODIFICATI
171800050504     C**                   CON FLAG CONSEGNA A MAGAZZINO = A BLANK
171900050504     C**               SULLE BOLLE EXPORT ASSEGNATO NON DPD
172000050504     C**               SULLE BOLLE IMPORT ASSEGNATO TUTTE (ANCHE DPD)
172100990915     C* FRANCO
172200000919     C*!!*       TIPBOL    IFNE 'A'
172300000919     C*!!*       TASFDN    ANDEQ'S'
172400000919     C*!!*                 GOTO ENDRIT
172500000919     C*!!*                 ENDIF
172600990915     C*
172700990915     C* FRANCO CON CODICE CLIENTE UGUALE A 8888 OPPURE UGUALE ALLA TARIFFA DI
172800990915     C* CARTELLO
172900000919     C     TIPBOL        IFNE      'A'
173000000919     C     KSCCOD        ANDNE     8888
173100020417     C****       TASKSC    ANDNECARKSC
173200020417     C* SE NON SI TRATTA NEMMENO DI UNA TARIFFA DI CARTELLO NON TASSO
173300020417     C     TASKSC        LOOKUP    TAC                                    15
173400020417     C  N15              GOTO      ENDRIT
173500000919     C                   ENDIF
173600990930     C** DA CALCOLARE SOLO PER PORTI ASSEGNATI RITIRATI
173700000919     C*!!*       TIPBOL    CABNE'A'       ENDRIT
173800990930     C*
173900970213     C*
174000990914     C* SE BOLLA ASSEGNATA EXPORT --> NON CONSIDERO IL FLAG CONSEGNA A MAGAZZINO
174100990914     C*                               PERCHE' DEVE SEMPRE CALCOLARE LA VARIA "U"
174200970213     C     TASFDN        IFEQ      'S'
174300020319     C* PER ASSEGNATO EXPORT --> SEMPRE  (ANCHE DPD CHE FINO A OGGI
174400050504     C*                          ESCLUDEVA 4/4/02--> RIESCLUDO DPD)
174500050504     C* PER ASSEGNATO IMPORT --> SEMPRE  (ANCHE DPD 04/05/05)
174600050504     C*
174700001204     C* PER ALTRI ASSEGNATI  --> SOLO SE NON E' PORTATA A MAGAZZINO
174800001204     C     TIPBOL        IFEQ      'A'
174900001204     C*
175000980605     C     TASLNA        LOOKUP    EST                                    05
175100050504     c
175200050504     C  n05TASLNP        LOOKUP    ESTtot                                 05
175300050504     c
175400980605     C  N05              GOTO      ENDRIT
175500001204     C                   ELSE
175600001204     C* PER 8888 --> NON APPLICARE SE PORTATA A MAGAZZINO SIA PER EXP
175700001204     C*              CHE PER LE ALTRE SPEDIZIONI
175800001204     C                   GOTO      ENDRIT
175900970213     C                   ENDIF
176000001204     C                   ENDIF
176100060720      *----------------------------------------------------------------------------*
176200060720      * In caso di bolla di reso non si può NON CALCOLARE la varia U in quanto     *
176300060720      * questa serve per far pagare la tratta int.le della spedizione.             *
176400060720      * Infatti quando in Aprile 2006 è stata tolta la signora Villa ha reclamato. *
176500060720      * asterisco le seguenti specifiche                                           *
176600060720     c*** Per bolla import, non applico varia "U" se bolla di reso                 *
176700060720     C***     TASLNP        LOOKUP    ESTtot                                 05    *
176800060720     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
176900060720     C***                   GOTO      ENDRIT                                       *
177000060720     c***                   endif                                                  *
177100060720     c***                                                                          *
177200060720     c*** Per bolla EXport, non applico varia "U" se bolla di reso                 *
177300060720     C***     TASLNA        LOOKUP    EST                                    05    *
177400060720     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
177500060720     C***                   GOTO      ENDRIT                                       *
177600060720     c***                   endif                                                  *
177700060720      *----------------------------------------------------------------------------*
177800060424     c
177900970213     C**
178000970521     C* TARIFFA PARTICOLARE --> U
178100941121     C                   MOVEL     'U'           TASTC
178200970521     C**
178300110617     C** 17/06/2011 LB DN visto che sono state cambiate le tariffe della cartello
178400110617     c** per la tariffa particolare di ritiro per l'Estero e non essendo + uguale
178500110617     c** a quelle per l'italia in caso di prepagati (quindi non codificati) per la GB
178600110617     c** la tariffa di ritiro diventa di 50 euro circa rispetto i 5 euro circa di prima ?
178700110617     c** quindi risulta necessario correggere un errore che ci portiamo dietro da anni
178800110617     c** che tassiamo controllando il codice tassazione mittente solo per gli assegnati
178900110617     c** invece si deve fare anche per i poveri franchi (prepagati) per i quali viene
179000110617     c** calcolato il ritiro. Quindi sia per i franchi che assegnati il codice tassazione
179100110617     c** per la tariffa particolare di ritiro diventa il codice tassazione mittente
179200110617     c** come è giusto che venga tassata da dove viene ritirata e non dove viene
179300110617     c** consegnata la merce. A.G.
179400110617     C**   TIPBOL        IFEQ      'A'
179500970521     C                   MOVEL     TASCTS        WCTS
179600970521     C                   MOVEL     TASMCT        TASCTS
179700110617     C**                 END
179800970521     C*
179900970521     C                   EXSR      TASPAR
180000970521     C*
180100110617     C**   TIPBOL        IFEQ      'A'
180200970521     C                   MOVEL     WCTS          TASCTS
180300110617     C**                 END
180400941121     C     ENDRIT        TAG
180500990922     C************************
180600990922     C* SI DDT
180700031104      * Tutti i pgm che richiamano il TNSF20R x tassare le bolle arrivi
180800031104      * testano il flag DDT e passano 'S' nel caso di DDT SI, in questi pgm ho aggiunto anche i
180900031104      * nuovi flag 'P' e 'Q'
181000031104      * l'unico che non lo fa è FNLG20R così ho aggiunto anche qua il controllo con i 2 nuovi flag
181100031104      * 'P' è uguale a 'S' e viene impostato in arrivo quando si stampa il ddt si
181200031104      * 'Q' è uguale a 'C' e viene impostato in arrivo quando si stampa il ddt si
181300990922     C************************
181400970409     C     TASDDT        IFEQ      'S'
181500031104     C     TASDDT        orEQ      'P'
181600031104     C     TASDDT        orEQ      'Q'
181700970521     C* TARIFFA PARTICOLARE --> B
181800970521     C                   MOVEL     'B'           TASTC
181900970521     C                   EXSR      TASPAR
182000970409     C                   END
182100030203      ***************
182200030203      * BANCALI
182300030203      ***************
182400030203     C*
182500030203      * Non si tassano i bancali per le bolle in porto assegnato
182600030203     c                   If        TasBan > *Zeros and TipBol <> 'A'
182700030203     c                   Movel     §$2Vba        TASTC
182800030203     c                   ExSr      TasPar
182900030203     c                   EndIf
183000001205     C*
183100040506      ************************************************
183200040506      * SUPPLEMENTO CARBURANTE SPEDIZIONI FEDEX
183300040506      ************************************************
183400040506     C                   IF        WBOFED = 'S'
183500060905     C                   EXSR      SR_SUPCARB
183600060905     C                   ELSE
183700060905      ************************************************
183800060905      * FUEL SURCHARGE  SPEDIZIONI NO FEDEX
183900060905      ************************************************
184000060905     C                   EXSR      SR_FUEL
184100040506     C                   ENDIF
184200040506      *
184300941110     C                   SETOFF                                       49
184400941110     C                   ENDSR
184500060414     C*****************************************************************
184600090922     C** Cerco la posizione per la varia della maggiorazione
184700060414     C*****************************************************************
184800060414     c     CerESPRESSO   BEGSR
184900060414     c                   if        esp=0
185000060414     c                   clear                   giaespr           1
185100090923     c                   clear                   wVariaMag         1
185200060414     C                   Z-ADD     1             A
185300090922
185400090922     c* Maggiorazione per tipo servizio E-priority
185500090922     c                   select
185600090922     c                   when      tastsp='E'
185700090922     c                   eval      wvariaMag=§$2tsp
185800090922
185900090922     c* Maggiorazione per tipo servizio H-H1030
186000090922     c                   when      tastsp='H'
186100090922     c                   eval      wvariaMag=§$2tsh
186200090922     c                   endsl
186300090922
186400090922     C     wvariaMag     LOOKUP    SV(a)                                  96
186500090922     c* gia esiste la varia della maggioraz --> non la calcolo
186600060414     c                   if        *in96
186700060414     c                   z-add     a             ESP               3 0
186800060414     c                   eval      giaespr='S'
186900060414     c                   else
187000060414     C                   Z-ADD     1             A
187100060414     C     ' '           LOOKUP    SV(A)                                  96
187200060414     c   96              z-add     a             ESP               3 0
187300060414     C  n96              ADD       999           ESP
187400060414     c                   endif
187500060414     c                   endif
187600060414     c                   ENDSR
187700980508     C*****************************************************************
187800980508     C** VALORE COL QUALE  MOLTIPLICARE LA TARIFFA
187900980508     C*****************************************************************
188000980508     C     CALQLI        BEGSR
188100991013     C                   Z-ADD     0             PESQLI           14 6
188200980508     C** SE LO SCAGLIONE SCELTO PER TASSARE E' INFERIORE AL MINIMO
188300980508     C** TASSABILE DA TARIFFA FORZO PESO TASSABILE = MINTAS
188400980512     C**
188500980512     C* WATA MI IDENTIFICA SE L'APPLICAZIONE TARIFFA FINITA E' DOVUTA
188600980512     C*  ALLO SCAGLIONE CONFRONTATO CON MINTAR (R)
188700980512     C*  O SE E' DOVUTA DAL PESO TASSABILE CONFRONTATO COL MINIMO
188800980515     C*  TASSABILE DI QUELLA TARIFFA (S) - differenza che ora non serve
188900980515     C*  + ma dal momento che c'e' la lascio
189000980508     C                   CLEAR                   WATA
189100980508    1C                   SELECT
189200980508     C     TSCG          WHENLE    MINTAR
189300980508     C                   Z-ADD     1             PESQLI
189400980514     C                   MOVEL     'R'           WATA              1
189500980508     C**
189600980515     C** SE PESO TASSABILE E' ANCORA INFERIORE
189700980514     C*  AL MINIMO TASSABILE DA TARIFFA IMPOSTO PESO TASS = 1
189800980515     C*  NON LO CONTROLLO PER LA TARIFFA A SCALARE PERCHE' ABBIAMO
189900980515     C*  SEMPRE INSERITO UNO SCAGLIONE=A MINIMO TASSABILE E
190000980515     C*  QUINDI E' GIA' CONTROLLATO NEL WHLE PRECEDENTE
190100980515     C     WPTAS3        WHENLE    MINTAS
190200980515     C     §TAF02        ANDNE     'S'
190300980508     C                   Z-ADD     1             PESQLI
190400980508     C                   MOVEL     'S'           WATA
190500980508   X1C                   OTHER
190600980508     C** CALCOLO : PESO IN Q.LI PER TARIFFA A PESO
190700980508     C** CALCOLO   VALORE/100   PER TARIFFA A VALORE (ESSENDO UNA %)
190800980508     C** PER LE ALTRE TARIFFE PESO TASSABILE INVARIATO
190900980508    2C     UNOCTR        IFEQ      0
191000980508     C     UNOCTR        OREQ      4
191100980508     C     WPTAS3        DIV       100           PESQLI
191200980508   X2C                   ELSE
191300980508     C                   Z-ADD     WPTAS3        PESQLI
191400980508    2C                   END
191500980508    1C                   ENDSL
191600980508     C                   ENDSR
191700980508     C*****************************************************************
191800980508     C* MOLTIPLICATO TARIFFA PER VALORE TROVATO (PESO TASSABILE)
191900980508     C*****************************************************************
192000980508     C     MULTAR        BEGSR
192100980508     C     *LIKE         DEFINE    TASPOR        WPOR
192200980508     C     *LIKE         DEFINE    TASINL        WINL
192300980518     C                   CLEAR                   WPOR
192400980518     C                   CLEAR                   WINL
192500980508     C**
192600980508     C     TITR          MULT(H)   PESQLI        WPOR                           ** PORTO
192700980508     C**
192800980508     C** SE TROVATA TARIFFA DEL PAESE NON SI APPLICA MAI INOLTRO
192900110624     C*****  N49FLGINO        IFNE      'C'
193000110624     C** Sostituisco il controllo del flag inoltro diverso da C=città ma
193100110624     c** anche da "T" Città centro storico
193200110624     C                   IF        NOT *IN49  AND
193300110624     C                             FLGINO <> 'C' AND FLGINO <> 'T'
193400980508     C     TINO          MULT(H)   PESQLI        WINL                           ** INOLTRO
193500980508     C                   END
193600980508     C                   ENDSR
193700980508     C*****************************************************************
193800980508     C* CALCOLO TARIFFA A SCALARE
193900980508     C*****************************************************************
194000980508     C     SCALAR        BEGSR
194100980508     C**
194200980514     C     *LIKE         DEFINE    PESTAS        WPTAS1
194300980508     C     *LIKE         DEFINE    PESTAS        WDIFSC
194400980508     C     *LIKE         DEFINE    PESTAS        WPTAS3
194500980511     C     *LIKE         DEFINE    PESQLI        WPESQ
194600980511     C     *LIKE         DEFINE    PESQLI        WPESSV
194700980511     C     *LIKE         DEFINE    TITR          WTITR
194800980511     C     *LIKE         DEFINE    TITR          WSTITR
194900980511     C     *LIKE         DEFINE    TINO          WTINO
195000980511     C     *LIKE         DEFINE    TINO          WSTINO
195100980508     C     *LIKE         DEFINE    TSCG          WPRESC
195200980508     C**
195300980508     C                   CLEAR                   TASPOR
195400980508     C                   CLEAR                   TASINL
195500980508     C                   CLEAR                   WTROVA
195600980508     C                   CLEAR                   WPTAS3
195700980508     C                   CLEAR                   WDIFSC
195800980508     C                   CLEAR                   WPRESC
195900980511     C                   CLEAR                   WPESSV
196000980511     C                   CLEAR                   WPESQ
196100980511     C                   CLEAR                   WTITR
196200980511     C                   CLEAR                   WSTITR
196300980511     C                   CLEAR                   WTINO
196400980511     C                   CLEAR                   WSTINO
196500980508     C                   Z-ADD     PESTAS        WPTAS1
196600980508     C                   Z-ADD     1             A
196700980508     C**
196800020218    1C     A             DOWLE     200
196900980508     C     WTROVA        ANDEQ     ' '
197000980508     C     TSCG          ANDGT     0
197100980508     C     A             OCCUR     TARDS
197200980512     C**
197300980508     C* FACCIO LA DIFFERENZA TRA LO SCAGLIONE LETTO ED IL PRECEDENTE
197400980508     C     TSCG          SUB       WPRESC        WDIFSC
197500980508     C**
197600980508     C* SOTRAGGO DAL PESO TASSABILE RIMASTO LA PARTE DA TASSARE ORA
197700980508     C* IN WPTAS1 HO SEMPRE LA PARTE DI PESO TASSABILE ANCORA DA TASSAR
197800980508     C                   SUB       WDIFSC        WPTAS1
197900980508     C**
198000980508     C** SE IL VALORE E' DIVENTATO < DI 0 SIGNIFICA CHE HO TERMINATO LA
198100980508     C**  TASSAZIONE PER CUI USO SOLO LA PARTE RIMASTA DI WPTAS1
198200980508    2C     WPTAS1        IFLE      0
198300980511     C     WDIFSC        ADD       WPTAS1        WPTAS3
198400980514     C* PESO TASSABILE GLOBALE DELLA SPEDIZIONE
198500980508     C                   MOVEL     'S'           WTROVA            1
198600980508     C                   ELSE
198700980508     C**
198800980508     C* PARTE DI PESO DA TASSARE CON QUESTO SCAGLIONE
198900980508     C                   Z-ADD     WDIFSC        WPTAS3
199000980508     C* SALVO LO SCAGLIONE PRECEDENTE
199100980508     C                   Z-ADD     TSCG          WPRESC
199200980508    2C                   ENDIF
199300980508     C**
199400980508     C* IMPOSTO IL VALORE DA MOLTIPLICARE PER LA TARIFFA
199500980508     C                   EXSR      CALQLI
199600980512     C**
199700980511     C* SE LO SCAGLIONE E' APPLICAZIONE TARIFFA FINITA DEVO CONSIDERARE
199800980512     C*  SOLO L'ULTIMO SCAGLIONE COSI' E NON I PRECEDENTI
199900980514    2C     WATA          IFNE      ' '
200000980511     C     WTROVA        ANDEQ     ' '
200100980511     C* MI SALVO IL VALORE MA PER ORA NON LO MOLTIPLICO
200200980511     C                   Z-ADD     PESQLI        WPESQ
200300980511     C                   Z-ADD     TITR          WTITR
200400980511     C                   Z-ADD     TINO          WTINO
200500980511   X2C                   ELSE
200600980511     C**
200700980511     C* SE LO SCAGLIONE TROVATO E' L'ULTIMO MA COMUNQUE TARIFFA FINITA
200800980511     C* DEVO MOLTIPLICARE SOLO QUESTO --> AZZERO UN EVENTUALE PRECEDENT
200900980511     C*  CHE AVEVA LA TASSAZIONE TARIFFA FINITA
201000980511    3C     WTROVA        IFEQ      'S'
201100980514     C     WATA          ANDNE     ' '
201200980511     C                   CLEAR                   WPESQ
201300980511     C                   CLEAR                   WTITR
201400980511     C                   CLEAR                   WTINO
201500980511    3C                   ENDIF
201600980511     C*
201700980511     C* PRIMA DI MOLTIPLICARE LA TARIFFA TROVATA VEDO SE CE NE E'
201800980511     C*  UNA PRECEDENTE TARIFFA FINITA DA CONSIDERARE
201900980511    3C     WPESQ         IFGT      0
202000980511     C* SALVO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
202100980511     C*  APPENA LETTO
202200980511     C                   Z-ADD     PESQLI        WPESSV
202300980511     C                   Z-ADD     TITR          WSTITR
202400980511     C                   Z-ADD     TINO          WSTINO
202500980511     C* IMPOSTO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
202600980511     C*  PRECEDENTE PER IL CALCOLO
202700980511     C                   Z-ADD     WPESQ         PESQLI
202800980511     C                   Z-ADD     WTITR         TITR
202900980511     C                   Z-ADD     WTINO         TINO
203000980511     C                   EXSR      MULTAR
203100980511     C                   ADD       WPOR          TASPOR
203200980511     C                   ADD       WINL          TASINL
203300980511     C                   CLEAR                   WPESQ
203400980511     C                   CLEAR                   WTITR
203500980511     C                   CLEAR                   WTINO
203600980511     C* REIMPOSTO I DATI DELLO SCAGLIONE APPENA LETTO
203700980511     C                   Z-ADD     WPESSV        PESQLI
203800980511     C                   Z-ADD     WSTITR        TITR
203900980511     C                   Z-ADD     WSTINO        TINO
204000980511    3C                   ENDIF
204100980511     C**
204200980508     C* MOLTIPLICAZIONE TARIFFA
204300980508     C                   EXSR      MULTAR
204400980508     C                   ADD       WPOR          TASPOR
204500980508     C                   ADD       WINL          TASINL
204600980511    2C                   ENDIF
204700980515     C                   ADD       1             A
204800980508     C**
204900980508    1C                   ENDDO
205000980515     C**
205100980515     C* APPLICO MINIMO E MASSIMO
205200980515     C                   Z-ADD     TASPOR        WPOR
205300980515     C                   Z-ADD     TASINL        WINL
205400980515     C                   EXSR      MINMAX
205500980515     C                   Z-ADD     WPOR          TASPOR
205600980515     C                   Z-ADD     WINL          TASINL
205700980508     C                   ENDSR
205800980512     C*****************************************************************
205900980512     C* CALCOLO MAX SCAGLIONE PRECEDENTE
206000980512     C*****************************************************************
206100980512     C     MAXSCA        BEGSR
206200980513     C* SOLO SE LO SCAGLIONE APPLICATO NON ERA IL PRIMO
206300980513     C     WA            IFGT      1
206400980512     C* PER CONFRONTARLO CON IL VALORE NORMALE CALCOLATO
206500980512     C* IL CONFRONTO E' DATO DALLA SOMMA TRA PORT E INOLTRO
206600980512     C                   SUB       1             WA
206700980512     C     WA            OCCUR     TARDS
206800980512     C* CALCOLO PESO DA MOLTIPLICARE
206900980512     C                   Z-ADD     TSCG          WPTAS3
207000980512     C                   EXSR      CALQLI
207100980515     C** MOLTIPLICO LA TARIFFA
207200980512     C                   EXSR      MULTAR
207300980515     C** MINIMO E MASSIMO
207400980515     C                   EXSR      MINMAX
207500980515     C**
207600990923     C     TASPOR        ADD       TASINL        T0100            15 3
207700990923     C     WPOR          ADD       WINL          W0100            15 3
207800980515     C** PRENDO DELLE 2 LA TARIFFA PIU' ALTA
207900980512     C     W0100         IFGT      T0100
208000980512     C                   Z-ADD     WPOR          TASPOR
208100980512     C                   Z-ADD     WINL          TASINL
208200980512     C                   ENDIF
208300980513     C                   ENDIF
208400980512     C**
208500980512     C                   ENDSR
208600980515     C*****************************************************************
208700980515     C** MINIMO E MASSIMO DA APPLICARE -SE APPLICATO TUTTO NEL PORTO
208800980515     C*****************************************************************
208900980515     C     MINMAX        BEGSR
209000980515     C** NON ESISTE MINIMO E MASSIMO
209100980515     C     TMIN          IFEQ      0
209200980515     C     TMAX          ANDEQ     0
209300980515     C                   GOTO      DOPOMM
209400980515     C                   END
209500980515     C** MINIMO
209600990923     C     WPOR          ADD       WINL          WMAX             15 3
209700980515     C     TMIN          IFGT      WMAX
209800980515     C                   Z-ADD     TMIN          WPOR
209900980515     C                   Z-ADD     0             WINL
210000980515     C                   Z-ADD     TMIN          WMAX
210100980515     C                   END
210200980515     C** MASSIMO
210300980515     C     TMAX          IFGT      0
210400980515     C     WMAX          IFGT      TMAX
210500980515     C                   Z-ADD     TMAX          WPOR
210600980515     C                   Z-ADD     0             WINL
210700980515     C                   END
210800980515     C                   END
210900980515     C     DOPOMM        ENDSR
211000980429     C*****************************************************************
211100980429     C* CERCO VARIA ISOLA LOCALITA' DISAGIATA CENTRI STORICI
211200090922     c* cerco anche la maggiorazione
211300980429     C*****************************************************************
211400980429     C     CERISO        BEGSR
211500980429     C                   CLEAR                   VAISO
211600060414     C                   CLEAR                   VATSP
211700980429     C     *LIKE         DEFINE    TASVA1        VAISO
211800060414     C     *LIKE         DEFINE    TASVA1        VATSP
211900981020     C                   Z-ADD     1             CC                3 0
212000980429     C     §$2VRJ        LOOKUP    SV(CC)                                 96     ISOLA
212100980429     C   96              ADD       VA(CC)        VAISO
212200981008     C                   Z-ADD     1             CC
212300980429     C     §$2VRK        LOOKUP    SV(CC)                                 96     DISAGIATE
212400980429     C   96              ADD       VA(CC)        VAISO
212500981008     C                   Z-ADD     1             CC
212600980429     C     §$2VRQ        LOOKUP    SV(CC)                                 96     C.STORICI
212700980429     C   96              ADD       VA(CC)        VAISO
212800060414     C                   Z-ADD     1             CC                3 0
212900090923     c* "E" o "H"
213000090923     c                   if        WvariaMag<>' '
213100090923     C     WVariaMag     LOOKUP    SV(CC)                                 96     ISOLA
213200060414     C   96              z-add     VA(CC)        VAtsp
213300090923     c                   endif
213400090923     c
213500980429     C                   ENDSR
213600060905     C*****************************************************************
213700060905     C* Fuel Surcharge
213800060905     C*****************************************************************
213900060905     c     SR_fuel       BEGSR
214000060906     C                   CLEAR                   SCA_PB
214100060906     C                   CLEAR                   SCA_SPE
214200060906     C                   CLEAR                   SCA_SCA
214300060905      *
214400060907      * verifico se esiste già la varia non la calcolo
214500060905      *
214600060905     c     §$2VFS        lookup    sv                                     96
214700060907     c                   IF        %found
214800060907     c                   goto      Endfuel
214900060907     c                   endif
215000060905
215100060905      * verifico se cliente ha in tariffa il calcolo del FUEL Surcharge
215200060907      * altrimenti non vado a cercare nella cartello e vado a fine
215300060906     C                   MOVEL     'f'           FISO
215400060906     C                   MOVEL     'f'           WFISO
215500060905
215600060906     c                   z-add     1             a
215700060906     c     Wfiso         Lookup    Cp(a)                                  05
215800060906     C  N05              GOTO      ENDFUEL
215900060906      * aggancio la ds della tabella 1P
216000060906     c     A             Occur     DS1P
216100060906
216200060905     C     KISOT         CHAIN     TITPT01L
216300060907      * cliente non ha la tariffa  non faccio calcolo
216400060907     C                   IF        not %FOUND(TITPT01L)
216500060907     c                   goto      Endfuel
216600060907     c                   endif
216601140115      * recupero la % di apllicazione minima del cliente
216602140115     c                   eval      dtpt01 = tptflo
216700060907      * data prezzo base minore della data spedizone non faccio calcolo
216800060907     C                   IF        TPTDPB > TASDSP
216900060907     c                   goto      Endfuel
217000060907     c                   endif
217100060906
217200060905      * Ricerco scaglione di appartenenza del prezzo base
217300060905
217400060911     C     TPTDPB        SETLL     TIPMG01L
217500060906     C                   READ      TIPMG01L
217600060906     C                   IF        NOT %EOF(TIPMG01L)
217700060906      *
217800060906     C                   Z-ADD     PMGSCA        SCA_PB
217900060906     C                   ENDIF
218000060906
218100060906      * Ricerco scaglione di appartenenza del prezzo del gasolio al momento della spedizione
218200060906
218300060911     C     TASDSP        SETLL     TIPMG01L
218400060906     C                   READ      TIPMG01L
218500060906     C                   IF        NOT %EOF(TIPMG01L)
218600060906      *
218700080909      * recupero il prezzo per ricercare il F.D. in tariffa
218800080909      *
218900080909     c                   z-add     pmgpmg        PMG_SGL
219000080909
219100060906     C                   Z-ADD     PMGSCA        SCA_SPE
219200060906     C                   ENDIF
219300060906
219400060906      * calcolo gli scaglioni di aumento
219500060906
219600060906     C                   EVAL      SCA_SCA = SCA_SPE - SCA_PB
219700060906     C                   IF        SCA_SCA < 0
219800060906     C                   CLEAR                   SCA_SCA
219900060906     C                   ENDIF
220000060906
220100060906      * se gli scaglioni scattati sono maggiore di zero cerco l'incidenza percentuale di
220200060906      * aumento nel dettaglio tariffario
220201140115      * anche se gli scatti sono a zero proseguo nel calcolo x applicare eventualmente
220202140115      * la % minima
220300140115     C****               IF        SCA_SCA > 0
220400080909      * cambia con il progetto 666 aumento supplemento carburante in tariffa
220500080909      * per cercare il Fattore demoltiplicatore non usiamo più il numero di scaglioni
220600080909      * di aumento ma il prezzo medio del gasolio della settimana della data di spedizione
220700080909     C****               Z-ADD     SCA_SCA       TPDPES
220800080909     C                   Z-ADD     PMG_SGL       TPDPES
220900060906     C                   EXSR      CALACD
221000060907      * calcolo dell'importo da addebitare al cliente
221100060905
221200060905     C                   CLEAR                   VARIA
221300060905     C                   CLEAR                   IMP_SCA
221301140115      * calcolo la % di aplicazione all'imponibile fuel
221302140115     c                   eval      Per_fuel = aitr * sca_sca
221303140115     c                   if        Per_fuel < §TPTpma
221304140115     c                   eval      Per_Fuel = §TPTpma
221305140115     c                   Endif
221400140115      * Se la percentuale Fuel  è maggiore di zero
221500140115    1C                   IF        Per_Fuel  > 0
221600060905      * CALCOLO L'IMPONIBILE
221700060905      * recupero importi di isola e località disagiate ed espresso
221800060905     C                   EXSR      CERISO
221900060905      *
222000060905     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP
222100060905      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
222200060905      *                     per abbandonare il concetto di monete diverse dall'EURO)
222300140115     C                   EVAL(H)   VARIA = (IMP_SCA * per_fuel) / 100
222400060905      **
222500060905     C                   Z-ADD     1             A
222600060905     C     ' '           LOOKUP    SV(A)                                  96
222700060907     C   96              MOVEL     §$2VFS        SV(A)
222800060905     C   96              Z-ADD     VARIA         VA(A)
222900060905     C  N96              ADD       VARIA         TASPOR
223000060905    1C                   ENDIF
223100060905      *
223200140115     C****               ENDIF
223300060905      *
223400060906     C     ENDFUEL       ENDSR
223500040506     C*****************************************************************
223600060905     C* SUPPLEMENTO CARBURANTE FEDEX
223700040506     C*****************************************************************
223800060905     C     SR_SUPCARB    BEGSR
223900040507      *
224000040507      * verifico se esiste già la varia
224100040507      *
224200040507     c     §$2VSC        lookup    sv                                     96
224300040507     c                   IF        not %found
224400040506
224500040506      * RICERCO IN BASE ALLA DATA SPEDIZIONE LA GIUSTA PERCENTUALE DI
224600040506      * SUPPLEMNENTO
224700040506
224800040507     C                   CLEAR                   VARIA
224900040507     C                   CLEAR                   IMP_SCA
225000040507     C                   CLEAR                   PER_SCA
225100040507     C                   Z-ADD     1             KX
225200040507    1c                   DO        *HIVAL
225300040507      * SCHIERA IN ORDINE DISCENDENTE
225400040507     C                   MOVE      DSCA(KX)      WDSPSC
225500040507      * SE DATA UGUALE A ZERO VADO A FINE
225600040507    2C                   IF        DS_DSC = *ZEROS
225700040507     C                   LEAVE
225800040507    2C                   ENDIF
225900040507      * SE DATA SPEDIZIONE MAGGIORE DI QUELLA IN SCHIERA RECUPERO LA PERCENTUALE DI CALCOLO
226000040510    2C                   IF        TASDSP >=  DS_DSC
226100040507     c                   z-add     DS_psc        PER_SCA
226200040507     C                   LEAVE
226300040507     C                   ELSE
226400040507      * SE DATA MINORE LEGGO LA DATA SUCCESSIVA
226500040507    3C                   IF        KX < 999
226600040507     C                   ADD       1             KX
226700040507     C                   ELSE
226800040507     C                   LEAVE
226900040507    3C                   ENDIF
227000040507    2C                   ENDIF
227100040507
227200040507    1C                   ENDDO
227300040507      * SE PERCENTUALE MAGGIORE DI ZERO ESEGUO IL CALCOLO
227400040507    1C                   IF        PER_SCA > 0
227500040507      * CALCOLO L'IMPONIBILE
227600060414      * recupero importi di isola e località disagiate ed espresso
227700040507     C                   EXSR      CERISO
227800040507      *
227900060414     c* Di fatto qui l'espresso non ci sarà mai!!! bolla estera
228000060414     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP
228100040507      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
228200040507      *                     per abbandonare il concetto di monete diverse dall'EURO)
228300040507     C                   EVAL(H)   VARIA = (IMP_SCA * PER_SCA) / 100
228400040507      **
228500040507     C                   Z-ADD     1             A
228600040507     C     ' '           LOOKUP    SV(A)                                  96
228700040507     C   96              MOVEL     §$2VSC        SV(A)
228800040507     C   96              Z-ADD     VARIA         VA(A)
228900040507     C  N96              ADD       VARIA         TASPOR
229000040507    1C                   ENDIF
229100040507      *
229200040507     C                   ENDIF
229300040507      *
229400040507     C                   ENDSR
229500970521     C*****************************************************************
229600970521     C* TASSAZIONE TARIFFE PARTICOLARI
229700970521     C*****************************************************************
229800970521     C     TASPAR        BEGSR
229900970521     C     *LIKE         DEFINE    TASVA1        TASCP
230000970522     C     *LIKE         DEFINE    TASVA1        TASISO
230100970521     C     *LIKE         DEFINE    TASTC1        TASTC
230200970721     C                   CLEAR                   TASCP
230300970721     C                   CLEAR                   WNOEST
230400970721     C* SE NON ESISTE LA SIGLA DELLA VARIA O E' GIA' IN TASSAZIONE
230500970721     C*  NON RITASSO NE APPLICO L'ESPRESSO
230600970521     C**
230700970521     C                   Z-ADD     1             A
230800970521     C     TASTC         LOOKUP    CP(A)                                  05
230900970521     C  N05              GOTO      ENDPAR
231000970521     C**
231100970521     C     A             OCCUR     DS1P
231200970521     C                   Z-ADD     1             A
231300970521     C     §1PVAR        LOOKUP    SV(A)                                  05
231400970521     C* SIGLA VARIA NON PRESENTE --> CALCOLO
231500970521     C     *IN05         IFEQ      *OFF
231600970721     C                   MOVEL     'S'           WNOEST            1
231700970521     C                   EXSR      CALCP
231800970521     C**
231900970521     C     TASCP         IFGT      0
232000130927      * se sto tassando la tariffa particolare m = avviso affidamento spedizione
232100130927      * ed il flag EMD è uguale a "E" (via sms che mail) raddoppio la varia
232200130927     c                   if        §1pvar = §$2VAD and
232300130927     c                             §asemd = 'E'
232400130927     c                   eval      tascp = tascp * 2
232500130927     c                   endif
232600130927      *
232700970521     C                   Z-ADD     1             A
232800970521     C     ' '           LOOKUP    SV(A)                                  96
232900970521     C   96              MOVEL     §1PVAR        SV(A)
233000970521     C   96              Z-ADD     TASCP         VA(A)
233100970521     C  N96              ADD       TASCP         TASPOR
233200970521     C                   END
233300970521     C                   END
233400970521     C     ENDPAR        ENDSR
233500941112     C******************************************************
233600941107     C** CALCOLO PESO TASSABILE PER TARIFFA A PESO
233700941112     C******************************************************
233800941107     C     CALPT         BEGSR
233900980313     C** MI MEMORIZZO SU BLTAS IL PESO TASSABILE SE
234000980313     C** IL PESO E' SUPERIORE AL MINIMO TASSABILE
234100980313     C** SE ESISTE IL RAPPORTO PESO VOLUME IN TARIFFA  E VOL IN BOLLA
234200980313     C** IL PESO TASSABILE SU BLTAS E' SEMPRE ARROTONDATO AL KG SUPERIO
234300980311     C                   CLEAR                   TASPVL
234400050912     C***  UNOCTR        IFEQ      0
234500050912     C**** UNOCTR        OREQ      3
234600050912      * modifico --- il peso tassabile si stampa e si calcola nei seguenti casi :
234700050912      * Tariffa a quintale "0" il peso da fatturare maggiore del  minimo tassabile
234800050912      *                        (definito nella tabella TR = 100) oppure volume da fatturare
234900050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
235000050912      *                        tariffa maggiore di zero
235100050912      * Tariffa a kg       "3" non controllo il peso da fatturare maggiore del minimo tassabile
235200050912      *                        (definito nella tabella TR = 0,1) ma solo volume da fatturare
235300050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
235400050912      *                        tariffa maggiore di zero
235500050912     C***  TARIFFA A QUINTALE
235600050912
235700050912     C     UNOCTR        IFEQ      0
235800050912
235900020826     C     USAPKF        IFGT      MINTAS
236000950901     C     PESTAS        ADD       0,9           TASPVL
236100941107     C                   ELSE
236200941107     C     TASVLF        IFNE      0
236300980311     C     RAPPV         ANDNE     0
236400950901     C     PESTAS        ADD       0,9           TASPVL
236500980313     C                   ENDIF
236600980313     C                   ENDIF
236700980313     C                   ENDIF
236800050912
236900050912     C***  TARIFFA A KG
237000050912
237100050912     C     UNOCTR        IFEQ      3
237200050912
237300050912     C     TASVLF        IFNE      0
237400050912     C     RAPPV         ANDNE     0
237500050912     C     PESTAS        ADD       0,9           TASPVL
237600050912     C                   ENDIF
237700050912     C                   ENDIF
237800050912
237900980313     C                   ENDSR
238000950119     C******************************************************
238100950119     C** CALCOLA VALORE DA ASSICURARE PER TARIFFA A VALORE
238200950119     C******************************************************
238300950119     C     CALVAS        BEGSR
238400950119     C     *LIKE         DEFINE    TASIAS        IMPVAS
238500950119     C                   Z-ADD     0             IMPVAS
238600950119     C                   MOVEL     'C'           FISO
238700990722     C                   MOVEL     'C'           WFISO             1
238800950119     C**FLAG ASSICURAZIONE X CONTO = C
238900950119     C                   Z-ADD     1             A
239000990722     C     WFISO         LOOKUP    CP(A)                                  05
239100950119     C  N05              GOTO      DOPVA1
239200950119     C**----------------------------------------
239300990721     C     KISOT         CHAIN     TITPT01L                           28
239400980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
239500980528     C     *IN28         IFEQ      *ON
239600980528     C     TPTATB        ORNE      ' '
239700980528     C                   GOTO      DOPVA1
239800980528     C                   ENDIF
239900980528     C**
240000950119     C** ESCLUDO GLI ANNULLATI
240100950119     C     TPTFVM        IFEQ      '3'
240200020826     C     TPTVLM        MULT      USAPKF        IMPVAS
240300950119     C                   END
240400950119     C*PESO
240500950119     C     TPTFVM        IFEQ      '1'
240600950119     C     TPTVLM        MULT      TASNCL        IMPVAS
240700950119     C                   END
240800950119     C* COLLI
240900950119     C     TPTFVM        IFEQ      '2'
241000950119     C                   Z-ADD     TPTVLM        IMPVAS
241100950119     C                   END
241200950119     C*SPEDIZIONE
241300950119     C     DOPVA1        ENDSR
241400941112     C******************************************************
241500941107     C** CALCOLA VARIA R  --- ASSICURAZIONE PER CONTO
241600941112     C******************************************************
241700941107     C     CALVR         BEGSR
241800050525     c* Il falg task88 cambia significato:indica se bolla con mandato o no
241900050525     c*
242000050525     c* con mandato significa: x mandato a val variabile--> tutte le bolle
242100050525     c*                        x mandato a val.indicato --> solo bolle con
242200050525     c*                        tasias=0 o con tasias <=al valore del mandat
242300050525     c*
242400050525     c* Imposto task88=N se mandato
242500050525     c* Imposto task88=S se no mandato ma tasias>0
242600050525     c* Imposto task88=X per bolle senza mandato e senza importo
242700050525     c
242800050525     C                   MOVEL     'X'           TASK88
242900980603     C** 88 = SI CALCOLA SOLO SE ESISTE IL VALORE MERCE IN BOLLA
243000941112     C     KSCCOD        IFEQ      8888
243100980527     C     KSCCOD        OREQ      9999
243200980709     C     TASIAS        IFEQ      0
243300050525     C**                 MOVEL     'S'           TASK88
243400980709     C                   GOTO      ENDVR
243500941112     C                   END
243600980709     C                   END
243700980603     C**
243800980603     C** SE TIPO BOLLA CHE NON PREVEDE L'ASSICURAZIONE, ESCO SENZA
243900980603     C**  DARE ERRORE
244000050525     C**   TASIAS        IFEQ      0
244100050525     C**   TASTBL        LOOKUP    TBN                                    05
244200050525     C** 05              MOVEL     'S'           TASK88
244300050525     C** 05              GOTO      ENDVR
244400050525     C**                 ENDIF
244500991012     C**
244600991012     C** SE IMPORTO D'ASSICURARE MAGGIORE DI ZEROS MUOVO 'S' TASK88
244700991012     C** PER STAMPA BOLLE CON ASSICURAZIONE PER TABULATONE DELLA CONSULDANNI
244800050525     C**   TASIAS        IFGT      0
244900050525     C**                 MOVEL     'S'           TASK88
245000050525     C**                 ENDIF
245100980603     C**
245200941116     C                   SETOFF                                       94
245300941116     C     *LIKE         DEFINE    TASIAS        IASLIT
245400941116     C                   Z-ADD     TASIAS        IASLIT
245500941116     C     IASLIT        IFNE      0
245600941116     C                   Z-ADD     1             K
245700941116     C     TASVAS        LOOKUP    CV(K)                                  05
245800980504     C     *IN05         IFEQ      *OFF
245900980504     C                   SETON                                        94
246000980504     C                   MOVEL     MSG(2)        TASMSG
246100980504     C                   GOTO      FINE
246200980504     C                   ENDIF
246300941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
246400990922     C**
246500990922     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA SE E' DIVERSA
246600990922     C* DALLA VALUTA DELLA TARIFFA
246700990922     C     §TADIV        IFNE      TASVAS
246800990922     C                   CLEAR                   YEUR
246900990922     C                   MOVEL     TASVAS        YECDVI
247000990922     C                   MOVEL     §TADIV        YECDVO
247100990922     C                   Z-ADD     IASLIT        YECIMI
247200011129     C   12              MOVE      '2'           YECDCO
247300991014     C  N12              MOVE      '0'           YECDCO
247400990922     C*
247500990922     C                   CALL      'YEURCO'
247600990922     C                   PARM                    YEUR
247700990922     C*
247800990922     C     YECESI        IFEQ      ' '
247900990922     C                   Z-ADD     YECIMO        IASLIT
248000990922     C                   END
248100990922     C                   ENDIF
248200990922     C                   ENDIF
248300941123     C**----------------------------------------
248400941107     C                   MOVEL     'C'           FISO
248500990722     C                   MOVEL     'C'           WFISO
248600941107     C**FLAG ASSICURAZIONE X CONTO = C
248700941123     C                   Z-ADD     1             A
248800990722     C     WFISO         LOOKUP    CP(A)                                  05
248900941123     C  N05              GOTO      ENDVR
249000020226      * AGGANCIO LA DS DELA TABELLA 1P
249100020226     C     A             OCCUR     DS1P
249200941123     C**----------------------------------------
249300990721     C     KISOT         CHAIN     TITPT01L                           28
249400980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
249500980528     C     *IN28         IFEQ      *ON
249600980528     C     TPTATB        ORNE      *BLANKS
249700980528     C                   GOTO      DOPAC1
249800980528     C                   ENDIF
249900980505     C**
250000980505     C** CONTROLLO IL TIPO APPLICAZIONE DELLA TARIFFA ASSIC X CONTO
250100980505     C**  SE INCONGRUENTE CON TIPO BOLLA PRENDO LA CARTELLO
250200980505     C     TPTAPL        IFEQ      'A'
250300980505     C     TIPBOL        ANDEQ     'F'
250400980505     C     TPTAPL        OREQ      'P'
250500980505     C     TIPBOL        ANDEQ     'A'
250600980505     C                   GOTO      DOPAC1
250700980505     C                   ENDIF
250800980505     C**
250900980504     C* VALORE MERCE = 0 --> ESCLUDO SE NON E' UN VERO MANDATO
251000980505     C                   SETOFF                                       94
251100980504    1C     TPTVLM        IFEQ      0
251200980709     C* CHE SIA UN MANDATO FITTIZIO O UN MANDATO A VALORE VARIABILE
251300980709     C*  DEVO SEMPRE SCRIVERE LA BOLLA COME 8888 PERCHE L'ASSICURAZIONE
251400980709     C*  VUOLE VEDERE BOLLA PER BOLLA QUANTO SONO ASSICURATE
251500050525     C****               MOVEL     'S'           TASK88
251600980709     C**
251700980504    2C     IASLIT        IFEQ      0
251800980504     c* MANDATO FITTIZIO
251900990922    3C     TPTTMA        IFEQ      'F'
252000980504     C                   GOTO      ENDVR
252100980504    3C                   ENDIF
252200050525     c**
252300050526     C** ES - non serve: già fatto    sopra!! Ripetizione inutile
252400050526    3C**   TPTAPL        IFEQ      'P'
252500050526     C**   TIPBOL        COMP      'F'                                    94
252600050526    3C**                 END
252700050526    3C**   TPTAPL        IFEQ      'A'
252800050526     C**   TIPBOL        COMP      'A'                                    94
252900050526    3C**                 END
253000050526    3C**   TPTAPL        IFEQ      ' '
253100050526     C**                 SETON                                            94
253200050526    3C**                 END
253300050525     c
253400050525     c
253500050525     c* anche se manca tariffa è comunque con mandato
253600050525     c                   eval      task88='N'
253700050525     c*
253800050525     c* non devo dare manca tariffa se tipo bolla che non prevede
253900050525     c* Assicurazione
254000050525     C     TASTBL        LOOKUP    TBN                                    05
254100050525     c                   if        not *in05
254200050526     c                   eval      *in94='1'
254300050525     C                   MOVEL     MSG(4)        TASMSG
254400050525     C                   GOTO      FINE
254500050525     c                   endif
254600050525     c
254700050525   x2c                   else
254800050525     c* Mandato a valore variabile con importo: se fittizio 8888 altriment
254900050525     c* con mandato
255000050525    3C     TPTTMA        IFEQ      'F'
255100050525     c                   eval      task88='S'
255200050525     c                   else
255300050525     c                   eval      task88='N'
255400050525    3C                   END
255500050525    2C                   END
255600050525     c
255700941107     C** MANCA TARIFFA: MANDATO CON VALORE = 0
255800941107     C**                E NON ESISTE VALORE IN BOLLA (SOLO COD.)
255900980504   X1C                   ELSE
256000980504    2C     IASLIT        IFGT      0
256100941112     C                   Z-ADD     0             VALSPE           15 3
256200980504    3C     TPTFVM        IFEQ      '3'
256300020826     C     TPTVLM        MULT      USAPKF        VALSPE
256400980504    3C                   END
256500941107     C*PESO
256600980504    3C     TPTFVM        IFEQ      '1'
256700941107     C     TPTVLM        MULT      TASNCL        VALSPE
256800980504    3C                   END
256900941107     C* COLLI
257000980504    3C     TPTFVM        IFEQ      '2'
257100941107     C                   Z-ADD     TPTVLM        VALSPE
257200980504    3C                   END
257300030908      *
257400030908      * ARROTONDO AL DECIMALE DELLA TARIFFA
257500030908     C                   CLEAR                   YEUR
257600030908     C                   MOVEL     §TADIV        YECDVI
257700030908     C                   MOVEL     §TADIV        YECDVO
257800030908     C                   Z-ADD     VALSPE        YECIMI
257900030908     C                   CALL      'YEURCO'
258000030908     C                   PARM                    YEUR
258100030908     C*
258200030908     C     YECESI        IFEQ      ' '
258300030908     C                   Z-ADD     YECIMO        VALSPE
258400030908     C                   END
258500941107     C*SPEDIZIONE
258600980504    3C     IASLIT        IFGT      VALSPE
258700941107     C                   GOTO      DOPAC1
258800050526     c                   else
258900050526     c
259000050526     c* Se valore rientra nel mandato, imposto che si tratta di mandato
259100050526     C                   MOVEL     'N'           TASK88
259200980504    3C                   END
259300050526     c
259400050526   x2c                   else
259500050526     c* se senza valore in bolla ma madanto con valore -->ok da mandato
259600050526     c                   eval      task88='N'
259700980504    2C                   END
259800980504    1C                   END
259900050526     c
260000980525     C** SE IL CLIENTE E' 8888 HO IMPOSTATO PRIMA LA TARIFFA DI CARTELL
260100980525     C     KSCCOD        IFEQ      8888
260200050525     C                   MOVEL     'S'           TASK88
260300980525     C                   ENDIF
260400050525     c
260500050525     c** Anche se lo considero con mandato, se non c'e' importo e tipo
260600050525     c**  bolla senza assicurazione, non calcolo varia R
260700050525     C     TASIAS        IFEQ      0
260800050525     C     TASTBL        LOOKUP    TBN                                    05
260900050525     C   05              GOTO      ENDVR
261000050525     C                   ENDIF
261100980505     C**
261200941107     C                   GOTO      DOPAC2
261300941107     C     DOPAC1        TAG
261400941107     C** NON ESISTE TARIFFA AXC DEL CLIENTE
261500941121     C*-----------------------------------------------------------------
261600980505     C* SE USATA SI TRATTA DI TARIFFA DI CARTELLO
261700050525     C***                MOVEL     'S'           TASK88
261800941112     C     IASLIT        CABEQ     0             ENDVR
261900050525     c                   eval      task88='S'
262000020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
262100020226     C     KTPTC         CHAIN     TITPT01L                           28
262200020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
262300020226     C     *IN28         IFEQ      *ON
262400020226     C     TPTATB        OREQ      'A'
262500020226     C                   GOTO      ENDVR
262600020226     C                   ENDIF
262700941107     C*-----------------
262800941107     C     DOPAC2        TAG
262900980504     C** SE NON SERVE --> ESCO DAL CALCOLO
263000980504     C** TIPO APPLICAZIONE
263100980504     C** P - SOLO PER FRANCHI
263200980504     C** A - SOLO PER ASSEGNATI
263300980504    1C     TPTAPL        IFNE      *BLANKS
263400980504     C     IASLIT        ANDEQ     0
263500980504    2C     TIPBOL        IFEQ      'F'
263600980504    3C     TPTAPL        IFEQ      'A'
263700980504     C                   GOTO      ENDVR
263800980504    3C                   ENDIF
263900980504   X2C                   ELSE
264000980504    3C     TPTAPL        IFEQ      'P'
264100980504     C                   GOTO      ENDVR
264200980504    3C                   ENDIF
264300980504    2C                   END
264400980504    1C                   END
264500980504     C**
264600941112     C     *LIKE         DEFINE    TASIAS        VALVR
264700941112     C     *LIKE         DEFINE    TASIAS        ASCPES
264800941112     C     *LIKE         DEFINE    TASIAS        ASCPE2
264900941112     C                   Z-ADD     0             VALVR
265000980526    1C     IASLIT        IFEQ      0
265100980526    2C     TPTFVM        IFEQ      '3'
265200020826     C     USAPKF        MULT      TPTVLM        VALVR
265300980526    2C                   END
265400980526    2C     TPTFVM        IFEQ      '1'
265500941107     C     TASNCL        MULT      TPTVLM        VALVR
265600980526    2C                   END
265700980526    2C     TPTFVM        IFEQ      '2'
265800941107     C                   Z-ADD     TPTVLM        VALVR
265900980526    2C                   END
266000980526   X1C                   ELSE
266100941112     C                   Z-ADD     IASLIT        VALVR
266200980526    1C                   END
266300011119     C***** OBSOLETO NON RESTITUISCO + IL VALORE IN LIRE MA NELLA DIVISA DELLA TARIFFA
266400011129     C***** PERO' ARROTONDO AL DECIMALE DELLA TARIFFA
266500011129     C                   CLEAR                   YEUR
266600011129     C                   MOVEL     §TADIV        YECDVI
266700011129     C                   MOVEL     §TADIV        YECDVO
266800011129     C                   Z-ADD     VALVR         YECIMI
266900011129     C                   CALL      'YEURCO'
267000011129     C                   PARM                    YEUR
267100011129     C*
267200011129     C     YECESI        IFEQ      ' '
267300011129     C                   Z-ADD     YECIMO        VALVR
267400011129     C                   END
267500011129     C* RESTITUISCO IN OUTPUT L'IMPORTO DA ASSICURARE
267600011129     C                   Z-ADD     VALVR         TASIAL
267700941107     C** VALORE SPEDIZIONE PER CALCOLO ASSICURAZIONE
267800980312     C**
267900980526     C** CALCOLO SOLO SE NON HO IMPOSTATO IL FLAG
268000980526    1C     PARCA         IFEQ      ' '
268100980505     C**
268200980505     C** SE C'E' GIA' LA VARIA R NON LA RICALCOLO
268300980505     C                   Z-ADD     1             A
268400980505     C     §$2VRR        LOOKUP    SV(A)                                  05
268500980526    2C     *IN05         IFEQ      *OFF
268600980312     C                   MOVEL     TPTTPG        WTPG
268700980312     C                   MOVEL     TPTRPV        WRPV
268800980313     C                   CLEAR                   WDET
268900980313     C                   Z-ADD     TPTARL        WARL
269000980313     C                   Z-ADD     TPTARF        WARF
269100980313     C                   Z-ADD     TPTARO        WARO
269200980312     C                   EXSR      CALCOL
269300980312     C**
269400980312     C** A VALORE
269500980526    3C     TPTTPG        IFEQ      'V'
269600980312     C                   Z-ADD     VALVR         ISOPES
269700980526    3C                   END
269800980312     C     *LIKE         DEFINE    TASIAS        TPDPES
269900980504     C**
270000980504     C**CALCOLO IMPORTO ASSICURAZIONE PER CONT
270100980504     C                   Z-ADD     ISOPES        TPDPES
270200941107     C                   EXSR      CALACD
270300980525     C                   Z-ADD     VARIA         WVARIR
270400980526    2C                   ENDIF
270500980526    1C                   ENDIF
270600980504     C     ENDVR         ENDSR
270700060320     C******************************************************
270800060320     C** CALCOLA VARIA d  --- A C BASE
270900060320     C******************************************************
271000060321     c     CALVACB       BEGSR
271100060320     c* Il falg task88 cambia significato:indica se bolla con mandato o no
271200060320     c* Imposto task88=N se mandato
271300060321     c* Lascio  task88=X per bolle senza mandato e senza importo
271400060321
271500060321      **----------------------------------------
271600060321     c                   movel     'd'           Fiso
271700060321     c                   movel     'd'           WFiso
271800060321      ** Flag A C BASE  = d
271900060321     c                   z-add     1             a
272000060321     c     Wfiso         Lookup    Cp(a)                                  05
272100060321     c  N05              Goto      EndVacb
272200060321      * aggancio la ds della tabella 1P
272300060321     c     A             Occur     DS1P
272400060321      **----------------------------------------
272500060321     c     Kisot         Chain     TITPT01L
272600060321      ** esiste Tariffa A C base
272700060321    1c                   If        %found(titpt01l)
272800060321      **
272900060321      ** Controllo il tipo applicazione della tariffa A C Base
273000060321      **
273100060321    2c                   If        (tptapl = 'A' and tipbol = 'A') or
273200060321     c                             (tptapl = 'P' and tipbol = 'F') or
273300060321     c                              tptapl = ' '
273400060321      *
273500060321     c                   eval      task88='N'
273600060321      ** Anche se lo considero con mandato,  tipo
273700060321      **  bolla senza assicurazione, non calcolo varia d
273800060321     c     TAStbl        Lookup    Tbn                                    05
273900060321     c   05              Goto      ENDVacb
274000060321
274100060321      * calcolo valore assicurato
274200060321
274300060321     c     *Like         Define    TASias        VALVacb
274400060321
274500060321     c                   clear                   VALVacb
274600060321
274700060321      * Peso
274800060321    3c                   If        TPTfvm = '3'
274900060321     c     TPTvlm        Mult      USAPkf        Valvacb
275000060321    3c                   Endif
275100060321      * Colli
275200060321    3c                   If        TPTfvm = '1'
275300060321     c     TPTvlm        Mult      TASncl        Valvacb
275400060321    3c                   Endif
275500060321      * Spedizione
275600060321    3c                   If        TPTfvm = '2'
275700060321     c                   Z-add     TPTvlm        Valvacb
275800060321    3c                   Endif
275900060321      *
276000060321      * Arrotondo al decimale della tariffa
276100060321     c                   Clear                   YEUR
276200060321     c                   Movel     §TAdiv        YECdvi
276300060321     c                   Movel     §TAdiv        YECdvo
276400060321     c                   Z-add     VALvacb       YECimi
276500060321     c                   call      'YEURCO'
276600060321     c                   Parm                    YEUR
276700060321      *
276800060321     c                   If        YECesi = ' '
276900060321     c                   Z-add     YECimo        VALvacb
277000060321     c                   Endif
277100060321
277200060321      ** Calcolo solo se non ho impostato il flag
277300060321    3c                   IF        Parca = ' '
277400060321
277500060321      ** Se c'è' già la varia non la ricalcolo
277600060321     c                   Z-add     1             a
277700060321     c     §$2acb        Lookup    sv(a)                                  05
277800060321    4c     *IN05         Ifeq      *off
277900060321     c                   movel     TPTtpg        Wtpg
278000060321     c                   Movel     TPTrpv        Wrpv
278100060321     c                   clear                   Wdet
278200060321     c                   Z-add     TPTarl        Warl
278300060321     c                   Z-add     TPTarf        Warf
278400060321     c                   Z-add     TPTaro        Waro
278500060321     c                   Exsr      Calcol
278600060321
278700060321      ** A Valore
278800060321    5c                   IF        tpttpg =  'V'
278900060321     c                   z-add     valvacb       ISOpes
279000060321    5c                   Endif
279100060321
279200060321     c                   Z-add     ISOpes        TPDpes
279300060321     c                   Exsr      Calacd
279400060321     c                   Z-add     varia         Wvariacb
279500060321    4c                   endif
279600060321
279700060321    3c                   endif
279800060321
279900060321    2c                   endif
280000060321    1c                   Endif
280100060321
280200060321     c     EndVacb       ENDSR
280300941112     C******************************************************
280400980312     C** CALCOLA TARIFFA PARTICOLARE
280500941112     C******************************************************
280600941107     C     CALACD        BEGSR
280700990923     C* CAMPO VARIA CON  DECIMALI
280800990923     C                   Z-ADD     0             VARIA            15 3
280900990923     C* CAMPO VARIA SENZA DECIMALI ALTRIMENTI NON FUNZIONA L'ARROTONDAMENTO
281000990923     C                   Z-ADD     0             VARIAD           15 0
281100990923     C*
281200941107     C                   DO        100           A
281300941122     C     A             OCCUR     ACDS
281400941107     C                   CLEAR                   ACDS
281500941107     C                   END
281600941112     C** PULIZIA DS TARIFFA
281700020412     C*
281800020412     C* SE ESISTE ALMENO UNA RIGA DI DETTAGLIO E NON RIESCO AD APPLICAR
281900020412     C*  QUESTA TAR.PARTICOLARE --> MANCA TARIFFA
282000980605     C* BOLLA IMPORT / ESXPORT
282100980605     C     WBOEST        IFEQ      'E'
282200020226     C     §1PCTE        IFNE      *BLANKS
282300020226     C                   MOVEL     §1PCTE        ISOCTS
282400941112     C                   GOTO      POIAC
282500941112     C                   END
282600941112     C                   ELSE
282700020226     C     §1PCTS        IFNE      *BLANKS
282800020226     C                   MOVEL     §1PCTS        ISOCTS
282900941112     C                   GOTO      POIAC
283000941112     C                   END
283100941112     C                   END
283200941112     C** CODICI TASSAZIONE FISSI DA TABELLA TARIFFE PARTICOLARI
283300941110     C                   MOVEL     TASCTS        ISOCTS            2
283400060414     c
283500060414      * Se assegnato e tariffa FEDEX metto in ISOCTS  quello mittente
283600050124     c                   If        TIPBOL = 'A' and WBOFED = 'S'
283700050124     c                   movel     tasmct        isocts
283800050124     c                   endif
283900060414     c* Se devo utilizzare quello del mittente per via della
284000060414     c*  reversibilità --> lo uso
284100060414     c                   if        applicarevers='S'
284200060414     c                   movel     tasmct        isocts
284300060414     c                   endif
284400050124      *
284500990721     C     KISOD         SETLL     TITPD02L                               28
284600941107     C   28              GOTO      DOPAC3
284700941107     C                   Z-ADD     1             A
284800941107     C     ISOCTS        LOOKUP    CTS(A)                                 05
284900941107     C   05A             OCCUR     DSCT
285000941107     C   05              MOVEL     §CTRAP        ISOCTS
285100941107     C  N05              MOVEL     *BLANKS       ISOCTS
285200941107     C** REGIONE
285300990721     C     KISOD         SETLL     TITPD02L                               28
285400941107     C   28              GOTO      DOPAC3
285500980605     C     WBOEST        IFEQ      'E'
285600941112     C                   MOVEL     'EE'          ISOCTS
285700941112     C                   ELSE
285800941112     C                   MOVEL     'IT'          ISOCTS
285900941112     C                   END
286000941112     C     POIAC         TAG
286100990721     C     KISOD         SETLL     TITPD02L                               28
286200020412     C**N28                GOTO ENDACD
286300020412     C** N 28 --> MANCA TARIFFA
286400020412     C     *IN28         IFEQ      *OFF
286500020412     C* SE NON È PRESENTE NESSUNA RIGA DI DETTAGLIO, NON DO
286600020412     C*  MANCA TARIFFA: SIGNIFICA CHE È INSERITA UNA TASSAZIONE =0
286700020412     C     KISOD2        SETLL     TITPD02L                               28
286800020412     C  N28              GOTO      ENDACD
286900020412     C*
287000020412     C                   SETON                                        94
287100020412     C                   MOVEL     MSG(17)       TASMSG
287200020412     C                   GOTO      FINE
287300020412     C                   ENDIF
287400020412     C**
287500020412     C** NON TROVATA TARIFFA
287600941107     C     DOPAC3        TAG
287700941107     C                   DO        *HIVAL        A
287800990721     C     KISOD         READE     TITPD02L                               28
287900941107     C   28              GOTO      DOPAC4
288000941107     C     A             OCCUR     ACDS
288100990923     C* VERIFICO SE STO CARICANDO LA TARIFFA DI CARTELLO
288200990923     C     TPTKSC        IFEQ      CARKSC
288300990923     C     TPTCTR        ANDEQ     CARCTR
288400990923     C     TPTPRG        ANDEQ     CARPRG
288500990923     C* E LA DIVISA DELLA TARIFFA DI CARTELLO E' DIVERSA DALLA DIVISA DELLA
288600990923     C* TARIFFA DEL CLIENTE  CONVERTO I VALORI O SCAGLIONI NELLA DIVISA
288700990923     C* DELLA TARIFFA DEL CLIENTE
288800990923     C     §TADIV        ANDNE     DIVCAR
288900990923     C*
289000990923     C                   EXSR      CONTPD
289100990923     C*
289200990923     C                   ENDIF
289300990923     C*
289400941112     C                   Z-ADD     TPDSGL        ASGL
289500941112     C                   Z-ADD     TPDITR        AITR
289600941112     C                   Z-ADD     TPDMIN        AMIN
289700941112     C                   Z-ADD     TPDMAX        AMAX
289800941112     C                   MOVEL     TPDAIN        AAIN
289900941107     C                   END
290000941107     C     DOPAC4        TAG
290100941107     C                   DO        100           A
290200941107     C     A             OCCUR     ACDS
290300941112     C     ASGL          IFGE      TPDPES
290400941107     C                   GOTO      DOPAC5
290500941107     C                   END
290600941107     C                   END
290700941123     C     ASGL          IFLT      TPDPES
290800941122     C                   SETON                                        94
290900980527     C                   MOVEL     MSG(7)        TASMSG
291000980527     C                   GOTO      FINE
291100980527     C                   ENDIF
291200941122     C** NON TROVATO SCAGLIONE ESATTO
291300941107     C     DOPAC5        TAG
291400941112     C*-----------
291500941122     C     ASGL          IFLE      MINTAS
291600941122     C                   Z-ADD     MINTAS        TPDPES
291700941112     C                   END
291800941122     C     TPDPES        IFLE      MINTAS
291900941122     C                   Z-ADD     MINTAS        TPDPES
292000941122     C                   END
292100991013     C                   Z-ADD     0             TPDQLI           18 6
292200980429     C* DIVIDO PER 100 TARIFFA 0 PER AVERE QUINTALI
292300980429     C*                TARIFFA 4 PERCHE' E' TARIFFA %
292400980429     C     WTPG          IFEQ      '0'
292500980429     C     WTPG          OREQ      '4'
292600941112     C     TPDPES        DIV       100           TPDQLI
292700941112     C                   ELSE
292800941112     C                   Z-ADD     TPDPES        TPDQLI
292900941112     C                   END
293000980429     C** PESO TASSABILE
293100060906      * se trovato dettaglio tariffario della tariffa "f" vado a fine routine in quanto il calcolo
293200060906      * viene eseguito nella routine FUEL
293300060906     c                   If        wfiso = 'f'
293400060906     c                   goto      endacd
293500060906     c                   endif
293600060906
293700990923     C   12AITR          MULT(H)   TPDQLI        VARIA
293800990923     C  N12AITR          MULT(H)   TPDQLI        VARIAD
293900941112     C                   MOVEL     AAIN          FLGISO
294000980429     C*
294100980430     C     VARIA         IFGT      0
294200991012     C     *IN12         ANDEQ     *ON
294300991012     C     VARIAD        ORGT      0
294400991012     C     *IN12         ANDEQ     *OFF
294500991012     C*
294600980430     C     WTPG          IFEQ      'V'
294700980430     C     WTPG          OREQ      'T'
294800990923     C   12VARIA         DIV(H)    100           VARIA
294900990923     C  N12VARIAD        DIV(H)    100           VARIAD
295000980430     C                   ENDIF
295100980430     C                   ENDIF
295200980429     C**
295300991012     C* SPOSTO L'IMPORTO DEL CAMPO VARIAD NEL CAMPO VARIA SOLO SE E' STATO USATO
295400991012     C  N12              Z-ADD     VARIAD        VARIA
295500991012     C*
295600991012     C** MINIMO
295700941112     C     AMIN          IFGT      VARIA
295800991012     C                   Z-ADD     AMIN          VARIA
295900941107     C                   END
296000991012     C** MASSIMO
296100941115     C     AMAX          IFGT      0
296200941112     C     AMAX          IFLT      VARIA
296300991012     C                   Z-ADD     AMAX          VARIA
296400941112     C                   END
296500941115     C                   END
296600000103     C** SE VARIA CON ESENZIONE IVA DEVE AVERE SOLO 2 DCEIMALI
296700000103     C* CERCO LA SIGLA VARIA CORRISPONDENTE
296800000103     C                   MOVEL     TPTFTC        WFTC              1
296900000103     C                   Z-ADD     1             A
297000000103     C     WFTC          LOOKUP    CP(A)                                  10
297100000103     C     *IN10         IFEQ      *ON
297200000103     C     A             OCCUR     DS1P
297300000103     C     §1PVAR        LOOKUP    VES                                    10
297400000103     C     *IN10         IFEQ      *ON
297500000103     C                   MOVE      VARIA         W001A             1
297600000103     C     W001A         IFNE      '0'
297700000103     C     VARIA         ADD       0,001         COMOD2           15 2
297800000103     C                   Z-ADD     COMOD2        VARIA
297900000103     C                   ENDIF
298000000103     C                   ENDIF
298100000103     C                   ENDIF
298200000103     C**
298300941107     C     ENDACD        TAG
298400990923     C*
298500941107     C                   ENDSR
298600941107     C/SPACE
298700941112     C******************************************************
298800980424     C** CALCOLO TARIFFE  PARTICOLARI
298900970521     C******************************************************
299000980428     C     CALCP         BEGSR
299100990721     C** CERO LA TARIFFA PARTICOLARE IN TITPT O CARTELLO
299200980430     C                   MOVEL     TASTC         FISO
299300980506     C                   MOVEL     'S'           WCAR
299400110908      ** se sto calcolando la varia "Q" = Centro storico o ZTL
299500110908      ** non devo cercare la tariffa nella Cartello in mancanza
299600110908      ** di quella del cliente
299700111102      ** E' stata aggiuna una tabella che pilota il recupero della
299800111102      ** tariffa di cartello se la filiale del cliente di tassazione
299900111102      ** è presente nella tabella ztl e la data spedizione è maggiore
300000111102      ** uguale alla data attivazione della tassazione
300100111102     c                   clear                   indztl            3 0
300200110908     c                   If        tastc = 'Q'
300300111102     c                   movel     'N'           WCAR
300400111102      * cerco la filiale del cliente nella tabella ZTL
300500111102     c                   eval      indztl = %lookup(kscfil:sfilztl)
300600111102      * se indice =  a zero cerco con la filiale 999
300700111102    2c                   if        indztl = *zeros
300800111102     c                   eval      indztl = %lookup(999:sfilztl)
300900111102    2c                   endif
301000111102      * imposto il flag della ricerca tariffa di cartello = 'S'  se filiale cliente
301100111102      * abilitata e la data spedizione > = data attivazione tassazione
301200111102    2c                   If        indztl > *zeros and
301300111102    3c                             tasdsp >= sdtaztl(indztl)
301400111102     c                   movel     'S'           WCAR
301500111102    3c                   endif
301600110908     c                   endif
301700110908      *
301800980428     C                   EXSR      CERTPT
301900980428     C** SOLO SE L'HO TROVATA
302000980428    1C     WEND          IFEQ      ' '
302100970521     C                   Z-ADD     ISOPES        TPDPES
302200970521     C                   EXSR      CALACD
302300970521     C                   Z-ADD     VARIA         TASCP
302400980428    1C                   ENDIF
302500970521     C*
302600970521     C                   ENDSR
302700001205     C/SPACE
302800001205     C******************************************************
302900001205     C** CALCOLO ADDIZIONALE DI GESTIONE + ISTAT
303000001205     C******************************************************
303100001205     C     ADGIST        BEGSR
303200001205     C**
303300001205     C*
303400001205     C****************
303500041129     C** ADDIZIONALE DI GESTIONE  anni precedenti       varia "Z"
303600001205     C****************
303700001205     C*
303800031121     C     *LIKE         DEFINE    TASVA1        TASADG
303900001205     C     TAMPAG        IFNE      0
304000001205     C                   Z-ADD     1             A
304100001205     C     §$2VRZ        LOOKUP    SV(A)                                  05
304200001205     C  N05              DO
304300001205     C                   Z-ADD     0             TASADG
304400001205     C* TOTALE IMPONIBILE
304500001205     C                   Z-ADD     O17IMV        IMPADG           15 3
304600001205     C**
304700001205     C                   Z-ADD     0             COMADG           15 3
304800001205     C     IMPADG        MULT      TAMPAG        COMADG
304900001205     C     COMADG        IFGT      0
305000001205     C     COMADG        DIV(H)    100           TASADG
305100001205     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
305200001205     C     *IN12         IFEQ      '0'
305300001205     C     TASADG        ADD       0,5           CAMPOW           11 0
305400001205     C                   Z-ADD     CAMPOW        TASADG
305500001205     C                   END
305600001205     C                   END
305700001205     C*
305800040128     c     tasadg        ifgt      0
305900040128      *
306000001205     C                   Z-ADD     1             A
306100001205     C     ' '           LOOKUP    SV(A)                                  96
306200001205     C   96              MOVEL     §$2VRZ        SV(A)
306300001205     C   96              Z-ADD     TASADG        VA(A)
306400001205     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
306500001205     C  N96              ADD       TASADG        TASPOR
306600001205     C*
306700001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
306800001205     C*
306900001205     C                   CLEAR                   DSLV16
307000001205     C                   MOVEL     'T'           D17FIM
307100001205     C                   Z-ADD     TASPOR        D17POR
307200001205     C                   CALL      'FNLV16R'
307300001205     C                   PARM                    DSLV16
307400001205     C                   PARM                    DTASV
307500040128      *
307600040128     c                   endif
307700001205     C*
307800001205     C                   END
307900001205     C                   END
308000041129     C*
308100041129     C****************
308200041129     C** ADDIZIONALE DI GESTIONE    anno corrente       varia "b"
308300041129     C****************
308400041129     C*
308500041129     c                   clear                   tasadg
308600041129     c                   clear                   impadg
308700041129     c                   clear                   comadg
308800041129
308900041129     C     TAMPPA        IFNE      0
309000041129     C                   Z-ADD     1             A
309100041129     C     §$2VAG        LOOKUP    SV(A)                                  05
309200041129     C  N05              DO
309300041129     C                   Z-ADD     0             TASADG
309400041129     C* TOTALE IMPONIBILE
309500041129     C                   Z-ADD     O17IMV        IMPADG
309600041129     C**
309700041129     C                   Z-ADD     0             COMADG
309800041129     C     IMPADG        MULT      TAMPPA        COMADG
309900041129     C     COMADG        IFGT      0
310000041129     C     COMADG        DIV(H)    100           TASADG
310100041129     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
310200041129     C     *IN12         IFEQ      '0'
310300041129     C     TASADG        ADD       0,5           CAMPOW           11 0
310400041129     C                   Z-ADD     CAMPOW        TASADG
310500041129     C                   END
310600041129     C                   END
310700041129     C*
310800041129     c     tasadg        ifgt      0
310900041129      *
311000041129     C                   Z-ADD     1             A
311100041129     C     ' '           LOOKUP    SV(A)                                  96
311200041129     C   96              MOVEL     §$2VAG        SV(A)
311300041129     C   96              Z-ADD     TASADG        VA(A)
311400041129     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
311500041129     C  N96              ADD       TASADG        TASPOR
311600041129     C*
311700041129     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
311800041129     C*
311900041129     C                   CLEAR                   DSLV16
312000041129     C                   MOVEL     'T'           D17FIM
312100041129     C                   Z-ADD     TASPOR        D17POR
312200041129     C                   CALL      'FNLV16R'
312300041129     C                   PARM                    DSLV16
312400041129     C                   PARM                    DTASV
312500041129      *
312600041129     c                   endif
312700041129     C*
312800041129     C                   END
312900041129     C                   END
313000001205     C**
313100001205     C***************
313200001205     C** ISTAT
313300001205     C***************
313400001205     C*
313500031121     C     *LIKE         DEFINE    TASVA1        TASIAC
313600001205     C                   Z-ADD     1             A
313700001205     C     §$2VRV        LOOKUP    SV(A)                                  05
313800101014    1C  N05TAMrct        IFGT      0
313900001205     C                   Z-ADD     0             TASIAC
314000001205     C*
314100001205     C* CALCOLO ISTAT SU TOTALE IMPONIBILE
314200001205     C*
314300001205     C                   Z-ADD     O17IMV        IMPIAC           15 3
314400101014     C                   Z-ADD     0             PUNIAC            7 4
314500101014
314600101014      * richiamo nuovo pgm per calcolo % istat da aggiungere all'imponibile
314700101014     c
314800101014     c                   clear                   trulistds
314900101014     c                   eval      IISTsca = tamrct
315000101014     c                   eval      IISTdsp = tasdsp
315100101014     c                   call      'TRULISTR'
315200101014     c                   parm                    trulistds
315300101014     c                   IF        OISTerr = *blanks
315400101014     c                   eval      puniac = OISTiac
315500101014     c                   ENDIF
315600101014     c
315700050629     C                   Z-ADD     0             PERIAC           15 6
315800050629     C                   Z-ADD     0             PERIAX           15 4
315900001205     C     PUNIAC        MULT      TAMPRI        PERIAX
316000001205    3C     PERIAX        IFNE      0
316100031121     C     PERIAX        DIV       100           PERIAC
316200001205    3C                   END
316300001205     C** PERCENTUALE DI ISTAT DA APPLICARE
316400001205     C** APPLICO I PUNTI X TRIMESTRE SOLO SE
316500001205     C** LA DATA DI SPEDIZIONE E'
316600001205     C** MAGGIORE O UGUALE ALLA DATA DI SCATTO
316700001205     C** DEI PUNTI NEL TRIMESTRE
316800001205    3C     PERIAC        IFNE      0
316900031121     C                   DIV       100           PERIAC
317000001205     C     IMPIAC        MULT(H)   PERIAC        TASIAC
317100001205    3C                   END
317200001205     C*
317300001205    3C     TASIAC        IFGT      0
317400001205     C*  SENZA DECIMALI
317500001205    4C     *IN12         IFEQ      *OFF
317600001205     C     TASIAC        ADD       0,5           CAMPOW           11 0
317700001205     C                   Z-ADD     CAMPOW        TASIAC
317800001205    4C                   END
317900001205     C*
318000001205     C                   Z-ADD     1             A
318100001205     C     ' '           LOOKUP    SV(A)                                  96
318200001205     C   96              MOVEL     §$2VRV        SV(A)
318300001205     C   96              Z-ADD     TASIAC        VA(A)
318400001205     C  N96              ADD       TASIAC        TASPOR
318500001205     C*
318600001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
318700001205     C*
318800001205     C                   CLEAR                   DSLV16
318900001205     C                   MOVEL     'T'           D17FIM
319000001205     C                   Z-ADD     TASPOR        D17POR
319100001205     C                   CALL      'FNLV16R'
319200001205     C                   PARM                    DSLV16
319300001205     C                   PARM                    DTASV
319400001205     C*
319500001205    3C                   END
319600001205    1C                   ENDIF
319700001205     C*
319800001205     C                   ENDSR
319900030922     C*******************************************************
320000030922     C** CALCOLO DIRITTO DI FATTURAZIONE  + TOTALE IMPONIBILE
320100030922     C******************************************************
320200030922     C     DIRFAT        BEGSR
320300030922     C**
320400030922      * prima verifico se esiste già
320500030922     C                   Z-ADD     1             A
320600030922     C     §$2VRD        LOOKUP    SV(A)                                  05
320700030922     C* SIGLA VARIA NON PRESENTE --> CALCOLO
320800030922     C     *IN05         IFEQ      *OFF
320900030922     C                   Z-ADD     1             A
321000030922     C     ' '           LOOKUP    SV(A)                                  96
321100030922     C   96              MOVEL     §$2VRD        SV(A)
321200030922     C   96              Z-ADD     TASIVDF       VA(A)
321300030922     C  N96              ADD       TASIVDF       TASPOR
321400030922     C*
321500030922     C* RICALCOLO L'IMPONIBILE SOMMANDO IL DIRITTO DI FATTURAZIONE
321600030922     C*
321700030922     C                   CLEAR                   DSLV16
321800030922     C                   MOVEL     'T'           D17FIM
321900030922     C                   Z-ADD     TASPOR        D17POR
322000030922     C                   CALL      'FNLV16R'
322100030922     C                   PARM                    DSLV16
322200030922     C                   PARM                    DTASV
322300030924     C*
322400030924     C                   Z-ADD     O17IMV        TASIMV
322500030924     C*
322600030922     C*
322700030922     C                   ENDIF
322800030922     C*
322900030922     C                   ENDSR
323000980428     C******************************************************
323100980428     C* CERCO TARIFFA PARTICOLARE
323200980428     C******************************************************
323300980428     C     CERTPT        BEGSR
323400980428     C                   CLEAR                   TASCP
323500980428     C                   CLEAR                   WEND
323600980428     C                   MOVEL     *BLANKS       FLGISO            1            FLAG.APPLICAZI.
323700990722     C                   MOVEL     FISO          WFISO
323800980428     C                   Z-ADD     1             A
323900990722     C     WFISO         LOOKUP    CP(A)                                  05
324000980428     C**
324100980428    1C     *IN05         IFEQ      *ON
324200020226     C     A             OCCUR     DS1P
324300980428     C*-----------------------------------------
324400990721     C     KISOT         CHAIN     TITPT01L                           28
324500980528     C** ESISTE TARIFFA DEL CLIENTE
324600980528     C     *IN28         IFEQ      *OFF
324700980528     C     TPTATB        ANDEQ     *BLANKS
324800980428     C                   GOTO      DOPIS2
324900980528     C                   ENDIF
325000980428     C*-----------------------------------------
325100980428     C** MEMORIZZO DATI DA TARIFFA DI CARTELLO
325200980506     C**  SOLO SE HO DETTO CHE LA POSSO USARE E SE C'E'
325300980428     C*-----------------------------------------
325400020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
325500020226     C     WCAR          IFEQ      'S'
325600020226     C     KTPTC         CHAIN     TITPT01L                           28
325700020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
325800020226     C     *IN28         IFEQ      *ON
325900020226     C     TPTATB        OREQ      'A'
326000020226     C                   MOVEL     'S'           WEND
326100020226     C                   GOTO      DOPIS3
326200020226     C                   ENDIF
326300020226      * E SE NON DEVO CARICARE LA CARTELLO ERRORE NON TROVATA TARIFFA PARTICOLARE
326400020226     C                   ELSE
326500020226     C                   MOVEL     'S'           WEND
326600020226     C                   GOTO      DOPIS3
326700020226     C                   ENDIF
326800980430     C     DOPIS2        TAG
326900980430     C* ARROTONDAMENTI E RPV
327000980430     C                   MOVEL     TPTTPG        WTPG
327100090310      * in caso di pod-immage varia "a" imposto come tipo pagamento a colli
327200090310
327300090310     c                   if        fiso = §$2POD
327400090310     C                   MOVEL     '1'           WTPG
327500090310     c                   endif
327600090310
327700980430     C                   MOVEL     TPTRPV        WRPV
327800980430     C                   CLEAR                   WDET
327900980430     C                   Z-ADD     TPTARL        WARL
328000980430     C                   Z-ADD     TPTARF        WARF
328100980430     C                   Z-ADD     TPTARO        WARO
328200980430     C                   EXSR      CALCOL
328300980428     C** NON TROVATA TARIFFA PARTICOLARE
328400980428   X1C                   ELSE
328500980428     C                   MOVEL     'S'           WEND              1
328600980428    1C                   ENDIF
328700020226     C     DOPIS3        ENDSR
328800980312     C******************************************************
328900980312     C** CALCOLO IL VALORE DA MOLTIPLICARE/RPV/ARROTONDAMENTI
329000980312     C******************************************************
329100980312     C     CALCOL        BEGSR
329200980313     C     *LIKE         DEFINE    TAMARL        ARR
329300980313     C     *LIKE         DEFINE    TASIAS        PESTAS
329400980313     C     *LIKE         DEFINE    TASIAS        ISOPES
329500980312     C     *LIKE         DEFINE    TASIAS        ISOPE2
329600980312     C     *LIKE         DEFINE    TPTRPV        WRPV
329700980313     C     *LIKE         DEFINE    TPTARL        WARL
329800980313     C     *LIKE         DEFINE    TPTARF        WARF
329900980313     C     *LIKE         DEFINE    TPTARO        WARO
330000980312     C                   Z-ADD     0             ISOPES
330100980316     C                   SELECT
330200030203
330300030204     c                   When      wFiso = '='
330400030203     c                   Z-Add     TasBan        Isopes
330500030203
330600980429     C     WTPG          WHENEQ    '0'
330700980429     C     WTPG          OREQ      '3'                                          KG
330800020826     C                   Z-ADD     USAPKF        ISOPES
330900980316     C*
331000980429     C     WTPG          WHENEQ    '1'                                          COLLI
331100980312     C                   Z-ADD     TASNCL        ISOPES
331200980316     C*
331300980429     C     WTPG          WHENEQ    '2'                                          SPEDIZ
331400980312     C                   Z-ADD     1             ISOPES
331500980316     C*
331600980429     C     WTPG          WHENEQ    '4'                                          VALORE
331700980429     C     WTPG          OREQ      '5'                                          Q.TA'
331800980312     C                   Z-ADD     TASQFT        ISOPES
331900980429     C*
332000980429     C     WTPG          WHENEQ    'T'                                          TRASPORTO
332100980429     C                   Z-ADD     TASPOR        ISOPES
332200980429     C                   ADD       TASINL        ISOPES
332300980429     C                   EXSR      CERISO
332400980429     C                   ADD       VAISO         ISOPES
332500060414     C                   ADD       VAtsp         ISOPES
332600980316     C                   ENDSL
332700980312     C** MINIMO TASSABILE STD
332800980312     C                   Z-ADD     0             MINTAS
332900980312     C**
333000980312     C                   Z-ADD     1             A
333100980312     C     WTPG          LOOKUP    CTR(A)                                 05
333200980312     C   05A             OCCUR     DSTR
333300980312     C   05              Z-ADD     §TRATA        MINTAS
333400980312     C**
333500980312     C** SOLO PER TAD (PER TPD NON MI SERVE)
333600980428    1C     WDET          IFEQ      'TAD'
333700980312     C                   CLEAR                   MINTAR
333800980312     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
333900980312     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
334000980312     C   05              Z-ADD     §TRATA        MINTAR
334100980428    2C     TAMATA        IFGT      0
334200980312     C                   Z-ADD     TAMATA        MINTAR
334300980428    2C                   ENDIF
334400980428    1C                   ENDIF
334500980312     C**
334600980428     C** RPV SOLO PER TARIFFA 0 E 3
334700980428    1C     §TRRPV        IFEQ      'S'
334800980428     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
334900980428    2C     WDET          IFEQ      'TAD'
335000980428     C     TASVLF        ANDGT     0
335100020218    3C                   DO        200           A
335200980428     C     A             OCCUR     TARDS
335300980428    4C     TSCG          IFGE      ISOPES
335400980428     C                   GOTO      POITAS
335500980428    4C                   END
335600980428    3C                   END
335700980428     C** RICERCO RPV CON PESO REALE
335800980428    3C     TSCG          IFLT      ISOPES
335900980428     C                   SETON                                        94
336000980527     C                   MOVEL     MSG(5)        TASMSG
336100980527     C                   GOTO      FINE
336200980428    3C                   END
336300980428     C     POITAS        TAG
336400980428     C                   Z-ADD     TRPV          WRPV
336500040603     C                   Z-ADD     TRPV          rappv
336600980428    2C                   ENDIF
336700980428     C**
336800980428    2C     TASVLF        IFNE      0
336900980428     C     WRPV          ANDNE     0
337000980428     C                   Z-ADD     0             COMPRV            9 3
337100980428     C     TASVLF        MULT      WRPV          COMPRV
337200980428     C     COMPRV        MULT      100           ISOPE2
337300980428    3C     ISOPE2        IFGT      ISOPES
337400980428     C                   Z-ADD     ISOPE2        ISOPES
337500980428    3C                   ENDIF
337600980428    2C                   ENDIF
337700980428    1C                   ENDIF
337800990923     C**
337900990923     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
338000990923    0C     §TRARR        IFEQ      'S'
338100980313     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
338200980312     C     ISOPES        IFGE      MINTAS
338300980312     C                   Z-ADD     0             ARR
338400980313     C     ISOPES        IFGT      WARL
338500980313     C                   Z-ADD     WARO          ARR
338600980312     C                   ELSE
338700980313     C                   Z-ADD     WARF          ARR
338800980312     C                   END
338900980313     C**
339000980313     C                   Z-ADD     0             RESTO            15 5
339100980312     C                   Z-ADD     0             ISOPEX           13 0
339200980312     C     ARR           IFNE      0
339300980312     C     ISOPES        ANDNE     0
339400980312     C     ISOPES        DIV       ARR           ISOPEX
339500980312     C                   MVR                     RESTO
339600980312     C     RESTO         IFGT      0
339700980312     C     ISOPEX        MULT      ARR           ISOPES
339800980312     C                   ADD       ARR           ISOPES
339900980312     C                   ENDIF
340000980312     C                   ENDIF
340100980312     C                   ENDIF
340200980312     C**
340300980312     C***        GIUARR    TAG
340400980312    0C                   ENDIF
340500980312     C                   ENDSR
340600970521     C******************************************************
340700941107     C** CALCOLA DIRITTO ASSICURATIVO - VARIA E
340800941112     C******************************************************
340900941107     C     CALVE         BEGSR
341000941107     C                   MOVEL     'R'           FISO
341100980506     C                   MOVEL     'N'           WCAR
341200980430     C                   EXSR      CERTPT
341300980430     C     WEND          IFEQ      ' '
341400990922     C** TARIFFA A VALORE TOLGO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DI CONTO
341500990922     C*  E CALCOLO VALORE AL KG (FISSO) PER LA PARTE RESTANTE
341600980428    1C     TPTTPG        IFEQ      'V'
341700980312     C                   CLEAR                   ISOPES
341800990922     C* CONVERTO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DELLA TARIFFA
341900990922     C*
342000990922     C     *LIKE         DEFINE    §GELRP        WLRP
342100990922     C*
342200990922     C     §TADIV        IFNE      §GEDCN
342300990922     C                   CLEAR                   YEUR
342400990922     C                   MOVEL     §GEDCN        YECDVI
342500990922     C                   MOVEL     §TADIV        YECDVO
342600990922     C                   Z-ADD     §GELRP        YECIMI
342700991014     C   12              MOVE      '3'           YECDCO
342800991014     C  N12              MOVE      '0'           YECDCO
342900990922     C*
343000990922     C                   CALL      'YEURCO'
343100990922     C                   PARM                    YEUR
343200990922     C*
343300990922     C     YECESI        IFEQ      ' '
343400990922     C                   Z-ADD     YECIMO        WLRP
343500990922     C                   END
343600991112     C* SE TARIFFE UGUALI VALORIZZO WLRP
343700991112     C                   ELSE
343800991118     C                   Z-ADD     §GELRP        WLRP
343900990922     C                   ENDIF
344000990922     C*
344100990922    2C     TPTVLM        IFGT      WLRP
344200990922     C     TPTVLM        SUB       WLRP          ISOPES
344300020826     C     ISOPES        MULT      USAPKF        ISOPES
344400980312    2C                   ENDIF
344500980312    1C                   ENDIF
344600980430     C**
344700980312     C                   Z-ADD     ISOPES        TPDPES
344800941112     C                   EXSR      CALACD
344900941112     C                   Z-ADD     VARIA         DIRITT
345000941112     C**
345100941112     C     TPTAPL        IFNE      *BLANKS
345200941112     C     TIPBOL        IFEQ      'F'
345300941112     C     TPTAPL        IFEQ      'A'
345400941112     C                   Z-ADD     0             DIRITT
345500941112     C                   END
345600941112     C                   ELSE
345700941112     C     TPTAPL        IFEQ      'P'
345800941112     C                   Z-ADD     0             DIRITT
345900941112     C                   END
346000941112     C                   END
346100941112     C                   END
346200941112     C** TIPO APPLICAZIONE
346300941112     C** P - SOLO PER FRANCHI
346400941112     C** A - SOLO PER ASSEGNATI
346500980430     C                   ENDIF
346600941107     C                   ENDSR
346700941107     C/SPACE
346800941112     C******************************************************
346900941112     C** CALCOLA MAGGIORAZIONE TIPO SERVIZIO
347000941112     C******************************************************
347100941112     C     CALTS         BEGSR
347200980506     C                   MOVEL     'S'           WCAR              1
347300090924     c
347400090924     C* 24/09/09 --> Ora applichiamo sempre la maggiorazine espresso
347500090924     c
347600980506     C* SE TIPO SERVIZIO DELLA TARIFFA = A TIPO SERVIZIO BOLLA
347700980506     C*  NON PRENDO LA TARIFFA DI CARTELLO
347800090922     C***  TAMTSP        IFEQ      TASTSP
347900090922     C***                MOVEL     'N'           WCAR              1
348000090922     C***                ENDIF
348100980506     C* SIGLA VARIA CORRISPONDENTE
348200980506     C                   MOVEL     §5EFTC        FISO
348300980506     C                   EXSR      CERTPT
348400980506     C     WEND          IFEQ      ' '
348500980506     C**
348600980506     C** A TRASPORTO
348700980430     C     TPTTPG        IFEQ      'T'
348800980312     C                   Z-ADD     IMPON         ISOPES
348900941112     C                   END
349000980312     C**
349100980312     C                   Z-ADD     ISOPES        TPDPES
349200941112     C                   EXSR      CALACD
349300941112     C                   Z-ADD     VARIA         WTSP
349400980506     C                   ENDIF
349500941112     C**
349600941112     C                   ENDSR
349700941112     C******************************************************
349800020225     C** CERCA TARIFFA TNTAM PIU' RELATIVA CARTELLO
349900941112     C******************************************************
350000941107     C     CERTAM        BEGSR
350100020225      *
350200941111     C     *LIKE         DEFINE    TAMPRG        PRG2
350300941111     C     *LIKE         DEFINE    TAMKSC        KSC2
350400941111     C     *LIKE         DEFINE    TAMCTR        CTR2
350500061219      * se in tasflo c'è una data riferimento per il recupero della tariffa la imposto
350600061219      * solo temporaneamente al posto della data spedizione
350700061219     c                   clear                   Sav_dsp
350800070108     c                   if        §asdrt  > *zeros
350900061219     c                   eval      Sav_dsp = Tasdsp
351000070108     c***                eval      tasdsp = §asdrt
351100070108     c                   movel     §asdrt        tasdsp
351200061219     c                   endif
351300020225      *
351400941112     C                   SETOFF                                       180717
351500020227     C                   SETOFF                                       0890
351600941111     C                   Z-ADD     0             PRG2
351700941111     C                   Z-ADD     0             KSC2
351800941111     C                   Z-ADD     0             CTR2
351900941107     C                   Z-ADD     0             TAMKSC
352000941107     C                   Z-ADD     0             TAMPRG
352100941107     C                   Z-ADD     0             TAMCTR
352200980605     C     KTAM          SETLL     TNTAM000                               08
352300980605     C* MANCA TARIFFA CLIENTE
352400980605    1C     *IN08         IFEQ      *OFF
352500980605     C                   MOVEL     MSG(3)        TASMSG
352600980527     C                   SETON                                        94
352700980527     C                   GOTO      ENDTAM
352800980605    1C                   ENDIF
352900980605     C**
353000941107     C     SUTAM         TAG
353100980605     C**
353200941107     C     KTAM          READE     TNTAM000                               07
353300980605     c**
353400941107     C** SE HO LETTO UNA TARIFFA SCADUTA (17) E IL
353500980605     C** RECORD SUCCESSIVO MI ACCENDE FINE FILE
353600980605     C** chaino vecchio progressivo per tassare con tariffa scaduTa
353700980605    1C     *IN07         IFEQ      *ON
353800980605     C   17KTAM2         CHAIN     TNTAM000                           18
353900980605     c**
354000980605     C     *IN17         IFEQ      *OFF
354100980605    2C     *IN18         OREQ      *ON
354200980605     C                   MOVEL     MSG(3)        TASMSG
354300980605     C                   SETON                                        94
354400980605    2C                   ENDIF
354500980605     C**
354600980605     C                   GOTO      ENDTAM
354700980605     C**
354800980605     C                   ELSE
354900980605     C**
355000980605     C** SE NON E' TARIFFA ADATTA PER LA BOLLA (ITALIA INVECE CHE ESTER
355100980605     C**  O VICEVERSA) --> RILEGGO COME SE NON AVESSI LETTO
355200980605     C     TAMFIE        IFNE      WBOEST
355300980605     C     TAMFIE        ANDNE     ' '
355400980605     C                   GOTO      SUTAM
355500980605     C                   ENDIF
355600980605    1C                   ENDIF
355700980605     C**
355800980605     C** TARIFFA DA DECORRERE : SEGNALO SE NON C'E' UNA TARIFFA SCADUTA
355900980605     C**    (di fatto non e' possibile perche' se c'e' una da decorrere
356000980605     C**    e ce n'e' una prima e' la tariffa valida dal momento che
356100980605     C**    non e' possibile che esista una buco di decorrenze scadenze
356200980605     C**    tra i vari progressivi)
356300980605    1C     TASDSP        IFLT      TAMDDT
356400980605     C   17KTAM2         CHAIN     TNTAM000                           18
356500980605     C* NON C'E' TARIFFA SCADUTA O NON TROVATA DI NUOVO
356600980605    2C     *IN17         IFEQ      *OFF
356700980605     C     *IN18         OREQ      *ON
356800980605     C                   MOVEL     MSG(10)       TASMSG
356900980605     C                   SETON                                        94
357000980605    2C                   ENDIF
357100980605     C**
357200941107     C                   GOTO      ENDTAM
357300980605    1C                   END
357400980605     C**
357500980605     C** TARIFFA SCADUTA: DATA SPEDIZIONE MAGGIORE DELLA DATA  SCADENZA
357600980605    1C     TASDSP        IFGT      TAMDST
357700941107     C                   SETON                                        17
357800980605     C                   Z-ADD     TAMKSC        KSC2
357900980605     C                   Z-ADD     TAMCTR        CTR2
358000980605     C                   Z-ADD     TAMPRG        PRG2
358100941107     C                   GOTO      SUTAM
358200980605    1C                   ENDIF
358300020225     C*
358400020225     C** CERCO LA CARTELLO RELATIVA ALLA TARIFFA DEL CLIENTE
358500020227     C*  SE NON HO GIA' UNA TARIFFA DI CARTELLO E SE 94 SPENTO
358600020227     C     ENDTAM        TAG
358700020227     C*
358800020225     C*
358900020227    0C     *IN94         IFEQ      *OFF
359000020227      *
359100020227     C                   SETOFF                                       90
359200020227     C                   Z-ADD     1             T
359300020227     C     TAMKSC        LOOKUP    TAC(T)                                 90
359400020227      *
359500020227      * PER 90 SPENTO VUOL DIRE CHE NON HO TROVATO NESSUNA CARTELLO
359600020227    1C     *IN90         IFEQ      *OFF
359700020319     C***                  CLEARWNET
359800020225     C                   MOVEL     TAMFLO        DSTA01
359900020225     C*
360000020319     C**                   SELEC
360100020225     C* VERIFICO IL TIPO SERVIZIO
360200020319     C**         TAMTSP    WHEQ 'P'
360300020319     C**                   MOVEL'PPT'     WNET    3
360400020225     C* VERIFICO SE TARIFFA ITALIA VERIFICO SE NETWORK DPD
360500020319     C**         TAMFIE    WHEQ 'I'
360600020319     C**         §TADPD    ANDEQ'S'
360700020319     C**                   MOVEL'DPD'     WNET
360800020225     C* VERIFICO SE TARIFFA ESTERA VERIFICO SE NETWORK FEDEX
360900020319     C**         TAMFIE    WHEQ 'E'
361000020319     C**         §TAFED    ANDEQ'S'
361100020319     C**                   MOVEL'FED'     WNET
361200020225     C* VERIFICO SE TARIFFA EUROEXPRESS
361300020319     C**         TAMFIE    WHEQ 'E'
361400020319     C**                   MOVEL'EEX'     WNET
361500020225     C* VERIFICO SE TARIFFA ITALIA
361600020319     C**         TAMFIE    WHEQ 'I'
361700020319     C**                   MOVEL'COR'     WNET
361800020225     C*
361900020319     C**                   ENDSL
362000020225     C* SE NETWORK UGUALE A BLANK CERCO LA TARIFFA DI CARTELLO CORRIERE
362100020319     C**         WNET      IFEQ *BLANK
362200020319     C**                   MOVEL'COR'     WNET
362300020319     C**                   END
362400020225     C* AGGANCIO LA TABELLA DELLE TARIFFE DI CARTELLO
362500020319     C                   EXSR      CERTCA
362600020319     C**                   SETOF                     90
362700020319     C**         WNET      LOKUPNTW,T                    90
362800020225     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
362900020319     C**         *IN90     IFEQ *OFF
363000020319     C**                   MOVELMSG,9     TASMSG
363100020319     C**                   GOTO ENDTA2
363200020319     C**                   ENDIF
363300020319    1C                   ENDIF
363400020227     C*
363500020319     C**                   Z-ADDTAC,T     CARKSC
363600020319     C**                   Z-ADDCTC,T     CARCTR
363700020319     C**                   Z-ADDPRC,T     CARPRG
363800020319     C**                   MOVE DIC,T     DIVCAR
363900020227     C*
364000020227    0C                   ENDIF
364100061220      * se impostato il campo del salvataggio della data spedizione
364200061220     c                   if        Sav_dsp <> *zeros
364300061220     c                   eval      tasdsp = Sav_dsp
364400061220     c                   clear                   Sav_dsp
364500061220     c                   endif
364600020225     C*
364700941107     C*
364800020227     C     ENDTA2        ENDSR
364900020618     C******************************************************
365000020618     C* RICERCA DETTAGLIO SU TUTAD
365100020618     C******************************************************
365200020618     C     RICTAD        BEGSR
365300020618     C     KTAD          SETLL     TITAD000                               09
365400020618     C** 49  TROVATA TARIFFA DEL CAP   DI ARRIVO
365500020618     C   09              SETON                                        49
365600020618     C**
365700020618    1C     *IN09         IFEQ      *OFF
365800020618     C                   MOVEL     *BLANKS       COMNAZ
365900020618     C                   MOVEL     *BLANKS       COMCAP
366000020618     C     KTAD          SETLL     TITAD000                               09
366100020618    2C     *IN09         IFEQ      *OFF
366200020618     C*
366300020618     C                   Z-ADD     1             A
366400020618     C     COMCTS        LOOKUP    CTS(A)                                 05
366500020618     C**
366600020618     C* SE TROVATO COD.TASSAZIONE
366700020618    3C     *IN05         IFEQ      *ON
366800020618     C     A             OCCUR     DSCT
366900020618     C                   MOVEL     §CTRAP        COMCTS
367000020618     C** CARICO REGIONE AL POSTO DEL CODICE DI TASSAZIONE
367100020618     C     KTAD          SETLL     TITAD000                               09
367200020618    3C                   ENDIF
367300020618     C**
367400020618    3C     *IN09         IFEQ      *OFF
367500020618     C*
367600020618     C     WBOEST        IFEQ      'E'
367700020618     C                   MOVEL     'EE'          COMCTS
367800020618     C                   ELSE
367900020618     C                   MOVEL     'IT'          COMCTS
368000020618     C                   END
368100020618     C     KTAD          SETLL     TITAD000                               09
368200020618    3C                   ENDIF
368300020618    2C                   ENDIF
368400020618    1C                   ENDIF
368500020618     C                   ENDSR
368600941107     C/SPACE
368700941112     C******************************************************
368800990721     C** CERCA TARIFFA TITAD DETTAGLIO E CARICA DS
368900941112     C******************************************************
369000941107     C     CARTAR        BEGSR
369100941112C    C                   SETOFF                                       495694
369200941112     C                   MOVEL     TASCAD        COMCAP            9
369300990923     C                   MOVEL     TASNZD        COMNAZ            3
369400941112     C                   MOVEL     TASCTS        COMCTS
369500020618     C                   MOVEL     TASLNP        WLNP
369600020618     C                   EXSR      RICTAD
369700020618     C   09              GOTO      LATAR
369800020618     C**
369900020618     C** SE NON TROVATO TITAD CON LA LINEA DI PARTENZA, CERCO CON LA
370000020618     C**  REGIONE TROVATA DA ORGANIGRANNA DI TASLNP
370100020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
370200020618     C     TASLNP        IFNE      SAVLNP
370300020618     C     TASLNP        CHAIN     AZORG01L                           31
370400020618     C   31              CLEAR                   ORGDE7
370500020618     C                   MOVEL     ORGDE7        OG147
370600020618     C                   MOVEL     TASLNP        SAVLNP
370700020618     C                   ENDIF
370800020618     C**
370900020618     C                   MOVEL     TASCAD        COMCAP            9
371000020618     C                   MOVEL     TASNZD        COMNAZ            3
371100020618     C                   MOVEL     TASCTS        COMCTS
371200020618     C                   MOVEL     §OGRET        WLNP
371300020618     C                   EXSR      RICTAD
371400020618     C   09              GOTO      LATAR
371500020618     C**
371600020618     C** SE NON TROVATO TITAD CON LA REGIONE DI TASLNP, CERCO CON LA
371700020618     C**  LINEA ITALIA 950
371800020618     C                   MOVEL     TASCAD        COMCAP            9
371900020618     C                   MOVEL     TASNZD        COMNAZ            3
372000020618     C                   MOVEL     TASCTS        COMCTS
372100020618     C                   MOVEL     950           WLNP
372200020618     C                   EXSR      RICTAD
372300020618     C   09              GOTO      LATAR
372400941112     C*
372500941112     C*--------------------TARIFFA REVERSIBILE----------------------------------
372600941107     C     TIPBOL        CABNE     'A'           MANTAR
372700941107     C** TARIFFA REVERSIBILE SOLO PER RITORNO(TBL=A)
372800941107     C     TASLNP        CABEQ     68            MANTAR
372900941107     C** NO TARIFFA REVERSIBILE PER 68
373000981027     C     KSCFIL        IFEQ      888
373100981027     C                   Z-ADD     TASLNA        KFIL
373200981027     C                   ELSE
373300981027     C                   MOVEL     KSCFIL        KFIL
373400981027     C                   ENDIF
373500981008     C**
373600941107     C                   SETON                                        56
373700941112     C                   MOVEL     TASCAM        COMCAP
373800990923     C                   MOVEL     TASNZM        COMNAZ
373900941123     C                   MOVEL     TASMCT        COMCTS
374000020618     C                   MOVEL     KFIL          WLNP
374100020618     C                   EXSR      RICTAD
374200020618     C   09              GOTO      LATAR
374300020618     C*
374400020618     C** SE NON TROVATO TITAD CON LA LINEA CLIENTE, CERCO CON LA
374500020618     C**  REGIONE TROVATA DA ORGANIGRANNA DELLA LIENA CLIENTE
374600020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
374700020618     C     KFIL          IFNE      SAVLNP
374800020618     C     KFIL          CHAIN     AZORG01L                           31
374900020618     C   31              CLEAR                   ORGDE7
375000020618     C                   MOVEL     ORGDE7        OG147
375100020618     C                   MOVEL     KFIL          SAVLNP
375200020618     C                   ENDIF
375300020618     C*
375400020618     C                   MOVEL     TASCAM        COMCAP
375500020618     C                   MOVEL     TASNZM        COMNAZ
375600020618     C                   MOVEL     TASMCT        COMCTS
375700020618     C                   MOVEL     §OGRET        WLNP
375800020618     C                   EXSR      RICTAD
375900020618     C   09              GOTO      LATAR
376000020618     C**
376100020618     C** SE NON TROVATO TITAD CON LA REGIONE DI KFIL, CERCO CON LA
376200020618     C**  LINEA ITALIA 950
376300020618     C                   MOVEL     TASCAM        COMCAP
376400020618     C                   MOVEL     TASNZM        COMNAZ
376500020618     C                   MOVEL     TASMCT        COMCTS
376600020618     C                   MOVEL     950           WLNP
376700020618     C                   EXSR      RICTAD
376800020618     C   09              GOTO      LATAR
376900020618     C**
377000020618     C**         KTDREV    SETLLTITAD000                 09
377100020618     C** 09                SETON                     49
377200020618     C** 09                GOTO LATAR
377300020618     C**
377400020618     C**                   MOVEL*BLANKS   COMNAZ
377500020618     C**                   MOVEL*BLANKS   COMCAP
377600020618     C**         KTDREV    SETLLTITAD000                 09
377700020618     C** 09                GOTO LATAR
377800020618     C**
377900020618     C**                   Z-ADD1         A
378000020618     C**         COMCTS    LOKUPCTS,A                    05
378100020618     C**N05                GOTO POIT2
378200020618     C**         A         OCUR DSCT
378300020618     C**                   MOVEL§CTRAP    COMCTS
378400020618     C**         KTDREV    SETLLTITAD000                 09
378500020618     C** 09                GOTO LATAR
378600020618     C**         POIT2     TAG
378700020618     C**         WBOEST    IFEQ 'E'
378800020618     C**                   MOVEL'EE'      COMCTS
378900020618     C**                   ELSE
379000020618     C**                   MOVEL'IT'      COMCTS
379100020618     C**                   END
379200020618     C**         KTDREV    SETLLTITAD000                 09
379300020618     C** 09                GOTO LATAR
379400941107     C** NON TROVATO NEMMENO LA TARIFFA REVERSIBILE
379500941107     C     MANTAR        TAG
379600980527     C     *IN09         IFEQ      *OFF
379700980527     C                   SETON                                        94
379800980527     C                   MOVEL     MSG(8)        TASMSG
379900980527     C                   GOTO      FINE
380000980527     C                   ENDIF
380100941107     C** MANCA TARIFFA
380200941107     C     LATAR         TAG
380300030902     C                   EXSR      CARDST
380400941112     C** CARICA DS DELLA TARIFFA
380500941107     C                   ENDSR
380600941107     C/SPACE
380700941112     C******************************************************
380800941107     C** CARICA DS TARIFFA
380900941112     C******************************************************
381000941107     C     CARDST        BEGSR
381100980514     C     *LIKE         DEFINE    TADRPV        RAPPV
381200980514     C**
381300020218     C                   DO        200           A
381400941112     C     A             OCCUR     TARDS
381500941112     C                   CLEAR                   TARDS
381600941112     C                   END
381700941112     C                   Z-ADD     0             RAPPV
381800980514     C                   CLEAR                   WELAMT
381900941112     C**
382000980514    1C                   DO        *HIVAL        A
382100980514     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
382200980514    2C     WELAMT        IFNE      'S'
382300020618     C** 56      KTDREV    READETITAD000                 15
382400020618     C**N56      KTAD      READETITAD000                 15
382500020618     C     KTAD          READE     TITAD000                               15
382600941107     C   15              GOTO      ENDDST
382700980514    2C                   ENDIF
382800980514     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
382900980514    2C     WELAMT        IFEQ      'S'
383000980515     C                   MOVEL     'E'           WELAMT            1
383100980514    2C                   ENDIF
383200980514     C**
383300941107     C     A             OCCUR     TARDS
383400980514     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
383500980514     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
383600980514     C**
383700941120     C                   Z-ADD     TADLNP        TLNP
383800941107     C                   MOVEL     TADCTS        TCTS
383900990721     C                   MOVEL     TADNAZ        TNAZ
384000941110     C                   MOVEL     TADCAP        TCAP
384100941107     C                   Z-ADD     TADCTR        TCTR
384200941107     C                   Z-ADD     TADRPV        TRPV
384300941122     C                   Z-ADD     TADMIN        TMIN
384400941122     C                   Z-ADD     TADMAX        TMAX
384500040603     C***!!!****         Z-ADD     TADRPV        RAPPV
384600980514     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
384700980514    2C     WELAMT        IFNE      'E'
384800980514     C**
384900980514    3C     TADSGL        IFEQ      MINTAS
385000980514     C                   MOVEL     'E'           WELAMT
385100980514   X3C                   ELSE
385200980514     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
385300980514     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
385400980514     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
385500980514     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
385600980515    4C     TADSGL        IFGT      MINTAS
385700980514     C                   MOVEL     'S'           WELAMT
385800980514     C                   Z-ADD     MINTAS        TSCG
385900980514     C**
386000980522     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
386100980522    5C     TAMATA        IFEQ      0
386200980522     C     TAMATA        ORLE      MINTAS
386300980522     C     TADSGL        ORGT      TAMATA
386400980522     C**
386500980522    6C     UNOCTR        IFEQ      4
386600980514     C     UNOCTR        OREQ      0
386700980514     C     MINTAS        DIV(H)    100           WMTAS            15 5
386800980514     C                   ELSE
386900980514     C                   Z-ADD     MINTAS        WMTAS
387000980522    6C                   ENDIF
387100980514     C* TARIFFA
387200980514     C     WMTAS         MULT(H)   TADITR        TITR
387300980514     C     WMTAS         MULT(H)   TADINO        TINO
387400980522     C                   ELSE
387500980522     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
387600980522     C                   Z-ADD     TADITR        TITR
387700980522     C                   Z-ADD     TADINO        TINO
387800980522    5C                   ENDIF
387900980522    4C                   ENDIF
388000980514    3C                   ENDIF
388100980514    2C                   ENDIF
388200980514     C*
388300980514     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
388400980514    2C     WELAMT        IFNE      'S'
388500980514     C                   Z-ADD     TADSGL        TSCG
388600980514     C                   Z-ADD     TADITR        TITR
388700980514     C                   Z-ADD     TADINO        TINO
388800980514    2C                   ENDIF
388900980514    1C                   ENDDO
389000941107     C     ENDDST        ENDSR
389100941107     C/SPACE
389200941112     C******************************************************
389300941110     C** CARICAMENTO TABELLE SOLO LA 1 VOLTA
389400941112     C******************************************************
389500941110     C     CARTAB        BEGSR
389600980605     C     *LIKE         DEFINE    TAMATA        MINTAS
389700980605     C     *LIKE         DEFINE    TAMATA        MINTAR
389800990923     C     *LIKE         DEFINE    §TADIV        DIVCAR
389900020618     C                   CLEAR                   SAVLNP
390000980605     C**
390100980605     C                   OPEN      TABEL00F
390200980605     C**
390300941110     C                   EXSR      CARQT
390400941110     C** CARICAMENTO IAC
390500941110     C                   EXSR      CARTR
390600941110     C** CARICAMENTO TIPI TARIFFA
390700941110     C                   EXSR      CAR$2
390800941110     C** CARICAMENTO SIGLE VARIE
390900000103     C                   EXSR      CARCC
391000000103     C** CARICAMENTO SIGLE VARIE ESENTI
391100941112     C                   EXSR      CARCV
391200941112     C** CARICAMENTO CAMBI
391300941110     C                   EXSR      CARCT
391400941110     C** CARICAMENTO CODIFICI TASSAZIONE
391500941112     C                   EXSR      CAR5E
391600941112     C** CARICA TIPI SERVIZIO
391700980429     C                   EXSR      CAR1A
391800980429     C** CARICA TIPI INCASSO C/A MITTENTI
391900980603     C                   EXSR      CARTB
392000980603     C** CARICA DS TIPI BOLLA CHE NON PREVEDONO ASSICURAZIONE
392100020319     C                   EXSR      CAREST
392200980605     C** CARICA P.O. ESTERI
392300990917     C                   EXSR      CARDIV
392400990917     C** CARICAMENTO MONETA DI CONTO
392500020320     C                   EXSR      CARPAR
392600020320     C** CARICA TARIFFE DI CARTELLO
392700031120     C                   EXSR      CAR3A
392800031120     C** CARICO VARIE DELLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
392900980605     C                   CLOSE     TABEL00F
393000040505     C**
393100040505     C                   OPEN      TNTBE01L
393200040505     C** caricamento percentuale carburante
393300040505     C                   EXSR      CARPSC
393400080617     C** caricamento filiali addebito lasciato avviso
393500080617     C                   EXSR      CARLAV
393600111102     C** caricamento filiali addebito centro storico da cartello
393700111102     C                   EXSR      CARZTL
393800040505     C**
393900040505     C                   CLOSE     TNTBE01L
394000040505     C**
394100941110     C                   ENDSR
394200941110     C**********************************************************
394300941110     C** CARICAMENTO DS MULTIPLA  CODICI TASSAZIONE
394400941110     C**********************************************************
394500941110     C     CARCT         BEGSR
394600041220     C                   DO        600           A
394700941110     C     A             OCCUR     DSCT
394800941110     C                   CLEAR                   DSCT
394900941110     C                   END
395000941110     C** PULIZIA DS
395100941110     C                   MOVEL     'CT'          COD
395200941110     C     KTAB          SETLL     TABEL                                  07
395300941110     C  N07              SETON                                          H7
395400941110     C   H7              GOTO      FINE
395500941110     C                   Z-ADD     0             K
395600941110     C                   DO        *HIVAL        A
395700941110     C     KTAB          READE     TABEL                                  07
395800941110     C   07              GOTO      ENDCTS
395900941110     C     TBLKEY        IFNE      *BLANKS
396000941110     C                   ADD       1             K
396100941110     C                   MOVEL     TBLKEY        CTS(K)
396200941110     C     K             OCCUR     DSCT
396300941110     C                   MOVEL     TBLUNI        DSCT
396400941110     C                   END
396500941110     C                   END
396600941110     C     ENDCTS        TAG
396700941110     C                   ENDSR
396800941110     C/SPACE
396900941112     C**********************************************************
397000941112     C** CARICAMENTO TIPI SERVIZIO
397100941112     C**********************************************************
397200941112     C     CAR5E         BEGSR
397300941112     C                   DO        10            A
397400941112     C     A             OCCUR     DS5E
397500941112     C                   CLEAR                   DS5E
397600941112     C                   END
397700941112     C** PULIZIA DS
397800941112     C                   MOVEL     '5E'          COD
397900941112     C     KTAB          SETLL     TABEL                                  07
398000941112     C  N07              SETON                                          H7
398100941112     C   H7              GOTO      FINE
398200941112     C                   Z-ADD     0             K
398300941112     C                   DO        *HIVAL        A
398400941112     C     KTAB          READE     TABEL                                  07
398500941112     C   07              GOTO      END5E
398600941112     C     TBLKEY        IFNE      *BLANKS
398700941112     C                   ADD       1             K
398800941112     C                   MOVEL     TBLKEY        TS(K)
398900941112     C     K             OCCUR     DS5E
399000941112     C                   MOVEL     TBLUNI        DS5E
399100941112     C                   END
399200941112     C                   END
399300941112     C     END5E         TAG
399400941112     C                   ENDSR
399500980429     C**********************************************************
399600980429     C** CARICAMENTO TIPI INCASSO C/A
399700980429     C**********************************************************
399800980429     C     CAR1A         BEGSR
399900980430     C                   MOVEA     *ALL'9'       TIC
400000980429     C                   MOVEL     '1A'          COD
400100980429     C                   Z-ADD     0             K
400200980429     C     KTAB          SETLL     TABEL                                  07
400300980429     C     KTAB          READE     TABEL                                  07
400400980430    1C     *IN07         DOWEQ     *OFF
400500980430    2C     TBLKEY        IFNE      *BLANKS
400600980430     C     TBLFLG        ANDEQ     ' '
400700980429     C                   MOVEL     TBLUNI        DS1A
400800980430    3C     §1AFMB        IFEQ      'M'
400900980429     C                   ADD       1             K
401000980429     C                   MOVEL     TBLKEY        TIC(K)
401100980430    3C                   ENDIF
401200980430    2C                   ENDIF
401300980429     C     KTAB          READE     TABEL                                  07
401400980430    1C                   ENDDO
401500980429     C                   ENDSR
401600980603     C**********************************************************
401700980603     C** CARICAMENTO TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
401800980603     C**********************************************************
401900980603     C     CARTB         BEGSR
402000980603     C                   MOVEL     'TB'          COD
402100980603     C                   Z-ADD     0             K
402200980603     C     KTAB          SETLL     TABEL                                  07
402300980603     C     KTAB          READE     TABEL                                  07
402400980603    1C     *IN07         DOWEQ     *OFF
402500980603    2C     TBLKEY        IFNE      *BLANKS
402600980603     C     TBLFLG        ANDEQ     ' '
402700980603     C                   MOVEL     TBLUNI        DSTB
402800980603    3C     §TBFAS        IFEQ      '0'
402900980603     C                   ADD       1             K
403000980603     C                   MOVEL     TBLKEY        TBN(K)
403100980603    3C                   ENDIF
403200980603    2C                   ENDIF
403300980603     C     KTAB          READE     TABEL                                  07
403400980603    1C                   ENDDO
403500980603     C                   ENDSR
403600031120     C**********************************************************
403700031120     C** CARICAMENTO VARIE PER TIPI BOLLE CHE ACCETTANO UNA SINGOLA VARIA
403800031120     C**********************************************************
403900031120     C     CAR3A         BEGSR
404000031120     C                   MOVEL     '3A'          COD
404100031120     C                   Z-ADD     0             K
404200031120     C     KTAB          SETLL     TABEL                                  07
404300031120     C     KTAB          READE     TABEL                                  07
404400031120    1C     *IN07         DOWEQ     *OFF
404500031120    2C     TBLKEY        IFNE      *BLANKS
404600031120     C     TBLFLG        ANDEQ     ' '
404700031120     C                   MOVEL     TBLUNI        DS3A
404800031120    3C     §3ASVA        IFNE      ' '
404900031120     C                   ADD       1             K
405000031120     C                   MOVEL     §3ASVA        T3A(K)
405100031120    3C                   ENDIF
405200031120    2C                   ENDIF
405300031120     C     KTAB          READE     TABEL                                  07
405400031120    1C                   ENDDO
405500031120     C                   ENDSR
405600020319     C**********************************************************
405700020319     C** CARICAMENTO PO ESTERI
405800020319     C**********************************************************
405900020319     C     CAREST        BEGSR
406000020319     C*
406100020319     C                   CLEAR                   K
406200050504     C                   CLEAR                   KK                4 0
406300020319     C**
406400020319     C                   CLEAR                   CC
406500020319     C     *LOVAL        SETLL     AZORG01L
406600020319     C                   READ      AZORG01L                               07
406700020319    1C     *IN07         DOWEQ     *OFF
406800020319     C** FIL O AGENZIA
406900020319    2C     ORGFAG        IFEQ      'F'
407000020319     C     ORGFAG        OREQ      'A'
407100020319     C* NON ANNULLATO
407200020319    3C     ORGFVA        IFEQ      ' '
407300020319    4C                   MOVEL     ORGDE3        OG143
407400050504     c* p.o. esteri senza dpd
407500020319     C     §OGNTW        IFEQ      'EEX'
407600020319     C     §OGNTW        OREQ      'EUP'
407700020404     C*******    §OGNTW    OREQ 'DPD'
407800020319     C     §OGNTW        OREQ      'FED'
407900020319     C                   ADD       1             K
408000020319     C                   MOVEL     ORGFIL        EST(K)
408100020319    4C                   ENDIF
408200050504     c* p.o. estero totale, anche con DPD
408300050504    4C     §OGNTW        IFEQ      'EEX'
408400050504     C     §OGNTW        OREQ      'EUP'
408500050504     C     §OGNTW        OREQ      'DPD'
408600050504     C     §OGNTW        OREQ      'FED'
408700050504     C                   ADD       1             KK
408800050504     C                   MOVEL     ORGFIL        ESTtot(KK)
408900050504    4C                   ENDIF
409000020319    3C                   ENDIF
409100020319    2C                   ENDIF
409200020319     C**
409300020319     C                   READ      AZORG01L                               07
409400020319    1C                   ENDDO
409500020319     C                   ENDSR
409600941110     C********************************************************
409700941110     C** CARICAMENTO SIGLE VARIE
409800941110     C********************************************************
409900941110     C     CAR$2         BEGSR
410000941110     C                   CLEAR                   DS$2
410100941110     C                   MOVEL     '$2'          COD
410200941110     C                   MOVEL     *BLANKS       KEY
410300941110     C                   MOVEL     '1'           KEY
410400941110     C     KTABC         CHAIN     TABEL                              07
410500941110     C   07              SETON                                          H7
410600941110     C   H7              GOTO      FINE
410700941110     C                   MOVEL     TBLUNI        DS$2
410800941110     C                   ENDSR
410900000103     C********************************************************
411000000103     C** CARICAMENTO SIGLE VARIE  ESENTI IVA
411100000103     C********************************************************
411200000103     C     CARCC         BEGSR
411300000103     C                   CLEAR                   DSCC
411400020416     C                   CLEAR                   KK
411500000103     C                   MOVEL     'CC'          COD
411600000103     C                   MOVEL     *BLANKS       KEY
411700000103     C                   MOVEL     'VARIE'       KEY
411800000103     C     KTABC         SETLL     TABEL                                  07
411900020416     C**N07                GOTO ENDCC
412000000103     C                   DO        *HIVAL
412100000103     C     KTAB          READE     TABEL                                  07
412200000103     C   07              LEAVE
412300000103     C                   MOVEL     TBLUNI        DSCC
412400000103     C* CONTROLLO SE VARIA ESENTE CARICO LA SIGLA IN TABELLA
412500000103     C     §CCJEI        IFNE      *BLANKS
412600000103     C                   ADD       1             K
412700000103     C                   MOVE      TBLKEY        VES(K)
412800000103     C                   ENDIF
412900020416     C* CONTROLLO SE DA RESTITUIRE CON SIGAL VARIA E IMPORTO =0
413000020416     C     §CCSV0        IFEQ      'S'
413100050504     C                   ADD       1             KK                4 0
413200020416     C                   MOVE      TBLKEY        SV0(KK)
413300020416     C                   ENDIF
413400000103     C                   ENDDO
413500000103     C                   Z-ADD     0             K
413600020416     C                   Z-ADD     0             KK
413700000103     C     ENDCC         ENDSR
413800941112     C********************************************************
413900941112     C** CARICAMENTO CAMBI
414000941112     C********************************************************
414100941112     C     CARCV         BEGSR
414200941116     C                   Z-ADD     0             K
414300941116     C                   DO        50            A
414400941116     C     A             OCCUR     DSCV
414500941112     C                   CLEAR                   DSCV
414600941116     C                   END
414700941116     C                   MOVEL     *BLANKS       CV
414800941116     C*
414900941112     C                   MOVEL     'CV'          COD
415000941116     C     KTAB          SETLL     TABEL                                  07
415100941116     C  N07              GOTO      ENDCV
415200941116     C                   DO        *HIVAL        A
415300941116     C     KTAB          READE     TABEL                                  07
415400941116     C   07              GOTO      ENDCV
415500941116     C     TBLKEY        IFNE      *BLANKS
415600941116     C                   ADD       1             K
415700941116     C                   MOVEL     TBLKEY        CV(K)
415800941116     C     K             OCCUR     DSCV
415900941116     C                   MOVEL     TBLUNI        DSCV
416000941116     C                   END
416100941116     C                   END
416200941112     C     ENDCV         ENDSR
416300941110     C******************************************************
416400941110     C** CARICAMENTO TIPI TARIFFA
416500941110     C******************************************************
416600941110     C     CARTR         BEGSR
416700030205     C                   DO        50            A
416800941110     C     A             OCCUR     DSTR
416900941110     C                   CLEAR                   DSTR
417000941110     C                   END
417100941110     C**
417200941110     C                   MOVEL     'TR'          COD
417300941112     C                   Z-ADD     0             K
417400941110     C     KTAB          SETLL     TABEL                                  07
417500941110     C  N07              SETON                                          H7
417600941110     C   H7              GOTO      FINE
417700941110     C                   DO        *HIVAL        A
417800941110     C     KTAB          READE     TABEL                                  07
417900941110     C   07              GOTO      TAB1
418000941110     C     TBLKEY        IFNE      *BLANKS
418100941110     C                   ADD       1             K
418200941110     C                   MOVEL     TBLKEY        CTR(K)
418300941110     C     K             OCCUR     DSTR
418400941110     C                   MOVEL     TBLUNI        DSTR
418500941110     C                   END
418600941110     C                   END
418700941110     C     TAB1          ENDSR
418800941110     C**********************************************************
418900020221     C** MEMORIZZA
419000941110     C** TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
419100941110     C**********************************************************
419200941110     C     CARPAR        BEGSR
419300020225      *
419400020225     C     *LIKE         DEFINE    TAMKSC        CARKSC
419500020225     C     *LIKE         DEFINE    TAMCTR        CARCTR
419600020225     C     *LIKE         DEFINE    TAMPRG        CARPRG
419700020221      *
419800020222      * RICHIAMO IL TRUL27R PER OGNI NETWORK PER CARICARMI LE TARIFFE DI CARTELLO IN SCHIERA
419900020221      *
420000020221     C* CORRIERE BARTOLINI
420100020221     C                   CLEAR                   TRUL27
420200020221     C                   MOVEL     'COR'         I27NTW
420300020222     C                   EXSR      SRTRUL
420400020221      * POSTE
420500020222     C                   CLEAR                   TRUL27
420600020222     C                   MOVEL     'PPT'         I27NTW
420700020222     C                   EXSR      SRTRUL
420800020222      * DPD
420900020222     C                   CLEAR                   TRUL27
421000020222     C                   MOVEL     'DPD'         I27NTW
421100020222     C                   EXSR      SRTRUL
421200020222      * FEDEX
421300020222     C                   CLEAR                   TRUL27
421400020222     C                   MOVEL     'FED'         I27NTW
421500020222     C                   EXSR      SRTRUL
421600020222      * EUROEXPRESS
421700020222     C                   CLEAR                   TRUL27
421800020226     C                   MOVE      'L'           I27TLA
421900020222     C                   MOVEL     'EEX'         I27NTW
422000020222     C                   EXSR      SRTRUL
422100020226      *
422200020226     C                   CLEAR                   CARKSC
422300020226     C                   CLEAR                   CARCTR
422400020226     C                   CLEAR                   CARPRG
422500020222      *
422600020222      ** CARICO LE TARIFFE PARTICOLARI
422700941111     C                   EXSR      CAR1P
422800020222      *
422900941110     C                   ENDSR
423000040505     C**********************************************************
423100040505     C** CARICAMENTO SCHIERA SUPPLEMEMNTO CARBURANTE
423200040505     C**********************************************************
423300040505     C     CARPSC        BEGSR
423400040505
423500040507     C                   CLEAR                   KX
423600040505
423700040505     C                   MOVEL     'PSC'         KEYTBE
423800040505     C     KEYTBE        SETLL     TNTBE01L
423900040505     C                   DO        *HIVAL
424000040505     C     KEYTBE        READE     TNTBE01L
424100040505
424200040505     C                   IF        %EOF(TNTBE01L)
424300040505     C                   LEAVE
424400040505     C                   ENDIF
424500040505
424600040505     C                   IF        TBEATB = ' '
424700040507     C                   ADD       1             KX
424800040507     C                   IF        KX <= 999
424900040505     C                   MOVEL     TBEUNI        DPSC
425000040507     C                   Z-ADD     §PSCDAD       DS_DSC
425100040507     C                   Z-ADD     §PSCPER       DS_PSC
425200040507     C                   MOVE      WDSPSC        DSCA(KX)
425300040507     C                   ENDIF
425400040505     C                   ENDIF
425500040505
425600040505     C                   ENDDO
425700040507
425800040507     C                   SORTA     DSCA
425900040505
426000040505     C                   ENDSR
426100080617      *****************************************************************
426200080617      ** CARICAMENTO SCHIERA FILIALI ABILITATE ADDEBITO LASCIATO AVVISO
426300080617      *****************************************************************
426400080617     C     CARLAV        BEGSR
426500080617
426600080617     C                   CLEAR                   KX
426700080617
426800080617     C                   MOVEL     'LAV'         KEYTBE
426900080617     C     KEYTBE        SETLL     TNTBE01L
427000080617     C                   DO        *HIVAL
427100080617     C     KEYTBE        READE     TNTBE01L
427200080617
427300080617     C                   IF        %EOF(TNTBE01L)
427400080617     C                   LEAVE
427500080617     C                   ENDIF
427600080617
427700080617     C                   IF        TBEATB = ' '
427800080617     C                   ADD       1             KX
427900080617     C                   IF        KX <= 999
428000080617     C                   MOVEL     TBEUNI        DLAV
428100080617     C                   EVAL      SFILLAV(KX) = %dec(tbeke1:3:0)
428200080618     C                   EVAL      SDTALAV(KX) = D§LAVTAS
428300080617     C                   ENDIF
428400080617     C                   ENDIF
428500080617
428600080617     C                   ENDDO
428700080617
428800080617
428900080617     C                   ENDSR
429000111102      ******************************************************************
429100111102      ** CARICAMENTO SCHIERA FILIALI ADDEBITO DA CARTELLO CENTRO STORICO
429200111102      ******************************************************************
429300111102     C     CARZTL        BEGSR
429400111102
429500111102     C                   CLEAR                   KX
429600111102
429700111102     C                   MOVEL     'ZTL'         KEYTBE
429800111102     C     KEYTBE        SETLL     TNTBE01L
429900111102     C                   DO        *HIVAL
430000111102     C     KEYTBE        READE     TNTBE01L
430100111102
430200111102     C                   IF        %EOF(TNTBE01L)
430300111102     C                   LEAVE
430400111102     C                   ENDIF
430500111102
430600111102     C                   IF        TBEATB = ' '
430700111102     C                   ADD       1             KX
430800111102     C                   IF        KX <= 999
430900111102     C                   MOVEL     TBEUNI        DZTL
431000111102     C                   EVAL      SFILZTL(KX) = %dec(tbeke1:3:0)
431100111102     C                   EVAL      SDTAZTL(KX) = D§ZTLTAS
431200111102     C                   ENDIF
431300111102     C                   ENDIF
431400111102
431500111102     C                   ENDDO
431600111102
431700111102
431800111102     C                   ENDSR
431900020222     C*************************************************************
432000020222     C** RICHIAMO DEL TRUL27R
432100020222     C*************************************************************
432200020222     C     SRTRUL        BEGSR
432300020222      *
432400020222     C                   Z-ADD     TASDCT        I27DTA
432500021122     c                   Movel     TasCtr        I27Ctb
432600020222     C                   CALL      'TRUL27R'
432700020222     C                   PARM                    TRUL27
432800020222      *
432900020222     C     O27ERR        IFEQ      *BLANKS
433000020319     C     O27PRG        ANDNE     *BLANKS
433100020222     C                   ADD       1             T                 2 0
433200020222     C                   MOVE      I27NTW        NTW(T)
433300020222     C                   Z-ADD     O27KSC        TAC(T)
433400020222     C                   Z-ADD     O27CTR        CTC(T)
433500020226     C                   MOVE      O27PRG        PRC(T)
433600020222      *
433700020222      * AGGANCIO LA TARIFFA PER RECUPERARE LA DIVISA
433800020222      *
433900020226     C                   Z-ADD     O27KSC        CARKSC
434000020226     C                   Z-ADD     O27CTR        CARCTR
434100020226     C                   MOVE      O27PRG        CARPRG
434200020222     C     KTAMC         CHAIN     TNTAM00L                           94
434300020222      *
434400020222     C     *IN94         IFEQ      *OFF
434500020222     C                   MOVEL     TAMFLO        DSTA01
434600020222     C                   MOVE      §TADIV        DIC(T)
434700020226      * SE C'E ERRORE VADO A FINE PROGRAMMA
434800020226     C                   ELSE
434900020226     C                   MOVEL     MSG(9)        TASMSG
435000020226     C                   ENDIF
435100020226      *
435200020222      * SE C'E ERRORE VADO A FINE PROGRAMMA
435300020222     C                   ELSE
435400020222     C                   SETON                                        94
435500020222     C                   MOVEL     MSG(9)        TASMSG
435600020222     C                   ENDIF
435700020222      *
435800020222     C                   ENDSR
435900020225     C*************************************************************
436000020225     C** CERCO LA TARIFFA DI CARTELLO
436100020225     C*************************************************************
436200020225     C     CERTCA        BEGSR
436300020225      *
436400020225      * RICHIAMO IL TRUL27R PASANDOGLI LE CARATTERISTICHE DELLA SPEDIZIONE
436500020225      *
436600020318     C**                   CLEARTRUL27
436700020318     C**                   MOVE 'L'       I27TLA
436800020318     C**                   MOVE TASTSP    I27TSP
436900020318     C**                   MOVE TASLNA    I27LNA
437000020318     C**                   MOVE TASLNP    I27LNP
437100020318     C**                   MOVE TASKSC    I27CLI
437200020318     C**                   MOVE TASDCT    I27DTA
437300020318     C**                   CALL 'TRUL27R'
437400020318     C**                   PARM           TRUL27
437500020225      *
437600020318     C**         O27ERR    IFEQ ' '
437700020318     C**                   Z-ADDO27KSC    KSC2
437800020318     C**                   Z-ADDO27CTR    CTR2
437900020318     C**                   Z-ADDO27CTR    CODCTR
438000020318     C**                   MOVE O27PRG    PRG2
438100020225      * CARICO ANCHE I CAMPI  RELATIVI ALLA TARIFFA DI CARTELLO
438200020318     C**                   Z-ADDO27KSC    CARKSC
438300020318     C**                   Z-ADDO27CTR    CARCTR
438400020318     C**                   MOVE O27PRG    CARPRG
438500020225      *
438600020318     C**                   ENDIF
438700020318     C** DALLE SCHIERE CARICO LA TARIFFA DI CARTELLO
438800020318     C                   SETOFF                                       90
438900020319     C                   Z-ADD     1             T
439000020318     C     O27NTW        LOOKUP    NTW(T)                                 90
439100020318     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
439200020318     C     *IN90         IFEQ      *OFF
439300020318     C                   MOVEL     MSG(9)        TASMSG
439400020319     C                   SETON                                        94
439500020318     C                   GOTO      FINE
439600020318     C                   ENDIF
439700020318     C*
439800020318     C                   Z-ADD     TAC(T)        CARKSC
439900020318     C                   Z-ADD     CTC(T)        CARCTR
440000020318     C                   Z-ADD     PRC(T)        CARPRG
440100020318     C                   MOVE      DIC(T)        DIVCAR
440200020225      *
440300020225     C                   ENDSR
440400941114     C*************************************************************
440500941121     C** CARICAMENTO CODICI E TARIFFE PARTICOLARI DI CARTELLO
440600941114     C*************************************************************
440700941110     C     CAR1P         BEGSR
440800941111     C*
440900020114     C                   DO        50            A
441000941112     C     A             OCCUR     DS1P
441100941111     C                   CLEAR                   DS1P
441200941112     C                   END
441300941111     C                   MOVEL     *BLANKS       CP
441400941110     C**
441500941110     C                   MOVEL     '1P'          COD
441600941111     C                   Z-ADD     0             K                 5 0
441700941110     C     KTAB          SETLL     TABEL                                  07
441800941110     C  N07              SETON                                          H7
441900941110     C   H7              GOTO      FINE
442000941110     C                   DO        *HIVAL        A
442100941110     C     KTAB          READE     TABEL                                  07
442200941110     C   07              GOTO      TABCP
442300941110     C     TBLKEY        IFNE      *BLANKS
442400941115     C                   MOVEL     TBLUNI        CAMPO            26
442500941115     C                   MOVE      CAMPO         WFCP              1
442600941115     C     WFCP          IFEQ      'S'
442700941110     C                   ADD       1             K
442800941110     C                   MOVEL     TBLKEY        CP(K)
442900941112     C     K             OCCUR     DS1P
443000941112     C                   MOVEL     TBLUNI        DS1P
443100941112     C**
443200941110     C                   END
443300941110     C                   END
443400941111     C                   ENDDO
443500020222     C*
443600941110     C     TABCP         ENDSR
443700941110     C****************************************************
443800941110     C** CARICAMENTO DATI VARI TASSAZIONE
443900941110     C****************************************************
444000941110     C     CARQT         BEGSR
444100941110     C                   MOVEL     'QT'          COD
444200941110     C                   MOVEL     *BLANKS       KEY
444300941110     C                   MOVEL     '1'           KEY
444400941110     C     KTABC         CHAIN     TABEL                              07
444500941113     C   07              SETON                                        H7
444600941113     C   H7              GOTO      FINE
444700941110     C  N07              MOVEL     TBLUNI        DSQT1
444800941110     C*
444900941110     C                   ENDSR
445000990917     C****************************************************
445100990922     C** CARICAMENTO MONETE DI CONTO / IMPORTI STANDARD
445200990917     C****************************************************
445300990917     C     CARDIV        BEGSR
445400990917     C*****
445500990922      *  RICERCA DELLA MONETA DI CONTO
445600990917     C                   CLEAR                   DSBS02
445700990917     C                   CLEAR                   DGED
445800990922     C*
445900990917     C                   MOVEL     'C'           T02MOD
446000990917     C                   MOVEL     KNSIF         T02SIF
446100990917     C                   MOVEL     'GED'         T02COD
446200991112     C                   MOVEL     '1'           T02KE1
446300990917      *
446400990917     C                   CALL      'TIBS02R'
446500990917     C                   PARM                    KPJBA
446600990917     C                   PARM                    DSBS02
446700990917      *
446800990917     C     T02ERR        IFEQ      *BLANKS
446900990917     C                   MOVEL     T02UNI        DGED
447000990917     C                   ENDIF
447100990922      * RICERCA DEGLI IMPORTI STANDARD NELLA MONETA DI CONTO
447200990922     C                   CLEAR                   DSBS02
447300990922     C                   CLEAR                   DGEI
447400990922     C                   MOVEL     'L'           T02TLA
447500990922     C                   MOVEL     'C'           T02MOD
447600990922     C                   MOVEL     KNSIF         T02SIF
447700990922     C                   MOVEL     'GEI'         T02COD
447800991112     C                   MOVEL     §GEDCN        T02KE1
447900990922      *
448000990922     C                   CALL      'TIBS02R'
448100990922     C                   PARM                    KPJBA
448200990922     C                   PARM                    DSBS02
448300990922      *
448400990922     C     T02ERR        IFEQ      *BLANKS
448500990922     C                   MOVEL     T02UNI        DGEI
448600990922     C                   ENDIF
448700990922      *
448800990917      *
448900990917     C                   ENDSR
449000011127     C**********************************************************
449100011127     C** RECUPERO LA DATA DEL GIORNO
449200011127     C**********************************************************
449300011127     C     RUDATE        BEGSR
449400011127      *
449500011127     C                   TIME                    W0140            14 0
449600011127     C* UDATE IN GGMMAAAA
449700011127     C                   MOVE      W0140         WDTGIO            8 0
449800011127     C*
449900011127     C                   Z-ADD     WDTGIO        G02DAT
450000011127     C                   MOVEL     *BLANK        G02ERR
450100011127     C                   CALL      'XSRDA8'
450200011127     C                   PARM                    WLBDAT
450300011127     C*
450400011127     C* UDATE IN AAAAMMGG
450500011127     C                   Z-ADD     G02INV        WDTGIO
450600011127      *
450700011127     C                   ENDSR
450800011127     C**********************************************************
450900011127     C** RECUPERO LA DATA TASSAZIONE DA USARE PER QUESTA BOLLA
451000011127     C**********************************************************
451100030131     C**!!!RECDAT        BEGSR
451200011127      *
451300030131     C**!!!              SETOFF                                       31
451400011127      * VERIFICO SE DATA VALORIZZATA E SOPRATTUTTO NUMERICA
451500020826     C**         §ASDAT    IFNE *BLANK
451600020826     C**         §ASDAT    ANDNE*ZEROS
451700011127      *
451800020826     C**                   TESTN          §ASDAT     31
451900011127      * SE 31 ACCESO VUOL DIRE CHE È NUMERICO MA VERIFICO ANCHE L'ULTIMO CARATTERE
452000020826     C**         *IN31     IFEQ *ON
452100020826     C**                   MOVE §ASDAT    W01A    1
452200020826     C**         W01A      COMP '0'                  31  31
452300020826     C**                   ENDIF
452400011127      *
452500020826     C**                   ENDIF
452600011127      * SE 31 ACCESO DATA VALIDA ALTRIMENTI PRENDO LA DATA DEL GIORNO
452700020826     C**         *IN31     IFEQ *ON
452800020826     C**                   MOVE §ASDAT    DATTAS  80
452900020826     C**                   ELSE
453000020826     C**                   Z-ADDWDTGIO    DATTAS
453100020826     C**                   ENDIF
453200020618     C*
453300011127      *
453400030203     C**!!!              ENDSR
453500990916     C****************************************************
453600990916     C** CARICAMENTO IMPORTI PASSATI DALLE DS
453700990916     C****************************************************
453800990916     C     SAVDS         BEGSR
453900990916     C*
454000990922     C     *LIKE         DEFINE    TASPOR        TASINL
454100990916     C*
454200990916     C* SALVO LE SCHIERE DELLE VARIE (SIGLE VARIE+IMPORTI)
454300990916     C                   MOVEA     SV            SVW
454400990916     C                   MOVEA     VA            VAW
454500991012     C                   CLEAR                   TASINL
454600991012     C                   CLEAR                   TASDIF
454700990916     C*
454800990916     C* RECUPERO L'IMPORTO DELL'INOLTRO SE C'E' NELLA SCHIERA DELLE VARIE
454900990916     C                   Z-ADD     1             X                 2 0
455000990923     C     §$2FIN        LOOKUP    SV(X)                                  30
455100990916     C* SE LO TROVO VALORIZZO TASINL
455200990923     C   30              Z-ADD     VA(X)         TASINL
455300991111     C  N30              Z-ADD     0             X
455400990921     C*
455500990921     C* RECUPERO L'IMPORTO DEL DIRITTO FISSO  NELLA SCHIERA DELLE VARIE
455600990921     C                   Z-ADD     1             Y                 2 0
455700990921     C     §$2VRH        LOOKUP    SV(Y)                                  30
455800990921     C* SE LO TROVO VALORIZZO TASINL
455900990923     C   30              Z-ADD     VA(Y)         TASDIF           11 3
456000990916     C*
456100990916     C                   ENDSR
456200990921     C****************************************************
456300990921     C** CONVERSIONE DELLE VARIE IN MONETA DELLA TARIFFA
456400990921     C****************************************************
456500990921     C     CNVTAS        BEGSR
456600990921     C*
456700990921     C* SE DIVISA PASSATA (DELLA BOLLA) E' DIVERSA DALLA DIVISA DELLA
456800990921     C* TARIFFA CONVERTO IN DIVISA TARIFFA
456900990921     C                   Z-ADD     1             A
457000990923    1C                   DO        20            A
457100990923    2C     VA(A)         IFNE      *ZEROS
457200990921     C                   CLEAR                   YEUR
457300011127     C                   MOVEL     DIVINP        YECDVI
457400011127     C                   MOVEL     DIVOUT        YECDVO
457500990921     C                   Z-ADD     VA(A)         YECIMI
457600991014     C   12              MOVE      '3'           YECDCO
457700991014     C  N12              MOVE      '0'           YECDCO
457800990921     C*
457900990921     C                   CALL      'YEURCO'
458000990921     C                   PARM                    YEUR
458100990921     C*
458200990923    3C     YECESI        IFEQ      ' '
458300990921     C                   Z-ADD     YECIMO        VA(A)
458400990923    3C                   END
458500990923    2C                   END
458600990923    1C                   END
458700990921     C* CONVERTO L'IMPORTO DEL PORTO
458800990921     C* SE PORTO DIVERSO DA 0
458900990923    1C     TASPOR        IFGT      0
459000990921     C                   CLEAR                   YEUR
459100011127     C                   MOVEL     DIVINP        YECDVI
459200011127     C                   MOVEL     DIVOUT        YECDVO
459300990921     C                   Z-ADD     TASPOR        YECIMI
459400991014     C   12              MOVE      '3'           YECDCO
459500991014     C  N12              MOVE      '0'           YECDCO
459600990921     C*
459700990921     C                   CALL      'YEURCO'
459800990921     C                   PARM                    YEUR
459900990921     C*
460000990923    2C     YECESI        IFEQ      ' '
460100990921     C                   Z-ADD     YECIMO        TASPOR
460200990921     C                   ELSE
460300990921     C                   MOVEL     MSG(10)       TASMSG
460400990921     C                   GOTO      FINE
460500990923    2C                   END
460600990923    1C                   ENDIF
460700990921     C* SE INOLTRO DIVERSO DA 0
460800990923    1C     TASINL        IFGT      0
460900990921     C                   CLEAR                   YEUR
461000011127     C                   MOVEL     DIVINP        YECDVI
461100011127     C                   MOVEL     DIVOUT        YECDVO
461200990921     C                   Z-ADD     TASINL        YECIMI
461300991014     C   12              MOVE      '3'           YECDCO
461400991014     C  N12              MOVE      '0'           YECDCO
461500990921     C*
461600990921     C                   CALL      'YEURCO'
461700990921     C                   PARM                    YEUR
461800990921     C*
461900990923    2C     YECESI        IFEQ      ' '
462000990921     C                   Z-ADD     YECIMO        TASINL
462100990921     C                   ELSE
462200990921     C                   MOVEL     MSG(10)       TASMSG
462300990921     C                   GOTO      FINE
462400990923    2C                   END
462500990923    1C                   ENDIF
462600990921     C* SE DIRITTO FISSO DIVERSO DA 0
462700990923    1C     TASDIF        IFGT      0
462800990921     C                   CLEAR                   YEUR
462900011127     C                   MOVEL     DIVINP        YECDVI
463000011127     C                   MOVEL     DIVOUT        YECDVO
463100990921     C                   Z-ADD     TASDIF        YECIMI
463200991014     C   12              MOVE      '3'           YECDCO
463300991014     C  N12              MOVE      '0'           YECDCO
463400990921     C*
463500990921     C                   CALL      'YEURCO'
463600990921     C                   PARM                    YEUR
463700990921     C*
463800990923    2C     YECESI        IFEQ      ' '
463900990921     C                   Z-ADD     YECIMO        TASDIF
464000990923    2C                   END
464100990923    1C                   ENDIF
464200990929     C* SE IMPORTO IMPONIBILE DIVERSO DA ZEROS
464300991014     C* LO RICALCOLO PER SICUREZZA
464400990929    1C     TASIMV        IFGT      0
464500991014     C                   CLEAR                   DSLV16
464600991014     C                   MOVEL     'T'           D17FIM
464700991014     C                   Z-ADD     TASPOR        D17POR
464800991014     C                   CALL      'FNLV16R'
464900991014     C                   PARM                    DSLV16
465000991014     C                   PARM                    DTASV
465100991014     C                   Z-ADD     O17IMV        TASIMV
465200990929    1C                   ENDIF
465300990921     C* SE QUANTITA' DA FATTURARE E' DIVERSA DA 0 E LA TARIFFA E' UNA TARIFFA
465400011127     C* A VALORE SPECIFICO DEVO CONVERTIRE IL CAMPO SE RICHIESTO
465500990921     C*
465600011127    1C     TASQFT        IFGT      0
465700011127     C     TAMFVD        ANDEQ     '4'
465800011127     C     CONQTA        ANDNE     'N'
465900011127     C                   CLEAR                   YEUR
466000011127     C                   MOVEL     DIVINP        YECDVI
466100011127     C                   MOVEL     DIVOUT        YECDVO
466200011127     C                   Z-ADD     TASQFT        YECIMI
466300011127     C   12              MOVE      '3'           YECDCO
466400011127     C  N12              MOVE      '0'           YECDCO
466500011127     C*
466600011127     C                   CALL      'YEURCO'
466700011127     C                   PARM                    YEUR
466800011127     C*
466900011127    2C     YECESI        IFEQ      ' '
467000011127     C                   Z-ADD     YECIMO        TASQFT
467100011127    2C                   END
467200011127    1C                   ENDIF
467300011127     C* SE TASIAL VALORIZZATO DEVO CONVERTIRLO SOLO SE RICHIESTO
467400011127     C*
467500011127    1C     TASIAL        IFGT      0
467600011127     C     CONIAL        ANDNE     'N'
467700011127     C                   CLEAR                   YEUR
467800011127     C                   MOVEL     DIVINP        YECDVI
467900011127     C                   MOVEL     DIVOUT        YECDVO
468000011127     C                   Z-ADD     TASIAL        YECIMI
468100011129     C   12              MOVE      '2'           YECDCO
468200011127     C  N12              MOVE      '0'           YECDCO
468300011127     C*
468400011127     C                   CALL      'YEURCO'
468500011127     C                   PARM                    YEUR
468600011127     C*
468700011127    2C     YECESI        IFEQ      ' '
468800011127     C                   Z-ADD     YECIMO        TASIAL
468900011127    2C                   END
469000011127    1C                   ENDIF
469100990921     C*
469200990921     C                   ENDSR
469300990923     C************************************************************
469400990923     C** CONVERSIONE DEL DETTAGLIO TARIFFA PARTICOLARE DI CARTELLO
469500990923     C************************************************************
469600990923     C     CONTPD        BEGSR
469700990923     C*
469800990923     C                   CLEAR                   YEUR
469900990923     C                   MOVEL     DIVCAR        YECDVI
470000990923     C                   MOVEL     §TADIV        YECDVO
470100991014     C   12              MOVE      '3'           YECDCO
470200991014     C  N12              MOVE      '0'           YECDCO
470300990923     C*
470400990923     C* CONTROLLLO IL TIPO TARIFFA SE E' VALORE OPPURE NO PER CONVERTIRE
470500990923     C* LO SCAGLIONE OPPURE NO
470600990923     C*
470700990923    1C     WTPG          IFEQ      'T'
470800990923     C     WTPG          OREQ      '4'
470900990923     C     WTPG          OREQ      'V'
471000990923     C*
471100990923     C                   Z-ADD     TPDSGL        YECIMI
471200990923     C                   CALL      'YEURCO'
471300990923     C                   PARM                    YEUR
471400990923    2C     YECESI        IFEQ      ' '
471500990923     C                   Z-ADD     YECIMO        TPDSGL
471600990923    2C                   END
471700990923     C* NON CONVERTO L'IMPORTO TARIFFA PERCHE' E' UNA PERCENTUALE
471800990923    1C                   ELSE
471900990923     C*
472000990923     C                   Z-ADD     TPDITR        YECIMI
472100990923     C                   CALL      'YEURCO'
472200990923     C                   PARM                    YEUR
472300990923    2C     YECESI        IFEQ      ' '
472400990923     C                   Z-ADD     YECIMO        TPDITR
472500990923    2C                   END
472600990923    1C                   ENDIF
472700990923     C*
472800990923     C* MINIMO
472900990923     C                   Z-ADD     TPDMIN        YECIMI
473000990923     C                   CALL      'YEURCO'
473100990923     C                   PARM                    YEUR
473200990923    1C     YECESI        IFEQ      ' '
473300990923     C                   Z-ADD     YECIMO        TPDMIN
473400990923    1C                   END
473500990923     C* MASSIMO
473600990923     C                   Z-ADD     TPDMAX        YECIMI
473700990923     C                   CALL      'YEURCO'
473800990923     C                   PARM                    YEUR
473900990923    1C     YECESI        IFEQ      ' '
474000990923     C                   Z-ADD     YECIMO        TPDMAX
474100990923    1C                   END
474200990923     C*
474300990923     C                   ENDSR
474400020826     C************************************************************
474500020826     C** CONTROLLO SE APPLICARE PESO NORMALE O VDL
474600020826     C************************************************************
474700020826     C     CTRPES        BEGSR
474800020826     C                   CLEAR                   USAPKF
474900030131     C**!!!              CLEAR                   §ASSPC
475000030131     C                   CLEAR                   TASSPC
475100030429     C                   CLEAR                   TASPKCN
475200020826     C*
475300020826     C* SE NON C'E' IL PESO VDL, PRENDO QUELLO DA FATTURARE
475400030131     C**!!!§ASPKC        IFEQ      0
475500030131     C     TASPKC        IFEQ      0
475600020826     C                   Z-ADD     TASPKF        USAPKF
475700020826     C                   GOTO      ENDPES
475800020826     C                   ELSE
475900020826     C* SE IL VDL NON + MAI DA APPLICARE --> USO IL DA FATTURARE
476000020826     C     §TATAP        IFEQ      'M'
476100020826     C                   Z-ADD     TASPKF        USAPKF
476200020826     C                   GOTO      ENDPES
476300020826     C                   ENDIF
476400020826     C                   ENDIF
476500020826     C* PESO VDL - TARA
476600030131     C**!!!§QTTPC        MULT      §ASNCP        WTARA
476700030131     C     §QTTPC        MULT      TASNCP        WTARA
476800030131     C**!!!§ASPKC        SUB(h)    WTARA         WNTARA
476900030131     C     TASPKC        SUB(h)    WTARA         WNTARA
477000020826     C* SE PESO DA FATTURARE COMPRESO DA VDL E VDL-TARA
477100020826     C*  APPLICO SEMPRE IL PESO DA FATTURARE
477200020826     C*
477300020827     C     TASPKF        IFGE      WNTARA
477400030131     C**!!!TASPKF        ANDLE     §ASPKC
477500030131     C     TASPKF        ANDLE     TASPKC
477600020826     C                   Z-ADD     TASPKF        USAPKF
477700020826     C                   GOTO      ENDPES
477800020826     C                   ENDIF
477900020826     C* SE VDL-TARA > PESO FATTURARE LO APPLICO SIA PER " " CHE PER "S"
478000020827     C     WNTARA        IFGT      TASPKF
478100020827     C                   Z-ADD     WNTARA        USAPKF
478200030131     C**!!!              MOVEL     'S'           §ASSPC
478300030131     C**!!!              z-add     wntara        §ASpkc
478400030131     C                   MOVEL     'S'           TASSPC
478500030429     C                   z-add     wntara        TASpkcN
478600020826     C                   GOTO      ENDPES
478700020826     C                   ENDIF
478800020826     C* SE VDL-TARA < PESO FATTURARE LO APPLICO SOLO SE TOTALE E SE
478900020826     C* "S"
479000020827     C     WNTARA        IFLT      TASPKF
479100030131     C**!!!§ASNCP        ANDEQ     TASNCL
479200030131     C     TASNCP        ANDEQ     TASNCL
479300020826     C     §TATAP        ANDEQ     'S'
479400020827     C                   Z-ADD     WNTARA        USAPKF
479500030131     C**!!!              MOVEL     'S'           §ASSPC
479600030131     C**!!!              z-add     wntara        §ASpkc
479700030131     C                   MOVEL     'S'           TASSPC
479800030429     C                   z-add     wntara        TASpkcN
479900020826     C                   GOTO      ENDPES
480000020826     C                   ENDIF
480100020826     C* SE ANCORA A 0 (IMPOSSIBILE) USO IL PESO DA FATTURARE
480200020826     C     USAPKF        IFEQ      0
480300020826     C                   Z-ADD     TASPKF        USAPKF
480400020826     C                   ENDIF
480500020827     C**
480600020829     c     endpes        tag
480700020829     C                   ENDSR
480800020412**
480900980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo del CONTRASSEGNO  1
481000980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo da ASSICURARE     2
481100980529Non trovata tariffa cliente                                                    3
481200980512Mancano importo da assicurare in bolla e valore merce in tariffa               4
481300980603Non trovato SCAGLIONE in tariffa                                               5
481400980512Manca la quantita' da fatturare                                                6
481500980603Non trovato SCAGLIONE in tariffa particolare
481600980603Non trovato CODICE TASSAZIONE in tariffa                                       8
481700980529Non trovata tariffa di CARTELLO                                                9
481800000125Tariffa cliente con decorrenza maggiore della data spedizione                 10
481900990917Divisa errata                                                                 11
482000000125Tariffa DPD ma bolla Italia                                                   12
482100000125Bolla import/export DPD  ma  tariffa NON DPD                                  13
482200011127Divisa tariffa del cliente non più utilizzabile                               14
482300020318Tariffa FEDEX ma bolla Italia                                                 15
482400020318Bolla import/export FEDEX ma  tariffa NON FEDEX                               16
482500020412Non trovato CODICE TASSAZIONE in tariffa PARTICOLARE                          17
482600030527Non presente tassazione per addebito insoluti                                 18
482700080221Tariffa valida ma imponibile minore di 0,000                                  19
