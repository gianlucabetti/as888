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
020200130926     c**                 if        §asemd = 'S' and tasdsp < 20110901
020300130926     c**                 clear                   §asemd
020400130926     c**                 endif
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
158500130926     C                   If        §asemd = 'S' or §asemd = 'X' or
158600130926     c                             §asemd = 'E'
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
220300060906     C                   IF        SCA_SCA > 0
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
221400060907      * Se la percentuale di incidenza è maggiore di zero
221500060907    1C                   IF        AITR > 0
221600060905      * CALCOLO L'IMPONIBILE
221700060905      * recupero importi di isola e località disagiate ed espresso
221800060905     C                   EXSR      CERISO
221900060905      *
222000060905     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP
222100060905      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
222200060905      *                     per abbandonare il concetto di monete diverse dall'EURO)
222300060907     C                   EVAL(H)   VARIA = (IMP_SCA * aitr * sca_sca) / 100
222400060905      **
222500060905     C                   Z-ADD     1             A
222600060905     C     ' '           LOOKUP    SV(A)                                  96
222700060907     C   96              MOVEL     §$2VFS        SV(A)
222800060905     C   96              Z-ADD     VARIA         VA(A)
222900060905     C  N96              ADD       VARIA         TASPOR
223000060905    1C                   ENDIF
223100060905      *
223200060905     C                   ENDIF
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
232000970521     C                   Z-ADD     1             A
232100970521     C     ' '           LOOKUP    SV(A)                                  96
232200970521     C   96              MOVEL     §1PVAR        SV(A)
232300970521     C   96              Z-ADD     TASCP         VA(A)
232400970521     C  N96              ADD       TASCP         TASPOR
232500970521     C                   END
232600970521     C                   END
232700970521     C     ENDPAR        ENDSR
232800941112     C******************************************************
232900941107     C** CALCOLO PESO TASSABILE PER TARIFFA A PESO
233000941112     C******************************************************
233100941107     C     CALPT         BEGSR
233200980313     C** MI MEMORIZZO SU BLTAS IL PESO TASSABILE SE
233300980313     C** IL PESO E' SUPERIORE AL MINIMO TASSABILE
233400980313     C** SE ESISTE IL RAPPORTO PESO VOLUME IN TARIFFA  E VOL IN BOLLA
233500980313     C** IL PESO TASSABILE SU BLTAS E' SEMPRE ARROTONDATO AL KG SUPERIO
233600980311     C                   CLEAR                   TASPVL
233700050912     C***  UNOCTR        IFEQ      0
233800050912     C**** UNOCTR        OREQ      3
233900050912      * modifico --- il peso tassabile si stampa e si calcola nei seguenti casi :
234000050912      * Tariffa a quintale "0" il peso da fatturare maggiore del  minimo tassabile
234100050912      *                        (definito nella tabella TR = 100) oppure volume da fatturare
234200050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
234300050912      *                        tariffa maggiore di zero
234400050912      * Tariffa a kg       "3" non controllo il peso da fatturare maggiore del minimo tassabile
234500050912      *                        (definito nella tabella TR = 0,1) ma solo volume da fatturare
234600050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
234700050912      *                        tariffa maggiore di zero
234800050912     C***  TARIFFA A QUINTALE
234900050912
235000050912     C     UNOCTR        IFEQ      0
235100050912
235200020826     C     USAPKF        IFGT      MINTAS
235300950901     C     PESTAS        ADD       0,9           TASPVL
235400941107     C                   ELSE
235500941107     C     TASVLF        IFNE      0
235600980311     C     RAPPV         ANDNE     0
235700950901     C     PESTAS        ADD       0,9           TASPVL
235800980313     C                   ENDIF
235900980313     C                   ENDIF
236000980313     C                   ENDIF
236100050912
236200050912     C***  TARIFFA A KG
236300050912
236400050912     C     UNOCTR        IFEQ      3
236500050912
236600050912     C     TASVLF        IFNE      0
236700050912     C     RAPPV         ANDNE     0
236800050912     C     PESTAS        ADD       0,9           TASPVL
236900050912     C                   ENDIF
237000050912     C                   ENDIF
237100050912
237200980313     C                   ENDSR
237300950119     C******************************************************
237400950119     C** CALCOLA VALORE DA ASSICURARE PER TARIFFA A VALORE
237500950119     C******************************************************
237600950119     C     CALVAS        BEGSR
237700950119     C     *LIKE         DEFINE    TASIAS        IMPVAS
237800950119     C                   Z-ADD     0             IMPVAS
237900950119     C                   MOVEL     'C'           FISO
238000990722     C                   MOVEL     'C'           WFISO             1
238100950119     C**FLAG ASSICURAZIONE X CONTO = C
238200950119     C                   Z-ADD     1             A
238300990722     C     WFISO         LOOKUP    CP(A)                                  05
238400950119     C  N05              GOTO      DOPVA1
238500950119     C**----------------------------------------
238600990721     C     KISOT         CHAIN     TITPT01L                           28
238700980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
238800980528     C     *IN28         IFEQ      *ON
238900980528     C     TPTATB        ORNE      ' '
239000980528     C                   GOTO      DOPVA1
239100980528     C                   ENDIF
239200980528     C**
239300950119     C** ESCLUDO GLI ANNULLATI
239400950119     C     TPTFVM        IFEQ      '3'
239500020826     C     TPTVLM        MULT      USAPKF        IMPVAS
239600950119     C                   END
239700950119     C*PESO
239800950119     C     TPTFVM        IFEQ      '1'
239900950119     C     TPTVLM        MULT      TASNCL        IMPVAS
240000950119     C                   END
240100950119     C* COLLI
240200950119     C     TPTFVM        IFEQ      '2'
240300950119     C                   Z-ADD     TPTVLM        IMPVAS
240400950119     C                   END
240500950119     C*SPEDIZIONE
240600950119     C     DOPVA1        ENDSR
240700941112     C******************************************************
240800941107     C** CALCOLA VARIA R  --- ASSICURAZIONE PER CONTO
240900941112     C******************************************************
241000941107     C     CALVR         BEGSR
241100050525     c* Il falg task88 cambia significato:indica se bolla con mandato o no
241200050525     c*
241300050525     c* con mandato significa: x mandato a val variabile--> tutte le bolle
241400050525     c*                        x mandato a val.indicato --> solo bolle con
241500050525     c*                        tasias=0 o con tasias <=al valore del mandat
241600050525     c*
241700050525     c* Imposto task88=N se mandato
241800050525     c* Imposto task88=S se no mandato ma tasias>0
241900050525     c* Imposto task88=X per bolle senza mandato e senza importo
242000050525     c
242100050525     C                   MOVEL     'X'           TASK88
242200980603     C** 88 = SI CALCOLA SOLO SE ESISTE IL VALORE MERCE IN BOLLA
242300941112     C     KSCCOD        IFEQ      8888
242400980527     C     KSCCOD        OREQ      9999
242500980709     C     TASIAS        IFEQ      0
242600050525     C**                 MOVEL     'S'           TASK88
242700980709     C                   GOTO      ENDVR
242800941112     C                   END
242900980709     C                   END
243000980603     C**
243100980603     C** SE TIPO BOLLA CHE NON PREVEDE L'ASSICURAZIONE, ESCO SENZA
243200980603     C**  DARE ERRORE
243300050525     C**   TASIAS        IFEQ      0
243400050525     C**   TASTBL        LOOKUP    TBN                                    05
243500050525     C** 05              MOVEL     'S'           TASK88
243600050525     C** 05              GOTO      ENDVR
243700050525     C**                 ENDIF
243800991012     C**
243900991012     C** SE IMPORTO D'ASSICURARE MAGGIORE DI ZEROS MUOVO 'S' TASK88
244000991012     C** PER STAMPA BOLLE CON ASSICURAZIONE PER TABULATONE DELLA CONSULDANNI
244100050525     C**   TASIAS        IFGT      0
244200050525     C**                 MOVEL     'S'           TASK88
244300050525     C**                 ENDIF
244400980603     C**
244500941116     C                   SETOFF                                       94
244600941116     C     *LIKE         DEFINE    TASIAS        IASLIT
244700941116     C                   Z-ADD     TASIAS        IASLIT
244800941116     C     IASLIT        IFNE      0
244900941116     C                   Z-ADD     1             K
245000941116     C     TASVAS        LOOKUP    CV(K)                                  05
245100980504     C     *IN05         IFEQ      *OFF
245200980504     C                   SETON                                        94
245300980504     C                   MOVEL     MSG(2)        TASMSG
245400980504     C                   GOTO      FINE
245500980504     C                   ENDIF
245600941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
245700990922     C**
245800990922     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA SE E' DIVERSA
245900990922     C* DALLA VALUTA DELLA TARIFFA
246000990922     C     §TADIV        IFNE      TASVAS
246100990922     C                   CLEAR                   YEUR
246200990922     C                   MOVEL     TASVAS        YECDVI
246300990922     C                   MOVEL     §TADIV        YECDVO
246400990922     C                   Z-ADD     IASLIT        YECIMI
246500011129     C   12              MOVE      '2'           YECDCO
246600991014     C  N12              MOVE      '0'           YECDCO
246700990922     C*
246800990922     C                   CALL      'YEURCO'
246900990922     C                   PARM                    YEUR
247000990922     C*
247100990922     C     YECESI        IFEQ      ' '
247200990922     C                   Z-ADD     YECIMO        IASLIT
247300990922     C                   END
247400990922     C                   ENDIF
247500990922     C                   ENDIF
247600941123     C**----------------------------------------
247700941107     C                   MOVEL     'C'           FISO
247800990722     C                   MOVEL     'C'           WFISO
247900941107     C**FLAG ASSICURAZIONE X CONTO = C
248000941123     C                   Z-ADD     1             A
248100990722     C     WFISO         LOOKUP    CP(A)                                  05
248200941123     C  N05              GOTO      ENDVR
248300020226      * AGGANCIO LA DS DELA TABELLA 1P
248400020226     C     A             OCCUR     DS1P
248500941123     C**----------------------------------------
248600990721     C     KISOT         CHAIN     TITPT01L                           28
248700980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
248800980528     C     *IN28         IFEQ      *ON
248900980528     C     TPTATB        ORNE      *BLANKS
249000980528     C                   GOTO      DOPAC1
249100980528     C                   ENDIF
249200980505     C**
249300980505     C** CONTROLLO IL TIPO APPLICAZIONE DELLA TARIFFA ASSIC X CONTO
249400980505     C**  SE INCONGRUENTE CON TIPO BOLLA PRENDO LA CARTELLO
249500980505     C     TPTAPL        IFEQ      'A'
249600980505     C     TIPBOL        ANDEQ     'F'
249700980505     C     TPTAPL        OREQ      'P'
249800980505     C     TIPBOL        ANDEQ     'A'
249900980505     C                   GOTO      DOPAC1
250000980505     C                   ENDIF
250100980505     C**
250200980504     C* VALORE MERCE = 0 --> ESCLUDO SE NON E' UN VERO MANDATO
250300980505     C                   SETOFF                                       94
250400980504    1C     TPTVLM        IFEQ      0
250500980709     C* CHE SIA UN MANDATO FITTIZIO O UN MANDATO A VALORE VARIABILE
250600980709     C*  DEVO SEMPRE SCRIVERE LA BOLLA COME 8888 PERCHE L'ASSICURAZIONE
250700980709     C*  VUOLE VEDERE BOLLA PER BOLLA QUANTO SONO ASSICURATE
250800050525     C****               MOVEL     'S'           TASK88
250900980709     C**
251000980504    2C     IASLIT        IFEQ      0
251100980504     c* MANDATO FITTIZIO
251200990922    3C     TPTTMA        IFEQ      'F'
251300980504     C                   GOTO      ENDVR
251400980504    3C                   ENDIF
251500050525     c**
251600050526     C** ES - non serve: già fatto    sopra!! Ripetizione inutile
251700050526    3C**   TPTAPL        IFEQ      'P'
251800050526     C**   TIPBOL        COMP      'F'                                    94
251900050526    3C**                 END
252000050526    3C**   TPTAPL        IFEQ      'A'
252100050526     C**   TIPBOL        COMP      'A'                                    94
252200050526    3C**                 END
252300050526    3C**   TPTAPL        IFEQ      ' '
252400050526     C**                 SETON                                            94
252500050526    3C**                 END
252600050525     c
252700050525     c
252800050525     c* anche se manca tariffa è comunque con mandato
252900050525     c                   eval      task88='N'
253000050525     c*
253100050525     c* non devo dare manca tariffa se tipo bolla che non prevede
253200050525     c* Assicurazione
253300050525     C     TASTBL        LOOKUP    TBN                                    05
253400050525     c                   if        not *in05
253500050526     c                   eval      *in94='1'
253600050525     C                   MOVEL     MSG(4)        TASMSG
253700050525     C                   GOTO      FINE
253800050525     c                   endif
253900050525     c
254000050525   x2c                   else
254100050525     c* Mandato a valore variabile con importo: se fittizio 8888 altriment
254200050525     c* con mandato
254300050525    3C     TPTTMA        IFEQ      'F'
254400050525     c                   eval      task88='S'
254500050525     c                   else
254600050525     c                   eval      task88='N'
254700050525    3C                   END
254800050525    2C                   END
254900050525     c
255000941107     C** MANCA TARIFFA: MANDATO CON VALORE = 0
255100941107     C**                E NON ESISTE VALORE IN BOLLA (SOLO COD.)
255200980504   X1C                   ELSE
255300980504    2C     IASLIT        IFGT      0
255400941112     C                   Z-ADD     0             VALSPE           15 3
255500980504    3C     TPTFVM        IFEQ      '3'
255600020826     C     TPTVLM        MULT      USAPKF        VALSPE
255700980504    3C                   END
255800941107     C*PESO
255900980504    3C     TPTFVM        IFEQ      '1'
256000941107     C     TPTVLM        MULT      TASNCL        VALSPE
256100980504    3C                   END
256200941107     C* COLLI
256300980504    3C     TPTFVM        IFEQ      '2'
256400941107     C                   Z-ADD     TPTVLM        VALSPE
256500980504    3C                   END
256600030908      *
256700030908      * ARROTONDO AL DECIMALE DELLA TARIFFA
256800030908     C                   CLEAR                   YEUR
256900030908     C                   MOVEL     §TADIV        YECDVI
257000030908     C                   MOVEL     §TADIV        YECDVO
257100030908     C                   Z-ADD     VALSPE        YECIMI
257200030908     C                   CALL      'YEURCO'
257300030908     C                   PARM                    YEUR
257400030908     C*
257500030908     C     YECESI        IFEQ      ' '
257600030908     C                   Z-ADD     YECIMO        VALSPE
257700030908     C                   END
257800941107     C*SPEDIZIONE
257900980504    3C     IASLIT        IFGT      VALSPE
258000941107     C                   GOTO      DOPAC1
258100050526     c                   else
258200050526     c
258300050526     c* Se valore rientra nel mandato, imposto che si tratta di mandato
258400050526     C                   MOVEL     'N'           TASK88
258500980504    3C                   END
258600050526     c
258700050526   x2c                   else
258800050526     c* se senza valore in bolla ma madanto con valore -->ok da mandato
258900050526     c                   eval      task88='N'
259000980504    2C                   END
259100980504    1C                   END
259200050526     c
259300980525     C** SE IL CLIENTE E' 8888 HO IMPOSTATO PRIMA LA TARIFFA DI CARTELL
259400980525     C     KSCCOD        IFEQ      8888
259500050525     C                   MOVEL     'S'           TASK88
259600980525     C                   ENDIF
259700050525     c
259800050525     c** Anche se lo considero con mandato, se non c'e' importo e tipo
259900050525     c**  bolla senza assicurazione, non calcolo varia R
260000050525     C     TASIAS        IFEQ      0
260100050525     C     TASTBL        LOOKUP    TBN                                    05
260200050525     C   05              GOTO      ENDVR
260300050525     C                   ENDIF
260400980505     C**
260500941107     C                   GOTO      DOPAC2
260600941107     C     DOPAC1        TAG
260700941107     C** NON ESISTE TARIFFA AXC DEL CLIENTE
260800941121     C*-----------------------------------------------------------------
260900980505     C* SE USATA SI TRATTA DI TARIFFA DI CARTELLO
261000050525     C***                MOVEL     'S'           TASK88
261100941112     C     IASLIT        CABEQ     0             ENDVR
261200050525     c                   eval      task88='S'
261300020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
261400020226     C     KTPTC         CHAIN     TITPT01L                           28
261500020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
261600020226     C     *IN28         IFEQ      *ON
261700020226     C     TPTATB        OREQ      'A'
261800020226     C                   GOTO      ENDVR
261900020226     C                   ENDIF
262000941107     C*-----------------
262100941107     C     DOPAC2        TAG
262200980504     C** SE NON SERVE --> ESCO DAL CALCOLO
262300980504     C** TIPO APPLICAZIONE
262400980504     C** P - SOLO PER FRANCHI
262500980504     C** A - SOLO PER ASSEGNATI
262600980504    1C     TPTAPL        IFNE      *BLANKS
262700980504     C     IASLIT        ANDEQ     0
262800980504    2C     TIPBOL        IFEQ      'F'
262900980504    3C     TPTAPL        IFEQ      'A'
263000980504     C                   GOTO      ENDVR
263100980504    3C                   ENDIF
263200980504   X2C                   ELSE
263300980504    3C     TPTAPL        IFEQ      'P'
263400980504     C                   GOTO      ENDVR
263500980504    3C                   ENDIF
263600980504    2C                   END
263700980504    1C                   END
263800980504     C**
263900941112     C     *LIKE         DEFINE    TASIAS        VALVR
264000941112     C     *LIKE         DEFINE    TASIAS        ASCPES
264100941112     C     *LIKE         DEFINE    TASIAS        ASCPE2
264200941112     C                   Z-ADD     0             VALVR
264300980526    1C     IASLIT        IFEQ      0
264400980526    2C     TPTFVM        IFEQ      '3'
264500020826     C     USAPKF        MULT      TPTVLM        VALVR
264600980526    2C                   END
264700980526    2C     TPTFVM        IFEQ      '1'
264800941107     C     TASNCL        MULT      TPTVLM        VALVR
264900980526    2C                   END
265000980526    2C     TPTFVM        IFEQ      '2'
265100941107     C                   Z-ADD     TPTVLM        VALVR
265200980526    2C                   END
265300980526   X1C                   ELSE
265400941112     C                   Z-ADD     IASLIT        VALVR
265500980526    1C                   END
265600011119     C***** OBSOLETO NON RESTITUISCO + IL VALORE IN LIRE MA NELLA DIVISA DELLA TARIFFA
265700011129     C***** PERO' ARROTONDO AL DECIMALE DELLA TARIFFA
265800011129     C                   CLEAR                   YEUR
265900011129     C                   MOVEL     §TADIV        YECDVI
266000011129     C                   MOVEL     §TADIV        YECDVO
266100011129     C                   Z-ADD     VALVR         YECIMI
266200011129     C                   CALL      'YEURCO'
266300011129     C                   PARM                    YEUR
266400011129     C*
266500011129     C     YECESI        IFEQ      ' '
266600011129     C                   Z-ADD     YECIMO        VALVR
266700011129     C                   END
266800011129     C* RESTITUISCO IN OUTPUT L'IMPORTO DA ASSICURARE
266900011129     C                   Z-ADD     VALVR         TASIAL
267000941107     C** VALORE SPEDIZIONE PER CALCOLO ASSICURAZIONE
267100980312     C**
267200980526     C** CALCOLO SOLO SE NON HO IMPOSTATO IL FLAG
267300980526    1C     PARCA         IFEQ      ' '
267400980505     C**
267500980505     C** SE C'E' GIA' LA VARIA R NON LA RICALCOLO
267600980505     C                   Z-ADD     1             A
267700980505     C     §$2VRR        LOOKUP    SV(A)                                  05
267800980526    2C     *IN05         IFEQ      *OFF
267900980312     C                   MOVEL     TPTTPG        WTPG
268000980312     C                   MOVEL     TPTRPV        WRPV
268100980313     C                   CLEAR                   WDET
268200980313     C                   Z-ADD     TPTARL        WARL
268300980313     C                   Z-ADD     TPTARF        WARF
268400980313     C                   Z-ADD     TPTARO        WARO
268500980312     C                   EXSR      CALCOL
268600980312     C**
268700980312     C** A VALORE
268800980526    3C     TPTTPG        IFEQ      'V'
268900980312     C                   Z-ADD     VALVR         ISOPES
269000980526    3C                   END
269100980312     C     *LIKE         DEFINE    TASIAS        TPDPES
269200980504     C**
269300980504     C**CALCOLO IMPORTO ASSICURAZIONE PER CONT
269400980504     C                   Z-ADD     ISOPES        TPDPES
269500941107     C                   EXSR      CALACD
269600980525     C                   Z-ADD     VARIA         WVARIR
269700980526    2C                   ENDIF
269800980526    1C                   ENDIF
269900980504     C     ENDVR         ENDSR
270000060320     C******************************************************
270100060320     C** CALCOLA VARIA d  --- A C BASE
270200060320     C******************************************************
270300060321     c     CALVACB       BEGSR
270400060320     c* Il falg task88 cambia significato:indica se bolla con mandato o no
270500060320     c* Imposto task88=N se mandato
270600060321     c* Lascio  task88=X per bolle senza mandato e senza importo
270700060321
270800060321      **----------------------------------------
270900060321     c                   movel     'd'           Fiso
271000060321     c                   movel     'd'           WFiso
271100060321      ** Flag A C BASE  = d
271200060321     c                   z-add     1             a
271300060321     c     Wfiso         Lookup    Cp(a)                                  05
271400060321     c  N05              Goto      EndVacb
271500060321      * aggancio la ds della tabella 1P
271600060321     c     A             Occur     DS1P
271700060321      **----------------------------------------
271800060321     c     Kisot         Chain     TITPT01L
271900060321      ** esiste Tariffa A C base
272000060321    1c                   If        %found(titpt01l)
272100060321      **
272200060321      ** Controllo il tipo applicazione della tariffa A C Base
272300060321      **
272400060321    2c                   If        (tptapl = 'A' and tipbol = 'A') or
272500060321     c                             (tptapl = 'P' and tipbol = 'F') or
272600060321     c                              tptapl = ' '
272700060321      *
272800060321     c                   eval      task88='N'
272900060321      ** Anche se lo considero con mandato,  tipo
273000060321      **  bolla senza assicurazione, non calcolo varia d
273100060321     c     TAStbl        Lookup    Tbn                                    05
273200060321     c   05              Goto      ENDVacb
273300060321
273400060321      * calcolo valore assicurato
273500060321
273600060321     c     *Like         Define    TASias        VALVacb
273700060321
273800060321     c                   clear                   VALVacb
273900060321
274000060321      * Peso
274100060321    3c                   If        TPTfvm = '3'
274200060321     c     TPTvlm        Mult      USAPkf        Valvacb
274300060321    3c                   Endif
274400060321      * Colli
274500060321    3c                   If        TPTfvm = '1'
274600060321     c     TPTvlm        Mult      TASncl        Valvacb
274700060321    3c                   Endif
274800060321      * Spedizione
274900060321    3c                   If        TPTfvm = '2'
275000060321     c                   Z-add     TPTvlm        Valvacb
275100060321    3c                   Endif
275200060321      *
275300060321      * Arrotondo al decimale della tariffa
275400060321     c                   Clear                   YEUR
275500060321     c                   Movel     §TAdiv        YECdvi
275600060321     c                   Movel     §TAdiv        YECdvo
275700060321     c                   Z-add     VALvacb       YECimi
275800060321     c                   call      'YEURCO'
275900060321     c                   Parm                    YEUR
276000060321      *
276100060321     c                   If        YECesi = ' '
276200060321     c                   Z-add     YECimo        VALvacb
276300060321     c                   Endif
276400060321
276500060321      ** Calcolo solo se non ho impostato il flag
276600060321    3c                   IF        Parca = ' '
276700060321
276800060321      ** Se c'è' già la varia non la ricalcolo
276900060321     c                   Z-add     1             a
277000060321     c     §$2acb        Lookup    sv(a)                                  05
277100060321    4c     *IN05         Ifeq      *off
277200060321     c                   movel     TPTtpg        Wtpg
277300060321     c                   Movel     TPTrpv        Wrpv
277400060321     c                   clear                   Wdet
277500060321     c                   Z-add     TPTarl        Warl
277600060321     c                   Z-add     TPTarf        Warf
277700060321     c                   Z-add     TPTaro        Waro
277800060321     c                   Exsr      Calcol
277900060321
278000060321      ** A Valore
278100060321    5c                   IF        tpttpg =  'V'
278200060321     c                   z-add     valvacb       ISOpes
278300060321    5c                   Endif
278400060321
278500060321     c                   Z-add     ISOpes        TPDpes
278600060321     c                   Exsr      Calacd
278700060321     c                   Z-add     varia         Wvariacb
278800060321    4c                   endif
278900060321
279000060321    3c                   endif
279100060321
279200060321    2c                   endif
279300060321    1c                   Endif
279400060321
279500060321     c     EndVacb       ENDSR
279600941112     C******************************************************
279700980312     C** CALCOLA TARIFFA PARTICOLARE
279800941112     C******************************************************
279900941107     C     CALACD        BEGSR
280000990923     C* CAMPO VARIA CON  DECIMALI
280100990923     C                   Z-ADD     0             VARIA            15 3
280200990923     C* CAMPO VARIA SENZA DECIMALI ALTRIMENTI NON FUNZIONA L'ARROTONDAMENTO
280300990923     C                   Z-ADD     0             VARIAD           15 0
280400990923     C*
280500941107     C                   DO        100           A
280600941122     C     A             OCCUR     ACDS
280700941107     C                   CLEAR                   ACDS
280800941107     C                   END
280900941112     C** PULIZIA DS TARIFFA
281000020412     C*
281100020412     C* SE ESISTE ALMENO UNA RIGA DI DETTAGLIO E NON RIESCO AD APPLICAR
281200020412     C*  QUESTA TAR.PARTICOLARE --> MANCA TARIFFA
281300980605     C* BOLLA IMPORT / ESXPORT
281400980605     C     WBOEST        IFEQ      'E'
281500020226     C     §1PCTE        IFNE      *BLANKS
281600020226     C                   MOVEL     §1PCTE        ISOCTS
281700941112     C                   GOTO      POIAC
281800941112     C                   END
281900941112     C                   ELSE
282000020226     C     §1PCTS        IFNE      *BLANKS
282100020226     C                   MOVEL     §1PCTS        ISOCTS
282200941112     C                   GOTO      POIAC
282300941112     C                   END
282400941112     C                   END
282500941112     C** CODICI TASSAZIONE FISSI DA TABELLA TARIFFE PARTICOLARI
282600941110     C                   MOVEL     TASCTS        ISOCTS            2
282700060414     c
282800060414      * Se assegnato e tariffa FEDEX metto in ISOCTS  quello mittente
282900050124     c                   If        TIPBOL = 'A' and WBOFED = 'S'
283000050124     c                   movel     tasmct        isocts
283100050124     c                   endif
283200060414     c* Se devo utilizzare quello del mittente per via della
283300060414     c*  reversibilità --> lo uso
283400060414     c                   if        applicarevers='S'
283500060414     c                   movel     tasmct        isocts
283600060414     c                   endif
283700050124      *
283800990721     C     KISOD         SETLL     TITPD02L                               28
283900941107     C   28              GOTO      DOPAC3
284000941107     C                   Z-ADD     1             A
284100941107     C     ISOCTS        LOOKUP    CTS(A)                                 05
284200941107     C   05A             OCCUR     DSCT
284300941107     C   05              MOVEL     §CTRAP        ISOCTS
284400941107     C  N05              MOVEL     *BLANKS       ISOCTS
284500941107     C** REGIONE
284600990721     C     KISOD         SETLL     TITPD02L                               28
284700941107     C   28              GOTO      DOPAC3
284800980605     C     WBOEST        IFEQ      'E'
284900941112     C                   MOVEL     'EE'          ISOCTS
285000941112     C                   ELSE
285100941112     C                   MOVEL     'IT'          ISOCTS
285200941112     C                   END
285300941112     C     POIAC         TAG
285400990721     C     KISOD         SETLL     TITPD02L                               28
285500020412     C**N28                GOTO ENDACD
285600020412     C** N 28 --> MANCA TARIFFA
285700020412     C     *IN28         IFEQ      *OFF
285800020412     C* SE NON È PRESENTE NESSUNA RIGA DI DETTAGLIO, NON DO
285900020412     C*  MANCA TARIFFA: SIGNIFICA CHE È INSERITA UNA TASSAZIONE =0
286000020412     C     KISOD2        SETLL     TITPD02L                               28
286100020412     C  N28              GOTO      ENDACD
286200020412     C*
286300020412     C                   SETON                                        94
286400020412     C                   MOVEL     MSG(17)       TASMSG
286500020412     C                   GOTO      FINE
286600020412     C                   ENDIF
286700020412     C**
286800020412     C** NON TROVATA TARIFFA
286900941107     C     DOPAC3        TAG
287000941107     C                   DO        *HIVAL        A
287100990721     C     KISOD         READE     TITPD02L                               28
287200941107     C   28              GOTO      DOPAC4
287300941107     C     A             OCCUR     ACDS
287400990923     C* VERIFICO SE STO CARICANDO LA TARIFFA DI CARTELLO
287500990923     C     TPTKSC        IFEQ      CARKSC
287600990923     C     TPTCTR        ANDEQ     CARCTR
287700990923     C     TPTPRG        ANDEQ     CARPRG
287800990923     C* E LA DIVISA DELLA TARIFFA DI CARTELLO E' DIVERSA DALLA DIVISA DELLA
287900990923     C* TARIFFA DEL CLIENTE  CONVERTO I VALORI O SCAGLIONI NELLA DIVISA
288000990923     C* DELLA TARIFFA DEL CLIENTE
288100990923     C     §TADIV        ANDNE     DIVCAR
288200990923     C*
288300990923     C                   EXSR      CONTPD
288400990923     C*
288500990923     C                   ENDIF
288600990923     C*
288700941112     C                   Z-ADD     TPDSGL        ASGL
288800941112     C                   Z-ADD     TPDITR        AITR
288900941112     C                   Z-ADD     TPDMIN        AMIN
289000941112     C                   Z-ADD     TPDMAX        AMAX
289100941112     C                   MOVEL     TPDAIN        AAIN
289200941107     C                   END
289300941107     C     DOPAC4        TAG
289400941107     C                   DO        100           A
289500941107     C     A             OCCUR     ACDS
289600941112     C     ASGL          IFGE      TPDPES
289700941107     C                   GOTO      DOPAC5
289800941107     C                   END
289900941107     C                   END
290000941123     C     ASGL          IFLT      TPDPES
290100941122     C                   SETON                                        94
290200980527     C                   MOVEL     MSG(7)        TASMSG
290300980527     C                   GOTO      FINE
290400980527     C                   ENDIF
290500941122     C** NON TROVATO SCAGLIONE ESATTO
290600941107     C     DOPAC5        TAG
290700941112     C*-----------
290800941122     C     ASGL          IFLE      MINTAS
290900941122     C                   Z-ADD     MINTAS        TPDPES
291000941112     C                   END
291100941122     C     TPDPES        IFLE      MINTAS
291200941122     C                   Z-ADD     MINTAS        TPDPES
291300941122     C                   END
291400991013     C                   Z-ADD     0             TPDQLI           18 6
291500980429     C* DIVIDO PER 100 TARIFFA 0 PER AVERE QUINTALI
291600980429     C*                TARIFFA 4 PERCHE' E' TARIFFA %
291700980429     C     WTPG          IFEQ      '0'
291800980429     C     WTPG          OREQ      '4'
291900941112     C     TPDPES        DIV       100           TPDQLI
292000941112     C                   ELSE
292100941112     C                   Z-ADD     TPDPES        TPDQLI
292200941112     C                   END
292300980429     C** PESO TASSABILE
292400060906      * se trovato dettaglio tariffario della tariffa "f" vado a fine routine in quanto il calcolo
292500060906      * viene eseguito nella routine FUEL
292600060906     c                   If        wfiso = 'f'
292700060906     c                   goto      endacd
292800060906     c                   endif
292900060906
293000990923     C   12AITR          MULT(H)   TPDQLI        VARIA
293100990923     C  N12AITR          MULT(H)   TPDQLI        VARIAD
293200941112     C                   MOVEL     AAIN          FLGISO
293300980429     C*
293400980430     C     VARIA         IFGT      0
293500991012     C     *IN12         ANDEQ     *ON
293600991012     C     VARIAD        ORGT      0
293700991012     C     *IN12         ANDEQ     *OFF
293800991012     C*
293900980430     C     WTPG          IFEQ      'V'
294000980430     C     WTPG          OREQ      'T'
294100990923     C   12VARIA         DIV(H)    100           VARIA
294200990923     C  N12VARIAD        DIV(H)    100           VARIAD
294300980430     C                   ENDIF
294400980430     C                   ENDIF
294500980429     C**
294600991012     C* SPOSTO L'IMPORTO DEL CAMPO VARIAD NEL CAMPO VARIA SOLO SE E' STATO USATO
294700991012     C  N12              Z-ADD     VARIAD        VARIA
294800991012     C*
294900991012     C** MINIMO
295000941112     C     AMIN          IFGT      VARIA
295100991012     C                   Z-ADD     AMIN          VARIA
295200941107     C                   END
295300991012     C** MASSIMO
295400941115     C     AMAX          IFGT      0
295500941112     C     AMAX          IFLT      VARIA
295600991012     C                   Z-ADD     AMAX          VARIA
295700941112     C                   END
295800941115     C                   END
295900000103     C** SE VARIA CON ESENZIONE IVA DEVE AVERE SOLO 2 DCEIMALI
296000000103     C* CERCO LA SIGLA VARIA CORRISPONDENTE
296100000103     C                   MOVEL     TPTFTC        WFTC              1
296200000103     C                   Z-ADD     1             A
296300000103     C     WFTC          LOOKUP    CP(A)                                  10
296400000103     C     *IN10         IFEQ      *ON
296500000103     C     A             OCCUR     DS1P
296600000103     C     §1PVAR        LOOKUP    VES                                    10
296700000103     C     *IN10         IFEQ      *ON
296800000103     C                   MOVE      VARIA         W001A             1
296900000103     C     W001A         IFNE      '0'
297000000103     C     VARIA         ADD       0,001         COMOD2           15 2
297100000103     C                   Z-ADD     COMOD2        VARIA
297200000103     C                   ENDIF
297300000103     C                   ENDIF
297400000103     C                   ENDIF
297500000103     C**
297600941107     C     ENDACD        TAG
297700990923     C*
297800941107     C                   ENDSR
297900941107     C/SPACE
298000941112     C******************************************************
298100980424     C** CALCOLO TARIFFE  PARTICOLARI
298200970521     C******************************************************
298300980428     C     CALCP         BEGSR
298400990721     C** CERO LA TARIFFA PARTICOLARE IN TITPT O CARTELLO
298500980430     C                   MOVEL     TASTC         FISO
298600980506     C                   MOVEL     'S'           WCAR
298700110908      ** se sto calcolando la varia "Q" = Centro storico o ZTL
298800110908      ** non devo cercare la tariffa nella Cartello in mancanza
298900110908      ** di quella del cliente
299000111102      ** E' stata aggiuna una tabella che pilota il recupero della
299100111102      ** tariffa di cartello se la filiale del cliente di tassazione
299200111102      ** è presente nella tabella ztl e la data spedizione è maggiore
299300111102      ** uguale alla data attivazione della tassazione
299400111102     c                   clear                   indztl            3 0
299500110908     c                   If        tastc = 'Q'
299600111102     c                   movel     'N'           WCAR
299700111102      * cerco la filiale del cliente nella tabella ZTL
299800111102     c                   eval      indztl = %lookup(kscfil:sfilztl)
299900111102      * se indice =  a zero cerco con la filiale 999
300000111102    2c                   if        indztl = *zeros
300100111102     c                   eval      indztl = %lookup(999:sfilztl)
300200111102    2c                   endif
300300111102      * imposto il flag della ricerca tariffa di cartello = 'S'  se filiale cliente
300400111102      * abilitata e la data spedizione > = data attivazione tassazione
300500111102    2c                   If        indztl > *zeros and
300600111102    3c                             tasdsp >= sdtaztl(indztl)
300700111102     c                   movel     'S'           WCAR
300800111102    3c                   endif
300900110908     c                   endif
301000110908      *
301100980428     C                   EXSR      CERTPT
301200980428     C** SOLO SE L'HO TROVATA
301300980428    1C     WEND          IFEQ      ' '
301400970521     C                   Z-ADD     ISOPES        TPDPES
301500970521     C                   EXSR      CALACD
301600970521     C                   Z-ADD     VARIA         TASCP
301700980428    1C                   ENDIF
301800970521     C*
301900970521     C                   ENDSR
302000001205     C/SPACE
302100001205     C******************************************************
302200001205     C** CALCOLO ADDIZIONALE DI GESTIONE + ISTAT
302300001205     C******************************************************
302400001205     C     ADGIST        BEGSR
302500001205     C**
302600001205     C*
302700001205     C****************
302800041129     C** ADDIZIONALE DI GESTIONE  anni precedenti       varia "Z"
302900001205     C****************
303000001205     C*
303100031121     C     *LIKE         DEFINE    TASVA1        TASADG
303200001205     C     TAMPAG        IFNE      0
303300001205     C                   Z-ADD     1             A
303400001205     C     §$2VRZ        LOOKUP    SV(A)                                  05
303500001205     C  N05              DO
303600001205     C                   Z-ADD     0             TASADG
303700001205     C* TOTALE IMPONIBILE
303800001205     C                   Z-ADD     O17IMV        IMPADG           15 3
303900001205     C**
304000001205     C                   Z-ADD     0             COMADG           15 3
304100001205     C     IMPADG        MULT      TAMPAG        COMADG
304200001205     C     COMADG        IFGT      0
304300001205     C     COMADG        DIV(H)    100           TASADG
304400001205     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
304500001205     C     *IN12         IFEQ      '0'
304600001205     C     TASADG        ADD       0,5           CAMPOW           11 0
304700001205     C                   Z-ADD     CAMPOW        TASADG
304800001205     C                   END
304900001205     C                   END
305000001205     C*
305100040128     c     tasadg        ifgt      0
305200040128      *
305300001205     C                   Z-ADD     1             A
305400001205     C     ' '           LOOKUP    SV(A)                                  96
305500001205     C   96              MOVEL     §$2VRZ        SV(A)
305600001205     C   96              Z-ADD     TASADG        VA(A)
305700001205     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
305800001205     C  N96              ADD       TASADG        TASPOR
305900001205     C*
306000001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
306100001205     C*
306200001205     C                   CLEAR                   DSLV16
306300001205     C                   MOVEL     'T'           D17FIM
306400001205     C                   Z-ADD     TASPOR        D17POR
306500001205     C                   CALL      'FNLV16R'
306600001205     C                   PARM                    DSLV16
306700001205     C                   PARM                    DTASV
306800040128      *
306900040128     c                   endif
307000001205     C*
307100001205     C                   END
307200001205     C                   END
307300041129     C*
307400041129     C****************
307500041129     C** ADDIZIONALE DI GESTIONE    anno corrente       varia "b"
307600041129     C****************
307700041129     C*
307800041129     c                   clear                   tasadg
307900041129     c                   clear                   impadg
308000041129     c                   clear                   comadg
308100041129
308200041129     C     TAMPPA        IFNE      0
308300041129     C                   Z-ADD     1             A
308400041129     C     §$2VAG        LOOKUP    SV(A)                                  05
308500041129     C  N05              DO
308600041129     C                   Z-ADD     0             TASADG
308700041129     C* TOTALE IMPONIBILE
308800041129     C                   Z-ADD     O17IMV        IMPADG
308900041129     C**
309000041129     C                   Z-ADD     0             COMADG
309100041129     C     IMPADG        MULT      TAMPPA        COMADG
309200041129     C     COMADG        IFGT      0
309300041129     C     COMADG        DIV(H)    100           TASADG
309400041129     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
309500041129     C     *IN12         IFEQ      '0'
309600041129     C     TASADG        ADD       0,5           CAMPOW           11 0
309700041129     C                   Z-ADD     CAMPOW        TASADG
309800041129     C                   END
309900041129     C                   END
310000041129     C*
310100041129     c     tasadg        ifgt      0
310200041129      *
310300041129     C                   Z-ADD     1             A
310400041129     C     ' '           LOOKUP    SV(A)                                  96
310500041129     C   96              MOVEL     §$2VAG        SV(A)
310600041129     C   96              Z-ADD     TASADG        VA(A)
310700041129     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
310800041129     C  N96              ADD       TASADG        TASPOR
310900041129     C*
311000041129     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
311100041129     C*
311200041129     C                   CLEAR                   DSLV16
311300041129     C                   MOVEL     'T'           D17FIM
311400041129     C                   Z-ADD     TASPOR        D17POR
311500041129     C                   CALL      'FNLV16R'
311600041129     C                   PARM                    DSLV16
311700041129     C                   PARM                    DTASV
311800041129      *
311900041129     c                   endif
312000041129     C*
312100041129     C                   END
312200041129     C                   END
312300001205     C**
312400001205     C***************
312500001205     C** ISTAT
312600001205     C***************
312700001205     C*
312800031121     C     *LIKE         DEFINE    TASVA1        TASIAC
312900001205     C                   Z-ADD     1             A
313000001205     C     §$2VRV        LOOKUP    SV(A)                                  05
313100101014    1C  N05TAMrct        IFGT      0
313200001205     C                   Z-ADD     0             TASIAC
313300001205     C*
313400001205     C* CALCOLO ISTAT SU TOTALE IMPONIBILE
313500001205     C*
313600001205     C                   Z-ADD     O17IMV        IMPIAC           15 3
313700101014     C                   Z-ADD     0             PUNIAC            7 4
313800101014
313900101014      * richiamo nuovo pgm per calcolo % istat da aggiungere all'imponibile
314000101014     c
314100101014     c                   clear                   trulistds
314200101014     c                   eval      IISTsca = tamrct
314300101014     c                   eval      IISTdsp = tasdsp
314400101014     c                   call      'TRULISTR'
314500101014     c                   parm                    trulistds
314600101014     c                   IF        OISTerr = *blanks
314700101014     c                   eval      puniac = OISTiac
314800101014     c                   ENDIF
314900101014     c
315000050629     C                   Z-ADD     0             PERIAC           15 6
315100050629     C                   Z-ADD     0             PERIAX           15 4
315200001205     C     PUNIAC        MULT      TAMPRI        PERIAX
315300001205    3C     PERIAX        IFNE      0
315400031121     C     PERIAX        DIV       100           PERIAC
315500001205    3C                   END
315600001205     C** PERCENTUALE DI ISTAT DA APPLICARE
315700001205     C** APPLICO I PUNTI X TRIMESTRE SOLO SE
315800001205     C** LA DATA DI SPEDIZIONE E'
315900001205     C** MAGGIORE O UGUALE ALLA DATA DI SCATTO
316000001205     C** DEI PUNTI NEL TRIMESTRE
316100001205    3C     PERIAC        IFNE      0
316200031121     C                   DIV       100           PERIAC
316300001205     C     IMPIAC        MULT(H)   PERIAC        TASIAC
316400001205    3C                   END
316500001205     C*
316600001205    3C     TASIAC        IFGT      0
316700001205     C*  SENZA DECIMALI
316800001205    4C     *IN12         IFEQ      *OFF
316900001205     C     TASIAC        ADD       0,5           CAMPOW           11 0
317000001205     C                   Z-ADD     CAMPOW        TASIAC
317100001205    4C                   END
317200001205     C*
317300001205     C                   Z-ADD     1             A
317400001205     C     ' '           LOOKUP    SV(A)                                  96
317500001205     C   96              MOVEL     §$2VRV        SV(A)
317600001205     C   96              Z-ADD     TASIAC        VA(A)
317700001205     C  N96              ADD       TASIAC        TASPOR
317800001205     C*
317900001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
318000001205     C*
318100001205     C                   CLEAR                   DSLV16
318200001205     C                   MOVEL     'T'           D17FIM
318300001205     C                   Z-ADD     TASPOR        D17POR
318400001205     C                   CALL      'FNLV16R'
318500001205     C                   PARM                    DSLV16
318600001205     C                   PARM                    DTASV
318700001205     C*
318800001205    3C                   END
318900001205    1C                   ENDIF
319000001205     C*
319100001205     C                   ENDSR
319200030922     C*******************************************************
319300030922     C** CALCOLO DIRITTO DI FATTURAZIONE  + TOTALE IMPONIBILE
319400030922     C******************************************************
319500030922     C     DIRFAT        BEGSR
319600030922     C**
319700030922      * prima verifico se esiste già
319800030922     C                   Z-ADD     1             A
319900030922     C     §$2VRD        LOOKUP    SV(A)                                  05
320000030922     C* SIGLA VARIA NON PRESENTE --> CALCOLO
320100030922     C     *IN05         IFEQ      *OFF
320200030922     C                   Z-ADD     1             A
320300030922     C     ' '           LOOKUP    SV(A)                                  96
320400030922     C   96              MOVEL     §$2VRD        SV(A)
320500030922     C   96              Z-ADD     TASIVDF       VA(A)
320600030922     C  N96              ADD       TASIVDF       TASPOR
320700030922     C*
320800030922     C* RICALCOLO L'IMPONIBILE SOMMANDO IL DIRITTO DI FATTURAZIONE
320900030922     C*
321000030922     C                   CLEAR                   DSLV16
321100030922     C                   MOVEL     'T'           D17FIM
321200030922     C                   Z-ADD     TASPOR        D17POR
321300030922     C                   CALL      'FNLV16R'
321400030922     C                   PARM                    DSLV16
321500030922     C                   PARM                    DTASV
321600030924     C*
321700030924     C                   Z-ADD     O17IMV        TASIMV
321800030924     C*
321900030922     C*
322000030922     C                   ENDIF
322100030922     C*
322200030922     C                   ENDSR
322300980428     C******************************************************
322400980428     C* CERCO TARIFFA PARTICOLARE
322500980428     C******************************************************
322600980428     C     CERTPT        BEGSR
322700980428     C                   CLEAR                   TASCP
322800980428     C                   CLEAR                   WEND
322900980428     C                   MOVEL     *BLANKS       FLGISO            1            FLAG.APPLICAZI.
323000990722     C                   MOVEL     FISO          WFISO
323100980428     C                   Z-ADD     1             A
323200990722     C     WFISO         LOOKUP    CP(A)                                  05
323300980428     C**
323400980428    1C     *IN05         IFEQ      *ON
323500020226     C     A             OCCUR     DS1P
323600980428     C*-----------------------------------------
323700990721     C     KISOT         CHAIN     TITPT01L                           28
323800980528     C** ESISTE TARIFFA DEL CLIENTE
323900980528     C     *IN28         IFEQ      *OFF
324000980528     C     TPTATB        ANDEQ     *BLANKS
324100980428     C                   GOTO      DOPIS2
324200980528     C                   ENDIF
324300980428     C*-----------------------------------------
324400980428     C** MEMORIZZO DATI DA TARIFFA DI CARTELLO
324500980506     C**  SOLO SE HO DETTO CHE LA POSSO USARE E SE C'E'
324600980428     C*-----------------------------------------
324700020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
324800020226     C     WCAR          IFEQ      'S'
324900020226     C     KTPTC         CHAIN     TITPT01L                           28
325000020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
325100020226     C     *IN28         IFEQ      *ON
325200020226     C     TPTATB        OREQ      'A'
325300020226     C                   MOVEL     'S'           WEND
325400020226     C                   GOTO      DOPIS3
325500020226     C                   ENDIF
325600020226      * E SE NON DEVO CARICARE LA CARTELLO ERRORE NON TROVATA TARIFFA PARTICOLARE
325700020226     C                   ELSE
325800020226     C                   MOVEL     'S'           WEND
325900020226     C                   GOTO      DOPIS3
326000020226     C                   ENDIF
326100980430     C     DOPIS2        TAG
326200980430     C* ARROTONDAMENTI E RPV
326300980430     C                   MOVEL     TPTTPG        WTPG
326400090310      * in caso di pod-immage varia "a" imposto come tipo pagamento a colli
326500090310
326600090310     c                   if        fiso = §$2POD
326700090310     C                   MOVEL     '1'           WTPG
326800090310     c                   endif
326900090310
327000980430     C                   MOVEL     TPTRPV        WRPV
327100980430     C                   CLEAR                   WDET
327200980430     C                   Z-ADD     TPTARL        WARL
327300980430     C                   Z-ADD     TPTARF        WARF
327400980430     C                   Z-ADD     TPTARO        WARO
327500980430     C                   EXSR      CALCOL
327600980428     C** NON TROVATA TARIFFA PARTICOLARE
327700980428   X1C                   ELSE
327800980428     C                   MOVEL     'S'           WEND              1
327900980428    1C                   ENDIF
328000020226     C     DOPIS3        ENDSR
328100980312     C******************************************************
328200980312     C** CALCOLO IL VALORE DA MOLTIPLICARE/RPV/ARROTONDAMENTI
328300980312     C******************************************************
328400980312     C     CALCOL        BEGSR
328500980313     C     *LIKE         DEFINE    TAMARL        ARR
328600980313     C     *LIKE         DEFINE    TASIAS        PESTAS
328700980313     C     *LIKE         DEFINE    TASIAS        ISOPES
328800980312     C     *LIKE         DEFINE    TASIAS        ISOPE2
328900980312     C     *LIKE         DEFINE    TPTRPV        WRPV
329000980313     C     *LIKE         DEFINE    TPTARL        WARL
329100980313     C     *LIKE         DEFINE    TPTARF        WARF
329200980313     C     *LIKE         DEFINE    TPTARO        WARO
329300980312     C                   Z-ADD     0             ISOPES
329400980316     C                   SELECT
329500030203
329600030204     c                   When      wFiso = '='
329700030203     c                   Z-Add     TasBan        Isopes
329800030203
329900980429     C     WTPG          WHENEQ    '0'
330000980429     C     WTPG          OREQ      '3'                                          KG
330100020826     C                   Z-ADD     USAPKF        ISOPES
330200980316     C*
330300980429     C     WTPG          WHENEQ    '1'                                          COLLI
330400980312     C                   Z-ADD     TASNCL        ISOPES
330500980316     C*
330600980429     C     WTPG          WHENEQ    '2'                                          SPEDIZ
330700980312     C                   Z-ADD     1             ISOPES
330800980316     C*
330900980429     C     WTPG          WHENEQ    '4'                                          VALORE
331000980429     C     WTPG          OREQ      '5'                                          Q.TA'
331100980312     C                   Z-ADD     TASQFT        ISOPES
331200980429     C*
331300980429     C     WTPG          WHENEQ    'T'                                          TRASPORTO
331400980429     C                   Z-ADD     TASPOR        ISOPES
331500980429     C                   ADD       TASINL        ISOPES
331600980429     C                   EXSR      CERISO
331700980429     C                   ADD       VAISO         ISOPES
331800060414     C                   ADD       VAtsp         ISOPES
331900980316     C                   ENDSL
332000980312     C** MINIMO TASSABILE STD
332100980312     C                   Z-ADD     0             MINTAS
332200980312     C**
332300980312     C                   Z-ADD     1             A
332400980312     C     WTPG          LOOKUP    CTR(A)                                 05
332500980312     C   05A             OCCUR     DSTR
332600980312     C   05              Z-ADD     §TRATA        MINTAS
332700980312     C**
332800980312     C** SOLO PER TAD (PER TPD NON MI SERVE)
332900980428    1C     WDET          IFEQ      'TAD'
333000980312     C                   CLEAR                   MINTAR
333100980312     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
333200980312     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
333300980312     C   05              Z-ADD     §TRATA        MINTAR
333400980428    2C     TAMATA        IFGT      0
333500980312     C                   Z-ADD     TAMATA        MINTAR
333600980428    2C                   ENDIF
333700980428    1C                   ENDIF
333800980312     C**
333900980428     C** RPV SOLO PER TARIFFA 0 E 3
334000980428    1C     §TRRPV        IFEQ      'S'
334100980428     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
334200980428    2C     WDET          IFEQ      'TAD'
334300980428     C     TASVLF        ANDGT     0
334400020218    3C                   DO        200           A
334500980428     C     A             OCCUR     TARDS
334600980428    4C     TSCG          IFGE      ISOPES
334700980428     C                   GOTO      POITAS
334800980428    4C                   END
334900980428    3C                   END
335000980428     C** RICERCO RPV CON PESO REALE
335100980428    3C     TSCG          IFLT      ISOPES
335200980428     C                   SETON                                        94
335300980527     C                   MOVEL     MSG(5)        TASMSG
335400980527     C                   GOTO      FINE
335500980428    3C                   END
335600980428     C     POITAS        TAG
335700980428     C                   Z-ADD     TRPV          WRPV
335800040603     C                   Z-ADD     TRPV          rappv
335900980428    2C                   ENDIF
336000980428     C**
336100980428    2C     TASVLF        IFNE      0
336200980428     C     WRPV          ANDNE     0
336300980428     C                   Z-ADD     0             COMPRV            9 3
336400980428     C     TASVLF        MULT      WRPV          COMPRV
336500980428     C     COMPRV        MULT      100           ISOPE2
336600980428    3C     ISOPE2        IFGT      ISOPES
336700980428     C                   Z-ADD     ISOPE2        ISOPES
336800980428    3C                   ENDIF
336900980428    2C                   ENDIF
337000980428    1C                   ENDIF
337100990923     C**
337200990923     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
337300990923    0C     §TRARR        IFEQ      'S'
337400980313     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
337500980312     C     ISOPES        IFGE      MINTAS
337600980312     C                   Z-ADD     0             ARR
337700980313     C     ISOPES        IFGT      WARL
337800980313     C                   Z-ADD     WARO          ARR
337900980312     C                   ELSE
338000980313     C                   Z-ADD     WARF          ARR
338100980312     C                   END
338200980313     C**
338300980313     C                   Z-ADD     0             RESTO            15 5
338400980312     C                   Z-ADD     0             ISOPEX           13 0
338500980312     C     ARR           IFNE      0
338600980312     C     ISOPES        ANDNE     0
338700980312     C     ISOPES        DIV       ARR           ISOPEX
338800980312     C                   MVR                     RESTO
338900980312     C     RESTO         IFGT      0
339000980312     C     ISOPEX        MULT      ARR           ISOPES
339100980312     C                   ADD       ARR           ISOPES
339200980312     C                   ENDIF
339300980312     C                   ENDIF
339400980312     C                   ENDIF
339500980312     C**
339600980312     C***        GIUARR    TAG
339700980312    0C                   ENDIF
339800980312     C                   ENDSR
339900970521     C******************************************************
340000941107     C** CALCOLA DIRITTO ASSICURATIVO - VARIA E
340100941112     C******************************************************
340200941107     C     CALVE         BEGSR
340300941107     C                   MOVEL     'R'           FISO
340400980506     C                   MOVEL     'N'           WCAR
340500980430     C                   EXSR      CERTPT
340600980430     C     WEND          IFEQ      ' '
340700990922     C** TARIFFA A VALORE TOLGO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DI CONTO
340800990922     C*  E CALCOLO VALORE AL KG (FISSO) PER LA PARTE RESTANTE
340900980428    1C     TPTTPG        IFEQ      'V'
341000980312     C                   CLEAR                   ISOPES
341100990922     C* CONVERTO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DELLA TARIFFA
341200990922     C*
341300990922     C     *LIKE         DEFINE    §GELRP        WLRP
341400990922     C*
341500990922     C     §TADIV        IFNE      §GEDCN
341600990922     C                   CLEAR                   YEUR
341700990922     C                   MOVEL     §GEDCN        YECDVI
341800990922     C                   MOVEL     §TADIV        YECDVO
341900990922     C                   Z-ADD     §GELRP        YECIMI
342000991014     C   12              MOVE      '3'           YECDCO
342100991014     C  N12              MOVE      '0'           YECDCO
342200990922     C*
342300990922     C                   CALL      'YEURCO'
342400990922     C                   PARM                    YEUR
342500990922     C*
342600990922     C     YECESI        IFEQ      ' '
342700990922     C                   Z-ADD     YECIMO        WLRP
342800990922     C                   END
342900991112     C* SE TARIFFE UGUALI VALORIZZO WLRP
343000991112     C                   ELSE
343100991118     C                   Z-ADD     §GELRP        WLRP
343200990922     C                   ENDIF
343300990922     C*
343400990922    2C     TPTVLM        IFGT      WLRP
343500990922     C     TPTVLM        SUB       WLRP          ISOPES
343600020826     C     ISOPES        MULT      USAPKF        ISOPES
343700980312    2C                   ENDIF
343800980312    1C                   ENDIF
343900980430     C**
344000980312     C                   Z-ADD     ISOPES        TPDPES
344100941112     C                   EXSR      CALACD
344200941112     C                   Z-ADD     VARIA         DIRITT
344300941112     C**
344400941112     C     TPTAPL        IFNE      *BLANKS
344500941112     C     TIPBOL        IFEQ      'F'
344600941112     C     TPTAPL        IFEQ      'A'
344700941112     C                   Z-ADD     0             DIRITT
344800941112     C                   END
344900941112     C                   ELSE
345000941112     C     TPTAPL        IFEQ      'P'
345100941112     C                   Z-ADD     0             DIRITT
345200941112     C                   END
345300941112     C                   END
345400941112     C                   END
345500941112     C** TIPO APPLICAZIONE
345600941112     C** P - SOLO PER FRANCHI
345700941112     C** A - SOLO PER ASSEGNATI
345800980430     C                   ENDIF
345900941107     C                   ENDSR
346000941107     C/SPACE
346100941112     C******************************************************
346200941112     C** CALCOLA MAGGIORAZIONE TIPO SERVIZIO
346300941112     C******************************************************
346400941112     C     CALTS         BEGSR
346500980506     C                   MOVEL     'S'           WCAR              1
346600090924     c
346700090924     C* 24/09/09 --> Ora applichiamo sempre la maggiorazine espresso
346800090924     c
346900980506     C* SE TIPO SERVIZIO DELLA TARIFFA = A TIPO SERVIZIO BOLLA
347000980506     C*  NON PRENDO LA TARIFFA DI CARTELLO
347100090922     C***  TAMTSP        IFEQ      TASTSP
347200090922     C***                MOVEL     'N'           WCAR              1
347300090922     C***                ENDIF
347400980506     C* SIGLA VARIA CORRISPONDENTE
347500980506     C                   MOVEL     §5EFTC        FISO
347600980506     C                   EXSR      CERTPT
347700980506     C     WEND          IFEQ      ' '
347800980506     C**
347900980506     C** A TRASPORTO
348000980430     C     TPTTPG        IFEQ      'T'
348100980312     C                   Z-ADD     IMPON         ISOPES
348200941112     C                   END
348300980312     C**
348400980312     C                   Z-ADD     ISOPES        TPDPES
348500941112     C                   EXSR      CALACD
348600941112     C                   Z-ADD     VARIA         WTSP
348700980506     C                   ENDIF
348800941112     C**
348900941112     C                   ENDSR
349000941112     C******************************************************
349100020225     C** CERCA TARIFFA TNTAM PIU' RELATIVA CARTELLO
349200941112     C******************************************************
349300941107     C     CERTAM        BEGSR
349400020225      *
349500941111     C     *LIKE         DEFINE    TAMPRG        PRG2
349600941111     C     *LIKE         DEFINE    TAMKSC        KSC2
349700941111     C     *LIKE         DEFINE    TAMCTR        CTR2
349800061219      * se in tasflo c'è una data riferimento per il recupero della tariffa la imposto
349900061219      * solo temporaneamente al posto della data spedizione
350000061219     c                   clear                   Sav_dsp
350100070108     c                   if        §asdrt  > *zeros
350200061219     c                   eval      Sav_dsp = Tasdsp
350300070108     c***                eval      tasdsp = §asdrt
350400070108     c                   movel     §asdrt        tasdsp
350500061219     c                   endif
350600020225      *
350700941112     C                   SETOFF                                       180717
350800020227     C                   SETOFF                                       0890
350900941111     C                   Z-ADD     0             PRG2
351000941111     C                   Z-ADD     0             KSC2
351100941111     C                   Z-ADD     0             CTR2
351200941107     C                   Z-ADD     0             TAMKSC
351300941107     C                   Z-ADD     0             TAMPRG
351400941107     C                   Z-ADD     0             TAMCTR
351500980605     C     KTAM          SETLL     TNTAM000                               08
351600980605     C* MANCA TARIFFA CLIENTE
351700980605    1C     *IN08         IFEQ      *OFF
351800980605     C                   MOVEL     MSG(3)        TASMSG
351900980527     C                   SETON                                        94
352000980527     C                   GOTO      ENDTAM
352100980605    1C                   ENDIF
352200980605     C**
352300941107     C     SUTAM         TAG
352400980605     C**
352500941107     C     KTAM          READE     TNTAM000                               07
352600980605     c**
352700941107     C** SE HO LETTO UNA TARIFFA SCADUTA (17) E IL
352800980605     C** RECORD SUCCESSIVO MI ACCENDE FINE FILE
352900980605     C** chaino vecchio progressivo per tassare con tariffa scaduTa
353000980605    1C     *IN07         IFEQ      *ON
353100980605     C   17KTAM2         CHAIN     TNTAM000                           18
353200980605     c**
353300980605     C     *IN17         IFEQ      *OFF
353400980605    2C     *IN18         OREQ      *ON
353500980605     C                   MOVEL     MSG(3)        TASMSG
353600980605     C                   SETON                                        94
353700980605    2C                   ENDIF
353800980605     C**
353900980605     C                   GOTO      ENDTAM
354000980605     C**
354100980605     C                   ELSE
354200980605     C**
354300980605     C** SE NON E' TARIFFA ADATTA PER LA BOLLA (ITALIA INVECE CHE ESTER
354400980605     C**  O VICEVERSA) --> RILEGGO COME SE NON AVESSI LETTO
354500980605     C     TAMFIE        IFNE      WBOEST
354600980605     C     TAMFIE        ANDNE     ' '
354700980605     C                   GOTO      SUTAM
354800980605     C                   ENDIF
354900980605    1C                   ENDIF
355000980605     C**
355100980605     C** TARIFFA DA DECORRERE : SEGNALO SE NON C'E' UNA TARIFFA SCADUTA
355200980605     C**    (di fatto non e' possibile perche' se c'e' una da decorrere
355300980605     C**    e ce n'e' una prima e' la tariffa valida dal momento che
355400980605     C**    non e' possibile che esista una buco di decorrenze scadenze
355500980605     C**    tra i vari progressivi)
355600980605    1C     TASDSP        IFLT      TAMDDT
355700980605     C   17KTAM2         CHAIN     TNTAM000                           18
355800980605     C* NON C'E' TARIFFA SCADUTA O NON TROVATA DI NUOVO
355900980605    2C     *IN17         IFEQ      *OFF
356000980605     C     *IN18         OREQ      *ON
356100980605     C                   MOVEL     MSG(10)       TASMSG
356200980605     C                   SETON                                        94
356300980605    2C                   ENDIF
356400980605     C**
356500941107     C                   GOTO      ENDTAM
356600980605    1C                   END
356700980605     C**
356800980605     C** TARIFFA SCADUTA: DATA SPEDIZIONE MAGGIORE DELLA DATA  SCADENZA
356900980605    1C     TASDSP        IFGT      TAMDST
357000941107     C                   SETON                                        17
357100980605     C                   Z-ADD     TAMKSC        KSC2
357200980605     C                   Z-ADD     TAMCTR        CTR2
357300980605     C                   Z-ADD     TAMPRG        PRG2
357400941107     C                   GOTO      SUTAM
357500980605    1C                   ENDIF
357600020225     C*
357700020225     C** CERCO LA CARTELLO RELATIVA ALLA TARIFFA DEL CLIENTE
357800020227     C*  SE NON HO GIA' UNA TARIFFA DI CARTELLO E SE 94 SPENTO
357900020227     C     ENDTAM        TAG
358000020227     C*
358100020225     C*
358200020227    0C     *IN94         IFEQ      *OFF
358300020227      *
358400020227     C                   SETOFF                                       90
358500020227     C                   Z-ADD     1             T
358600020227     C     TAMKSC        LOOKUP    TAC(T)                                 90
358700020227      *
358800020227      * PER 90 SPENTO VUOL DIRE CHE NON HO TROVATO NESSUNA CARTELLO
358900020227    1C     *IN90         IFEQ      *OFF
359000020319     C***                  CLEARWNET
359100020225     C                   MOVEL     TAMFLO        DSTA01
359200020225     C*
359300020319     C**                   SELEC
359400020225     C* VERIFICO IL TIPO SERVIZIO
359500020319     C**         TAMTSP    WHEQ 'P'
359600020319     C**                   MOVEL'PPT'     WNET    3
359700020225     C* VERIFICO SE TARIFFA ITALIA VERIFICO SE NETWORK DPD
359800020319     C**         TAMFIE    WHEQ 'I'
359900020319     C**         §TADPD    ANDEQ'S'
360000020319     C**                   MOVEL'DPD'     WNET
360100020225     C* VERIFICO SE TARIFFA ESTERA VERIFICO SE NETWORK FEDEX
360200020319     C**         TAMFIE    WHEQ 'E'
360300020319     C**         §TAFED    ANDEQ'S'
360400020319     C**                   MOVEL'FED'     WNET
360500020225     C* VERIFICO SE TARIFFA EUROEXPRESS
360600020319     C**         TAMFIE    WHEQ 'E'
360700020319     C**                   MOVEL'EEX'     WNET
360800020225     C* VERIFICO SE TARIFFA ITALIA
360900020319     C**         TAMFIE    WHEQ 'I'
361000020319     C**                   MOVEL'COR'     WNET
361100020225     C*
361200020319     C**                   ENDSL
361300020225     C* SE NETWORK UGUALE A BLANK CERCO LA TARIFFA DI CARTELLO CORRIERE
361400020319     C**         WNET      IFEQ *BLANK
361500020319     C**                   MOVEL'COR'     WNET
361600020319     C**                   END
361700020225     C* AGGANCIO LA TABELLA DELLE TARIFFE DI CARTELLO
361800020319     C                   EXSR      CERTCA
361900020319     C**                   SETOF                     90
362000020319     C**         WNET      LOKUPNTW,T                    90
362100020225     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
362200020319     C**         *IN90     IFEQ *OFF
362300020319     C**                   MOVELMSG,9     TASMSG
362400020319     C**                   GOTO ENDTA2
362500020319     C**                   ENDIF
362600020319    1C                   ENDIF
362700020227     C*
362800020319     C**                   Z-ADDTAC,T     CARKSC
362900020319     C**                   Z-ADDCTC,T     CARCTR
363000020319     C**                   Z-ADDPRC,T     CARPRG
363100020319     C**                   MOVE DIC,T     DIVCAR
363200020227     C*
363300020227    0C                   ENDIF
363400061220      * se impostato il campo del salvataggio della data spedizione
363500061220     c                   if        Sav_dsp <> *zeros
363600061220     c                   eval      tasdsp = Sav_dsp
363700061220     c                   clear                   Sav_dsp
363800061220     c                   endif
363900020225     C*
364000941107     C*
364100020227     C     ENDTA2        ENDSR
364200020618     C******************************************************
364300020618     C* RICERCA DETTAGLIO SU TUTAD
364400020618     C******************************************************
364500020618     C     RICTAD        BEGSR
364600020618     C     KTAD          SETLL     TITAD000                               09
364700020618     C** 49  TROVATA TARIFFA DEL CAP   DI ARRIVO
364800020618     C   09              SETON                                        49
364900020618     C**
365000020618    1C     *IN09         IFEQ      *OFF
365100020618     C                   MOVEL     *BLANKS       COMNAZ
365200020618     C                   MOVEL     *BLANKS       COMCAP
365300020618     C     KTAD          SETLL     TITAD000                               09
365400020618    2C     *IN09         IFEQ      *OFF
365500020618     C*
365600020618     C                   Z-ADD     1             A
365700020618     C     COMCTS        LOOKUP    CTS(A)                                 05
365800020618     C**
365900020618     C* SE TROVATO COD.TASSAZIONE
366000020618    3C     *IN05         IFEQ      *ON
366100020618     C     A             OCCUR     DSCT
366200020618     C                   MOVEL     §CTRAP        COMCTS
366300020618     C** CARICO REGIONE AL POSTO DEL CODICE DI TASSAZIONE
366400020618     C     KTAD          SETLL     TITAD000                               09
366500020618    3C                   ENDIF
366600020618     C**
366700020618    3C     *IN09         IFEQ      *OFF
366800020618     C*
366900020618     C     WBOEST        IFEQ      'E'
367000020618     C                   MOVEL     'EE'          COMCTS
367100020618     C                   ELSE
367200020618     C                   MOVEL     'IT'          COMCTS
367300020618     C                   END
367400020618     C     KTAD          SETLL     TITAD000                               09
367500020618    3C                   ENDIF
367600020618    2C                   ENDIF
367700020618    1C                   ENDIF
367800020618     C                   ENDSR
367900941107     C/SPACE
368000941112     C******************************************************
368100990721     C** CERCA TARIFFA TITAD DETTAGLIO E CARICA DS
368200941112     C******************************************************
368300941107     C     CARTAR        BEGSR
368400941112C    C                   SETOFF                                       495694
368500941112     C                   MOVEL     TASCAD        COMCAP            9
368600990923     C                   MOVEL     TASNZD        COMNAZ            3
368700941112     C                   MOVEL     TASCTS        COMCTS
368800020618     C                   MOVEL     TASLNP        WLNP
368900020618     C                   EXSR      RICTAD
369000020618     C   09              GOTO      LATAR
369100020618     C**
369200020618     C** SE NON TROVATO TITAD CON LA LINEA DI PARTENZA, CERCO CON LA
369300020618     C**  REGIONE TROVATA DA ORGANIGRANNA DI TASLNP
369400020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
369500020618     C     TASLNP        IFNE      SAVLNP
369600020618     C     TASLNP        CHAIN     AZORG01L                           31
369700020618     C   31              CLEAR                   ORGDE7
369800020618     C                   MOVEL     ORGDE7        OG147
369900020618     C                   MOVEL     TASLNP        SAVLNP
370000020618     C                   ENDIF
370100020618     C**
370200020618     C                   MOVEL     TASCAD        COMCAP            9
370300020618     C                   MOVEL     TASNZD        COMNAZ            3
370400020618     C                   MOVEL     TASCTS        COMCTS
370500020618     C                   MOVEL     §OGRET        WLNP
370600020618     C                   EXSR      RICTAD
370700020618     C   09              GOTO      LATAR
370800020618     C**
370900020618     C** SE NON TROVATO TITAD CON LA REGIONE DI TASLNP, CERCO CON LA
371000020618     C**  LINEA ITALIA 950
371100020618     C                   MOVEL     TASCAD        COMCAP            9
371200020618     C                   MOVEL     TASNZD        COMNAZ            3
371300020618     C                   MOVEL     TASCTS        COMCTS
371400020618     C                   MOVEL     950           WLNP
371500020618     C                   EXSR      RICTAD
371600020618     C   09              GOTO      LATAR
371700941112     C*
371800941112     C*--------------------TARIFFA REVERSIBILE----------------------------------
371900941107     C     TIPBOL        CABNE     'A'           MANTAR
372000941107     C** TARIFFA REVERSIBILE SOLO PER RITORNO(TBL=A)
372100941107     C     TASLNP        CABEQ     68            MANTAR
372200941107     C** NO TARIFFA REVERSIBILE PER 68
372300120511     C**** KSCFIL        IFEQ      888
372400981027     C                   Z-ADD     TASLNA        KFIL
372500120511     C****               ELSE
372600120511     C****               MOVEL     KSCFIL        KFIL
372700120511     C****               ENDIF
372800981008     C**
372900941107     C                   SETON                                        56
373000941112     C                   MOVEL     TASCAM        COMCAP
373100990923     C                   MOVEL     TASNZM        COMNAZ
373200941123     C                   MOVEL     TASMCT        COMCTS
373300020618     C                   MOVEL     KFIL          WLNP
373400020618     C                   EXSR      RICTAD
373500020618     C   09              GOTO      LATAR
373600020618     C*
373700020618     C** SE NON TROVATO TITAD CON LA LINEA CLIENTE, CERCO CON LA
373800020618     C**  REGIONE TROVATA DA ORGANIGRANNA DELLA LIENA CLIENTE
373900020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
374000020618     C     KFIL          IFNE      SAVLNP
374100020618     C     KFIL          CHAIN     AZORG01L                           31
374200020618     C   31              CLEAR                   ORGDE7
374300020618     C                   MOVEL     ORGDE7        OG147
374400020618     C                   MOVEL     KFIL          SAVLNP
374500020618     C                   ENDIF
374600020618     C*
374700020618     C                   MOVEL     TASCAM        COMCAP
374800020618     C                   MOVEL     TASNZM        COMNAZ
374900020618     C                   MOVEL     TASMCT        COMCTS
375000020618     C                   MOVEL     §OGRET        WLNP
375100020618     C                   EXSR      RICTAD
375200020618     C   09              GOTO      LATAR
375300020618     C**
375400020618     C** SE NON TROVATO TITAD CON LA REGIONE DI KFIL, CERCO CON LA
375500020618     C**  LINEA ITALIA 950
375600020618     C                   MOVEL     TASCAM        COMCAP
375700020618     C                   MOVEL     TASNZM        COMNAZ
375800020618     C                   MOVEL     TASMCT        COMCTS
375900020618     C                   MOVEL     950           WLNP
376000020618     C                   EXSR      RICTAD
376100020618     C   09              GOTO      LATAR
376200020618     C**
376300020618     C**         KTDREV    SETLLTITAD000                 09
376400020618     C** 09                SETON                     49
376500020618     C** 09                GOTO LATAR
376600020618     C**
376700020618     C**                   MOVEL*BLANKS   COMNAZ
376800020618     C**                   MOVEL*BLANKS   COMCAP
376900020618     C**         KTDREV    SETLLTITAD000                 09
377000020618     C** 09                GOTO LATAR
377100020618     C**
377200020618     C**                   Z-ADD1         A
377300020618     C**         COMCTS    LOKUPCTS,A                    05
377400020618     C**N05                GOTO POIT2
377500020618     C**         A         OCUR DSCT
377600020618     C**                   MOVEL§CTRAP    COMCTS
377700020618     C**         KTDREV    SETLLTITAD000                 09
377800020618     C** 09                GOTO LATAR
377900020618     C**         POIT2     TAG
378000020618     C**         WBOEST    IFEQ 'E'
378100020618     C**                   MOVEL'EE'      COMCTS
378200020618     C**                   ELSE
378300020618     C**                   MOVEL'IT'      COMCTS
378400020618     C**                   END
378500020618     C**         KTDREV    SETLLTITAD000                 09
378600020618     C** 09                GOTO LATAR
378700941107     C** NON TROVATO NEMMENO LA TARIFFA REVERSIBILE
378800941107     C     MANTAR        TAG
378900980527     C     *IN09         IFEQ      *OFF
379000980527     C                   SETON                                        94
379100980527     C                   MOVEL     MSG(8)        TASMSG
379200980527     C                   GOTO      FINE
379300980527     C                   ENDIF
379400941107     C** MANCA TARIFFA
379500941107     C     LATAR         TAG
379600030902     C                   EXSR      CARDST
379700941112     C** CARICA DS DELLA TARIFFA
379800941107     C                   ENDSR
379900941107     C/SPACE
380000941112     C******************************************************
380100941107     C** CARICA DS TARIFFA
380200941112     C******************************************************
380300941107     C     CARDST        BEGSR
380400980514     C     *LIKE         DEFINE    TADRPV        RAPPV
380500980514     C**
380600020218     C                   DO        200           A
380700941112     C     A             OCCUR     TARDS
380800941112     C                   CLEAR                   TARDS
380900941112     C                   END
381000941112     C                   Z-ADD     0             RAPPV
381100980514     C                   CLEAR                   WELAMT
381200941112     C**
381300980514    1C                   DO        *HIVAL        A
381400980514     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
381500980514    2C     WELAMT        IFNE      'S'
381600020618     C** 56      KTDREV    READETITAD000                 15
381700020618     C**N56      KTAD      READETITAD000                 15
381800020618     C     KTAD          READE     TITAD000                               15
381900941107     C   15              GOTO      ENDDST
382000980514    2C                   ENDIF
382100980514     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
382200980514    2C     WELAMT        IFEQ      'S'
382300980515     C                   MOVEL     'E'           WELAMT            1
382400980514    2C                   ENDIF
382500980514     C**
382600941107     C     A             OCCUR     TARDS
382700980514     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
382800980514     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
382900980514     C**
383000941120     C                   Z-ADD     TADLNP        TLNP
383100941107     C                   MOVEL     TADCTS        TCTS
383200990721     C                   MOVEL     TADNAZ        TNAZ
383300941110     C                   MOVEL     TADCAP        TCAP
383400941107     C                   Z-ADD     TADCTR        TCTR
383500941107     C                   Z-ADD     TADRPV        TRPV
383600941122     C                   Z-ADD     TADMIN        TMIN
383700941122     C                   Z-ADD     TADMAX        TMAX
383800040603     C***!!!****         Z-ADD     TADRPV        RAPPV
383900980514     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
384000980514    2C     WELAMT        IFNE      'E'
384100980514     C**
384200980514    3C     TADSGL        IFEQ      MINTAS
384300980514     C                   MOVEL     'E'           WELAMT
384400980514   X3C                   ELSE
384500980514     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
384600980514     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
384700980514     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
384800980514     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
384900980515    4C     TADSGL        IFGT      MINTAS
385000980514     C                   MOVEL     'S'           WELAMT
385100980514     C                   Z-ADD     MINTAS        TSCG
385200980514     C**
385300980522     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
385400980522    5C     TAMATA        IFEQ      0
385500980522     C     TAMATA        ORLE      MINTAS
385600980522     C     TADSGL        ORGT      TAMATA
385700980522     C**
385800980522    6C     UNOCTR        IFEQ      4
385900980514     C     UNOCTR        OREQ      0
386000980514     C     MINTAS        DIV(H)    100           WMTAS            15 5
386100980514     C                   ELSE
386200980514     C                   Z-ADD     MINTAS        WMTAS
386300980522    6C                   ENDIF
386400980514     C* TARIFFA
386500980514     C     WMTAS         MULT(H)   TADITR        TITR
386600980514     C     WMTAS         MULT(H)   TADINO        TINO
386700980522     C                   ELSE
386800980522     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
386900980522     C                   Z-ADD     TADITR        TITR
387000980522     C                   Z-ADD     TADINO        TINO
387100980522    5C                   ENDIF
387200980522    4C                   ENDIF
387300980514    3C                   ENDIF
387400980514    2C                   ENDIF
387500980514     C*
387600980514     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
387700980514    2C     WELAMT        IFNE      'S'
387800980514     C                   Z-ADD     TADSGL        TSCG
387900980514     C                   Z-ADD     TADITR        TITR
388000980514     C                   Z-ADD     TADINO        TINO
388100980514    2C                   ENDIF
388200980514    1C                   ENDDO
388300941107     C     ENDDST        ENDSR
388400941107     C/SPACE
388500941112     C******************************************************
388600941110     C** CARICAMENTO TABELLE SOLO LA 1 VOLTA
388700941112     C******************************************************
388800941110     C     CARTAB        BEGSR
388900980605     C     *LIKE         DEFINE    TAMATA        MINTAS
389000980605     C     *LIKE         DEFINE    TAMATA        MINTAR
389100990923     C     *LIKE         DEFINE    §TADIV        DIVCAR
389200020618     C                   CLEAR                   SAVLNP
389300980605     C**
389400980605     C                   OPEN      TABEL00F
389500980605     C**
389600941110     C                   EXSR      CARQT
389700941110     C** CARICAMENTO IAC
389800941110     C                   EXSR      CARTR
389900941110     C** CARICAMENTO TIPI TARIFFA
390000941110     C                   EXSR      CAR$2
390100941110     C** CARICAMENTO SIGLE VARIE
390200000103     C                   EXSR      CARCC
390300000103     C** CARICAMENTO SIGLE VARIE ESENTI
390400941112     C                   EXSR      CARCV
390500941112     C** CARICAMENTO CAMBI
390600941110     C                   EXSR      CARCT
390700941110     C** CARICAMENTO CODIFICI TASSAZIONE
390800941112     C                   EXSR      CAR5E
390900941112     C** CARICA TIPI SERVIZIO
391000980429     C                   EXSR      CAR1A
391100980429     C** CARICA TIPI INCASSO C/A MITTENTI
391200980603     C                   EXSR      CARTB
391300980603     C** CARICA DS TIPI BOLLA CHE NON PREVEDONO ASSICURAZIONE
391400020319     C                   EXSR      CAREST
391500980605     C** CARICA P.O. ESTERI
391600990917     C                   EXSR      CARDIV
391700990917     C** CARICAMENTO MONETA DI CONTO
391800020320     C                   EXSR      CARPAR
391900020320     C** CARICA TARIFFE DI CARTELLO
392000031120     C                   EXSR      CAR3A
392100031120     C** CARICO VARIE DELLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
392200980605     C                   CLOSE     TABEL00F
392300040505     C**
392400040505     C                   OPEN      TNTBE01L
392500040505     C** caricamento percentuale carburante
392600040505     C                   EXSR      CARPSC
392700080617     C** caricamento filiali addebito lasciato avviso
392800080617     C                   EXSR      CARLAV
392900111102     C** caricamento filiali addebito centro storico da cartello
393000111102     C                   EXSR      CARZTL
393100040505     C**
393200040505     C                   CLOSE     TNTBE01L
393300040505     C**
393400941110     C                   ENDSR
393500941110     C**********************************************************
393600941110     C** CARICAMENTO DS MULTIPLA  CODICI TASSAZIONE
393700941110     C**********************************************************
393800941110     C     CARCT         BEGSR
393900041220     C                   DO        600           A
394000941110     C     A             OCCUR     DSCT
394100941110     C                   CLEAR                   DSCT
394200941110     C                   END
394300941110     C** PULIZIA DS
394400941110     C                   MOVEL     'CT'          COD
394500941110     C     KTAB          SETLL     TABEL                                  07
394600941110     C  N07              SETON                                          H7
394700941110     C   H7              GOTO      FINE
394800941110     C                   Z-ADD     0             K
394900941110     C                   DO        *HIVAL        A
395000941110     C     KTAB          READE     TABEL                                  07
395100941110     C   07              GOTO      ENDCTS
395200941110     C     TBLKEY        IFNE      *BLANKS
395300941110     C                   ADD       1             K
395400941110     C                   MOVEL     TBLKEY        CTS(K)
395500941110     C     K             OCCUR     DSCT
395600941110     C                   MOVEL     TBLUNI        DSCT
395700941110     C                   END
395800941110     C                   END
395900941110     C     ENDCTS        TAG
396000941110     C                   ENDSR
396100941110     C/SPACE
396200941112     C**********************************************************
396300941112     C** CARICAMENTO TIPI SERVIZIO
396400941112     C**********************************************************
396500941112     C     CAR5E         BEGSR
396600941112     C                   DO        10            A
396700941112     C     A             OCCUR     DS5E
396800941112     C                   CLEAR                   DS5E
396900941112     C                   END
397000941112     C** PULIZIA DS
397100941112     C                   MOVEL     '5E'          COD
397200941112     C     KTAB          SETLL     TABEL                                  07
397300941112     C  N07              SETON                                          H7
397400941112     C   H7              GOTO      FINE
397500941112     C                   Z-ADD     0             K
397600941112     C                   DO        *HIVAL        A
397700941112     C     KTAB          READE     TABEL                                  07
397800941112     C   07              GOTO      END5E
397900941112     C     TBLKEY        IFNE      *BLANKS
398000941112     C                   ADD       1             K
398100941112     C                   MOVEL     TBLKEY        TS(K)
398200941112     C     K             OCCUR     DS5E
398300941112     C                   MOVEL     TBLUNI        DS5E
398400941112     C                   END
398500941112     C                   END
398600941112     C     END5E         TAG
398700941112     C                   ENDSR
398800980429     C**********************************************************
398900980429     C** CARICAMENTO TIPI INCASSO C/A
399000980429     C**********************************************************
399100980429     C     CAR1A         BEGSR
399200980430     C                   MOVEA     *ALL'9'       TIC
399300980429     C                   MOVEL     '1A'          COD
399400980429     C                   Z-ADD     0             K
399500980429     C     KTAB          SETLL     TABEL                                  07
399600980429     C     KTAB          READE     TABEL                                  07
399700980430    1C     *IN07         DOWEQ     *OFF
399800980430    2C     TBLKEY        IFNE      *BLANKS
399900980430     C     TBLFLG        ANDEQ     ' '
400000980429     C                   MOVEL     TBLUNI        DS1A
400100980430    3C     §1AFMB        IFEQ      'M'
400200980429     C                   ADD       1             K
400300980429     C                   MOVEL     TBLKEY        TIC(K)
400400980430    3C                   ENDIF
400500980430    2C                   ENDIF
400600980429     C     KTAB          READE     TABEL                                  07
400700980430    1C                   ENDDO
400800980429     C                   ENDSR
400900980603     C**********************************************************
401000980603     C** CARICAMENTO TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
401100980603     C**********************************************************
401200980603     C     CARTB         BEGSR
401300980603     C                   MOVEL     'TB'          COD
401400980603     C                   Z-ADD     0             K
401500980603     C     KTAB          SETLL     TABEL                                  07
401600980603     C     KTAB          READE     TABEL                                  07
401700980603    1C     *IN07         DOWEQ     *OFF
401800980603    2C     TBLKEY        IFNE      *BLANKS
401900980603     C     TBLFLG        ANDEQ     ' '
402000980603     C                   MOVEL     TBLUNI        DSTB
402100980603    3C     §TBFAS        IFEQ      '0'
402200980603     C                   ADD       1             K
402300980603     C                   MOVEL     TBLKEY        TBN(K)
402400980603    3C                   ENDIF
402500980603    2C                   ENDIF
402600980603     C     KTAB          READE     TABEL                                  07
402700980603    1C                   ENDDO
402800980603     C                   ENDSR
402900031120     C**********************************************************
403000031120     C** CARICAMENTO VARIE PER TIPI BOLLE CHE ACCETTANO UNA SINGOLA VARIA
403100031120     C**********************************************************
403200031120     C     CAR3A         BEGSR
403300031120     C                   MOVEL     '3A'          COD
403400031120     C                   Z-ADD     0             K
403500031120     C     KTAB          SETLL     TABEL                                  07
403600031120     C     KTAB          READE     TABEL                                  07
403700031120    1C     *IN07         DOWEQ     *OFF
403800031120    2C     TBLKEY        IFNE      *BLANKS
403900031120     C     TBLFLG        ANDEQ     ' '
404000031120     C                   MOVEL     TBLUNI        DS3A
404100031120    3C     §3ASVA        IFNE      ' '
404200031120     C                   ADD       1             K
404300031120     C                   MOVEL     §3ASVA        T3A(K)
404400031120    3C                   ENDIF
404500031120    2C                   ENDIF
404600031120     C     KTAB          READE     TABEL                                  07
404700031120    1C                   ENDDO
404800031120     C                   ENDSR
404900020319     C**********************************************************
405000020319     C** CARICAMENTO PO ESTERI
405100020319     C**********************************************************
405200020319     C     CAREST        BEGSR
405300020319     C*
405400020319     C                   CLEAR                   K
405500050504     C                   CLEAR                   KK                4 0
405600020319     C**
405700020319     C                   CLEAR                   CC
405800020319     C     *LOVAL        SETLL     AZORG01L
405900020319     C                   READ      AZORG01L                               07
406000020319    1C     *IN07         DOWEQ     *OFF
406100020319     C** FIL O AGENZIA
406200020319    2C     ORGFAG        IFEQ      'F'
406300020319     C     ORGFAG        OREQ      'A'
406400020319     C* NON ANNULLATO
406500020319    3C     ORGFVA        IFEQ      ' '
406600020319    4C                   MOVEL     ORGDE3        OG143
406700050504     c* p.o. esteri senza dpd
406800020319     C     §OGNTW        IFEQ      'EEX'
406900020319     C     §OGNTW        OREQ      'EUP'
407000020404     C*******    §OGNTW    OREQ 'DPD'
407100020319     C     §OGNTW        OREQ      'FED'
407200020319     C                   ADD       1             K
407300020319     C                   MOVEL     ORGFIL        EST(K)
407400020319    4C                   ENDIF
407500050504     c* p.o. estero totale, anche con DPD
407600050504    4C     §OGNTW        IFEQ      'EEX'
407700050504     C     §OGNTW        OREQ      'EUP'
407800050504     C     §OGNTW        OREQ      'DPD'
407900050504     C     §OGNTW        OREQ      'FED'
408000050504     C                   ADD       1             KK
408100050504     C                   MOVEL     ORGFIL        ESTtot(KK)
408200050504    4C                   ENDIF
408300020319    3C                   ENDIF
408400020319    2C                   ENDIF
408500020319     C**
408600020319     C                   READ      AZORG01L                               07
408700020319    1C                   ENDDO
408800020319     C                   ENDSR
408900941110     C********************************************************
409000941110     C** CARICAMENTO SIGLE VARIE
409100941110     C********************************************************
409200941110     C     CAR$2         BEGSR
409300941110     C                   CLEAR                   DS$2
409400941110     C                   MOVEL     '$2'          COD
409500941110     C                   MOVEL     *BLANKS       KEY
409600941110     C                   MOVEL     '1'           KEY
409700941110     C     KTABC         CHAIN     TABEL                              07
409800941110     C   07              SETON                                          H7
409900941110     C   H7              GOTO      FINE
410000941110     C                   MOVEL     TBLUNI        DS$2
410100941110     C                   ENDSR
410200000103     C********************************************************
410300000103     C** CARICAMENTO SIGLE VARIE  ESENTI IVA
410400000103     C********************************************************
410500000103     C     CARCC         BEGSR
410600000103     C                   CLEAR                   DSCC
410700020416     C                   CLEAR                   KK
410800000103     C                   MOVEL     'CC'          COD
410900000103     C                   MOVEL     *BLANKS       KEY
411000000103     C                   MOVEL     'VARIE'       KEY
411100000103     C     KTABC         SETLL     TABEL                                  07
411200020416     C**N07                GOTO ENDCC
411300000103     C                   DO        *HIVAL
411400000103     C     KTAB          READE     TABEL                                  07
411500000103     C   07              LEAVE
411600000103     C                   MOVEL     TBLUNI        DSCC
411700000103     C* CONTROLLO SE VARIA ESENTE CARICO LA SIGLA IN TABELLA
411800000103     C     §CCJEI        IFNE      *BLANKS
411900000103     C                   ADD       1             K
412000000103     C                   MOVE      TBLKEY        VES(K)
412100000103     C                   ENDIF
412200020416     C* CONTROLLO SE DA RESTITUIRE CON SIGAL VARIA E IMPORTO =0
412300020416     C     §CCSV0        IFEQ      'S'
412400050504     C                   ADD       1             KK                4 0
412500020416     C                   MOVE      TBLKEY        SV0(KK)
412600020416     C                   ENDIF
412700000103     C                   ENDDO
412800000103     C                   Z-ADD     0             K
412900020416     C                   Z-ADD     0             KK
413000000103     C     ENDCC         ENDSR
413100941112     C********************************************************
413200941112     C** CARICAMENTO CAMBI
413300941112     C********************************************************
413400941112     C     CARCV         BEGSR
413500941116     C                   Z-ADD     0             K
413600941116     C                   DO        50            A
413700941116     C     A             OCCUR     DSCV
413800941112     C                   CLEAR                   DSCV
413900941116     C                   END
414000941116     C                   MOVEL     *BLANKS       CV
414100941116     C*
414200941112     C                   MOVEL     'CV'          COD
414300941116     C     KTAB          SETLL     TABEL                                  07
414400941116     C  N07              GOTO      ENDCV
414500941116     C                   DO        *HIVAL        A
414600941116     C     KTAB          READE     TABEL                                  07
414700941116     C   07              GOTO      ENDCV
414800941116     C     TBLKEY        IFNE      *BLANKS
414900941116     C                   ADD       1             K
415000941116     C                   MOVEL     TBLKEY        CV(K)
415100941116     C     K             OCCUR     DSCV
415200941116     C                   MOVEL     TBLUNI        DSCV
415300941116     C                   END
415400941116     C                   END
415500941112     C     ENDCV         ENDSR
415600941110     C******************************************************
415700941110     C** CARICAMENTO TIPI TARIFFA
415800941110     C******************************************************
415900941110     C     CARTR         BEGSR
416000030205     C                   DO        50            A
416100941110     C     A             OCCUR     DSTR
416200941110     C                   CLEAR                   DSTR
416300941110     C                   END
416400941110     C**
416500941110     C                   MOVEL     'TR'          COD
416600941112     C                   Z-ADD     0             K
416700941110     C     KTAB          SETLL     TABEL                                  07
416800941110     C  N07              SETON                                          H7
416900941110     C   H7              GOTO      FINE
417000941110     C                   DO        *HIVAL        A
417100941110     C     KTAB          READE     TABEL                                  07
417200941110     C   07              GOTO      TAB1
417300941110     C     TBLKEY        IFNE      *BLANKS
417400941110     C                   ADD       1             K
417500941110     C                   MOVEL     TBLKEY        CTR(K)
417600941110     C     K             OCCUR     DSTR
417700941110     C                   MOVEL     TBLUNI        DSTR
417800941110     C                   END
417900941110     C                   END
418000941110     C     TAB1          ENDSR
418100941110     C**********************************************************
418200020221     C** MEMORIZZA
418300941110     C** TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
418400941110     C**********************************************************
418500941110     C     CARPAR        BEGSR
418600020225      *
418700020225     C     *LIKE         DEFINE    TAMKSC        CARKSC
418800020225     C     *LIKE         DEFINE    TAMCTR        CARCTR
418900020225     C     *LIKE         DEFINE    TAMPRG        CARPRG
419000020221      *
419100020222      * RICHIAMO IL TRUL27R PER OGNI NETWORK PER CARICARMI LE TARIFFE DI CARTELLO IN SCHIERA
419200020221      *
419300020221     C* CORRIERE BARTOLINI
419400020221     C                   CLEAR                   TRUL27
419500020221     C                   MOVEL     'COR'         I27NTW
419600020222     C                   EXSR      SRTRUL
419700020221      * POSTE
419800020222     C                   CLEAR                   TRUL27
419900020222     C                   MOVEL     'PPT'         I27NTW
420000020222     C                   EXSR      SRTRUL
420100020222      * DPD
420200020222     C                   CLEAR                   TRUL27
420300020222     C                   MOVEL     'DPD'         I27NTW
420400020222     C                   EXSR      SRTRUL
420500020222      * FEDEX
420600020222     C                   CLEAR                   TRUL27
420700020222     C                   MOVEL     'FED'         I27NTW
420800020222     C                   EXSR      SRTRUL
420900020222      * EUROEXPRESS
421000020222     C                   CLEAR                   TRUL27
421100020226     C                   MOVE      'L'           I27TLA
421200020222     C                   MOVEL     'EEX'         I27NTW
421300020222     C                   EXSR      SRTRUL
421400020226      *
421500020226     C                   CLEAR                   CARKSC
421600020226     C                   CLEAR                   CARCTR
421700020226     C                   CLEAR                   CARPRG
421800020222      *
421900020222      ** CARICO LE TARIFFE PARTICOLARI
422000941111     C                   EXSR      CAR1P
422100020222      *
422200941110     C                   ENDSR
422300040505     C**********************************************************
422400040505     C** CARICAMENTO SCHIERA SUPPLEMEMNTO CARBURANTE
422500040505     C**********************************************************
422600040505     C     CARPSC        BEGSR
422700040505
422800040507     C                   CLEAR                   KX
422900040505
423000040505     C                   MOVEL     'PSC'         KEYTBE
423100040505     C     KEYTBE        SETLL     TNTBE01L
423200040505     C                   DO        *HIVAL
423300040505     C     KEYTBE        READE     TNTBE01L
423400040505
423500040505     C                   IF        %EOF(TNTBE01L)
423600040505     C                   LEAVE
423700040505     C                   ENDIF
423800040505
423900040505     C                   IF        TBEATB = ' '
424000040507     C                   ADD       1             KX
424100040507     C                   IF        KX <= 999
424200040505     C                   MOVEL     TBEUNI        DPSC
424300040507     C                   Z-ADD     §PSCDAD       DS_DSC
424400040507     C                   Z-ADD     §PSCPER       DS_PSC
424500040507     C                   MOVE      WDSPSC        DSCA(KX)
424600040507     C                   ENDIF
424700040505     C                   ENDIF
424800040505
424900040505     C                   ENDDO
425000040507
425100040507     C                   SORTA     DSCA
425200040505
425300040505     C                   ENDSR
425400080617      *****************************************************************
425500080617      ** CARICAMENTO SCHIERA FILIALI ABILITATE ADDEBITO LASCIATO AVVISO
425600080617      *****************************************************************
425700080617     C     CARLAV        BEGSR
425800080617
425900080617     C                   CLEAR                   KX
426000080617
426100080617     C                   MOVEL     'LAV'         KEYTBE
426200080617     C     KEYTBE        SETLL     TNTBE01L
426300080617     C                   DO        *HIVAL
426400080617     C     KEYTBE        READE     TNTBE01L
426500080617
426600080617     C                   IF        %EOF(TNTBE01L)
426700080617     C                   LEAVE
426800080617     C                   ENDIF
426900080617
427000080617     C                   IF        TBEATB = ' '
427100080617     C                   ADD       1             KX
427200080617     C                   IF        KX <= 999
427300080617     C                   MOVEL     TBEUNI        DLAV
427400080617     C                   EVAL      SFILLAV(KX) = %dec(tbeke1:3:0)
427500080618     C                   EVAL      SDTALAV(KX) = D§LAVTAS
427600080617     C                   ENDIF
427700080617     C                   ENDIF
427800080617
427900080617     C                   ENDDO
428000080617
428100080617
428200080617     C                   ENDSR
428300111102      ******************************************************************
428400111102      ** CARICAMENTO SCHIERA FILIALI ADDEBITO DA CARTELLO CENTRO STORICO
428500111102      ******************************************************************
428600111102     C     CARZTL        BEGSR
428700111102
428800111102     C                   CLEAR                   KX
428900111102
429000111102     C                   MOVEL     'ZTL'         KEYTBE
429100111102     C     KEYTBE        SETLL     TNTBE01L
429200111102     C                   DO        *HIVAL
429300111102     C     KEYTBE        READE     TNTBE01L
429400111102
429500111102     C                   IF        %EOF(TNTBE01L)
429600111102     C                   LEAVE
429700111102     C                   ENDIF
429800111102
429900111102     C                   IF        TBEATB = ' '
430000111102     C                   ADD       1             KX
430100111102     C                   IF        KX <= 999
430200111102     C                   MOVEL     TBEUNI        DZTL
430300111102     C                   EVAL      SFILZTL(KX) = %dec(tbeke1:3:0)
430400111102     C                   EVAL      SDTAZTL(KX) = D§ZTLTAS
430500111102     C                   ENDIF
430600111102     C                   ENDIF
430700111102
430800111102     C                   ENDDO
430900111102
431000111102
431100111102     C                   ENDSR
431200020222     C*************************************************************
431300020222     C** RICHIAMO DEL TRUL27R
431400020222     C*************************************************************
431500020222     C     SRTRUL        BEGSR
431600020222      *
431700020222     C                   Z-ADD     TASDCT        I27DTA
431800021122     c                   Movel     TasCtr        I27Ctb
431900020222     C                   CALL      'TRUL27R'
432000020222     C                   PARM                    TRUL27
432100020222      *
432200020222     C     O27ERR        IFEQ      *BLANKS
432300020319     C     O27PRG        ANDNE     *BLANKS
432400020222     C                   ADD       1             T                 2 0
432500020222     C                   MOVE      I27NTW        NTW(T)
432600020222     C                   Z-ADD     O27KSC        TAC(T)
432700020222     C                   Z-ADD     O27CTR        CTC(T)
432800020226     C                   MOVE      O27PRG        PRC(T)
432900020222      *
433000020222      * AGGANCIO LA TARIFFA PER RECUPERARE LA DIVISA
433100020222      *
433200020226     C                   Z-ADD     O27KSC        CARKSC
433300020226     C                   Z-ADD     O27CTR        CARCTR
433400020226     C                   MOVE      O27PRG        CARPRG
433500020222     C     KTAMC         CHAIN     TNTAM00L                           94
433600020222      *
433700020222     C     *IN94         IFEQ      *OFF
433800020222     C                   MOVEL     TAMFLO        DSTA01
433900020222     C                   MOVE      §TADIV        DIC(T)
434000020226      * SE C'E ERRORE VADO A FINE PROGRAMMA
434100020226     C                   ELSE
434200020226     C                   MOVEL     MSG(9)        TASMSG
434300020226     C                   ENDIF
434400020226      *
434500020222      * SE C'E ERRORE VADO A FINE PROGRAMMA
434600020222     C                   ELSE
434700020222     C                   SETON                                        94
434800020222     C                   MOVEL     MSG(9)        TASMSG
434900020222     C                   ENDIF
435000020222      *
435100020222     C                   ENDSR
435200020225     C*************************************************************
435300020225     C** CERCO LA TARIFFA DI CARTELLO
435400020225     C*************************************************************
435500020225     C     CERTCA        BEGSR
435600020225      *
435700020225      * RICHIAMO IL TRUL27R PASANDOGLI LE CARATTERISTICHE DELLA SPEDIZIONE
435800020225      *
435900020318     C**                   CLEARTRUL27
436000020318     C**                   MOVE 'L'       I27TLA
436100020318     C**                   MOVE TASTSP    I27TSP
436200020318     C**                   MOVE TASLNA    I27LNA
436300020318     C**                   MOVE TASLNP    I27LNP
436400020318     C**                   MOVE TASKSC    I27CLI
436500020318     C**                   MOVE TASDCT    I27DTA
436600020318     C**                   CALL 'TRUL27R'
436700020318     C**                   PARM           TRUL27
436800020225      *
436900020318     C**         O27ERR    IFEQ ' '
437000020318     C**                   Z-ADDO27KSC    KSC2
437100020318     C**                   Z-ADDO27CTR    CTR2
437200020318     C**                   Z-ADDO27CTR    CODCTR
437300020318     C**                   MOVE O27PRG    PRG2
437400020225      * CARICO ANCHE I CAMPI  RELATIVI ALLA TARIFFA DI CARTELLO
437500020318     C**                   Z-ADDO27KSC    CARKSC
437600020318     C**                   Z-ADDO27CTR    CARCTR
437700020318     C**                   MOVE O27PRG    CARPRG
437800020225      *
437900020318     C**                   ENDIF
438000020318     C** DALLE SCHIERE CARICO LA TARIFFA DI CARTELLO
438100020318     C                   SETOFF                                       90
438200020319     C                   Z-ADD     1             T
438300020318     C     O27NTW        LOOKUP    NTW(T)                                 90
438400020318     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
438500020318     C     *IN90         IFEQ      *OFF
438600020318     C                   MOVEL     MSG(9)        TASMSG
438700020319     C                   SETON                                        94
438800020318     C                   GOTO      FINE
438900020318     C                   ENDIF
439000020318     C*
439100020318     C                   Z-ADD     TAC(T)        CARKSC
439200020318     C                   Z-ADD     CTC(T)        CARCTR
439300020318     C                   Z-ADD     PRC(T)        CARPRG
439400020318     C                   MOVE      DIC(T)        DIVCAR
439500020225      *
439600020225     C                   ENDSR
439700941114     C*************************************************************
439800941121     C** CARICAMENTO CODICI E TARIFFE PARTICOLARI DI CARTELLO
439900941114     C*************************************************************
440000941110     C     CAR1P         BEGSR
440100941111     C*
440200020114     C                   DO        50            A
440300941112     C     A             OCCUR     DS1P
440400941111     C                   CLEAR                   DS1P
440500941112     C                   END
440600941111     C                   MOVEL     *BLANKS       CP
440700941110     C**
440800941110     C                   MOVEL     '1P'          COD
440900941111     C                   Z-ADD     0             K                 5 0
441000941110     C     KTAB          SETLL     TABEL                                  07
441100941110     C  N07              SETON                                          H7
441200941110     C   H7              GOTO      FINE
441300941110     C                   DO        *HIVAL        A
441400941110     C     KTAB          READE     TABEL                                  07
441500941110     C   07              GOTO      TABCP
441600941110     C     TBLKEY        IFNE      *BLANKS
441700941115     C                   MOVEL     TBLUNI        CAMPO            26
441800941115     C                   MOVE      CAMPO         WFCP              1
441900941115     C     WFCP          IFEQ      'S'
442000941110     C                   ADD       1             K
442100941110     C                   MOVEL     TBLKEY        CP(K)
442200941112     C     K             OCCUR     DS1P
442300941112     C                   MOVEL     TBLUNI        DS1P
442400941112     C**
442500941110     C                   END
442600941110     C                   END
442700941111     C                   ENDDO
442800020222     C*
442900941110     C     TABCP         ENDSR
443000941110     C****************************************************
443100941110     C** CARICAMENTO DATI VARI TASSAZIONE
443200941110     C****************************************************
443300941110     C     CARQT         BEGSR
443400941110     C                   MOVEL     'QT'          COD
443500941110     C                   MOVEL     *BLANKS       KEY
443600941110     C                   MOVEL     '1'           KEY
443700941110     C     KTABC         CHAIN     TABEL                              07
443800941113     C   07              SETON                                        H7
443900941113     C   H7              GOTO      FINE
444000941110     C  N07              MOVEL     TBLUNI        DSQT1
444100941110     C*
444200941110     C                   ENDSR
444300990917     C****************************************************
444400990922     C** CARICAMENTO MONETE DI CONTO / IMPORTI STANDARD
444500990917     C****************************************************
444600990917     C     CARDIV        BEGSR
444700990917     C*****
444800990922      *  RICERCA DELLA MONETA DI CONTO
444900990917     C                   CLEAR                   DSBS02
445000990917     C                   CLEAR                   DGED
445100990922     C*
445200990917     C                   MOVEL     'C'           T02MOD
445300990917     C                   MOVEL     KNSIF         T02SIF
445400990917     C                   MOVEL     'GED'         T02COD
445500991112     C                   MOVEL     '1'           T02KE1
445600990917      *
445700990917     C                   CALL      'TIBS02R'
445800990917     C                   PARM                    KPJBA
445900990917     C                   PARM                    DSBS02
446000990917      *
446100990917     C     T02ERR        IFEQ      *BLANKS
446200990917     C                   MOVEL     T02UNI        DGED
446300990917     C                   ENDIF
446400990922      * RICERCA DEGLI IMPORTI STANDARD NELLA MONETA DI CONTO
446500990922     C                   CLEAR                   DSBS02
446600990922     C                   CLEAR                   DGEI
446700990922     C                   MOVEL     'L'           T02TLA
446800990922     C                   MOVEL     'C'           T02MOD
446900990922     C                   MOVEL     KNSIF         T02SIF
447000990922     C                   MOVEL     'GEI'         T02COD
447100991112     C                   MOVEL     §GEDCN        T02KE1
447200990922      *
447300990922     C                   CALL      'TIBS02R'
447400990922     C                   PARM                    KPJBA
447500990922     C                   PARM                    DSBS02
447600990922      *
447700990922     C     T02ERR        IFEQ      *BLANKS
447800990922     C                   MOVEL     T02UNI        DGEI
447900990922     C                   ENDIF
448000990922      *
448100990917      *
448200990917     C                   ENDSR
448300011127     C**********************************************************
448400011127     C** RECUPERO LA DATA DEL GIORNO
448500011127     C**********************************************************
448600011127     C     RUDATE        BEGSR
448700011127      *
448800011127     C                   TIME                    W0140            14 0
448900011127     C* UDATE IN GGMMAAAA
449000011127     C                   MOVE      W0140         WDTGIO            8 0
449100011127     C*
449200011127     C                   Z-ADD     WDTGIO        G02DAT
449300011127     C                   MOVEL     *BLANK        G02ERR
449400011127     C                   CALL      'XSRDA8'
449500011127     C                   PARM                    WLBDAT
449600011127     C*
449700011127     C* UDATE IN AAAAMMGG
449800011127     C                   Z-ADD     G02INV        WDTGIO
449900011127      *
450000011127     C                   ENDSR
450100011127     C**********************************************************
450200011127     C** RECUPERO LA DATA TASSAZIONE DA USARE PER QUESTA BOLLA
450300011127     C**********************************************************
450400030131     C**!!!RECDAT        BEGSR
450500011127      *
450600030131     C**!!!              SETOFF                                       31
450700011127      * VERIFICO SE DATA VALORIZZATA E SOPRATTUTTO NUMERICA
450800020826     C**         §ASDAT    IFNE *BLANK
450900020826     C**         §ASDAT    ANDNE*ZEROS
451000011127      *
451100020826     C**                   TESTN          §ASDAT     31
451200011127      * SE 31 ACCESO VUOL DIRE CHE È NUMERICO MA VERIFICO ANCHE L'ULTIMO CARATTERE
451300020826     C**         *IN31     IFEQ *ON
451400020826     C**                   MOVE §ASDAT    W01A    1
451500020826     C**         W01A      COMP '0'                  31  31
451600020826     C**                   ENDIF
451700011127      *
451800020826     C**                   ENDIF
451900011127      * SE 31 ACCESO DATA VALIDA ALTRIMENTI PRENDO LA DATA DEL GIORNO
452000020826     C**         *IN31     IFEQ *ON
452100020826     C**                   MOVE §ASDAT    DATTAS  80
452200020826     C**                   ELSE
452300020826     C**                   Z-ADDWDTGIO    DATTAS
452400020826     C**                   ENDIF
452500020618     C*
452600011127      *
452700030203     C**!!!              ENDSR
452800990916     C****************************************************
452900990916     C** CARICAMENTO IMPORTI PASSATI DALLE DS
453000990916     C****************************************************
453100990916     C     SAVDS         BEGSR
453200990916     C*
453300990922     C     *LIKE         DEFINE    TASPOR        TASINL
453400990916     C*
453500990916     C* SALVO LE SCHIERE DELLE VARIE (SIGLE VARIE+IMPORTI)
453600990916     C                   MOVEA     SV            SVW
453700990916     C                   MOVEA     VA            VAW
453800991012     C                   CLEAR                   TASINL
453900991012     C                   CLEAR                   TASDIF
454000990916     C*
454100990916     C* RECUPERO L'IMPORTO DELL'INOLTRO SE C'E' NELLA SCHIERA DELLE VARIE
454200990916     C                   Z-ADD     1             X                 2 0
454300990923     C     §$2FIN        LOOKUP    SV(X)                                  30
454400990916     C* SE LO TROVO VALORIZZO TASINL
454500990923     C   30              Z-ADD     VA(X)         TASINL
454600991111     C  N30              Z-ADD     0             X
454700990921     C*
454800990921     C* RECUPERO L'IMPORTO DEL DIRITTO FISSO  NELLA SCHIERA DELLE VARIE
454900990921     C                   Z-ADD     1             Y                 2 0
455000990921     C     §$2VRH        LOOKUP    SV(Y)                                  30
455100990921     C* SE LO TROVO VALORIZZO TASINL
455200990923     C   30              Z-ADD     VA(Y)         TASDIF           11 3
455300990916     C*
455400990916     C                   ENDSR
455500990921     C****************************************************
455600990921     C** CONVERSIONE DELLE VARIE IN MONETA DELLA TARIFFA
455700990921     C****************************************************
455800990921     C     CNVTAS        BEGSR
455900990921     C*
456000990921     C* SE DIVISA PASSATA (DELLA BOLLA) E' DIVERSA DALLA DIVISA DELLA
456100990921     C* TARIFFA CONVERTO IN DIVISA TARIFFA
456200990921     C                   Z-ADD     1             A
456300990923    1C                   DO        20            A
456400990923    2C     VA(A)         IFNE      *ZEROS
456500990921     C                   CLEAR                   YEUR
456600011127     C                   MOVEL     DIVINP        YECDVI
456700011127     C                   MOVEL     DIVOUT        YECDVO
456800990921     C                   Z-ADD     VA(A)         YECIMI
456900991014     C   12              MOVE      '3'           YECDCO
457000991014     C  N12              MOVE      '0'           YECDCO
457100990921     C*
457200990921     C                   CALL      'YEURCO'
457300990921     C                   PARM                    YEUR
457400990921     C*
457500990923    3C     YECESI        IFEQ      ' '
457600990921     C                   Z-ADD     YECIMO        VA(A)
457700990923    3C                   END
457800990923    2C                   END
457900990923    1C                   END
458000990921     C* CONVERTO L'IMPORTO DEL PORTO
458100990921     C* SE PORTO DIVERSO DA 0
458200990923    1C     TASPOR        IFGT      0
458300990921     C                   CLEAR                   YEUR
458400011127     C                   MOVEL     DIVINP        YECDVI
458500011127     C                   MOVEL     DIVOUT        YECDVO
458600990921     C                   Z-ADD     TASPOR        YECIMI
458700991014     C   12              MOVE      '3'           YECDCO
458800991014     C  N12              MOVE      '0'           YECDCO
458900990921     C*
459000990921     C                   CALL      'YEURCO'
459100990921     C                   PARM                    YEUR
459200990921     C*
459300990923    2C     YECESI        IFEQ      ' '
459400990921     C                   Z-ADD     YECIMO        TASPOR
459500990921     C                   ELSE
459600990921     C                   MOVEL     MSG(10)       TASMSG
459700990921     C                   GOTO      FINE
459800990923    2C                   END
459900990923    1C                   ENDIF
460000990921     C* SE INOLTRO DIVERSO DA 0
460100990923    1C     TASINL        IFGT      0
460200990921     C                   CLEAR                   YEUR
460300011127     C                   MOVEL     DIVINP        YECDVI
460400011127     C                   MOVEL     DIVOUT        YECDVO
460500990921     C                   Z-ADD     TASINL        YECIMI
460600991014     C   12              MOVE      '3'           YECDCO
460700991014     C  N12              MOVE      '0'           YECDCO
460800990921     C*
460900990921     C                   CALL      'YEURCO'
461000990921     C                   PARM                    YEUR
461100990921     C*
461200990923    2C     YECESI        IFEQ      ' '
461300990921     C                   Z-ADD     YECIMO        TASINL
461400990921     C                   ELSE
461500990921     C                   MOVEL     MSG(10)       TASMSG
461600990921     C                   GOTO      FINE
461700990923    2C                   END
461800990923    1C                   ENDIF
461900990921     C* SE DIRITTO FISSO DIVERSO DA 0
462000990923    1C     TASDIF        IFGT      0
462100990921     C                   CLEAR                   YEUR
462200011127     C                   MOVEL     DIVINP        YECDVI
462300011127     C                   MOVEL     DIVOUT        YECDVO
462400990921     C                   Z-ADD     TASDIF        YECIMI
462500991014     C   12              MOVE      '3'           YECDCO
462600991014     C  N12              MOVE      '0'           YECDCO
462700990921     C*
462800990921     C                   CALL      'YEURCO'
462900990921     C                   PARM                    YEUR
463000990921     C*
463100990923    2C     YECESI        IFEQ      ' '
463200990921     C                   Z-ADD     YECIMO        TASDIF
463300990923    2C                   END
463400990923    1C                   ENDIF
463500990929     C* SE IMPORTO IMPONIBILE DIVERSO DA ZEROS
463600991014     C* LO RICALCOLO PER SICUREZZA
463700990929    1C     TASIMV        IFGT      0
463800991014     C                   CLEAR                   DSLV16
463900991014     C                   MOVEL     'T'           D17FIM
464000991014     C                   Z-ADD     TASPOR        D17POR
464100991014     C                   CALL      'FNLV16R'
464200991014     C                   PARM                    DSLV16
464300991014     C                   PARM                    DTASV
464400991014     C                   Z-ADD     O17IMV        TASIMV
464500990929    1C                   ENDIF
464600990921     C* SE QUANTITA' DA FATTURARE E' DIVERSA DA 0 E LA TARIFFA E' UNA TARIFFA
464700011127     C* A VALORE SPECIFICO DEVO CONVERTIRE IL CAMPO SE RICHIESTO
464800990921     C*
464900011127    1C     TASQFT        IFGT      0
465000011127     C     TAMFVD        ANDEQ     '4'
465100011127     C     CONQTA        ANDNE     'N'
465200011127     C                   CLEAR                   YEUR
465300011127     C                   MOVEL     DIVINP        YECDVI
465400011127     C                   MOVEL     DIVOUT        YECDVO
465500011127     C                   Z-ADD     TASQFT        YECIMI
465600011127     C   12              MOVE      '3'           YECDCO
465700011127     C  N12              MOVE      '0'           YECDCO
465800011127     C*
465900011127     C                   CALL      'YEURCO'
466000011127     C                   PARM                    YEUR
466100011127     C*
466200011127    2C     YECESI        IFEQ      ' '
466300011127     C                   Z-ADD     YECIMO        TASQFT
466400011127    2C                   END
466500011127    1C                   ENDIF
466600011127     C* SE TASIAL VALORIZZATO DEVO CONVERTIRLO SOLO SE RICHIESTO
466700011127     C*
466800011127    1C     TASIAL        IFGT      0
466900011127     C     CONIAL        ANDNE     'N'
467000011127     C                   CLEAR                   YEUR
467100011127     C                   MOVEL     DIVINP        YECDVI
467200011127     C                   MOVEL     DIVOUT        YECDVO
467300011127     C                   Z-ADD     TASIAL        YECIMI
467400011129     C   12              MOVE      '2'           YECDCO
467500011127     C  N12              MOVE      '0'           YECDCO
467600011127     C*
467700011127     C                   CALL      'YEURCO'
467800011127     C                   PARM                    YEUR
467900011127     C*
468000011127    2C     YECESI        IFEQ      ' '
468100011127     C                   Z-ADD     YECIMO        TASIAL
468200011127    2C                   END
468300011127    1C                   ENDIF
468400990921     C*
468500990921     C                   ENDSR
468600990923     C************************************************************
468700990923     C** CONVERSIONE DEL DETTAGLIO TARIFFA PARTICOLARE DI CARTELLO
468800990923     C************************************************************
468900990923     C     CONTPD        BEGSR
469000990923     C*
469100990923     C                   CLEAR                   YEUR
469200990923     C                   MOVEL     DIVCAR        YECDVI
469300990923     C                   MOVEL     §TADIV        YECDVO
469400991014     C   12              MOVE      '3'           YECDCO
469500991014     C  N12              MOVE      '0'           YECDCO
469600990923     C*
469700990923     C* CONTROLLLO IL TIPO TARIFFA SE E' VALORE OPPURE NO PER CONVERTIRE
469800990923     C* LO SCAGLIONE OPPURE NO
469900990923     C*
470000990923    1C     WTPG          IFEQ      'T'
470100990923     C     WTPG          OREQ      '4'
470200990923     C     WTPG          OREQ      'V'
470300990923     C*
470400990923     C                   Z-ADD     TPDSGL        YECIMI
470500990923     C                   CALL      'YEURCO'
470600990923     C                   PARM                    YEUR
470700990923    2C     YECESI        IFEQ      ' '
470800990923     C                   Z-ADD     YECIMO        TPDSGL
470900990923    2C                   END
471000990923     C* NON CONVERTO L'IMPORTO TARIFFA PERCHE' E' UNA PERCENTUALE
471100990923    1C                   ELSE
471200990923     C*
471300990923     C                   Z-ADD     TPDITR        YECIMI
471400990923     C                   CALL      'YEURCO'
471500990923     C                   PARM                    YEUR
471600990923    2C     YECESI        IFEQ      ' '
471700990923     C                   Z-ADD     YECIMO        TPDITR
471800990923    2C                   END
471900990923    1C                   ENDIF
472000990923     C*
472100990923     C* MINIMO
472200990923     C                   Z-ADD     TPDMIN        YECIMI
472300990923     C                   CALL      'YEURCO'
472400990923     C                   PARM                    YEUR
472500990923    1C     YECESI        IFEQ      ' '
472600990923     C                   Z-ADD     YECIMO        TPDMIN
472700990923    1C                   END
472800990923     C* MASSIMO
472900990923     C                   Z-ADD     TPDMAX        YECIMI
473000990923     C                   CALL      'YEURCO'
473100990923     C                   PARM                    YEUR
473200990923    1C     YECESI        IFEQ      ' '
473300990923     C                   Z-ADD     YECIMO        TPDMAX
473400990923    1C                   END
473500990923     C*
473600990923     C                   ENDSR
473700020826     C************************************************************
473800020826     C** CONTROLLO SE APPLICARE PESO NORMALE O VDL
473900020826     C************************************************************
474000020826     C     CTRPES        BEGSR
474100020826     C                   CLEAR                   USAPKF
474200030131     C**!!!              CLEAR                   §ASSPC
474300030131     C                   CLEAR                   TASSPC
474400030429     C                   CLEAR                   TASPKCN
474500020826     C*
474600020826     C* SE NON C'E' IL PESO VDL, PRENDO QUELLO DA FATTURARE
474700030131     C**!!!§ASPKC        IFEQ      0
474800030131     C     TASPKC        IFEQ      0
474900020826     C                   Z-ADD     TASPKF        USAPKF
475000020826     C                   GOTO      ENDPES
475100020826     C                   ELSE
475200020826     C* SE IL VDL NON + MAI DA APPLICARE --> USO IL DA FATTURARE
475300020826     C     §TATAP        IFEQ      'M'
475400020826     C                   Z-ADD     TASPKF        USAPKF
475500020826     C                   GOTO      ENDPES
475600020826     C                   ENDIF
475700020826     C                   ENDIF
475800020826     C* PESO VDL - TARA
475900030131     C**!!!§QTTPC        MULT      §ASNCP        WTARA
476000030131     C     §QTTPC        MULT      TASNCP        WTARA
476100030131     C**!!!§ASPKC        SUB(h)    WTARA         WNTARA
476200030131     C     TASPKC        SUB(h)    WTARA         WNTARA
476300020826     C* SE PESO DA FATTURARE COMPRESO DA VDL E VDL-TARA
476400020826     C*  APPLICO SEMPRE IL PESO DA FATTURARE
476500020826     C*
476600020827     C     TASPKF        IFGE      WNTARA
476700030131     C**!!!TASPKF        ANDLE     §ASPKC
476800030131     C     TASPKF        ANDLE     TASPKC
476900020826     C                   Z-ADD     TASPKF        USAPKF
477000020826     C                   GOTO      ENDPES
477100020826     C                   ENDIF
477200020826     C* SE VDL-TARA > PESO FATTURARE LO APPLICO SIA PER " " CHE PER "S"
477300020827     C     WNTARA        IFGT      TASPKF
477400020827     C                   Z-ADD     WNTARA        USAPKF
477500030131     C**!!!              MOVEL     'S'           §ASSPC
477600030131     C**!!!              z-add     wntara        §ASpkc
477700030131     C                   MOVEL     'S'           TASSPC
477800030429     C                   z-add     wntara        TASpkcN
477900020826     C                   GOTO      ENDPES
478000020826     C                   ENDIF
478100020826     C* SE VDL-TARA < PESO FATTURARE LO APPLICO SOLO SE TOTALE E SE
478200020826     C* "S"
478300020827     C     WNTARA        IFLT      TASPKF
478400030131     C**!!!§ASNCP        ANDEQ     TASNCL
478500030131     C     TASNCP        ANDEQ     TASNCL
478600020826     C     §TATAP        ANDEQ     'S'
478700020827     C                   Z-ADD     WNTARA        USAPKF
478800030131     C**!!!              MOVEL     'S'           §ASSPC
478900030131     C**!!!              z-add     wntara        §ASpkc
479000030131     C                   MOVEL     'S'           TASSPC
479100030429     C                   z-add     wntara        TASpkcN
479200020826     C                   GOTO      ENDPES
479300020826     C                   ENDIF
479400020826     C* SE ANCORA A 0 (IMPOSSIBILE) USO IL PESO DA FATTURARE
479500020826     C     USAPKF        IFEQ      0
479600020826     C                   Z-ADD     TASPKF        USAPKF
479700020826     C                   ENDIF
479800020827     C**
479900020829     c     endpes        tag
480000020829     C                   ENDSR
480100020412**
480200980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo del CONTRASSEGNO  1
480300980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo da ASSICURARE     2
480400980529Non trovata tariffa cliente                                                    3
480500980512Mancano importo da assicurare in bolla e valore merce in tariffa               4
480600980603Non trovato SCAGLIONE in tariffa                                               5
480700980512Manca la quantita' da fatturare                                                6
480800980603Non trovato SCAGLIONE in tariffa particolare
480900980603Non trovato CODICE TASSAZIONE in tariffa                                       8
481000980529Non trovata tariffa di CARTELLO                                                9
481100000125Tariffa cliente con decorrenza maggiore della data spedizione                 10
481200990917Divisa errata                                                                 11
481300000125Tariffa DPD ma bolla Italia                                                   12
481400000125Bolla import/export DPD  ma  tariffa NON DPD                                  13
481500011127Divisa tariffa del cliente non più utilizzabile                               14
481600020318Tariffa FEDEX ma bolla Italia                                                 15
481700020318Bolla import/export FEDEX ma  tariffa NON FEDEX                               16
481800020412Non trovato CODICE TASSAZIONE in tariffa PARTICOLARE                          17
481900030527Non presente tassazione per addebito insoluti                                 18
482000080221Tariffa valida ma imponibile minore di 0,000                                  19
