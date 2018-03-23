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
219600150420     C     'z'           LOOKUP    SV(CC)                                 96     EXPO
219700150420     C   96              ADD       VA(CC)        VAAGGFUEL
219800150420     c
219900980429     C                   ENDSR
220000060905     C*****************************************************************
220100060905     C* Fuel Surcharge
220200060905     C*****************************************************************
220300060905     c     SR_fuel       BEGSR
220400060906     C                   CLEAR                   SCA_PB
220500060906     C                   CLEAR                   SCA_SPE
220600060906     C                   CLEAR                   SCA_SCA
220700060905      *
220800060907      * verifico se esiste già la varia non la calcolo
220900060905      *
221000060905     c     §$2VFS        lookup    sv                                     96
221100060907     c                   IF        %found
221200060907     c                   goto      Endfuel
221300060907     c                   endif
221400060905
221500060905      * verifico se cliente ha in tariffa il calcolo del FUEL Surcharge
221600060907      * altrimenti non vado a cercare nella cartello e vado a fine
221700060906     C                   MOVEL     'f'           FISO
221800060906     C                   MOVEL     'f'           WFISO
221900060905
222000060906     c                   z-add     1             a
222100060906     c     Wfiso         Lookup    Cp(a)                                  05
222200060906     C  N05              GOTO      ENDFUEL
222300060906      * aggancio la ds della tabella 1P
222400060906     c     A             Occur     DS1P
222500060906
222600060905     C     KISOT         CHAIN     TITPT01L
222700060907      * cliente non ha la tariffa  non faccio calcolo
222800060907     C                   IF        not %FOUND(TITPT01L)
222900060907     c                   goto      Endfuel
223000060907     c                   endif
223100140224      * ds x recupero % minima applicabile
223200140224     c                   eval      DTPT01 = TPTflo
223300140224      *
223400140224      * data prezzo base maggiore della data spedizone non faccio calcolo
223500060907     C                   IF        TPTDPB > TASDSP
223600060907     c                   goto      Endfuel
223700060907     c                   endif
223800140224      * Ricerco il valore minimo applicabile e lo scaglione corrispondente in vigore
223900140224      * alla data spedizione
224000150217      * solo se in tariffa non è stato specificato che non si deve applicare il VMA
224100140224
224200150217     c                   If        §TPTvma = 'N'
224300150217     c                   clear                   dpbvma
224400150217     c                   clear                   dpbsvm
224500150217     c                   else
224600150217      * si applicazione VMA
224700140224     C     TASDSP        SETLL     TIDPB01L
224800140224     C                   READ      TIDPB01L
224900140224     c                   if        not %found(tidpb01l)
225000140224     c                   clear                   dpbvma
225100140224     c                   clear                   dpbsvm
225200140224     c                   endif
225300150217     c                   endif
225400060906
225500060905      * Ricerco scaglione di appartenenza del prezzo base
225600060905
225700060911     C     TPTDPB        SETLL     TIPMG01L
225800060906     C                   READ      TIPMG01L
225900060906     C                   IF        NOT %EOF(TIPMG01L)
226000060906      *
226100060906     C                   Z-ADD     PMGSCA        SCA_PB
226200060906     C                   ENDIF
226300060906
226400060906      * Ricerco scaglione di appartenenza del prezzo del gasolio al momento della spedizione
226500060906
226600060911     C     TASDSP        SETLL     TIPMG01L
226700060906     C                   READ      TIPMG01L
226800060906     C                   IF        NOT %EOF(TIPMG01L)
226900060906      *
227000080909      * recupero il prezzo per ricercare il F.D. in tariffa
227100080909      *
227200080909     c                   z-add     pmgpmg        PMG_SGL
227300080909
227400060906     C                   Z-ADD     PMGSCA        SCA_SPE
227500060906     C                   ENDIF
227600140224      * verifico se il prezzo del gasolio al momento della spedizione è minore del valore minimo
227700140224      * applicabile sostituisco il prezzo del gasolio e il suo scaglione con il valore minimo
227800140224      * applicabile e lo scaglione corrispondente
227900140224
228000140224     c                   if        pmgpmg < dpbvma
228100140224     c                   z-add     dpbvma        PMG_SGL
228200140224     c                   z-add     dpbsvm        SCA_SPE
228300140224     c                   Endif
228400060906
228500060906      * calcolo gli scaglioni di aumento
228600060906
228700060906     C                   EVAL      SCA_SCA = SCA_SPE - SCA_PB
228800060906     C                   IF        SCA_SCA < 0
228900060906     C                   CLEAR                   SCA_SCA
229000060906     C                   ENDIF
229100140228      * calcolo dell'importo da addebitare al cliente
229200140228
229300140228     C                   CLEAR                   VARIA
229400140228     C                   CLEAR                   VARIAfuel_min
229500140228     C                   CLEAR                   IMP_SCA
229600140228
229700140228      * calcolo imponibile FUEL
229800140228      * recupero importi di isola e località disagiate ed espresso + nuove varie introdotte
229900140228      * dal 3/2/2014 dv 2473
230000140228     C                   EXSR      CERISO
230100140228      *
230200140228     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
230300140228     c                                       VAAGGFUEL
230400060906
230500060906      * se gli scaglioni scattati sono maggiore di zero cerco l'incidenza percentuale di
230600060906      * aumento nel dettaglio tariffario
230700140228    1C                   IF        SCA_SCA > 0
230800080909      * cambia con il progetto 666 aumento supplemento carburante in tariffa
230900080909      * per cercare il Fattore demoltiplicatore non usiamo più il numero di scaglioni
231000080909      * di aumento ma il prezzo medio del gasolio della settimana della data di spedizione
231100080909     C****               Z-ADD     SCA_SCA       TPDPES
231200080909     C                   Z-ADD     PMG_SGL       TPDPES
231300060906     C                   EXSR      CALACD
231400060907      * Se la percentuale di incidenza è maggiore di zero
231500140228    2C                   IF        AITR > 0
231600060905      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
231700060905      *                     per abbandonare il concetto di monete diverse dall'EURO)
231800060907     C                   EVAL(H)   VARIA = (IMP_SCA * aitr * sca_sca) / 100
231900140224
232000140228    2c                   Endif
232100140228    1c                   Endif
232200140224      * anche se il cliente ha negato la tariffa verifico
232300140224      * se nella tariffa particolare del cliente è stato espressa la % minima di apllicazione
232400140224      * calcolo l'importo del fuel utilizzando questa percentuale
232500140224
232600140224     c                   if        §tptpma > 0
232700140224     c                   eval(H)   variafuel_min =  (IMP_SCA * §TPTpma)/ 100
232800140224     c                   Endif
232900140224
233000140224      * prendo il valore della varia maggiore tra quella calcolata con la % minima applicabile
233100140224      * e quella calcolata con il FD della tariffa
233200140224
233300140224     c                   if        variafuel_min > Varia
233400140224     c                   eval      Varia =  variafuel_min
233500140224     c                   endif
233600140224
233700060905      **
233800140224    1c                   If        Varia > 0
233900060905     C                   Z-ADD     1             A
234000060905     C     ' '           LOOKUP    SV(A)                                  96
234100060907     C   96              MOVEL     §$2VFS        SV(A)
234200060905     C   96              Z-ADD     VARIA         VA(A)
234300060905     C  N96              ADD       VARIA         TASPOR
234400060905    1C                   ENDIF
234500060905      *
234600060905      *
234700060906     C     ENDFUEL       ENDSR
234800040506     C*****************************************************************
234900060905     C* SUPPLEMENTO CARBURANTE FEDEX
235000040506     C*****************************************************************
235100060905     C     SR_SUPCARB    BEGSR
235200040507      *
235300040507      * verifico se esiste già la varia
235400040507      *
235500040507     c     §$2VSC        lookup    sv                                     96
235600040507     c                   IF        not %found
235700040506
235800040506      * RICERCO IN BASE ALLA DATA SPEDIZIONE LA GIUSTA PERCENTUALE DI
235900040506      * SUPPLEMNENTO
236000040506
236100040507     C                   CLEAR                   VARIA
236200040507     C                   CLEAR                   IMP_SCA
236300040507     C                   CLEAR                   PER_SCA
236400040507     C                   Z-ADD     1             KX
236500040507    1c                   DO        *HIVAL
236600040507      * SCHIERA IN ORDINE DISCENDENTE
236700040507     C                   MOVE      DSCA(KX)      WDSPSC
236800040507      * SE DATA UGUALE A ZERO VADO A FINE
236900040507    2C                   IF        DS_DSC = *ZEROS
237000040507     C                   LEAVE
237100040507    2C                   ENDIF
237200040507      * SE DATA SPEDIZIONE MAGGIORE DI QUELLA IN SCHIERA RECUPERO LA PERCENTUALE DI CALCOLO
237300040510    2C                   IF        TASDSP >=  DS_DSC
237400040507     c                   z-add     DS_psc        PER_SCA
237500040507     C                   LEAVE
237600040507     C                   ELSE
237700040507      * SE DATA MINORE LEGGO LA DATA SUCCESSIVA
237800040507    3C                   IF        KX < 999
237900040507     C                   ADD       1             KX
238000040507     C                   ELSE
238100040507     C                   LEAVE
238200040507    3C                   ENDIF
238300040507    2C                   ENDIF
238400040507
238500040507    1C                   ENDDO
238600040507      * SE PERCENTUALE MAGGIORE DI ZERO ESEGUO IL CALCOLO
238700040507    1C                   IF        PER_SCA > 0
238800040507      * CALCOLO L'IMPONIBILE
238900060414      * recupero importi di isola e località disagiate ed espresso
239000040507     C                   EXSR      CERISO
239100040507      *
239200060414     c* Di fatto qui l'espresso non ci sarà mai!!! bolla estera
239300140203     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
239400140203     c                                       VAAGGFUEL
239500040507      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
239600040507      *                     per abbandonare il concetto di monete diverse dall'EURO)
239700040507     C                   EVAL(H)   VARIA = (IMP_SCA * PER_SCA) / 100
239800040507      **
239900040507     C                   Z-ADD     1             A
240000040507     C     ' '           LOOKUP    SV(A)                                  96
240100040507     C   96              MOVEL     §$2VSC        SV(A)
240200040507     C   96              Z-ADD     VARIA         VA(A)
240300040507     C  N96              ADD       VARIA         TASPOR
240400040507    1C                   ENDIF
240500040507      *
240600040507     C                   ENDIF
240700040507      *
240800040507     C                   ENDSR
240900970521     C*****************************************************************
241000970521     C* TASSAZIONE TARIFFE PARTICOLARI
241100970521     C*****************************************************************
241200970521     C     TASPAR        BEGSR
241300970521     C     *LIKE         DEFINE    TASVA1        TASCP
241400970522     C     *LIKE         DEFINE    TASVA1        TASISO
241500970521     C     *LIKE         DEFINE    TASTC1        TASTC
241600970721     C                   CLEAR                   TASCP
241700970721     C                   CLEAR                   WNOEST
241800970721     C* SE NON ESISTE LA SIGLA DELLA VARIA O E' GIA' IN TASSAZIONE
241900970721     C*  NON RITASSO NE APPLICO L'ESPRESSO
242000970521     C**
242100970521     C                   Z-ADD     1             A
242200970521     C     TASTC         LOOKUP    CP(A)                                  05
242300970521     C  N05              GOTO      ENDPAR
242400970521     C**
242500970521     C     A             OCCUR     DS1P
242600970521     C                   Z-ADD     1             A
242700970521     C     §1PVAR        LOOKUP    SV(A)                                  05
242800970521     C* SIGLA VARIA NON PRESENTE --> CALCOLO
242900970521     C     *IN05         IFEQ      *OFF
243000970721     C                   MOVEL     'S'           WNOEST            1
243100970521     C                   EXSR      CALCP
243200970521     C**
243300970521     C     TASCP         IFGT      0
243400130927      * se sto tassando la tariffa particolare m = avviso affidamento spedizione
243500130927      * ed il flag EMD è uguale a "E" (via sms che mail) raddoppio la varia
243600130927     c                   if        §1pvar = §$2VAD and
243700130927     c                             §asemd = 'E'
243800130927     c                   eval      tascp = tascp * 2
243900130927     c                   endif
244000130927      *
244100970521     C                   Z-ADD     1             A
244200970521     C     ' '           LOOKUP    SV(A)                                  96
244300970521     C   96              MOVEL     §1PVAR        SV(A)
244400970521     C   96              Z-ADD     TASCP         VA(A)
244500970521     C  N96              ADD       TASCP         TASPOR
244600970521     C                   END
244700970521     C                   END
244800970521     C     ENDPAR        ENDSR
244900941112     C******************************************************
245000941107     C** CALCOLO PESO TASSABILE PER TARIFFA A PESO
245100941112     C******************************************************
245200941107     C     CALPT         BEGSR
245300980313     C** MI MEMORIZZO SU BLTAS IL PESO TASSABILE SE
245400980313     C** IL PESO E' SUPERIORE AL MINIMO TASSABILE
245500980313     C** SE ESISTE IL RAPPORTO PESO VOLUME IN TARIFFA  E VOL IN BOLLA
245600980313     C** IL PESO TASSABILE SU BLTAS E' SEMPRE ARROTONDATO AL KG SUPERIO
245700980311     C                   CLEAR                   TASPVL
245800050912     C***  UNOCTR        IFEQ      0
245900050912     C**** UNOCTR        OREQ      3
246000050912      * modifico --- il peso tassabile si stampa e si calcola nei seguenti casi :
246100050912      * Tariffa a quintale "0" il peso da fatturare maggiore del  minimo tassabile
246200050912      *                        (definito nella tabella TR = 100) oppure volume da fatturare
246300050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
246400050912      *                        tariffa maggiore di zero
246500050912      * Tariffa a kg       "3" non controllo il peso da fatturare maggiore del minimo tassabile
246600050912      *                        (definito nella tabella TR = 0,1) ma solo volume da fatturare
246700050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
246800050912      *                        tariffa maggiore di zero
246900050912     C***  TARIFFA A QUINTALE
247000050912
247100050912     C     UNOCTR        IFEQ      0
247200050912
247300020826     C     USAPKF        IFGT      MINTAS
247400950901     C     PESTAS        ADD       0,9           TASPVL
247500941107     C                   ELSE
247600941107     C     TASVLF        IFNE      0
247700980311     C     RAPPV         ANDNE     0
247800950901     C     PESTAS        ADD       0,9           TASPVL
247900980313     C                   ENDIF
248000980313     C                   ENDIF
248100980313     C                   ENDIF
248200050912
248300050912     C***  TARIFFA A KG
248400050912
248500050912     C     UNOCTR        IFEQ      3
248600050912
248700050912     C     TASVLF        IFNE      0
248800050912     C     RAPPV         ANDNE     0
248900050912     C     PESTAS        ADD       0,9           TASPVL
249000050912     C                   ENDIF
249100050912     C                   ENDIF
249200050912
249300980313     C                   ENDSR
249400950119     C******************************************************
249500950119     C** CALCOLA VALORE DA ASSICURARE PER TARIFFA A VALORE
249600950119     C******************************************************
249700950119     C     CALVAS        BEGSR
249800950119     C     *LIKE         DEFINE    TASIAS        IMPVAS
249900950119     C                   Z-ADD     0             IMPVAS
250000950119     C                   MOVEL     'C'           FISO
250100990722     C                   MOVEL     'C'           WFISO             1
250200950119     C**FLAG ASSICURAZIONE X CONTO = C
250300950119     C                   Z-ADD     1             A
250400990722     C     WFISO         LOOKUP    CP(A)                                  05
250500950119     C  N05              GOTO      DOPVA1
250600950119     C**----------------------------------------
250700990721     C     KISOT         CHAIN     TITPT01L                           28
250800980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
250900980528     C     *IN28         IFEQ      *ON
251000980528     C     TPTATB        ORNE      ' '
251100980528     C                   GOTO      DOPVA1
251200980528     C                   ENDIF
251300980528     C**
251400950119     C** ESCLUDO GLI ANNULLATI
251500950119     C     TPTFVM        IFEQ      '3'
251600020826     C     TPTVLM        MULT      USAPKF        IMPVAS
251700950119     C                   END
251800950119     C*PESO
251900950119     C     TPTFVM        IFEQ      '1'
252000950119     C     TPTVLM        MULT      TASNCL        IMPVAS
252100950119     C                   END
252200950119     C* COLLI
252300950119     C     TPTFVM        IFEQ      '2'
252400950119     C                   Z-ADD     TPTVLM        IMPVAS
252500950119     C                   END
252600950119     C*SPEDIZIONE
252700950119     C     DOPVA1        ENDSR
252800941112     C******************************************************
252900941107     C** CALCOLA VARIA R  --- ASSICURAZIONE PER CONTO
253000941112     C******************************************************
253100941107     C     CALVR         BEGSR
253200050525     c* Il falg task88 cambia significato:indica se bolla con mandato o no
253300050525     c*
253400050525     c* con mandato significa: x mandato a val variabile--> tutte le bolle
253500050525     c*                        x mandato a val.indicato --> solo bolle con
253600050525     c*                        tasias=0 o con tasias <=al valore del mandat
253700050525     c*
253800050525     c* Imposto task88=N se mandato
253900050525     c* Imposto task88=S se no mandato ma tasias>0
254000050525     c* Imposto task88=X per bolle senza mandato e senza importo
254100050525     c
254200050525     C                   MOVEL     'X'           TASK88
254300980603     C** 88 = SI CALCOLA SOLO SE ESISTE IL VALORE MERCE IN BOLLA
254400941112     C     KSCCOD        IFEQ      8888
254500980527     C     KSCCOD        OREQ      9999
254600980709     C     TASIAS        IFEQ      0
254700050525     C**                 MOVEL     'S'           TASK88
254800980709     C                   GOTO      ENDVR
254900941112     C                   END
255000980709     C                   END
255100980603     C**
255200980603     C** SE TIPO BOLLA CHE NON PREVEDE L'ASSICURAZIONE, ESCO SENZA
255300980603     C**  DARE ERRORE
255400050525     C**   TASIAS        IFEQ      0
255500050525     C**   TASTBL        LOOKUP    TBN                                    05
255600050525     C** 05              MOVEL     'S'           TASK88
255700050525     C** 05              GOTO      ENDVR
255800050525     C**                 ENDIF
255900991012     C**
256000991012     C** SE IMPORTO D'ASSICURARE MAGGIORE DI ZEROS MUOVO 'S' TASK88
256100991012     C** PER STAMPA BOLLE CON ASSICURAZIONE PER TABULATONE DELLA CONSULDANNI
256200050525     C**   TASIAS        IFGT      0
256300050525     C**                 MOVEL     'S'           TASK88
256400050525     C**                 ENDIF
256500980603     C**
256600941116     C                   SETOFF                                       94
256700941116     C     *LIKE         DEFINE    TASIAS        IASLIT
256800941116     C                   Z-ADD     TASIAS        IASLIT
256900941116     C     IASLIT        IFNE      0
257000941116     C                   Z-ADD     1             K
257100941116     C     TASVAS        LOOKUP    CV(K)                                  05
257200980504     C     *IN05         IFEQ      *OFF
257300980504     C                   SETON                                        94
257400980504     C                   MOVEL     MSG(2)        TASMSG
257500980504     C                   GOTO      FINE
257600980504     C                   ENDIF
257700941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
257800990922     C**
257900990922     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA SE E' DIVERSA
258000990922     C* DALLA VALUTA DELLA TARIFFA
258100990922     C     §TADIV        IFNE      TASVAS
258200990922     C                   CLEAR                   YEUR
258300990922     C                   MOVEL     TASVAS        YECDVI
258400990922     C                   MOVEL     §TADIV        YECDVO
258500990922     C                   Z-ADD     IASLIT        YECIMI
258600011129     C   12              MOVE      '2'           YECDCO
258700991014     C  N12              MOVE      '0'           YECDCO
258800990922     C*
258900990922     C                   CALL      'YEURCO'
259000990922     C                   PARM                    YEUR
259100990922     C*
259200990922     C     YECESI        IFEQ      ' '
259300990922     C                   Z-ADD     YECIMO        IASLIT
259400990922     C                   END
259500990922     C                   ENDIF
259600990922     C                   ENDIF
259700941123     C**----------------------------------------
259800941107     C                   MOVEL     'C'           FISO
259900990722     C                   MOVEL     'C'           WFISO
260000941107     C**FLAG ASSICURAZIONE X CONTO = C
260100941123     C                   Z-ADD     1             A
260200990722     C     WFISO         LOOKUP    CP(A)                                  05
260300941123     C  N05              GOTO      ENDVR
260400020226      * AGGANCIO LA DS DELA TABELLA 1P
260500020226     C     A             OCCUR     DS1P
260600941123     C**----------------------------------------
260700990721     C     KISOT         CHAIN     TITPT01L                           28
260800980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
260900980528     C     *IN28         IFEQ      *ON
261000980528     C     TPTATB        ORNE      *BLANKS
261100980528     C                   GOTO      DOPAC1
261200980528     C                   ENDIF
261300980505     C**
261400980505     C** CONTROLLO IL TIPO APPLICAZIONE DELLA TARIFFA ASSIC X CONTO
261500980505     C**  SE INCONGRUENTE CON TIPO BOLLA PRENDO LA CARTELLO
261600980505     C     TPTAPL        IFEQ      'A'
261700980505     C     TIPBOL        ANDEQ     'F'
261800980505     C     TPTAPL        OREQ      'P'
261900980505     C     TIPBOL        ANDEQ     'A'
262000980505     C                   GOTO      DOPAC1
262100980505     C                   ENDIF
262200980505     C**
262300980504     C* VALORE MERCE = 0 --> ESCLUDO SE NON E' UN VERO MANDATO
262400980505     C                   SETOFF                                       94
262500980504    1C     TPTVLM        IFEQ      0
262600980709     C* CHE SIA UN MANDATO FITTIZIO O UN MANDATO A VALORE VARIABILE
262700980709     C*  DEVO SEMPRE SCRIVERE LA BOLLA COME 8888 PERCHE L'ASSICURAZIONE
262800980709     C*  VUOLE VEDERE BOLLA PER BOLLA QUANTO SONO ASSICURATE
262900050525     C****               MOVEL     'S'           TASK88
263000980709     C**
263100980504    2C     IASLIT        IFEQ      0
263200980504     c* MANDATO FITTIZIO
263300990922    3C     TPTTMA        IFEQ      'F'
263400980504     C                   GOTO      ENDVR
263500980504    3C                   ENDIF
263600050525     c**
263700050526     C** ES - non serve: già fatto    sopra!! Ripetizione inutile
263800050526    3C**   TPTAPL        IFEQ      'P'
263900050526     C**   TIPBOL        COMP      'F'                                    94
264000050526    3C**                 END
264100050526    3C**   TPTAPL        IFEQ      'A'
264200050526     C**   TIPBOL        COMP      'A'                                    94
264300050526    3C**                 END
264400050526    3C**   TPTAPL        IFEQ      ' '
264500050526     C**                 SETON                                            94
264600050526    3C**                 END
264700050525     c
264800050525     c
264900050525     c* anche se manca tariffa è comunque con mandato
265000050525     c                   eval      task88='N'
265100050525     c*
265200050525     c* non devo dare manca tariffa se tipo bolla che non prevede
265300050525     c* Assicurazione
265400050525     C     TASTBL        LOOKUP    TBN                                    05
265500050525     c                   if        not *in05
265600050526     c                   eval      *in94='1'
265700050525     C                   MOVEL     MSG(4)        TASMSG
265800050525     C                   GOTO      FINE
265900050525     c                   endif
266000050525     c
266100050525   x2c                   else
266200050525     c* Mandato a valore variabile con importo: se fittizio 8888 altriment
266300050525     c* con mandato
266400050525    3C     TPTTMA        IFEQ      'F'
266500050525     c                   eval      task88='S'
266600050525     c                   else
266700050525     c                   eval      task88='N'
266800050525    3C                   END
266900050525    2C                   END
267000050525     c
267100941107     C** MANCA TARIFFA: MANDATO CON VALORE = 0
267200941107     C**                E NON ESISTE VALORE IN BOLLA (SOLO COD.)
267300980504   X1C                   ELSE
267400980504    2C     IASLIT        IFGT      0
267500941112     C                   Z-ADD     0             VALSPE           15 3
267600980504    3C     TPTFVM        IFEQ      '3'
267700020826     C     TPTVLM        MULT      USAPKF        VALSPE
267800980504    3C                   END
267900941107     C*PESO
268000980504    3C     TPTFVM        IFEQ      '1'
268100941107     C     TPTVLM        MULT      TASNCL        VALSPE
268200980504    3C                   END
268300941107     C* COLLI
268400980504    3C     TPTFVM        IFEQ      '2'
268500941107     C                   Z-ADD     TPTVLM        VALSPE
268600980504    3C                   END
268700030908      *
268800030908      * ARROTONDO AL DECIMALE DELLA TARIFFA
268900030908     C                   CLEAR                   YEUR
269000030908     C                   MOVEL     §TADIV        YECDVI
269100030908     C                   MOVEL     §TADIV        YECDVO
269200030908     C                   Z-ADD     VALSPE        YECIMI
269300030908     C                   CALL      'YEURCO'
269400030908     C                   PARM                    YEUR
269500030908     C*
269600030908     C     YECESI        IFEQ      ' '
269700030908     C                   Z-ADD     YECIMO        VALSPE
269800030908     C                   END
269900941107     C*SPEDIZIONE
270000980504    3C     IASLIT        IFGT      VALSPE
270100941107     C                   GOTO      DOPAC1
270200050526     c                   else
270300050526     c
270400050526     c* Se valore rientra nel mandato, imposto che si tratta di mandato
270500050526     C                   MOVEL     'N'           TASK88
270600980504    3C                   END
270700050526     c
270800050526   x2c                   else
270900050526     c* se senza valore in bolla ma madanto con valore -->ok da mandato
271000050526     c                   eval      task88='N'
271100980504    2C                   END
271200980504    1C                   END
271300050526     c
271400980525     C** SE IL CLIENTE E' 8888 HO IMPOSTATO PRIMA LA TARIFFA DI CARTELL
271500980525     C     KSCCOD        IFEQ      8888
271600050525     C                   MOVEL     'S'           TASK88
271700980525     C                   ENDIF
271800050525     c
271900050525     c** Anche se lo considero con mandato, se non c'e' importo e tipo
272000050525     c**  bolla senza assicurazione, non calcolo varia R
272100050525     C     TASIAS        IFEQ      0
272200050525     C     TASTBL        LOOKUP    TBN                                    05
272300050525     C   05              GOTO      ENDVR
272400050525     C                   ENDIF
272500980505     C**
272600941107     C                   GOTO      DOPAC2
272700941107     C     DOPAC1        TAG
272800941107     C** NON ESISTE TARIFFA AXC DEL CLIENTE
272900941121     C*-----------------------------------------------------------------
273000980505     C* SE USATA SI TRATTA DI TARIFFA DI CARTELLO
273100050525     C***                MOVEL     'S'           TASK88
273200941112     C     IASLIT        CABEQ     0             ENDVR
273300050525     c                   eval      task88='S'
273400020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
273500020226     C     KTPTC         CHAIN     TITPT01L                           28
273600020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
273700020226     C     *IN28         IFEQ      *ON
273800020226     C     TPTATB        OREQ      'A'
273900020226     C                   GOTO      ENDVR
274000020226     C                   ENDIF
274100941107     C*-----------------
274200941107     C     DOPAC2        TAG
274300980504     C** SE NON SERVE --> ESCO DAL CALCOLO
274400980504     C** TIPO APPLICAZIONE
274500980504     C** P - SOLO PER FRANCHI
274600980504     C** A - SOLO PER ASSEGNATI
274700980504    1C     TPTAPL        IFNE      *BLANKS
274800980504     C     IASLIT        ANDEQ     0
274900980504    2C     TIPBOL        IFEQ      'F'
275000980504    3C     TPTAPL        IFEQ      'A'
275100980504     C                   GOTO      ENDVR
275200980504    3C                   ENDIF
275300980504   X2C                   ELSE
275400980504    3C     TPTAPL        IFEQ      'P'
275500980504     C                   GOTO      ENDVR
275600980504    3C                   ENDIF
275700980504    2C                   END
275800980504    1C                   END
275900980504     C**
276000941112     C     *LIKE         DEFINE    TASIAS        VALVR
276100941112     C     *LIKE         DEFINE    TASIAS        ASCPES
276200941112     C     *LIKE         DEFINE    TASIAS        ASCPE2
276300941112     C                   Z-ADD     0             VALVR
276400980526    1C     IASLIT        IFEQ      0
276500980526    2C     TPTFVM        IFEQ      '3'
276600020826     C     USAPKF        MULT      TPTVLM        VALVR
276700980526    2C                   END
276800980526    2C     TPTFVM        IFEQ      '1'
276900941107     C     TASNCL        MULT      TPTVLM        VALVR
277000980526    2C                   END
277100980526    2C     TPTFVM        IFEQ      '2'
277200941107     C                   Z-ADD     TPTVLM        VALVR
277300980526    2C                   END
277400980526   X1C                   ELSE
277500941112     C                   Z-ADD     IASLIT        VALVR
277600980526    1C                   END
277700011119     C***** OBSOLETO NON RESTITUISCO + IL VALORE IN LIRE MA NELLA DIVISA DELLA TARIFFA
277800011129     C***** PERO' ARROTONDO AL DECIMALE DELLA TARIFFA
277900011129     C                   CLEAR                   YEUR
278000011129     C                   MOVEL     §TADIV        YECDVI
278100011129     C                   MOVEL     §TADIV        YECDVO
278200011129     C                   Z-ADD     VALVR         YECIMI
278300011129     C                   CALL      'YEURCO'
278400011129     C                   PARM                    YEUR
278500011129     C*
278600011129     C     YECESI        IFEQ      ' '
278700011129     C                   Z-ADD     YECIMO        VALVR
278800011129     C                   END
278900011129     C* RESTITUISCO IN OUTPUT L'IMPORTO DA ASSICURARE
279000011129     C                   Z-ADD     VALVR         TASIAL
279100941107     C** VALORE SPEDIZIONE PER CALCOLO ASSICURAZIONE
279200980312     C**
279300980526     C** CALCOLO SOLO SE NON HO IMPOSTATO IL FLAG
279400980526    1C     PARCA         IFEQ      ' '
279500980505     C**
279600980505     C** SE C'E' GIA' LA VARIA R NON LA RICALCOLO
279700980505     C                   Z-ADD     1             A
279800980505     C     §$2VRR        LOOKUP    SV(A)                                  05
279900980526    2C     *IN05         IFEQ      *OFF
280000980312     C                   MOVEL     TPTTPG        WTPG
280100980312     C                   MOVEL     TPTRPV        WRPV
280200980313     C                   CLEAR                   WDET
280300980313     C                   Z-ADD     TPTARL        WARL
280400980313     C                   Z-ADD     TPTARF        WARF
280500980313     C                   Z-ADD     TPTARO        WARO
280600980312     C                   EXSR      CALCOL
280700980312     C**
280800980312     C** A VALORE
280900980526    3C     TPTTPG        IFEQ      'V'
281000980312     C                   Z-ADD     VALVR         ISOPES
281100980526    3C                   END
281200980312     C     *LIKE         DEFINE    TASIAS        TPDPES
281300980504     C**
281400980504     C**CALCOLO IMPORTO ASSICURAZIONE PER CONT
281500980504     C                   Z-ADD     ISOPES        TPDPES
281600941107     C                   EXSR      CALACD
281700980525     C                   Z-ADD     VARIA         WVARIR
281800980526    2C                   ENDIF
281900980526    1C                   ENDIF
282000980504     C     ENDVR         ENDSR
282100060320     C******************************************************
282200060320     C** CALCOLA VARIA d  --- A C BASE
282300060320     C******************************************************
282400060321     c     CALVACB       BEGSR
282500060320     c* Il falg task88 cambia significato:indica se bolla con mandato o no
282600060320     c* Imposto task88=N se mandato
282700060321     c* Lascio  task88=X per bolle senza mandato e senza importo
282800060321
282900060321      **----------------------------------------
283000060321     c                   movel     'd'           Fiso
283100060321     c                   movel     'd'           WFiso
283200060321      ** Flag A C BASE  = d
283300060321     c                   z-add     1             a
283400060321     c     Wfiso         Lookup    Cp(a)                                  05
283500060321     c  N05              Goto      EndVacb
283600060321      * aggancio la ds della tabella 1P
283700060321     c     A             Occur     DS1P
283800060321      **----------------------------------------
283900060321     c     Kisot         Chain     TITPT01L
284000060321      ** esiste Tariffa A C base
284100060321    1c                   If        %found(titpt01l)
284200060321      **
284300060321      ** Controllo il tipo applicazione della tariffa A C Base
284400060321      **
284500060321    2c                   If        (tptapl = 'A' and tipbol = 'A') or
284600060321     c                             (tptapl = 'P' and tipbol = 'F') or
284700060321     c                              tptapl = ' '
284800060321      *
284900060321     c                   eval      task88='N'
285000060321      ** Anche se lo considero con mandato,  tipo
285100060321      **  bolla senza assicurazione, non calcolo varia d
285200060321     c     TAStbl        Lookup    Tbn                                    05
285300060321     c   05              Goto      ENDVacb
285400060321
285500060321      * calcolo valore assicurato
285600060321
285700060321     c     *Like         Define    TASias        VALVacb
285800060321
285900060321     c                   clear                   VALVacb
286000060321
286100060321      * Peso
286200060321    3c                   If        TPTfvm = '3'
286300060321     c     TPTvlm        Mult      USAPkf        Valvacb
286400060321    3c                   Endif
286500060321      * Colli
286600060321    3c                   If        TPTfvm = '1'
286700060321     c     TPTvlm        Mult      TASncl        Valvacb
286800060321    3c                   Endif
286900060321      * Spedizione
287000060321    3c                   If        TPTfvm = '2'
287100060321     c                   Z-add     TPTvlm        Valvacb
287200060321    3c                   Endif
287300060321      *
287400060321      * Arrotondo al decimale della tariffa
287500060321     c                   Clear                   YEUR
287600060321     c                   Movel     §TAdiv        YECdvi
287700060321     c                   Movel     §TAdiv        YECdvo
287800060321     c                   Z-add     VALvacb       YECimi
287900060321     c                   call      'YEURCO'
288000060321     c                   Parm                    YEUR
288100060321      *
288200060321     c                   If        YECesi = ' '
288300060321     c                   Z-add     YECimo        VALvacb
288400060321     c                   Endif
288500060321
288600060321      ** Calcolo solo se non ho impostato il flag
288700060321    3c                   IF        Parca = ' '
288800060321
288900060321      ** Se c'è' già la varia non la ricalcolo
289000060321     c                   Z-add     1             a
289100060321     c     §$2acb        Lookup    sv(a)                                  05
289200060321    4c     *IN05         Ifeq      *off
289300060321     c                   movel     TPTtpg        Wtpg
289400060321     c                   Movel     TPTrpv        Wrpv
289500060321     c                   clear                   Wdet
289600060321     c                   Z-add     TPTarl        Warl
289700060321     c                   Z-add     TPTarf        Warf
289800060321     c                   Z-add     TPTaro        Waro
289900060321     c                   Exsr      Calcol
290000060321
290100060321      ** A Valore
290200060321    5c                   IF        tpttpg =  'V'
290300060321     c                   z-add     valvacb       ISOpes
290400060321    5c                   Endif
290500060321
290600060321     c                   Z-add     ISOpes        TPDpes
290700060321     c                   Exsr      Calacd
290800060321     c                   Z-add     varia         Wvariacb
290900060321    4c                   endif
291000060321
291100060321    3c                   endif
291200060321
291300060321    2c                   endif
291400060321    1c                   Endif
291500060321
291600060321     c     EndVacb       ENDSR
291700941112     C******************************************************
291800980312     C** CALCOLA TARIFFA PARTICOLARE
291900941112     C******************************************************
292000941107     C     CALACD        BEGSR
292100990923     C* CAMPO VARIA CON  DECIMALI
292200990923     C                   Z-ADD     0             VARIA            15 3
292300990923     C* CAMPO VARIA SENZA DECIMALI ALTRIMENTI NON FUNZIONA L'ARROTONDAMENTO
292400990923     C                   Z-ADD     0             VARIAD           15 0
292500990923     C*
292600941107     C                   DO        100           A
292700941122     C     A             OCCUR     ACDS
292800941107     C                   CLEAR                   ACDS
292900941107     C                   END
293000941112     C** PULIZIA DS TARIFFA
293100020412     C*
293200020412     C* SE ESISTE ALMENO UNA RIGA DI DETTAGLIO E NON RIESCO AD APPLICAR
293300020412     C*  QUESTA TAR.PARTICOLARE --> MANCA TARIFFA
293400980605     C* BOLLA IMPORT / ESXPORT
293500980605     C     WBOEST        IFEQ      'E'
293600020226     C     §1PCTE        IFNE      *BLANKS
293700020226     C                   MOVEL     §1PCTE        ISOCTS
293800941112     C                   GOTO      POIAC
293900941112     C                   END
294000941112     C                   ELSE
294100020226     C     §1PCTS        IFNE      *BLANKS
294200020226     C                   MOVEL     §1PCTS        ISOCTS
294300941112     C                   GOTO      POIAC
294400941112     C                   END
294500941112     C                   END
294600941112     C** CODICI TASSAZIONE FISSI DA TABELLA TARIFFE PARTICOLARI
294700941110     C                   MOVEL     TASCTS        ISOCTS            2
294800060414     c
294900060414      * Se assegnato e tariffa FEDEX metto in ISOCTS  quello mittente
295000050124     c                   If        TIPBOL = 'A' and WBOFED = 'S'
295100050124     c                   movel     tasmct        isocts
295200050124     c                   endif
295300060414     c* Se devo utilizzare quello del mittente per via della
295400060414     c*  reversibilità --> lo uso
295500060414     c                   if        applicarevers='S'
295600060414     c                   movel     tasmct        isocts
295700060414     c                   endif
295800050124      *
295900990721     C     KISOD         SETLL     TITPD02L                               28
296000941107     C   28              GOTO      DOPAC3
296100941107     C                   Z-ADD     1             A
296200941107     C     ISOCTS        LOOKUP    CTS(A)                                 05
296300941107     C   05A             OCCUR     DSCT
296400941107     C   05              MOVEL     §CTRAP        ISOCTS
296500941107     C  N05              MOVEL     *BLANKS       ISOCTS
296600941107     C** REGIONE
296700990721     C     KISOD         SETLL     TITPD02L                               28
296800941107     C   28              GOTO      DOPAC3
296900980605     C     WBOEST        IFEQ      'E'
297000941112     C                   MOVEL     'EE'          ISOCTS
297100941112     C                   ELSE
297200941112     C                   MOVEL     'IT'          ISOCTS
297300941112     C                   END
297400941112     C     POIAC         TAG
297500990721     C     KISOD         SETLL     TITPD02L                               28
297600020412     C**N28                GOTO ENDACD
297700020412     C** N 28 --> MANCA TARIFFA
297800020412     C     *IN28         IFEQ      *OFF
297900020412     C* SE NON È PRESENTE NESSUNA RIGA DI DETTAGLIO, NON DO
298000020412     C*  MANCA TARIFFA: SIGNIFICA CHE È INSERITA UNA TASSAZIONE =0
298100020412     C     KISOD2        SETLL     TITPD02L                               28
298200020412     C  N28              GOTO      ENDACD
298300020412     C*
298400020412     C                   SETON                                        94
298500020412     C                   MOVEL     MSG(17)       TASMSG
298600140224     c                   eval      %subst(tasmsg: 47: 2) = TPDFTC
298700020412     C                   GOTO      FINE
298800020412     C                   ENDIF
298900020412     C**
299000020412     C** NON TROVATA TARIFFA
299100941107     C     DOPAC3        TAG
299200941107     C                   DO        *HIVAL        A
299300990721     C     KISOD         READE     TITPD02L                               28
299400941107     C   28              GOTO      DOPAC4
299500941107     C     A             OCCUR     ACDS
299600990923     C* VERIFICO SE STO CARICANDO LA TARIFFA DI CARTELLO
299700990923     C     TPTKSC        IFEQ      CARKSC
299800990923     C     TPTCTR        ANDEQ     CARCTR
299900990923     C     TPTPRG        ANDEQ     CARPRG
300000990923     C* E LA DIVISA DELLA TARIFFA DI CARTELLO E' DIVERSA DALLA DIVISA DELLA
300100990923     C* TARIFFA DEL CLIENTE  CONVERTO I VALORI O SCAGLIONI NELLA DIVISA
300200990923     C* DELLA TARIFFA DEL CLIENTE
300300990923     C     §TADIV        ANDNE     DIVCAR
300400990923     C*
300500990923     C                   EXSR      CONTPD
300600990923     C*
300700990923     C                   ENDIF
300800990923     C*
300900941112     C                   Z-ADD     TPDSGL        ASGL
301000941112     C                   Z-ADD     TPDITR        AITR
301100941112     C                   Z-ADD     TPDMIN        AMIN
301200941112     C                   Z-ADD     TPDMAX        AMAX
301300941112     C                   MOVEL     TPDAIN        AAIN
301400941107     C                   END
301500941107     C     DOPAC4        TAG
301600941107     C                   DO        100           A
301700941107     C     A             OCCUR     ACDS
301800941112     C     ASGL          IFGE      TPDPES
301900941107     C                   GOTO      DOPAC5
302000941107     C                   END
302100941107     C                   END
302200941123     C     ASGL          IFLT      TPDPES
302300941122     C                   SETON                                        94
302400980527     C                   MOVEL     MSG(7)        TASMSG
302500140224     c                   eval      %subst(tasmsg: 47: 2) = TPDFTC
302600980527     C                   GOTO      FINE
302700980527     C                   ENDIF
302800941122     C** NON TROVATO SCAGLIONE ESATTO
302900941107     C     DOPAC5        TAG
303000941112     C*-----------
303100941122     C     ASGL          IFLE      MINTAS
303200941122     C                   Z-ADD     MINTAS        TPDPES
303300941112     C                   END
303400941122     C     TPDPES        IFLE      MINTAS
303500941122     C                   Z-ADD     MINTAS        TPDPES
303600941122     C                   END
303700991013     C                   Z-ADD     0             TPDQLI           18 6
303800980429     C* DIVIDO PER 100 TARIFFA 0 PER AVERE QUINTALI
303900980429     C*                TARIFFA 4 PERCHE' E' TARIFFA %
304000980429     C     WTPG          IFEQ      '0'
304100980429     C     WTPG          OREQ      '4'
304200941112     C     TPDPES        DIV       100           TPDQLI
304300941112     C                   ELSE
304400941112     C                   Z-ADD     TPDPES        TPDQLI
304500941112     C                   END
304600980429     C** PESO TASSABILE
304700060906      * se trovato dettaglio tariffario della tariffa "f" vado a fine routine in quanto il calcolo
304800060906      * viene eseguito nella routine FUEL
304900060906     c                   If        wfiso = 'f'
305000060906     c                   goto      endacd
305100060906     c                   endif
305200060906
305300990923     C   12AITR          MULT(H)   TPDQLI        VARIA
305400990923     C  N12AITR          MULT(H)   TPDQLI        VARIAD
305500941112     C                   MOVEL     AAIN          FLGISO
305600980429     C*
305700980430     C     VARIA         IFGT      0
305800991012     C     *IN12         ANDEQ     *ON
305900991012     C     VARIAD        ORGT      0
306000991012     C     *IN12         ANDEQ     *OFF
306100991012     C*
306200980430     C     WTPG          IFEQ      'V'
306300980430     C     WTPG          OREQ      'T'
306400990923     C   12VARIA         DIV(H)    100           VARIA
306500990923     C  N12VARIAD        DIV(H)    100           VARIAD
306600980430     C                   ENDIF
306700980430     C                   ENDIF
306800980429     C**
306900991012     C* SPOSTO L'IMPORTO DEL CAMPO VARIAD NEL CAMPO VARIA SOLO SE E' STATO USATO
307000991012     C  N12              Z-ADD     VARIAD        VARIA
307100991012     C*
307200991012     C** MINIMO
307300941112     C     AMIN          IFGT      VARIA
307400991012     C                   Z-ADD     AMIN          VARIA
307500941107     C                   END
307600991012     C** MASSIMO
307700941115     C     AMAX          IFGT      0
307800941112     C     AMAX          IFLT      VARIA
307900991012     C                   Z-ADD     AMAX          VARIA
308000941112     C                   END
308100941115     C                   END
308200000103     C** SE VARIA CON ESENZIONE IVA DEVE AVERE SOLO 2 DCEIMALI
308300000103     C* CERCO LA SIGLA VARIA CORRISPONDENTE
308400000103     C                   MOVEL     TPTFTC        WFTC              1
308500000103     C                   Z-ADD     1             A
308600000103     C     WFTC          LOOKUP    CP(A)                                  10
308700000103     C     *IN10         IFEQ      *ON
308800000103     C     A             OCCUR     DS1P
308900000103     C     §1PVAR        LOOKUP    VES                                    10
309000000103     C     *IN10         IFEQ      *ON
309100000103     C                   MOVE      VARIA         W001A             1
309200000103     C     W001A         IFNE      '0'
309300000103     C     VARIA         ADD       0,001         COMOD2           15 2
309400000103     C                   Z-ADD     COMOD2        VARIA
309500000103     C                   ENDIF
309600000103     C                   ENDIF
309700000103     C                   ENDIF
309800000103     C**
309900941107     C     ENDACD        TAG
310000990923     C*
310100941107     C                   ENDSR
310200941107     C/SPACE
310300941112     C******************************************************
310400980424     C** CALCOLO TARIFFE  PARTICOLARI
310500970521     C******************************************************
310600980428     C     CALCP         BEGSR
310700990721     C** CERO LA TARIFFA PARTICOLARE IN TITPT O CARTELLO
310800980430     C                   MOVEL     TASTC         FISO
310900980506     C                   MOVEL     'S'           WCAR
311000110908      ** se sto calcolando la varia "Q" = Centro storico o ZTL
311100110908      ** non devo cercare la tariffa nella Cartello in mancanza
311200110908      ** di quella del cliente
311300111102      ** E' stata aggiuna una tabella che pilota il recupero della
311400111102      ** tariffa di cartello se la filiale del cliente di tassazione
311500111102      ** è presente nella tabella ztl e la data spedizione è maggiore
311600111102      ** uguale alla data attivazione della tassazione
311700111102     c                   clear                   indztl            3 0
311800110908     c                   If        tastc = 'Q'
311900111102     c                   movel     'N'           WCAR
312000111102      * cerco la filiale del cliente nella tabella ZTL
312100111102     c                   eval      indztl = %lookup(kscfil:sfilztl)
312200111102      * se indice =  a zero cerco con la filiale 999
312300111102    2c                   if        indztl = *zeros
312400111102     c                   eval      indztl = %lookup(999:sfilztl)
312500111102    2c                   endif
312600111102      * imposto il flag della ricerca tariffa di cartello = 'S'  se filiale cliente
312700111102      * abilitata e la data spedizione > = data attivazione tassazione
312800111102    2c                   If        indztl > *zeros and
312900111102    3c                             tasdsp >= sdtaztl(indztl)
313000111102     c                   movel     'S'           WCAR
313100111102    3c                   endif
313200110908     c                   endif
313300110908      *
313400980428     C                   EXSR      CERTPT
313500980428     C** SOLO SE L'HO TROVATA
313600980428    1C     WEND          IFEQ      ' '
313700970521     C                   Z-ADD     ISOPES        TPDPES
313800970521     C                   EXSR      CALACD
313900970521     C                   Z-ADD     VARIA         TASCP
314000980428    1C                   ENDIF
314100970521     C*
314200970521     C                   ENDSR
314300001205     C/SPACE
314400001205     C******************************************************
314500001205     C** CALCOLO ADDIZIONALE DI GESTIONE + ISTAT
314600001205     C******************************************************
314700001205     C     ADGIST        BEGSR
314800001205     C**
314900001205     C*
315000001205     C****************
315100041129     C** ADDIZIONALE DI GESTIONE  anni precedenti       varia "Z"
315200001205     C****************
315300001205     C*
315400031121     C     *LIKE         DEFINE    TASVA1        TASADG
315500001205     C     TAMPAG        IFNE      0
315600001205     C                   Z-ADD     1             A
315700001205     C     §$2VRZ        LOOKUP    SV(A)                                  05
315800001205     C  N05              DO
315900001205     C                   Z-ADD     0             TASADG
316000001205     C* TOTALE IMPONIBILE
316100001205     C                   Z-ADD     O17IMV        IMPADG           15 3
316200001205     C**
316300001205     C                   Z-ADD     0             COMADG           15 3
316400001205     C     IMPADG        MULT      TAMPAG        COMADG
316500001205     C     COMADG        IFGT      0
316600001205     C     COMADG        DIV(H)    100           TASADG
316700001205     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
316800001205     C     *IN12         IFEQ      '0'
316900001205     C     TASADG        ADD       0,5           CAMPOW           11 0
317000001205     C                   Z-ADD     CAMPOW        TASADG
317100001205     C                   END
317200001205     C                   END
317300001205     C*
317400040128     c     tasadg        ifgt      0
317500040128      *
317600001205     C                   Z-ADD     1             A
317700001205     C     ' '           LOOKUP    SV(A)                                  96
317800001205     C   96              MOVEL     §$2VRZ        SV(A)
317900001205     C   96              Z-ADD     TASADG        VA(A)
318000001205     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
318100001205     C  N96              ADD       TASADG        TASPOR
318200001205     C*
318300001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
318400001205     C*
318500001205     C                   CLEAR                   DSLV16
318600001205     C                   MOVEL     'T'           D17FIM
318700001205     C                   Z-ADD     TASPOR        D17POR
318800001205     C                   CALL      'FNLV16R'
318900001205     C                   PARM                    DSLV16
319000001205     C                   PARM                    DTASV
319100040128      *
319200040128     c                   endif
319300001205     C*
319400001205     C                   END
319500001205     C                   END
319600041129     C*
319700041129     C****************
319800041129     C** ADDIZIONALE DI GESTIONE    anno corrente       varia "b"
319900041129     C****************
320000041129     C*
320100041129     c                   clear                   tasadg
320200041129     c                   clear                   impadg
320300041129     c                   clear                   comadg
320400041129
320500041129     C     TAMPPA        IFNE      0
320600041129     C                   Z-ADD     1             A
320700041129     C     §$2VAG        LOOKUP    SV(A)                                  05
320800041129     C  N05              DO
320900041129     C                   Z-ADD     0             TASADG
321000041129     C* TOTALE IMPONIBILE
321100041129     C                   Z-ADD     O17IMV        IMPADG
321200041129     C**
321300041129     C                   Z-ADD     0             COMADG
321400041129     C     IMPADG        MULT      TAMPPA        COMADG
321500041129     C     COMADG        IFGT      0
321600041129     C     COMADG        DIV(H)    100           TASADG
321700041129     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
321800041129     C     *IN12         IFEQ      '0'
321900041129     C     TASADG        ADD       0,5           CAMPOW           11 0
322000041129     C                   Z-ADD     CAMPOW        TASADG
322100041129     C                   END
322200041129     C                   END
322300041129     C*
322400041129     c     tasadg        ifgt      0
322500041129      *
322600041129     C                   Z-ADD     1             A
322700041129     C     ' '           LOOKUP    SV(A)                                  96
322800041129     C   96              MOVEL     §$2VAG        SV(A)
322900041129     C   96              Z-ADD     TASADG        VA(A)
323000041129     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
323100041129     C  N96              ADD       TASADG        TASPOR
323200041129     C*
323300041129     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
323400041129     C*
323500041129     C                   CLEAR                   DSLV16
323600041129     C                   MOVEL     'T'           D17FIM
323700041129     C                   Z-ADD     TASPOR        D17POR
323800041129     C                   CALL      'FNLV16R'
323900041129     C                   PARM                    DSLV16
324000041129     C                   PARM                    DTASV
324100041129      *
324200041129     c                   endif
324300041129     C*
324400041129     C                   END
324500041129     C                   END
324600001205     C**
324700001205     C***************
324800001205     C** ISTAT
324900001205     C***************
325000001205     C*
325100031121     C     *LIKE         DEFINE    TASVA1        TASIAC
325200001205     C                   Z-ADD     1             A
325300001205     C     §$2VRV        LOOKUP    SV(A)                                  05
325400101014    1C  N05TAMrct        IFGT      0
325500001205     C                   Z-ADD     0             TASIAC
325600001205     C*
325700001205     C* CALCOLO ISTAT SU TOTALE IMPONIBILE
325800001205     C*
325900001205     C                   Z-ADD     O17IMV        IMPIAC           15 3
326000101014     C                   Z-ADD     0             PUNIAC            7 4
326100101014
326200101014      * richiamo nuovo pgm per calcolo % istat da aggiungere all'imponibile
326300101014     c
326400101014     c                   clear                   trulistds
326500101014     c                   eval      IISTsca = tamrct
326600101014     c                   eval      IISTdsp = tasdsp
326700101014     c                   call      'TRULISTR'
326800101014     c                   parm                    trulistds
326900101014     c                   IF        OISTerr = *blanks
327000101014     c                   eval      puniac = OISTiac
327100101014     c                   ENDIF
327200101014     c
327300050629     C                   Z-ADD     0             PERIAC           15 6
327400050629     C                   Z-ADD     0             PERIAX           15 4
327500001205     C     PUNIAC        MULT      TAMPRI        PERIAX
327600001205    3C     PERIAX        IFNE      0
327700031121     C     PERIAX        DIV       100           PERIAC
327800001205    3C                   END
327900001205     C** PERCENTUALE DI ISTAT DA APPLICARE
328000001205     C** APPLICO I PUNTI X TRIMESTRE SOLO SE
328100001205     C** LA DATA DI SPEDIZIONE E'
328200001205     C** MAGGIORE O UGUALE ALLA DATA DI SCATTO
328300001205     C** DEI PUNTI NEL TRIMESTRE
328400001205    3C     PERIAC        IFNE      0
328500031121     C                   DIV       100           PERIAC
328600001205     C     IMPIAC        MULT(H)   PERIAC        TASIAC
328700001205    3C                   END
328800001205     C*
328900001205    3C     TASIAC        IFGT      0
329000001205     C*  SENZA DECIMALI
329100001205    4C     *IN12         IFEQ      *OFF
329200001205     C     TASIAC        ADD       0,5           CAMPOW           11 0
329300001205     C                   Z-ADD     CAMPOW        TASIAC
329400001205    4C                   END
329500001205     C*
329600001205     C                   Z-ADD     1             A
329700001205     C     ' '           LOOKUP    SV(A)                                  96
329800001205     C   96              MOVEL     §$2VRV        SV(A)
329900001205     C   96              Z-ADD     TASIAC        VA(A)
330000001205     C  N96              ADD       TASIAC        TASPOR
330100001205     C*
330200001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
330300001205     C*
330400001205     C                   CLEAR                   DSLV16
330500001205     C                   MOVEL     'T'           D17FIM
330600001205     C                   Z-ADD     TASPOR        D17POR
330700001205     C                   CALL      'FNLV16R'
330800001205     C                   PARM                    DSLV16
330900001205     C                   PARM                    DTASV
331000001205     C*
331100001205    3C                   END
331200001205    1C                   ENDIF
331300001205     C*
331400001205     C                   ENDSR
331500030922     C*******************************************************
331600030922     C** CALCOLO DIRITTO DI FATTURAZIONE  + TOTALE IMPONIBILE
331700030922     C******************************************************
331800030922     C     DIRFAT        BEGSR
331900030922     C**
332000030922      * prima verifico se esiste già
332100030922     C                   Z-ADD     1             A
332200030922     C     §$2VRD        LOOKUP    SV(A)                                  05
332300030922     C* SIGLA VARIA NON PRESENTE --> CALCOLO
332400030922     C     *IN05         IFEQ      *OFF
332500030922     C                   Z-ADD     1             A
332600030922     C     ' '           LOOKUP    SV(A)                                  96
332700030922     C   96              MOVEL     §$2VRD        SV(A)
332800030922     C   96              Z-ADD     TASIVDF       VA(A)
332900030922     C  N96              ADD       TASIVDF       TASPOR
333000030922     C*
333100030922     C* RICALCOLO L'IMPONIBILE SOMMANDO IL DIRITTO DI FATTURAZIONE
333200030922     C*
333300030922     C                   CLEAR                   DSLV16
333400030922     C                   MOVEL     'T'           D17FIM
333500030922     C                   Z-ADD     TASPOR        D17POR
333600030922     C                   CALL      'FNLV16R'
333700030922     C                   PARM                    DSLV16
333800030922     C                   PARM                    DTASV
333900030924     C*
334000030924     C                   Z-ADD     O17IMV        TASIMV
334100030924     C*
334200030922     C*
334300030922     C                   ENDIF
334400030922     C*
334500030922     C                   ENDSR
334600980428     C******************************************************
334700980428     C* CERCO TARIFFA PARTICOLARE
334800980428     C******************************************************
334900980428     C     CERTPT        BEGSR
335000980428     C                   CLEAR                   TASCP
335100980428     C                   CLEAR                   WEND
335200980428     C                   MOVEL     *BLANKS       FLGISO            1            FLAG.APPLICAZI.
335300990722     C                   MOVEL     FISO          WFISO
335400980428     C                   Z-ADD     1             A
335500990722     C     WFISO         LOOKUP    CP(A)                                  05
335600980428     C**
335700980428    1C     *IN05         IFEQ      *ON
335800020226     C     A             OCCUR     DS1P
335900980428     C*-----------------------------------------
336000990721     C     KISOT         CHAIN     TITPT01L                           28
336100980528     C** ESISTE TARIFFA DEL CLIENTE
336200980528     C     *IN28         IFEQ      *OFF
336300980528     C     TPTATB        ANDEQ     *BLANKS
336400980428     C                   GOTO      DOPIS2
336500980528     C                   ENDIF
336600980428     C*-----------------------------------------
336700980428     C** MEMORIZZO DATI DA TARIFFA DI CARTELLO
336800980506     C**  SOLO SE HO DETTO CHE LA POSSO USARE E SE C'E'
336900980428     C*-----------------------------------------
337000020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
337100020226     C     WCAR          IFEQ      'S'
337200020226     C     KTPTC         CHAIN     TITPT01L                           28
337300020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
337400020226     C     *IN28         IFEQ      *ON
337500020226     C     TPTATB        OREQ      'A'
337600020226     C                   MOVEL     'S'           WEND
337700020226     C                   GOTO      DOPIS3
337800020226     C                   ENDIF
337900020226      * E SE NON DEVO CARICARE LA CARTELLO ERRORE NON TROVATA TARIFFA PARTICOLARE
338000020226     C                   ELSE
338100020226     C                   MOVEL     'S'           WEND
338200020226     C                   GOTO      DOPIS3
338300020226     C                   ENDIF
338400980430     C     DOPIS2        TAG
338500980430     C* ARROTONDAMENTI E RPV
338600980430     C                   MOVEL     TPTTPG        WTPG
338700090310      * in caso di pod-immage varia "a" imposto come tipo pagamento a colli
338800090310
338900090310     c                   if        fiso = §$2POD
339000090310     C                   MOVEL     '1'           WTPG
339100090310     c                   endif
339200090310
339300980430     C                   MOVEL     TPTRPV        WRPV
339400980430     C                   CLEAR                   WDET
339500980430     C                   Z-ADD     TPTARL        WARL
339600980430     C                   Z-ADD     TPTARF        WARF
339700980430     C                   Z-ADD     TPTARO        WARO
339800980430     C                   EXSR      CALCOL
339900980428     C** NON TROVATA TARIFFA PARTICOLARE
340000980428   X1C                   ELSE
340100980428     C                   MOVEL     'S'           WEND              1
340200980428    1C                   ENDIF
340300020226     C     DOPIS3        ENDSR
340400980312     C******************************************************
340500980312     C** CALCOLO IL VALORE DA MOLTIPLICARE/RPV/ARROTONDAMENTI
340600980312     C******************************************************
340700980312     C     CALCOL        BEGSR
340800980313     C     *LIKE         DEFINE    TAMARL        ARR
340900980313     C     *LIKE         DEFINE    TASIAS        PESTAS
341000980313     C     *LIKE         DEFINE    TASIAS        ISOPES
341100980312     C     *LIKE         DEFINE    TASIAS        ISOPE2
341200980312     C     *LIKE         DEFINE    TPTRPV        WRPV
341300980313     C     *LIKE         DEFINE    TPTARL        WARL
341400980313     C     *LIKE         DEFINE    TPTARF        WARF
341500980313     C     *LIKE         DEFINE    TPTARO        WARO
341600980312     C                   Z-ADD     0             ISOPES
341700980316     C                   SELECT
341800030203
341900030204     c                   When      wFiso = '='
342000030203     c                   Z-Add     TasBan        Isopes
342100030203
342200980429     C     WTPG          WHENEQ    '0'
342300980429     C     WTPG          OREQ      '3'                                          KG
342400020826     C                   Z-ADD     USAPKF        ISOPES
342500980316     C*
342600980429     C     WTPG          WHENEQ    '1'                                          COLLI
342700980312     C                   Z-ADD     TASNCL        ISOPES
342800980316     C*
342900980429     C     WTPG          WHENEQ    '2'                                          SPEDIZ
343000980312     C                   Z-ADD     1             ISOPES
343100980316     C*
343200980429     C     WTPG          WHENEQ    '4'                                          VALORE
343300980429     C     WTPG          OREQ      '5'                                          Q.TA'
343400980312     C                   Z-ADD     TASQFT        ISOPES
343500980429     C*
343600980429     C     WTPG          WHENEQ    'T'                                          TRASPORTO
343700980429     C                   Z-ADD     TASPOR        ISOPES
343800980429     C                   ADD       TASINL        ISOPES
343900980429     C                   EXSR      CERISO
344000980429     C                   ADD       VAISO         ISOPES
344100060414     C                   ADD       VAtsp         ISOPES
344200980316     C                   ENDSL
344300980312     C** MINIMO TASSABILE STD
344400980312     C                   Z-ADD     0             MINTAS
344500980312     C**
344600980312     C                   Z-ADD     1             A
344700980312     C     WTPG          LOOKUP    CTR(A)                                 05
344800980312     C   05A             OCCUR     DSTR
344900980312     C   05              Z-ADD     §TRATA        MINTAS
345000980312     C**
345100980312     C** SOLO PER TAD (PER TPD NON MI SERVE)
345200980428    1C     WDET          IFEQ      'TAD'
345300980312     C                   CLEAR                   MINTAR
345400980312     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
345500980312     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
345600980312     C   05              Z-ADD     §TRATA        MINTAR
345700980428    2C     TAMATA        IFGT      0
345800980312     C                   Z-ADD     TAMATA        MINTAR
345900980428    2C                   ENDIF
346000980428    1C                   ENDIF
346100980312     C**
346200980428     C** RPV SOLO PER TARIFFA 0 E 3
346300980428    1C     §TRRPV        IFEQ      'S'
346400980428     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
346500980428    2C     WDET          IFEQ      'TAD'
346600980428     C     TASVLF        ANDGT     0
346700020218    3C                   DO        200           A
346800980428     C     A             OCCUR     TARDS
346900980428    4C     TSCG          IFGE      ISOPES
347000980428     C                   GOTO      POITAS
347100980428    4C                   END
347200980428    3C                   END
347300980428     C** RICERCO RPV CON PESO REALE
347400980428    3C     TSCG          IFLT      ISOPES
347500980428     C                   SETON                                        94
347600980527     C                   MOVEL     MSG(5)        TASMSG
347700980527     C                   GOTO      FINE
347800980428    3C                   END
347900980428     C     POITAS        TAG
348000980428     C                   Z-ADD     TRPV          WRPV
348100040603     C                   Z-ADD     TRPV          rappv
348200980428    2C                   ENDIF
348300980428     C**
348400980428    2C     TASVLF        IFNE      0
348500980428     C     WRPV          ANDNE     0
348600980428     C                   Z-ADD     0             COMPRV            9 3
348700980428     C     TASVLF        MULT      WRPV          COMPRV
348800980428     C     COMPRV        MULT      100           ISOPE2
348900980428    3C     ISOPE2        IFGT      ISOPES
349000980428     C                   Z-ADD     ISOPE2        ISOPES
349100980428    3C                   ENDIF
349200980428    2C                   ENDIF
349300980428    1C                   ENDIF
349400990923     C**
349500990923     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
349600990923    0C     §TRARR        IFEQ      'S'
349700980313     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
349800980312     C     ISOPES        IFGE      MINTAS
349900980312     C                   Z-ADD     0             ARR
350000980313     C     ISOPES        IFGT      WARL
350100980313     C                   Z-ADD     WARO          ARR
350200980312     C                   ELSE
350300980313     C                   Z-ADD     WARF          ARR
350400980312     C                   END
350500980313     C**
350600980313     C                   Z-ADD     0             RESTO            15 5
350700980312     C                   Z-ADD     0             ISOPEX           13 0
350800980312     C     ARR           IFNE      0
350900980312     C     ISOPES        ANDNE     0
351000980312     C     ISOPES        DIV       ARR           ISOPEX
351100980312     C                   MVR                     RESTO
351200980312     C     RESTO         IFGT      0
351300980312     C     ISOPEX        MULT      ARR           ISOPES
351400980312     C                   ADD       ARR           ISOPES
351500980312     C                   ENDIF
351600980312     C                   ENDIF
351700980312     C                   ENDIF
351800980312     C**
351900980312     C***        GIUARR    TAG
352000980312    0C                   ENDIF
352100980312     C                   ENDSR
352200970521     C******************************************************
352300941107     C** CALCOLA DIRITTO ASSICURATIVO - VARIA E
352400941112     C******************************************************
352500941107     C     CALVE         BEGSR
352600941107     C                   MOVEL     'R'           FISO
352700980506     C                   MOVEL     'N'           WCAR
352800980430     C                   EXSR      CERTPT
352900980430     C     WEND          IFEQ      ' '
353000990922     C** TARIFFA A VALORE TOLGO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DI CONTO
353100990922     C*  E CALCOLO VALORE AL KG (FISSO) PER LA PARTE RESTANTE
353200980428    1C     TPTTPG        IFEQ      'V'
353300980312     C                   CLEAR                   ISOPES
353400990922     C* CONVERTO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DELLA TARIFFA
353500990922     C*
353600990922     C     *LIKE         DEFINE    §GELRP        WLRP
353700990922     C*
353800990922     C     §TADIV        IFNE      §GEDCN
353900990922     C                   CLEAR                   YEUR
354000990922     C                   MOVEL     §GEDCN        YECDVI
354100990922     C                   MOVEL     §TADIV        YECDVO
354200990922     C                   Z-ADD     §GELRP        YECIMI
354300991014     C   12              MOVE      '3'           YECDCO
354400991014     C  N12              MOVE      '0'           YECDCO
354500990922     C*
354600990922     C                   CALL      'YEURCO'
354700990922     C                   PARM                    YEUR
354800990922     C*
354900990922     C     YECESI        IFEQ      ' '
355000990922     C                   Z-ADD     YECIMO        WLRP
355100990922     C                   END
355200991112     C* SE TARIFFE UGUALI VALORIZZO WLRP
355300991112     C                   ELSE
355400991118     C                   Z-ADD     §GELRP        WLRP
355500990922     C                   ENDIF
355600990922     C*
355700990922    2C     TPTVLM        IFGT      WLRP
355800990922     C     TPTVLM        SUB       WLRP          ISOPES
355900020826     C     ISOPES        MULT      USAPKF        ISOPES
356000980312    2C                   ENDIF
356100980312    1C                   ENDIF
356200980430     C**
356300980312     C                   Z-ADD     ISOPES        TPDPES
356400941112     C                   EXSR      CALACD
356500941112     C                   Z-ADD     VARIA         DIRITT
356600941112     C**
356700941112     C     TPTAPL        IFNE      *BLANKS
356800941112     C     TIPBOL        IFEQ      'F'
356900941112     C     TPTAPL        IFEQ      'A'
357000941112     C                   Z-ADD     0             DIRITT
357100941112     C                   END
357200941112     C                   ELSE
357300941112     C     TPTAPL        IFEQ      'P'
357400941112     C                   Z-ADD     0             DIRITT
357500941112     C                   END
357600941112     C                   END
357700941112     C                   END
357800941112     C** TIPO APPLICAZIONE
357900941112     C** P - SOLO PER FRANCHI
358000941112     C** A - SOLO PER ASSEGNATI
358100980430     C                   ENDIF
358200941107     C                   ENDSR
358300941107     C/SPACE
358400941112     C******************************************************
358500941112     C** CALCOLA MAGGIORAZIONE TIPO SERVIZIO
358600941112     C******************************************************
358700941112     C     CALTS         BEGSR
358800980506     C                   MOVEL     'S'           WCAR              1
358900090924     c
359000090924     C* 24/09/09 --> Ora applichiamo sempre la maggiorazine espresso
359100090924     c
359200980506     C* SE TIPO SERVIZIO DELLA TARIFFA = A TIPO SERVIZIO BOLLA
359300980506     C*  NON PRENDO LA TARIFFA DI CARTELLO
359400090922     C***  TAMTSP        IFEQ      TASTSP
359500090922     C***                MOVEL     'N'           WCAR              1
359600090922     C***                ENDIF
359700980506     C* SIGLA VARIA CORRISPONDENTE
359800980506     C                   MOVEL     §5EFTC        FISO
359900980506     C                   EXSR      CERTPT
360000980506     C     WEND          IFEQ      ' '
360100980506     C**
360200980506     C** A TRASPORTO
360300980430     C     TPTTPG        IFEQ      'T'
360400980312     C                   Z-ADD     IMPON         ISOPES
360500941112     C                   END
360600980312     C**
360700980312     C                   Z-ADD     ISOPES        TPDPES
360800941112     C                   EXSR      CALACD
360900941112     C                   Z-ADD     VARIA         WTSP
361000980506     C                   ENDIF
361100941112     C**
361200941112     C                   ENDSR
361300941112     C******************************************************
361400020225     C** CERCA TARIFFA TNTAM PIU' RELATIVA CARTELLO
361500941112     C******************************************************
361600941107     C     CERTAM        BEGSR
361700020225      *
361800941111     C     *LIKE         DEFINE    TAMPRG        PRG2
361900941111     C     *LIKE         DEFINE    TAMKSC        KSC2
362000941111     C     *LIKE         DEFINE    TAMCTR        CTR2
362100061219      * se in tasflo c'è una data riferimento per il recupero della tariffa la imposto
362200061219      * solo temporaneamente al posto della data spedizione
362300061219     c                   clear                   Sav_dsp
362400070108     c                   if        §asdrt  > *zeros
362500061219     c                   eval      Sav_dsp = Tasdsp
362600070108     c***                eval      tasdsp = §asdrt
362700070108     c                   movel     §asdrt        tasdsp
362800061219     c                   endif
362900020225      *
363000941112     C                   SETOFF                                       180717
363100020227     C                   SETOFF                                       0890
363200941111     C                   Z-ADD     0             PRG2
363300941111     C                   Z-ADD     0             KSC2
363400941111     C                   Z-ADD     0             CTR2
363500941107     C                   Z-ADD     0             TAMKSC
363600941107     C                   Z-ADD     0             TAMPRG
363700941107     C                   Z-ADD     0             TAMCTR
363800980605     C     KTAM          SETLL     TNTAM000                               08
363900980605     C* MANCA TARIFFA CLIENTE
364000980605    1C     *IN08         IFEQ      *OFF
364100980605     C                   MOVEL     MSG(3)        TASMSG
364200980527     C                   SETON                                        94
364300980527     C                   GOTO      ENDTAM
364400980605    1C                   ENDIF
364500980605     C**
364600941107     C     SUTAM         TAG
364700980605     C**
364800941107     C     KTAM          READE     TNTAM000                               07
364900980605     c**
365000941107     C** SE HO LETTO UNA TARIFFA SCADUTA (17) E IL
365100980605     C** RECORD SUCCESSIVO MI ACCENDE FINE FILE
365200980605     C** chaino vecchio progressivo per tassare con tariffa scaduTa
365300980605    1C     *IN07         IFEQ      *ON
365400980605     C   17KTAM2         CHAIN     TNTAM000                           18
365500980605     c**
365600980605     C     *IN17         IFEQ      *OFF
365700980605    2C     *IN18         OREQ      *ON
365800980605     C                   MOVEL     MSG(3)        TASMSG
365900980605     C                   SETON                                        94
366000980605    2C                   ENDIF
366100980605     C**
366200980605     C                   GOTO      ENDTAM
366300980605     C**
366400980605     C                   ELSE
366500980605     C**
366600980605     C** SE NON E' TARIFFA ADATTA PER LA BOLLA (ITALIA INVECE CHE ESTER
366700980605     C**  O VICEVERSA) --> RILEGGO COME SE NON AVESSI LETTO
366800980605     C     TAMFIE        IFNE      WBOEST
366900980605     C     TAMFIE        ANDNE     ' '
367000980605     C                   GOTO      SUTAM
367100980605     C                   ENDIF
367200980605    1C                   ENDIF
367300980605     C**
367400980605     C** TARIFFA DA DECORRERE : SEGNALO SE NON C'E' UNA TARIFFA SCADUTA
367500980605     C**    (di fatto non e' possibile perche' se c'e' una da decorrere
367600980605     C**    e ce n'e' una prima e' la tariffa valida dal momento che
367700980605     C**    non e' possibile che esista una buco di decorrenze scadenze
367800980605     C**    tra i vari progressivi)
367900980605    1C     TASDSP        IFLT      TAMDDT
368000980605     C   17KTAM2         CHAIN     TNTAM000                           18
368100980605     C* NON C'E' TARIFFA SCADUTA O NON TROVATA DI NUOVO
368200980605    2C     *IN17         IFEQ      *OFF
368300980605     C     *IN18         OREQ      *ON
368400980605     C                   MOVEL     MSG(10)       TASMSG
368500980605     C                   SETON                                        94
368600980605    2C                   ENDIF
368700980605     C**
368800941107     C                   GOTO      ENDTAM
368900980605    1C                   END
369000980605     C**
369100980605     C** TARIFFA SCADUTA: DATA SPEDIZIONE MAGGIORE DELLA DATA  SCADENZA
369200980605    1C     TASDSP        IFGT      TAMDST
369300941107     C                   SETON                                        17
369400980605     C                   Z-ADD     TAMKSC        KSC2
369500980605     C                   Z-ADD     TAMCTR        CTR2
369600980605     C                   Z-ADD     TAMPRG        PRG2
369700941107     C                   GOTO      SUTAM
369800980605    1C                   ENDIF
369900020225     C*
370000020225     C** CERCO LA CARTELLO RELATIVA ALLA TARIFFA DEL CLIENTE
370100020227     C*  SE NON HO GIA' UNA TARIFFA DI CARTELLO E SE 94 SPENTO
370200020227     C     ENDTAM        TAG
370300020227     C*
370400020225     C*
370500020227    0C     *IN94         IFEQ      *OFF
370600020227      *
370700020227     C                   SETOFF                                       90
370800020227     C                   Z-ADD     1             T
370900020227     C     TAMKSC        LOOKUP    TAC(T)                                 90
371000020227      *
371100020227      * PER 90 SPENTO VUOL DIRE CHE NON HO TROVATO NESSUNA CARTELLO
371200020227    1C     *IN90         IFEQ      *OFF
371300020319     C***                  CLEARWNET
371400020225     C                   MOVEL     TAMFLO        DSTA01
371500020225     C*
371600020319     C**                   SELEC
371700020225     C* VERIFICO IL TIPO SERVIZIO
371800020319     C**         TAMTSP    WHEQ 'P'
371900020319     C**                   MOVEL'PPT'     WNET    3
372000020225     C* VERIFICO SE TARIFFA ITALIA VERIFICO SE NETWORK DPD
372100020319     C**         TAMFIE    WHEQ 'I'
372200020319     C**         §TADPD    ANDEQ'S'
372300020319     C**                   MOVEL'DPD'     WNET
372400020225     C* VERIFICO SE TARIFFA ESTERA VERIFICO SE NETWORK FEDEX
372500020319     C**         TAMFIE    WHEQ 'E'
372600020319     C**         §TAFED    ANDEQ'S'
372700020319     C**                   MOVEL'FED'     WNET
372800020225     C* VERIFICO SE TARIFFA EUROEXPRESS
372900020319     C**         TAMFIE    WHEQ 'E'
373000020319     C**                   MOVEL'EEX'     WNET
373100020225     C* VERIFICO SE TARIFFA ITALIA
373200020319     C**         TAMFIE    WHEQ 'I'
373300020319     C**                   MOVEL'COR'     WNET
373400020225     C*
373500020319     C**                   ENDSL
373600020225     C* SE NETWORK UGUALE A BLANK CERCO LA TARIFFA DI CARTELLO CORRIERE
373700020319     C**         WNET      IFEQ *BLANK
373800020319     C**                   MOVEL'COR'     WNET
373900020319     C**                   END
374000020225     C* AGGANCIO LA TABELLA DELLE TARIFFE DI CARTELLO
374100020319     C                   EXSR      CERTCA
374200020319     C**                   SETOF                     90
374300020319     C**         WNET      LOKUPNTW,T                    90
374400020225     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
374500020319     C**         *IN90     IFEQ *OFF
374600020319     C**                   MOVELMSG,9     TASMSG
374700020319     C**                   GOTO ENDTA2
374800020319     C**                   ENDIF
374900020319    1C                   ENDIF
375000020227     C*
375100020319     C**                   Z-ADDTAC,T     CARKSC
375200020319     C**                   Z-ADDCTC,T     CARCTR
375300020319     C**                   Z-ADDPRC,T     CARPRG
375400020319     C**                   MOVE DIC,T     DIVCAR
375500020227     C*
375600020227    0C                   ENDIF
375700061220      * se impostato il campo del salvataggio della data spedizione
375800061220     c                   if        Sav_dsp <> *zeros
375900061220     c                   eval      tasdsp = Sav_dsp
376000061220     c                   clear                   Sav_dsp
376100061220     c                   endif
376200020225     C*
376300941107     C*
376400020227     C     ENDTA2        ENDSR
376500020618     C******************************************************
376600020618     C* RICERCA DETTAGLIO SU TUTAD
376700020618     C******************************************************
376800020618     C     RICTAD        BEGSR
376900020618     C     KTAD          SETLL     TITAD000                               09
377000020618     C** 49  TROVATA TARIFFA DEL CAP   DI ARRIVO
377100020618     C   09              SETON                                        49
377200020618     C**
377300020618    1C     *IN09         IFEQ      *OFF
377400020618     C                   MOVEL     *BLANKS       COMNAZ
377500020618     C                   MOVEL     *BLANKS       COMCAP
377600020618     C     KTAD          SETLL     TITAD000                               09
377700020618    2C     *IN09         IFEQ      *OFF
377800020618     C*
377900020618     C                   Z-ADD     1             A
378000020618     C     COMCTS        LOOKUP    CTS(A)                                 05
378100020618     C**
378200020618     C* SE TROVATO COD.TASSAZIONE
378300020618    3C     *IN05         IFEQ      *ON
378400020618     C     A             OCCUR     DSCT
378500020618     C                   MOVEL     §CTRAP        COMCTS
378600020618     C** CARICO REGIONE AL POSTO DEL CODICE DI TASSAZIONE
378700020618     C     KTAD          SETLL     TITAD000                               09
378800020618    3C                   ENDIF
378900020618     C**
379000020618    3C     *IN09         IFEQ      *OFF
379100020618     C*
379200020618     C     WBOEST        IFEQ      'E'
379300020618     C                   MOVEL     'EE'          COMCTS
379400020618     C                   ELSE
379500020618     C                   MOVEL     'IT'          COMCTS
379600020618     C                   END
379700020618     C     KTAD          SETLL     TITAD000                               09
379800020618    3C                   ENDIF
379900020618    2C                   ENDIF
380000020618    1C                   ENDIF
380100020618     C                   ENDSR
380200941107     C/SPACE
380300941112     C******************************************************
380400990721     C** CERCA TARIFFA TITAD DETTAGLIO E CARICA DS
380500941112     C******************************************************
380600941107     C     CARTAR        BEGSR
380700941112C    C                   SETOFF                                       495694
380800941112     C                   MOVEL     TASCAD        COMCAP            9
380900990923     C                   MOVEL     TASNZD        COMNAZ            3
381000941112     C                   MOVEL     TASCTS        COMCTS
381100020618     C                   MOVEL     TASLNP        WLNP
381200020618     C                   EXSR      RICTAD
381300020618     C   09              GOTO      LATAR
381400020618     C**
381500020618     C** SE NON TROVATO TITAD CON LA LINEA DI PARTENZA, CERCO CON LA
381600020618     C**  REGIONE TROVATA DA ORGANIGRANNA DI TASLNP
381700020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
381800020618     C     TASLNP        IFNE      SAVLNP
381900020618     C     TASLNP        CHAIN     AZORG01L                           31
382000020618     C   31              CLEAR                   ORGDE7
382100020618     C                   MOVEL     ORGDE7        OG147
382200020618     C                   MOVEL     TASLNP        SAVLNP
382300020618     C                   ENDIF
382400020618     C**
382500020618     C                   MOVEL     TASCAD        COMCAP            9
382600020618     C                   MOVEL     TASNZD        COMNAZ            3
382700020618     C                   MOVEL     TASCTS        COMCTS
382800020618     C                   MOVEL     §OGRET        WLNP
382900020618     C                   EXSR      RICTAD
383000020618     C   09              GOTO      LATAR
383100020618     C**
383200020618     C** SE NON TROVATO TITAD CON LA REGIONE DI TASLNP, CERCO CON LA
383300020618     C**  LINEA ITALIA 950
383400020618     C                   MOVEL     TASCAD        COMCAP            9
383500020618     C                   MOVEL     TASNZD        COMNAZ            3
383600020618     C                   MOVEL     TASCTS        COMCTS
383700020618     C                   MOVEL     950           WLNP
383800020618     C                   EXSR      RICTAD
383900020618     C   09              GOTO      LATAR
384000941112     C*
384100941112     C*--------------------TARIFFA REVERSIBILE----------------------------------
384200941107     C     TIPBOL        CABNE     'A'           MANTAR
384300941107     C** TARIFFA REVERSIBILE SOLO PER RITORNO(TBL=A)
384400941107     C     TASLNP        CABEQ     68            MANTAR
384500941107     C** NO TARIFFA REVERSIBILE PER 68
384600981027     C     KSCFIL        IFEQ      888
384700981027     C                   Z-ADD     TASLNA        KFIL
384800981027     C                   ELSE
384900981027     C                   MOVEL     KSCFIL        KFIL
385000981027     C                   ENDIF
385100981008     C**
385200941107     C                   SETON                                        56
385300941112     C                   MOVEL     TASCAM        COMCAP
385400990923     C                   MOVEL     TASNZM        COMNAZ
385500941123     C                   MOVEL     TASMCT        COMCTS
385600020618     C                   MOVEL     KFIL          WLNP
385700020618     C                   EXSR      RICTAD
385800020618     C   09              GOTO      LATAR
385900020618     C*
386000020618     C** SE NON TROVATO TITAD CON LA LINEA CLIENTE, CERCO CON LA
386100020618     C**  REGIONE TROVATA DA ORGANIGRANNA DELLA LIENA CLIENTE
386200020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
386300020618     C     KFIL          IFNE      SAVLNP
386400020618     C     KFIL          CHAIN     AZORG01L                           31
386500020618     C   31              CLEAR                   ORGDE7
386600020618     C                   MOVEL     ORGDE7        OG147
386700020618     C                   MOVEL     KFIL          SAVLNP
386800020618     C                   ENDIF
386900020618     C*
387000020618     C                   MOVEL     TASCAM        COMCAP
387100020618     C                   MOVEL     TASNZM        COMNAZ
387200020618     C                   MOVEL     TASMCT        COMCTS
387300020618     C                   MOVEL     §OGRET        WLNP
387400020618     C                   EXSR      RICTAD
387500020618     C   09              GOTO      LATAR
387600020618     C**
387700020618     C** SE NON TROVATO TITAD CON LA REGIONE DI KFIL, CERCO CON LA
387800020618     C**  LINEA ITALIA 950
387900020618     C                   MOVEL     TASCAM        COMCAP
388000020618     C                   MOVEL     TASNZM        COMNAZ
388100020618     C                   MOVEL     TASMCT        COMCTS
388200020618     C                   MOVEL     950           WLNP
388300020618     C                   EXSR      RICTAD
388400020618     C   09              GOTO      LATAR
388500020618     C**
388600020618     C**         KTDREV    SETLLTITAD000                 09
388700020618     C** 09                SETON                     49
388800020618     C** 09                GOTO LATAR
388900020618     C**
389000020618     C**                   MOVEL*BLANKS   COMNAZ
389100020618     C**                   MOVEL*BLANKS   COMCAP
389200020618     C**         KTDREV    SETLLTITAD000                 09
389300020618     C** 09                GOTO LATAR
389400020618     C**
389500020618     C**                   Z-ADD1         A
389600020618     C**         COMCTS    LOKUPCTS,A                    05
389700020618     C**N05                GOTO POIT2
389800020618     C**         A         OCUR DSCT
389900020618     C**                   MOVEL§CTRAP    COMCTS
390000020618     C**         KTDREV    SETLLTITAD000                 09
390100020618     C** 09                GOTO LATAR
390200020618     C**         POIT2     TAG
390300020618     C**         WBOEST    IFEQ 'E'
390400020618     C**                   MOVEL'EE'      COMCTS
390500020618     C**                   ELSE
390600020618     C**                   MOVEL'IT'      COMCTS
390700020618     C**                   END
390800020618     C**         KTDREV    SETLLTITAD000                 09
390900020618     C** 09                GOTO LATAR
391000941107     C** NON TROVATO NEMMENO LA TARIFFA REVERSIBILE
391100941107     C     MANTAR        TAG
391200980527     C     *IN09         IFEQ      *OFF
391300980527     C                   SETON                                        94
391400980527     C                   MOVEL     MSG(8)        TASMSG
391500980527     C                   GOTO      FINE
391600980527     C                   ENDIF
391700941107     C** MANCA TARIFFA
391800941107     C     LATAR         TAG
391900030902     C                   EXSR      CARDST
392000941112     C** CARICA DS DELLA TARIFFA
392100941107     C                   ENDSR
392200941107     C/SPACE
392300941112     C******************************************************
392400941107     C** CARICA DS TARIFFA
392500941112     C******************************************************
392600941107     C     CARDST        BEGSR
392700980514     C     *LIKE         DEFINE    TADRPV        RAPPV
392800980514     C**
392900020218     C                   DO        200           A
393000941112     C     A             OCCUR     TARDS
393100941112     C                   CLEAR                   TARDS
393200941112     C                   END
393300941112     C                   Z-ADD     0             RAPPV
393400980514     C                   CLEAR                   WELAMT
393500941112     C**
393600980514    1C                   DO        *HIVAL        A
393700980514     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
393800980514    2C     WELAMT        IFNE      'S'
393900020618     C** 56      KTDREV    READETITAD000                 15
394000020618     C**N56      KTAD      READETITAD000                 15
394100020618     C     KTAD          READE     TITAD000                               15
394200941107     C   15              GOTO      ENDDST
394300980514    2C                   ENDIF
394400980514     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
394500980514    2C     WELAMT        IFEQ      'S'
394600980515     C                   MOVEL     'E'           WELAMT            1
394700980514    2C                   ENDIF
394800980514     C**
394900941107     C     A             OCCUR     TARDS
395000980514     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
395100980514     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
395200980514     C**
395300941120     C                   Z-ADD     TADLNP        TLNP
395400941107     C                   MOVEL     TADCTS        TCTS
395500990721     C                   MOVEL     TADNAZ        TNAZ
395600941110     C                   MOVEL     TADCAP        TCAP
395700941107     C                   Z-ADD     TADCTR        TCTR
395800941107     C                   Z-ADD     TADRPV        TRPV
395900941122     C                   Z-ADD     TADMIN        TMIN
396000941122     C                   Z-ADD     TADMAX        TMAX
396100040603     C***!!!****         Z-ADD     TADRPV        RAPPV
396200980514     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
396300980514    2C     WELAMT        IFNE      'E'
396400980514     C**
396500980514    3C     TADSGL        IFEQ      MINTAS
396600980514     C                   MOVEL     'E'           WELAMT
396700980514   X3C                   ELSE
396800980514     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
396900980514     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
397000980514     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
397100980514     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
397200980515    4C     TADSGL        IFGT      MINTAS
397300980514     C                   MOVEL     'S'           WELAMT
397400980514     C                   Z-ADD     MINTAS        TSCG
397500980514     C**
397600980522     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
397700980522    5C     TAMATA        IFEQ      0
397800980522     C     TAMATA        ORLE      MINTAS
397900980522     C     TADSGL        ORGT      TAMATA
398000980522     C**
398100980522    6C     UNOCTR        IFEQ      4
398200980514     C     UNOCTR        OREQ      0
398300980514     C     MINTAS        DIV(H)    100           WMTAS            15 5
398400980514     C                   ELSE
398500980514     C                   Z-ADD     MINTAS        WMTAS
398600980522    6C                   ENDIF
398700980514     C* TARIFFA
398800980514     C     WMTAS         MULT(H)   TADITR        TITR
398900980514     C     WMTAS         MULT(H)   TADINO        TINO
399000980522     C                   ELSE
399100980522     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
399200980522     C                   Z-ADD     TADITR        TITR
399300980522     C                   Z-ADD     TADINO        TINO
399400980522    5C                   ENDIF
399500980522    4C                   ENDIF
399600980514    3C                   ENDIF
399700980514    2C                   ENDIF
399800980514     C*
399900980514     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
400000980514    2C     WELAMT        IFNE      'S'
400100980514     C                   Z-ADD     TADSGL        TSCG
400200980514     C                   Z-ADD     TADITR        TITR
400300980514     C                   Z-ADD     TADINO        TINO
400400980514    2C                   ENDIF
400500980514    1C                   ENDDO
400600941107     C     ENDDST        ENDSR
400700941107     C/SPACE
400800941112     C******************************************************
400900941110     C** CARICAMENTO TABELLE SOLO LA 1 VOLTA
401000941112     C******************************************************
401100941110     C     CARTAB        BEGSR
401200980605     C     *LIKE         DEFINE    TAMATA        MINTAS
401300980605     C     *LIKE         DEFINE    TAMATA        MINTAR
401400990923     C     *LIKE         DEFINE    §TADIV        DIVCAR
401500020618     C                   CLEAR                   SAVLNP
401600980605     C**
401700980605     C                   OPEN      TABEL00F
401800980605     C**
401900941110     C                   EXSR      CARQT
402000941110     C** CARICAMENTO IAC
402100941110     C                   EXSR      CARTR
402200941110     C** CARICAMENTO TIPI TARIFFA
402300941110     C                   EXSR      CAR$2
402400941110     C** CARICAMENTO SIGLE VARIE
402500000103     C                   EXSR      CARCC
402600000103     C** CARICAMENTO SIGLE VARIE ESENTI
402700941112     C                   EXSR      CARCV
402800941112     C** CARICAMENTO CAMBI
402900941110     C                   EXSR      CARCT
403000941110     C** CARICAMENTO CODIFICI TASSAZIONE
403100941112     C                   EXSR      CAR5E
403200941112     C** CARICA TIPI SERVIZIO
403300980429     C                   EXSR      CAR1A
403400980429     C** CARICA TIPI INCASSO C/A MITTENTI
403500980603     C                   EXSR      CARTB
403600980603     C** CARICA DS TIPI BOLLA CHE NON PREVEDONO ASSICURAZIONE
403700020319     C                   EXSR      CAREST
403800980605     C** CARICA P.O. ESTERI
403900990917     C                   EXSR      CARDIV
404000990917     C** CARICAMENTO MONETA DI CONTO
404100020320     C                   EXSR      CARPAR
404200020320     C** CARICA TARIFFE DI CARTELLO
404300031120     C                   EXSR      CAR3A
404400031120     C** CARICO VARIE DELLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
404500980605     C                   CLOSE     TABEL00F
404600040505     C**
404700040505     C                   OPEN      TNTBE01L
404800040505     C** caricamento percentuale carburante
404900040505     C                   EXSR      CARPSC
405000080617     C** caricamento filiali addebito lasciato avviso
405100080617     C                   EXSR      CARLAV
405200111102     C** caricamento filiali addebito centro storico da cartello
405300111102     C                   EXSR      CARZTL
405400040505     C**
405500040505     C                   CLOSE     TNTBE01L
405600040505     C**
405700941110     C                   ENDSR
405800941110     C**********************************************************
405900941110     C** CARICAMENTO DS MULTIPLA  CODICI TASSAZIONE
406000941110     C**********************************************************
406100941110     C     CARCT         BEGSR
406200041220     C                   DO        600           A
406300941110     C     A             OCCUR     DSCT
406400941110     C                   CLEAR                   DSCT
406500941110     C                   END
406600941110     C** PULIZIA DS
406700941110     C                   MOVEL     'CT'          COD
406800941110     C     KTAB          SETLL     TABEL                                  07
406900941110     C  N07              SETON                                          H7
407000941110     C   H7              GOTO      FINE
407100941110     C                   Z-ADD     0             K
407200941110     C                   DO        *HIVAL        A
407300941110     C     KTAB          READE     TABEL                                  07
407400941110     C   07              GOTO      ENDCTS
407500941110     C     TBLKEY        IFNE      *BLANKS
407600941110     C                   ADD       1             K
407700941110     C                   MOVEL     TBLKEY        CTS(K)
407800941110     C     K             OCCUR     DSCT
407900941110     C                   MOVEL     TBLUNI        DSCT
408000941110     C                   END
408100941110     C                   END
408200941110     C     ENDCTS        TAG
408300941110     C                   ENDSR
408400941110     C/SPACE
408500941112     C**********************************************************
408600941112     C** CARICAMENTO TIPI SERVIZIO
408700941112     C**********************************************************
408800941112     C     CAR5E         BEGSR
408900941112     C                   DO        10            A
409000941112     C     A             OCCUR     DS5E
409100941112     C                   CLEAR                   DS5E
409200941112     C                   END
409300941112     C** PULIZIA DS
409400941112     C                   MOVEL     '5E'          COD
409500941112     C     KTAB          SETLL     TABEL                                  07
409600941112     C  N07              SETON                                          H7
409700941112     C   H7              GOTO      FINE
409800941112     C                   Z-ADD     0             K
409900941112     C                   DO        *HIVAL        A
410000941112     C     KTAB          READE     TABEL                                  07
410100941112     C   07              GOTO      END5E
410200941112     C     TBLKEY        IFNE      *BLANKS
410300941112     C                   ADD       1             K
410400941112     C                   MOVEL     TBLKEY        TS(K)
410500941112     C     K             OCCUR     DS5E
410600941112     C                   MOVEL     TBLUNI        DS5E
410700941112     C                   END
410800941112     C                   END
410900941112     C     END5E         TAG
411000941112     C                   ENDSR
411100980429     C**********************************************************
411200980429     C** CARICAMENTO TIPI INCASSO C/A
411300980429     C**********************************************************
411400980429     C     CAR1A         BEGSR
411500980430     C                   MOVEA     *ALL'9'       TIC
411600980429     C                   MOVEL     '1A'          COD
411700980429     C                   Z-ADD     0             K
411800980429     C     KTAB          SETLL     TABEL                                  07
411900980429     C     KTAB          READE     TABEL                                  07
412000980430    1C     *IN07         DOWEQ     *OFF
412100980430    2C     TBLKEY        IFNE      *BLANKS
412200980430     C     TBLFLG        ANDEQ     ' '
412300980429     C                   MOVEL     TBLUNI        DS1A
412400980430    3C     §1AFMB        IFEQ      'M'
412500980429     C                   ADD       1             K
412600980429     C                   MOVEL     TBLKEY        TIC(K)
412700980430    3C                   ENDIF
412800980430    2C                   ENDIF
412900980429     C     KTAB          READE     TABEL                                  07
413000980430    1C                   ENDDO
413100980429     C                   ENDSR
413200980603     C**********************************************************
413300980603     C** CARICAMENTO TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
413400980603     C**********************************************************
413500980603     C     CARTB         BEGSR
413600980603     C                   MOVEL     'TB'          COD
413700980603     C                   Z-ADD     0             K
413800980603     C     KTAB          SETLL     TABEL                                  07
413900980603     C     KTAB          READE     TABEL                                  07
414000980603    1C     *IN07         DOWEQ     *OFF
414100980603    2C     TBLKEY        IFNE      *BLANKS
414200980603     C     TBLFLG        ANDEQ     ' '
414300980603     C                   MOVEL     TBLUNI        DSTB
414400980603    3C     §TBFAS        IFEQ      '0'
414500980603     C                   ADD       1             K
414600980603     C                   MOVEL     TBLKEY        TBN(K)
414700980603    3C                   ENDIF
414800980603    2C                   ENDIF
414900980603     C     KTAB          READE     TABEL                                  07
415000980603    1C                   ENDDO
415100980603     C                   ENDSR
415200031120     C**********************************************************
415300031120     C** CARICAMENTO VARIE PER TIPI BOLLE CHE ACCETTANO UNA SINGOLA VARIA
415400031120     C**********************************************************
415500031120     C     CAR3A         BEGSR
415600031120     C                   MOVEL     '3A'          COD
415700031120     C                   Z-ADD     0             K
415800031120     C     KTAB          SETLL     TABEL                                  07
415900031120     C     KTAB          READE     TABEL                                  07
416000031120    1C     *IN07         DOWEQ     *OFF
416100031120    2C     TBLKEY        IFNE      *BLANKS
416200031120     C     TBLFLG        ANDEQ     ' '
416300031120     C                   MOVEL     TBLUNI        DS3A
416400031120    3C     §3ASVA        IFNE      ' '
416500031120     C                   ADD       1             K
416600031120     C                   MOVEL     §3ASVA        T3A(K)
416700031120    3C                   ENDIF
416800031120    2C                   ENDIF
416900031120     C     KTAB          READE     TABEL                                  07
417000031120    1C                   ENDDO
417100031120     C                   ENDSR
417200020319     C**********************************************************
417300020319     C** CARICAMENTO PO ESTERI
417400020319     C**********************************************************
417500020319     C     CAREST        BEGSR
417600020319     C*
417700020319     C                   CLEAR                   K
417800050504     C                   CLEAR                   KK                4 0
417900020319     C**
418000020319     C                   CLEAR                   CC
418100020319     C     *LOVAL        SETLL     AZORG01L
418200020319     C                   READ      AZORG01L                               07
418300020319    1C     *IN07         DOWEQ     *OFF
418400020319     C** FIL O AGENZIA
418500020319    2C     ORGFAG        IFEQ      'F'
418600020319     C     ORGFAG        OREQ      'A'
418700020319     C* NON ANNULLATO
418800020319    3C     ORGFVA        IFEQ      ' '
418900020319    4C                   MOVEL     ORGDE3        OG143
419000050504     c* p.o. esteri senza dpd
419100020319     C     §OGNTW        IFEQ      'EEX'
419200020319     C     §OGNTW        OREQ      'EUP'
419300020404     C*******    §OGNTW    OREQ 'DPD'
419400020319     C     §OGNTW        OREQ      'FED'
419500020319     C                   ADD       1             K
419600020319     C                   MOVEL     ORGFIL        EST(K)
419700020319    4C                   ENDIF
419800050504     c* p.o. estero totale, anche con DPD
419900050504    4C     §OGNTW        IFEQ      'EEX'
420000050504     C     §OGNTW        OREQ      'EUP'
420100050504     C     §OGNTW        OREQ      'DPD'
420200050504     C     §OGNTW        OREQ      'FED'
420300050504     C                   ADD       1             KK
420400050504     C                   MOVEL     ORGFIL        ESTtot(KK)
420500050504    4C                   ENDIF
420600020319    3C                   ENDIF
420700020319    2C                   ENDIF
420800020319     C**
420900020319     C                   READ      AZORG01L                               07
421000020319    1C                   ENDDO
421100020319     C                   ENDSR
421200941110     C********************************************************
421300941110     C** CARICAMENTO SIGLE VARIE
421400941110     C********************************************************
421500941110     C     CAR$2         BEGSR
421600941110     C                   CLEAR                   DS$2
421700941110     C                   MOVEL     '$2'          COD
421800941110     C                   MOVEL     *BLANKS       KEY
421900941110     C                   MOVEL     '1'           KEY
422000941110     C     KTABC         CHAIN     TABEL                              07
422100941110     C   07              SETON                                          H7
422200941110     C   H7              GOTO      FINE
422300941110     C                   MOVEL     TBLUNI        DS$2
422400941110     C                   ENDSR
422500000103     C********************************************************
422600000103     C** CARICAMENTO SIGLE VARIE  ESENTI IVA
422700000103     C********************************************************
422800000103     C     CARCC         BEGSR
422900000103     C                   CLEAR                   DSCC
423000020416     C                   CLEAR                   KK
423100000103     C                   MOVEL     'CC'          COD
423200000103     C                   MOVEL     *BLANKS       KEY
423300000103     C                   MOVEL     'VARIE'       KEY
423400000103     C     KTABC         SETLL     TABEL                                  07
423500020416     C**N07                GOTO ENDCC
423600000103     C                   DO        *HIVAL
423700000103     C     KTAB          READE     TABEL                                  07
423800000103     C   07              LEAVE
423900000103     C                   MOVEL     TBLUNI        DSCC
424000000103     C* CONTROLLO SE VARIA ESENTE CARICO LA SIGLA IN TABELLA
424100000103     C     §CCJEI        IFNE      *BLANKS
424200000103     C                   ADD       1             K
424300000103     C                   MOVE      TBLKEY        VES(K)
424400000103     C                   ENDIF
424500020416     C* CONTROLLO SE DA RESTITUIRE CON SIGAL VARIA E IMPORTO =0
424600020416     C     §CCSV0        IFEQ      'S'
424700050504     C                   ADD       1             KK                4 0
424800020416     C                   MOVE      TBLKEY        SV0(KK)
424900020416     C                   ENDIF
425000000103     C                   ENDDO
425100000103     C                   Z-ADD     0             K
425200020416     C                   Z-ADD     0             KK
425300000103     C     ENDCC         ENDSR
425400941112     C********************************************************
425500941112     C** CARICAMENTO CAMBI
425600941112     C********************************************************
425700941112     C     CARCV         BEGSR
425800941116     C                   Z-ADD     0             K
425900941116     C                   DO        50            A
426000941116     C     A             OCCUR     DSCV
426100941112     C                   CLEAR                   DSCV
426200941116     C                   END
426300941116     C                   MOVEL     *BLANKS       CV
426400941116     C*
426500941112     C                   MOVEL     'CV'          COD
426600941116     C     KTAB          SETLL     TABEL                                  07
426700941116     C  N07              GOTO      ENDCV
426800941116     C                   DO        *HIVAL        A
426900941116     C     KTAB          READE     TABEL                                  07
427000941116     C   07              GOTO      ENDCV
427100941116     C     TBLKEY        IFNE      *BLANKS
427200941116     C                   ADD       1             K
427300941116     C                   MOVEL     TBLKEY        CV(K)
427400941116     C     K             OCCUR     DSCV
427500941116     C                   MOVEL     TBLUNI        DSCV
427600941116     C                   END
427700941116     C                   END
427800941112     C     ENDCV         ENDSR
427900941110     C******************************************************
428000941110     C** CARICAMENTO TIPI TARIFFA
428100941110     C******************************************************
428200941110     C     CARTR         BEGSR
428300030205     C                   DO        50            A
428400941110     C     A             OCCUR     DSTR
428500941110     C                   CLEAR                   DSTR
428600941110     C                   END
428700941110     C**
428800941110     C                   MOVEL     'TR'          COD
428900941112     C                   Z-ADD     0             K
429000941110     C     KTAB          SETLL     TABEL                                  07
429100941110     C  N07              SETON                                          H7
429200941110     C   H7              GOTO      FINE
429300941110     C                   DO        *HIVAL        A
429400941110     C     KTAB          READE     TABEL                                  07
429500941110     C   07              GOTO      TAB1
429600941110     C     TBLKEY        IFNE      *BLANKS
429700941110     C                   ADD       1             K
429800941110     C                   MOVEL     TBLKEY        CTR(K)
429900941110     C     K             OCCUR     DSTR
430000941110     C                   MOVEL     TBLUNI        DSTR
430100941110     C                   END
430200941110     C                   END
430300941110     C     TAB1          ENDSR
430400941110     C**********************************************************
430500020221     C** MEMORIZZA
430600941110     C** TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
430700941110     C**********************************************************
430800941110     C     CARPAR        BEGSR
430900020225      *
431000020225     C     *LIKE         DEFINE    TAMKSC        CARKSC
431100020225     C     *LIKE         DEFINE    TAMCTR        CARCTR
431200020225     C     *LIKE         DEFINE    TAMPRG        CARPRG
431300020221      *
431400020222      * RICHIAMO IL TRUL27R PER OGNI NETWORK PER CARICARMI LE TARIFFE DI CARTELLO IN SCHIERA
431500020221      *
431600020221     C* CORRIERE BARTOLINI
431700020221     C                   CLEAR                   TRUL27
431800020221     C                   MOVEL     'COR'         I27NTW
431900020222     C                   EXSR      SRTRUL
432000020221      * POSTE
432100020222     C                   CLEAR                   TRUL27
432200020222     C                   MOVEL     'PPT'         I27NTW
432300020222     C                   EXSR      SRTRUL
432400020222      * DPD
432500020222     C                   CLEAR                   TRUL27
432600020222     C                   MOVEL     'DPD'         I27NTW
432700020222     C                   EXSR      SRTRUL
432800020222      * FEDEX
432900020222     C                   CLEAR                   TRUL27
433000020222     C                   MOVEL     'FED'         I27NTW
433100020222     C                   EXSR      SRTRUL
433200020222      * EUROEXPRESS
433300020222     C                   CLEAR                   TRUL27
433400020226     C                   MOVE      'L'           I27TLA
433500020222     C                   MOVEL     'EEX'         I27NTW
433600020222     C                   EXSR      SRTRUL
433700020226      *
433800020226     C                   CLEAR                   CARKSC
433900020226     C                   CLEAR                   CARCTR
434000020226     C                   CLEAR                   CARPRG
434100020222      *
434200020222      ** CARICO LE TARIFFE PARTICOLARI
434300941111     C                   EXSR      CAR1P
434400020222      *
434500941110     C                   ENDSR
434600040505     C**********************************************************
434700040505     C** CARICAMENTO SCHIERA SUPPLEMEMNTO CARBURANTE
434800040505     C**********************************************************
434900040505     C     CARPSC        BEGSR
435000040505
435100040507     C                   CLEAR                   KX
435200040505
435300040505     C                   MOVEL     'PSC'         KEYTBE
435400040505     C     KEYTBE        SETLL     TNTBE01L
435500040505     C                   DO        *HIVAL
435600040505     C     KEYTBE        READE     TNTBE01L
435700040505
435800040505     C                   IF        %EOF(TNTBE01L)
435900040505     C                   LEAVE
436000040505     C                   ENDIF
436100040505
436200040505     C                   IF        TBEATB = ' '
436300040507     C                   ADD       1             KX
436400040507     C                   IF        KX <= 999
436500040505     C                   MOVEL     TBEUNI        DPSC
436600040507     C                   Z-ADD     §PSCDAD       DS_DSC
436700040507     C                   Z-ADD     §PSCPER       DS_PSC
436800040507     C                   MOVE      WDSPSC        DSCA(KX)
436900040507     C                   ENDIF
437000040505     C                   ENDIF
437100040505
437200040505     C                   ENDDO
437300040507
437400040507     C                   SORTA     DSCA
437500040505
437600040505     C                   ENDSR
437700080617      *****************************************************************
437800080617      ** CARICAMENTO SCHIERA FILIALI ABILITATE ADDEBITO LASCIATO AVVISO
437900080617      *****************************************************************
438000080617     C     CARLAV        BEGSR
438100080617
438200080617     C                   CLEAR                   KX
438300080617
438400080617     C                   MOVEL     'LAV'         KEYTBE
438500080617     C     KEYTBE        SETLL     TNTBE01L
438600080617     C                   DO        *HIVAL
438700080617     C     KEYTBE        READE     TNTBE01L
438800080617
438900080617     C                   IF        %EOF(TNTBE01L)
439000080617     C                   LEAVE
439100080617     C                   ENDIF
439200080617
439300080617     C                   IF        TBEATB = ' '
439400080617     C                   ADD       1             KX
439500080617     C                   IF        KX <= 999
439600080617     C                   MOVEL     TBEUNI        DLAV
439700080617     C                   EVAL      SFILLAV(KX) = %dec(tbeke1:3:0)
439800080618     C                   EVAL      SDTALAV(KX) = D§LAVTAS
439900080617     C                   ENDIF
440000080617     C                   ENDIF
440100080617
440200080617     C                   ENDDO
440300080617
440400080617
440500080617     C                   ENDSR
440600111102      ******************************************************************
440700111102      ** CARICAMENTO SCHIERA FILIALI ADDEBITO DA CARTELLO CENTRO STORICO
440800111102      ******************************************************************
440900111102     C     CARZTL        BEGSR
441000111102
441100111102     C                   CLEAR                   KX
441200111102
441300111102     C                   MOVEL     'ZTL'         KEYTBE
441400111102     C     KEYTBE        SETLL     TNTBE01L
441500111102     C                   DO        *HIVAL
441600111102     C     KEYTBE        READE     TNTBE01L
441700111102
441800111102     C                   IF        %EOF(TNTBE01L)
441900111102     C                   LEAVE
442000111102     C                   ENDIF
442100111102
442200111102     C                   IF        TBEATB = ' '
442300111102     C                   ADD       1             KX
442400111102     C                   IF        KX <= 999
442500111102     C                   MOVEL     TBEUNI        DZTL
442600111102     C                   EVAL      SFILZTL(KX) = %dec(tbeke1:3:0)
442700111102     C                   EVAL      SDTAZTL(KX) = D§ZTLTAS
442800111102     C                   ENDIF
442900111102     C                   ENDIF
443000111102
443100111102     C                   ENDDO
443200111102
443300111102
443400111102     C                   ENDSR
443500020222     C*************************************************************
443600020222     C** RICHIAMO DEL TRUL27R
443700020222     C*************************************************************
443800020222     C     SRTRUL        BEGSR
443900020222      *
444000020222     C                   Z-ADD     TASDCT        I27DTA
444100021122     c                   Movel     TasCtr        I27Ctb
444200020222     C                   CALL      'TRUL27R'
444300020222     C                   PARM                    TRUL27
444400020222      *
444500020222     C     O27ERR        IFEQ      *BLANKS
444600020319     C     O27PRG        ANDNE     *BLANKS
444700020222     C                   ADD       1             T                 2 0
444800020222     C                   MOVE      I27NTW        NTW(T)
444900020222     C                   Z-ADD     O27KSC        TAC(T)
445000020222     C                   Z-ADD     O27CTR        CTC(T)
445100020226     C                   MOVE      O27PRG        PRC(T)
445200020222      *
445300020222      * AGGANCIO LA TARIFFA PER RECUPERARE LA DIVISA
445400020222      *
445500020226     C                   Z-ADD     O27KSC        CARKSC
445600020226     C                   Z-ADD     O27CTR        CARCTR
445700020226     C                   MOVE      O27PRG        CARPRG
445800020222     C     KTAMC         CHAIN     TNTAM00L                           94
445900020222      *
446000020222     C     *IN94         IFEQ      *OFF
446100020222     C                   MOVEL     TAMFLO        DSTA01
446200020222     C                   MOVE      §TADIV        DIC(T)
446300020226      * SE C'E ERRORE VADO A FINE PROGRAMMA
446400020226     C                   ELSE
446500020226     C                   MOVEL     MSG(9)        TASMSG
446600020226     C                   ENDIF
446700020226      *
446800020222      * SE C'E ERRORE VADO A FINE PROGRAMMA
446900020222     C                   ELSE
447000020222     C                   SETON                                        94
447100020222     C                   MOVEL     MSG(9)        TASMSG
447200020222     C                   ENDIF
447300020222      *
447400020222     C                   ENDSR
447500020225     C*************************************************************
447600020225     C** CERCO LA TARIFFA DI CARTELLO
447700020225     C*************************************************************
447800020225     C     CERTCA        BEGSR
447900020225      *
448000020225      * RICHIAMO IL TRUL27R PASANDOGLI LE CARATTERISTICHE DELLA SPEDIZIONE
448100020225      *
448200020318     C**                   CLEARTRUL27
448300020318     C**                   MOVE 'L'       I27TLA
448400020318     C**                   MOVE TASTSP    I27TSP
448500020318     C**                   MOVE TASLNA    I27LNA
448600020318     C**                   MOVE TASLNP    I27LNP
448700020318     C**                   MOVE TASKSC    I27CLI
448800020318     C**                   MOVE TASDCT    I27DTA
448900020318     C**                   CALL 'TRUL27R'
449000020318     C**                   PARM           TRUL27
449100020225      *
449200020318     C**         O27ERR    IFEQ ' '
449300020318     C**                   Z-ADDO27KSC    KSC2
449400020318     C**                   Z-ADDO27CTR    CTR2
449500020318     C**                   Z-ADDO27CTR    CODCTR
449600020318     C**                   MOVE O27PRG    PRG2
449700020225      * CARICO ANCHE I CAMPI  RELATIVI ALLA TARIFFA DI CARTELLO
449800020318     C**                   Z-ADDO27KSC    CARKSC
449900020318     C**                   Z-ADDO27CTR    CARCTR
450000020318     C**                   MOVE O27PRG    CARPRG
450100020225      *
450200020318     C**                   ENDIF
450300020318     C** DALLE SCHIERE CARICO LA TARIFFA DI CARTELLO
450400020318     C                   SETOFF                                       90
450500020319     C                   Z-ADD     1             T
450600020318     C     O27NTW        LOOKUP    NTW(T)                                 90
450700020318     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
450800020318     C     *IN90         IFEQ      *OFF
450900020318     C                   MOVEL     MSG(9)        TASMSG
451000020319     C                   SETON                                        94
451100020318     C                   GOTO      FINE
451200020318     C                   ENDIF
451300020318     C*
451400020318     C                   Z-ADD     TAC(T)        CARKSC
451500020318     C                   Z-ADD     CTC(T)        CARCTR
451600020318     C                   Z-ADD     PRC(T)        CARPRG
451700020318     C                   MOVE      DIC(T)        DIVCAR
451800020225      *
451900020225     C                   ENDSR
452000941114     C*************************************************************
452100941121     C** CARICAMENTO CODICI E TARIFFE PARTICOLARI DI CARTELLO
452200941114     C*************************************************************
452300941110     C     CAR1P         BEGSR
452400941111     C*
452500020114     C                   DO        50            A
452600941112     C     A             OCCUR     DS1P
452700941111     C                   CLEAR                   DS1P
452800941112     C                   END
452900941111     C                   MOVEL     *BLANKS       CP
453000941110     C**
453100941110     C                   MOVEL     '1P'          COD
453200941111     C                   Z-ADD     0             K                 5 0
453300941110     C     KTAB          SETLL     TABEL                                  07
453400941110     C  N07              SETON                                          H7
453500941110     C   H7              GOTO      FINE
453600941110     C                   DO        *HIVAL        A
453700941110     C     KTAB          READE     TABEL                                  07
453800941110     C   07              GOTO      TABCP
453900941110     C     TBLKEY        IFNE      *BLANKS
454000941115     C                   MOVEL     TBLUNI        CAMPO            26
454100941115     C                   MOVE      CAMPO         WFCP              1
454200941115     C     WFCP          IFEQ      'S'
454300941110     C                   ADD       1             K
454400941110     C                   MOVEL     TBLKEY        CP(K)
454500941112     C     K             OCCUR     DS1P
454600941112     C                   MOVEL     TBLUNI        DS1P
454700941112     C**
454800941110     C                   END
454900941110     C                   END
455000941111     C                   ENDDO
455100020222     C*
455200941110     C     TABCP         ENDSR
455300941110     C****************************************************
455400941110     C** CARICAMENTO DATI VARI TASSAZIONE
455500941110     C****************************************************
455600941110     C     CARQT         BEGSR
455700941110     C                   MOVEL     'QT'          COD
455800941110     C                   MOVEL     *BLANKS       KEY
455900941110     C                   MOVEL     '1'           KEY
456000941110     C     KTABC         CHAIN     TABEL                              07
456100941113     C   07              SETON                                        H7
456200941113     C   H7              GOTO      FINE
456300941110     C  N07              MOVEL     TBLUNI        DSQT1
456400941110     C*
456500941110     C                   ENDSR
456600990917     C****************************************************
456700990922     C** CARICAMENTO MONETE DI CONTO / IMPORTI STANDARD
456800990917     C****************************************************
456900990917     C     CARDIV        BEGSR
457000990917     C*****
457100990922      *  RICERCA DELLA MONETA DI CONTO
457200990917     C                   CLEAR                   DSBS02
457300990917     C                   CLEAR                   DGED
457400990922     C*
457500990917     C                   MOVEL     'C'           T02MOD
457600990917     C                   MOVEL     KNSIF         T02SIF
457700990917     C                   MOVEL     'GED'         T02COD
457800991112     C                   MOVEL     '1'           T02KE1
457900990917      *
458000990917     C                   CALL      'TIBS02R'
458100990917     C                   PARM                    KPJBA
458200990917     C                   PARM                    DSBS02
458300990917      *
458400990917     C     T02ERR        IFEQ      *BLANKS
458500990917     C                   MOVEL     T02UNI        DGED
458600990917     C                   ENDIF
458700990922      * RICERCA DEGLI IMPORTI STANDARD NELLA MONETA DI CONTO
458800990922     C                   CLEAR                   DSBS02
458900990922     C                   CLEAR                   DGEI
459000990922     C                   MOVEL     'L'           T02TLA
459100990922     C                   MOVEL     'C'           T02MOD
459200990922     C                   MOVEL     KNSIF         T02SIF
459300990922     C                   MOVEL     'GEI'         T02COD
459400991112     C                   MOVEL     §GEDCN        T02KE1
459500990922      *
459600990922     C                   CALL      'TIBS02R'
459700990922     C                   PARM                    KPJBA
459800990922     C                   PARM                    DSBS02
459900990922      *
460000990922     C     T02ERR        IFEQ      *BLANKS
460100990922     C                   MOVEL     T02UNI        DGEI
460200990922     C                   ENDIF
460300990922      *
460400990917      *
460500990917     C                   ENDSR
460600011127     C**********************************************************
460700011127     C** RECUPERO LA DATA DEL GIORNO
460800011127     C**********************************************************
460900011127     C     RUDATE        BEGSR
461000011127      *
461100011127     C                   TIME                    W0140            14 0
461200011127     C* UDATE IN GGMMAAAA
461300011127     C                   MOVE      W0140         WDTGIO            8 0
461400011127     C*
461500011127     C                   Z-ADD     WDTGIO        G02DAT
461600011127     C                   MOVEL     *BLANK        G02ERR
461700011127     C                   CALL      'XSRDA8'
461800011127     C                   PARM                    WLBDAT
461900011127     C*
462000011127     C* UDATE IN AAAAMMGG
462100011127     C                   Z-ADD     G02INV        WDTGIO
462200011127      *
462300011127     C                   ENDSR
462400011127     C**********************************************************
462500011127     C** RECUPERO LA DATA TASSAZIONE DA USARE PER QUESTA BOLLA
462600011127     C**********************************************************
462700030131     C**!!!RECDAT        BEGSR
462800011127      *
462900030131     C**!!!              SETOFF                                       31
463000011127      * VERIFICO SE DATA VALORIZZATA E SOPRATTUTTO NUMERICA
463100020826     C**         §ASDAT    IFNE *BLANK
463200020826     C**         §ASDAT    ANDNE*ZEROS
463300011127      *
463400020826     C**                   TESTN          §ASDAT     31
463500011127      * SE 31 ACCESO VUOL DIRE CHE È NUMERICO MA VERIFICO ANCHE L'ULTIMO CARATTERE
463600020826     C**         *IN31     IFEQ *ON
463700020826     C**                   MOVE §ASDAT    W01A    1
463800020826     C**         W01A      COMP '0'                  31  31
463900020826     C**                   ENDIF
464000011127      *
464100020826     C**                   ENDIF
464200011127      * SE 31 ACCESO DATA VALIDA ALTRIMENTI PRENDO LA DATA DEL GIORNO
464300020826     C**         *IN31     IFEQ *ON
464400020826     C**                   MOVE §ASDAT    DATTAS  80
464500020826     C**                   ELSE
464600020826     C**                   Z-ADDWDTGIO    DATTAS
464700020826     C**                   ENDIF
464800020618     C*
464900011127      *
465000030203     C**!!!              ENDSR
465100990916     C****************************************************
465200990916     C** CARICAMENTO IMPORTI PASSATI DALLE DS
465300990916     C****************************************************
465400990916     C     SAVDS         BEGSR
465500990916     C*
465600990922     C     *LIKE         DEFINE    TASPOR        TASINL
465700990916     C*
465800990916     C* SALVO LE SCHIERE DELLE VARIE (SIGLE VARIE+IMPORTI)
465900990916     C                   MOVEA     SV            SVW
466000990916     C                   MOVEA     VA            VAW
466100991012     C                   CLEAR                   TASINL
466200991012     C                   CLEAR                   TASDIF
466300990916     C*
466400990916     C* RECUPERO L'IMPORTO DELL'INOLTRO SE C'E' NELLA SCHIERA DELLE VARIE
466500990916     C                   Z-ADD     1             X                 2 0
466600990923     C     §$2FIN        LOOKUP    SV(X)                                  30
466700990916     C* SE LO TROVO VALORIZZO TASINL
466800990923     C   30              Z-ADD     VA(X)         TASINL
466900991111     C  N30              Z-ADD     0             X
467000990921     C*
467100990921     C* RECUPERO L'IMPORTO DEL DIRITTO FISSO  NELLA SCHIERA DELLE VARIE
467200990921     C                   Z-ADD     1             Y                 2 0
467300990921     C     §$2VRH        LOOKUP    SV(Y)                                  30
467400990921     C* SE LO TROVO VALORIZZO TASINL
467500990923     C   30              Z-ADD     VA(Y)         TASDIF           11 3
467600990916     C*
467700990916     C                   ENDSR
467800990921     C****************************************************
467900990921     C** CONVERSIONE DELLE VARIE IN MONETA DELLA TARIFFA
468000990921     C****************************************************
468100990921     C     CNVTAS        BEGSR
468200990921     C*
468300990921     C* SE DIVISA PASSATA (DELLA BOLLA) E' DIVERSA DALLA DIVISA DELLA
468400990921     C* TARIFFA CONVERTO IN DIVISA TARIFFA
468500990921     C                   Z-ADD     1             A
468600990923    1C                   DO        20            A
468700990923    2C     VA(A)         IFNE      *ZEROS
468800990921     C                   CLEAR                   YEUR
468900011127     C                   MOVEL     DIVINP        YECDVI
469000011127     C                   MOVEL     DIVOUT        YECDVO
469100990921     C                   Z-ADD     VA(A)         YECIMI
469200991014     C   12              MOVE      '3'           YECDCO
469300991014     C  N12              MOVE      '0'           YECDCO
469400990921     C*
469500990921     C                   CALL      'YEURCO'
469600990921     C                   PARM                    YEUR
469700990921     C*
469800990923    3C     YECESI        IFEQ      ' '
469900990921     C                   Z-ADD     YECIMO        VA(A)
470000990923    3C                   END
470100990923    2C                   END
470200990923    1C                   END
470300990921     C* CONVERTO L'IMPORTO DEL PORTO
470400990921     C* SE PORTO DIVERSO DA 0
470500990923    1C     TASPOR        IFGT      0
470600990921     C                   CLEAR                   YEUR
470700011127     C                   MOVEL     DIVINP        YECDVI
470800011127     C                   MOVEL     DIVOUT        YECDVO
470900990921     C                   Z-ADD     TASPOR        YECIMI
471000991014     C   12              MOVE      '3'           YECDCO
471100991014     C  N12              MOVE      '0'           YECDCO
471200990921     C*
471300990921     C                   CALL      'YEURCO'
471400990921     C                   PARM                    YEUR
471500990921     C*
471600990923    2C     YECESI        IFEQ      ' '
471700990921     C                   Z-ADD     YECIMO        TASPOR
471800990921     C                   ELSE
471900990921     C                   MOVEL     MSG(10)       TASMSG
472000990921     C                   GOTO      FINE
472100990923    2C                   END
472200990923    1C                   ENDIF
472300990921     C* SE INOLTRO DIVERSO DA 0
472400990923    1C     TASINL        IFGT      0
472500990921     C                   CLEAR                   YEUR
472600011127     C                   MOVEL     DIVINP        YECDVI
472700011127     C                   MOVEL     DIVOUT        YECDVO
472800990921     C                   Z-ADD     TASINL        YECIMI
472900991014     C   12              MOVE      '3'           YECDCO
473000991014     C  N12              MOVE      '0'           YECDCO
473100990921     C*
473200990921     C                   CALL      'YEURCO'
473300990921     C                   PARM                    YEUR
473400990921     C*
473500990923    2C     YECESI        IFEQ      ' '
473600990921     C                   Z-ADD     YECIMO        TASINL
473700990921     C                   ELSE
473800990921     C                   MOVEL     MSG(10)       TASMSG
473900990921     C                   GOTO      FINE
474000990923    2C                   END
474100990923    1C                   ENDIF
474200990921     C* SE DIRITTO FISSO DIVERSO DA 0
474300990923    1C     TASDIF        IFGT      0
474400990921     C                   CLEAR                   YEUR
474500011127     C                   MOVEL     DIVINP        YECDVI
474600011127     C                   MOVEL     DIVOUT        YECDVO
474700990921     C                   Z-ADD     TASDIF        YECIMI
474800991014     C   12              MOVE      '3'           YECDCO
474900991014     C  N12              MOVE      '0'           YECDCO
475000990921     C*
475100990921     C                   CALL      'YEURCO'
475200990921     C                   PARM                    YEUR
475300990921     C*
475400990923    2C     YECESI        IFEQ      ' '
475500990921     C                   Z-ADD     YECIMO        TASDIF
475600990923    2C                   END
475700990923    1C                   ENDIF
475800990929     C* SE IMPORTO IMPONIBILE DIVERSO DA ZEROS
475900991014     C* LO RICALCOLO PER SICUREZZA
476000990929    1C     TASIMV        IFGT      0
476100991014     C                   CLEAR                   DSLV16
476200991014     C                   MOVEL     'T'           D17FIM
476300991014     C                   Z-ADD     TASPOR        D17POR
476400991014     C                   CALL      'FNLV16R'
476500991014     C                   PARM                    DSLV16
476600991014     C                   PARM                    DTASV
476700991014     C                   Z-ADD     O17IMV        TASIMV
476800990929    1C                   ENDIF
476900990921     C* SE QUANTITA' DA FATTURARE E' DIVERSA DA 0 E LA TARIFFA E' UNA TARIFFA
477000011127     C* A VALORE SPECIFICO DEVO CONVERTIRE IL CAMPO SE RICHIESTO
477100990921     C*
477200011127    1C     TASQFT        IFGT      0
477300011127     C     TAMFVD        ANDEQ     '4'
477400011127     C     CONQTA        ANDNE     'N'
477500011127     C                   CLEAR                   YEUR
477600011127     C                   MOVEL     DIVINP        YECDVI
477700011127     C                   MOVEL     DIVOUT        YECDVO
477800011127     C                   Z-ADD     TASQFT        YECIMI
477900011127     C   12              MOVE      '3'           YECDCO
478000011127     C  N12              MOVE      '0'           YECDCO
478100011127     C*
478200011127     C                   CALL      'YEURCO'
478300011127     C                   PARM                    YEUR
478400011127     C*
478500011127    2C     YECESI        IFEQ      ' '
478600011127     C                   Z-ADD     YECIMO        TASQFT
478700011127    2C                   END
478800011127    1C                   ENDIF
478900011127     C* SE TASIAL VALORIZZATO DEVO CONVERTIRLO SOLO SE RICHIESTO
479000011127     C*
479100011127    1C     TASIAL        IFGT      0
479200011127     C     CONIAL        ANDNE     'N'
479300011127     C                   CLEAR                   YEUR
479400011127     C                   MOVEL     DIVINP        YECDVI
479500011127     C                   MOVEL     DIVOUT        YECDVO
479600011127     C                   Z-ADD     TASIAL        YECIMI
479700011129     C   12              MOVE      '2'           YECDCO
479800011127     C  N12              MOVE      '0'           YECDCO
479900011127     C*
480000011127     C                   CALL      'YEURCO'
480100011127     C                   PARM                    YEUR
480200011127     C*
480300011127    2C     YECESI        IFEQ      ' '
480400011127     C                   Z-ADD     YECIMO        TASIAL
480500011127    2C                   END
480600011127    1C                   ENDIF
480700990921     C*
480800990921     C                   ENDSR
480900990923     C************************************************************
481000990923     C** CONVERSIONE DEL DETTAGLIO TARIFFA PARTICOLARE DI CARTELLO
481100990923     C************************************************************
481200990923     C     CONTPD        BEGSR
481300990923     C*
481400990923     C                   CLEAR                   YEUR
481500990923     C                   MOVEL     DIVCAR        YECDVI
481600990923     C                   MOVEL     §TADIV        YECDVO
481700991014     C   12              MOVE      '3'           YECDCO
481800991014     C  N12              MOVE      '0'           YECDCO
481900990923     C*
482000990923     C* CONTROLLLO IL TIPO TARIFFA SE E' VALORE OPPURE NO PER CONVERTIRE
482100990923     C* LO SCAGLIONE OPPURE NO
482200990923     C*
482300990923    1C     WTPG          IFEQ      'T'
482400990923     C     WTPG          OREQ      '4'
482500990923     C     WTPG          OREQ      'V'
482600990923     C*
482700990923     C                   Z-ADD     TPDSGL        YECIMI
482800990923     C                   CALL      'YEURCO'
482900990923     C                   PARM                    YEUR
483000990923    2C     YECESI        IFEQ      ' '
483100990923     C                   Z-ADD     YECIMO        TPDSGL
483200990923    2C                   END
483300990923     C* NON CONVERTO L'IMPORTO TARIFFA PERCHE' E' UNA PERCENTUALE
483400990923    1C                   ELSE
483500990923     C*
483600990923     C                   Z-ADD     TPDITR        YECIMI
483700990923     C                   CALL      'YEURCO'
483800990923     C                   PARM                    YEUR
483900990923    2C     YECESI        IFEQ      ' '
484000990923     C                   Z-ADD     YECIMO        TPDITR
484100990923    2C                   END
484200990923    1C                   ENDIF
484300990923     C*
484400990923     C* MINIMO
484500990923     C                   Z-ADD     TPDMIN        YECIMI
484600990923     C                   CALL      'YEURCO'
484700990923     C                   PARM                    YEUR
484800990923    1C     YECESI        IFEQ      ' '
484900990923     C                   Z-ADD     YECIMO        TPDMIN
485000990923    1C                   END
485100990923     C* MASSIMO
485200990923     C                   Z-ADD     TPDMAX        YECIMI
485300990923     C                   CALL      'YEURCO'
485400990923     C                   PARM                    YEUR
485500990923    1C     YECESI        IFEQ      ' '
485600990923     C                   Z-ADD     YECIMO        TPDMAX
485700990923    1C                   END
485800990923     C*
485900990923     C                   ENDSR
486000020826     C************************************************************
486100020826     C** CONTROLLO SE APPLICARE PESO NORMALE O VDL
486200020826     C************************************************************
486300020826     C     CTRPES        BEGSR
486400020826     C                   CLEAR                   USAPKF
486500030131     C**!!!              CLEAR                   §ASSPC
486600030131     C                   CLEAR                   TASSPC
486700030429     C                   CLEAR                   TASPKCN
486800020826     C*
486900020826     C* SE NON C'E' IL PESO VDL, PRENDO QUELLO DA FATTURARE
487000030131     C**!!!§ASPKC        IFEQ      0
487100030131     C     TASPKC        IFEQ      0
487200020826     C                   Z-ADD     TASPKF        USAPKF
487300020826     C                   GOTO      ENDPES
487400020826     C                   ELSE
487500020826     C* SE IL VDL NON + MAI DA APPLICARE --> USO IL DA FATTURARE
487600020826     C     §TATAP        IFEQ      'M'
487700020826     C                   Z-ADD     TASPKF        USAPKF
487800020826     C                   GOTO      ENDPES
487900020826     C                   ENDIF
488000020826     C                   ENDIF
488100020826     C* PESO VDL - TARA
488200030131     C**!!!§QTTPC        MULT      §ASNCP        WTARA
488300030131     C     §QTTPC        MULT      TASNCP        WTARA
488400030131     C**!!!§ASPKC        SUB(h)    WTARA         WNTARA
488500030131     C     TASPKC        SUB(h)    WTARA         WNTARA
488600020826     C* SE PESO DA FATTURARE COMPRESO DA VDL E VDL-TARA
488700020826     C*  APPLICO SEMPRE IL PESO DA FATTURARE
488800020826     C*
488900020827     C     TASPKF        IFGE      WNTARA
489000030131     C**!!!TASPKF        ANDLE     §ASPKC
489100030131     C     TASPKF        ANDLE     TASPKC
489200020826     C                   Z-ADD     TASPKF        USAPKF
489300020826     C                   GOTO      ENDPES
489400020826     C                   ENDIF
489500020826     C* SE VDL-TARA > PESO FATTURARE LO APPLICO SIA PER " " CHE PER "S"
489600020827     C     WNTARA        IFGT      TASPKF
489700020827     C                   Z-ADD     WNTARA        USAPKF
489800030131     C**!!!              MOVEL     'S'           §ASSPC
489900030131     C**!!!              z-add     wntara        §ASpkc
490000030131     C                   MOVEL     'S'           TASSPC
490100030429     C                   z-add     wntara        TASpkcN
490200020826     C                   GOTO      ENDPES
490300020826     C                   ENDIF
490400020826     C* SE VDL-TARA < PESO FATTURARE LO APPLICO SOLO SE TOTALE E SE
490500020826     C* "S"
490600020827     C     WNTARA        IFLT      TASPKF
490700030131     C**!!!§ASNCP        ANDEQ     TASNCL
490800030131     C     TASNCP        ANDEQ     TASNCL
490900020826     C     §TATAP        ANDEQ     'S'
491000020827     C                   Z-ADD     WNTARA        USAPKF
491100030131     C**!!!              MOVEL     'S'           §ASSPC
491200030131     C**!!!              z-add     wntara        §ASpkc
491300030131     C                   MOVEL     'S'           TASSPC
491400030429     C                   z-add     wntara        TASpkcN
491500020826     C                   GOTO      ENDPES
491600020826     C                   ENDIF
491700020826     C* SE ANCORA A 0 (IMPOSSIBILE) USO IL PESO DA FATTURARE
491800020826     C     USAPKF        IFEQ      0
491900020826     C                   Z-ADD     TASPKF        USAPKF
492000020826     C                   ENDIF
492100020827     C**
492200020829     c     endpes        tag
492300020829     C                   ENDSR
492400020412**
492500980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo del CONTRASSEGNO  1
492600980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo da ASSICURARE     2
492700980529Non trovata tariffa cliente                                                    3
492800980512Mancano importo da assicurare in bolla e valore merce in tariffa               4
492900980603Non trovato SCAGLIONE in tariffa                                               5
493000980512Manca la quantita' da fatturare                                                6
493100980603Non trovato SCAGLIONE in tariffa particolare
493200980603Non trovato CODICE TASSAZIONE in tariffa                                       8
493300980529Non trovata tariffa di CARTELLO                                                9
493400000125Tariffa cliente con decorrenza maggiore della data spedizione                 10
493500990917Divisa errata                                                                 11
493600000125Tariffa DPD ma bolla Italia                                                   12
493700000125Bolla import/export DPD  ma  tariffa NON DPD                                  13
493800011127Divisa tariffa del cliente non più utilizzabile                               14
493900020318Tariffa FEDEX ma bolla Italia                                                 15
494000020318Bolla import/export FEDEX ma  tariffa NON FEDEX                               16
494100140224Non trovato COD.TASSAZIONE in tar.PARTICOLARE                                 17
494200030527Non presente tassazione per addebito insoluti                                 18
494300080221Tariffa valida ma imponibile minore di 0,000                                  19
