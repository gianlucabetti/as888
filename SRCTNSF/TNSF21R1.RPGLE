000100160314     H DECEDIT('0,') DATEDIT(*DMY.) Option(*nodebugio)
000200160314     H* TNSF21R1 *---------------------------------------------------*
000300160314     H*   - TASSAZIONE SINGOLA SPEDIZIONE CONFRONTO FATTURAZIONE -
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
001800160129     fTNTAM05L  if   e           k disk    rename(TNTAM000:TNTAM05)
001900160211     f                                     prefix(k_)
002000990721     FTITAD00L  IF   E           K DISK
002100990721     FTITPT01L  IF   E           K DISK
002200990721     FTITPD02L  IF   E           K DISK
002300060906     FTIPMG01L  IF   E           K DISK
002400140227     FTIDPB01L  IF   E           K DISK
002500020319     FAZORG01L  IF   E           K DISK
002600030205     D CTR             S              1    DIM(50)                              CODICI TARIFFA
002700020114     D CP              S              1    DIM(50)                              TARIFFE PARTICOLARI
002800941112     D TS              S              1    DIM(10)                              TIPI SERVIZIO
002900041220     D CTS             S              2    DIM(600)                             CODICI TASSAZIONE
003000990916     D SVW             S              1    DIM(20)                              SALVO SIGLE VARIE
003100990916     D VAW             S             11  3 DIM(20)                              SALVO IMPORTI VARIE
003200941116     D CV              S              3    DIM(50)                              CAMBI
003300980429     D TIC             S              2    DIM(20)                              TIPI INCASSO
003400031120     D* VARIE LEGATE ALLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
003500031120     D T3A             S              1    DIM(20)                              VARIE X BOLLE SING.V
003600980605     D* TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
003700980605     D TBN             S              2    DIM(20)                              TIPI BOLLA NO ASSIC
003800050504     D* P.O. ESTERI senza DPD
003900020319     D EST             S              3  0 DIM(300)                             P.O. ESTERI
004000050504     D* P.O. ESTERI tutti con DPD
004100050504     D ESTtot          S              3  0 DIM(300)                             P.O. ESTERI
004200000103     D* SIGLE VARIE ESENTI IVA
004300000103     D VES             S              1    DIM(20)                              SIGLE VARIE ESENTI D
004400020416     D* SIGLE VARIE DA RESTITUIRE CON IMPORTO =0
004500020416     D SV0             S              1    DIM(20)                              SIGLE VARIE IMP0 I D
004600020221     D* TARIFFE DI CARTELLO PER OGNI NETWORK
004700020221     D NTW             S              3    DIM(10)                              NETWORK
004800020221     D TAC             S              7  0 DIM(10)                              TARIFFA CARTELLO
004900020221     D CTC             S              3  0 DIM(10)                              CODICE TARIFFA CARTE
005000020221     D PRC             S              3  0 DIM(10)                              PROGRESSIVO TARIFFA
005100020222     D DIC             S              3    DIM(10)                              DIVISA      TARIFFA
005101160314     D TIP             S              2    DIM(10)                              Tipologia FEDEX
005200040506      * date decorrenza + supplemento carburante
005300040506     D DSCA            S             13  0 DIM(999) DESCEND                     DTA DEC. + % carbur
005400080617      * filiali abilitate addebito lasciato avviso  + date inizio tassazione
005500080617     D SFilLav         S              3  0 DIM(999)                             Filiale
005600080617     D SDtaLav         S              8  0 DIM(999)                             Data tassazione
005700111102      * filiali abilitate addebito centro storico   + date inizio tassazione
005800111102     D SFilZtl         S              3  0 DIM(999)                             Filiale
005900111102     D SDtaZtl         S              8  0 DIM(999)                             Data tassazione
006000040506
006100040505
006200980605     D* MESSAGGI DI ERRORE DEI MANCA TARIFFA
006300080220     D MSG             S             78    DIM(19) CTDATA PERRCD(1)             MSG DI ERRORE
006400040505
006500040505
006600941112     D WLBDAT          DS                  INZ
006700941112     D  G02DAT                 1      8  0
006800941112     D  G02INV                 9     16  0
006900941112     D  G02ERR                17     17
007000941115     D  G02TGI                18     22  0
007100941107     D ACDS            DS                  OCCURS(100)
007200941112     D  ASGL                   1     13  3
007300941112     D  AITR                  14     24  3
007400990921     D  AMIN                  25     35  3
007500990921     D  AMAX                  36     46  3
007600990921     D  AAIN                  47     47
007700990721     D** VALORI TITPT DELLE TARIFFE PARTICOLARI
007800020218     D TARDS           DS                  OCCURS(200)
007900941107     D  TLNP                   1      3  0
008000941113     D  TCTS                   4      5
008100990721     D  TNAZ                   6      8
008200990721     D  TCAP                   9     17
008300990721     D  TCTR                  18     20  0
008400990721     D  TSCG                  21     33  3
008500990721     D  TITR                  34     44  3
008600990721     D  TINO                  45     55  3
008700990721     D  TRPV                  56     58  1
008800990921     D  TMIN                  59     69  3
008900990921     D  TMAX                  70     80  3
009000040507      * DS SUPPLEMMENTO CARBURANTE
009100040507     D WDSPSC          DS
009200040507     D  DS_DSC                 1      8  0
009300040507     D  DS_PSC                 9     13  2
009400941111     D** CODICI DI TASSAZIONE
009500041220     D DSCT          E DS                  OCCURS(600) INZ
009600941112     D*  TARIFFE PARTICOLARI
009700020114     D DS1P          E DS                  OCCURS(50) INZ
009800000103     D** SIGLE VARIE
009900000103     D DS$2          E DS
010000000103     D** VARIA ESENTE
010100000103     D DSCC          E DS
010200031120     D** 3A TIPIO BOLLE
010300031120     D DS3A          E DS
010400941112     D** CAMBI
010500980504     D DSCV          E DS                  OCCURS(50)
010600980504     D** DS DEL CAMPO TAMFLO DI TNTAM
010700980504     D DSTA01        E DS
010800020618     D OG147         E DS
010900980504     D** DS PASSATE DAI PGM RICHIAMANTI
011000990916     D DTAS          E DS
011100050928
011200060922     D DTAS01        E DS
011300140227     D** DS DEL CAMPO TPTFLO DI TITPT
011400140227     D DTPT01        E DS
011500060922
011600130124     D DTASDTA       E DS
011700130124
011800990916     D DTASV         E DS
011900941112     D  SV                     1     20
012000941112     D                                     DIM(20)                              SIGLE VARIE
012100990916     D  VA                    21    140P 3
012200990916     D                                     DIM(20)                              IMPORTI VARIE
012300990916     D  ES                   141    160
012400011127     D*
012500941110     D TAMDS         E DS                  EXTNAME(TNTAM00F) INZ
012600130123      *
012700130123     d Status         sds
012800130123     d  SDSprm           *parms
012900130123      *
013000000000     D**  FILE TARIFFE  - MASTER
013100000000     D KPJBA         E DS
013200000125     D OG143         E DS                  INZ
013300000125     D DSQT1         E DS                  INZ
013400941107     D* DATI VARI TASSAZIONE 1
013500980429     D DS1A          E DS                  INZ
013600980429     D** TIPI INCASSO C/A
013700030205     D DSTR          E DS                  OCCURS(50) INZ
013800941107     D** TIPI TARIFFA
013900980603     D DSTB          E DS
014000980603     D** TIPI BOLLA
014100980429     D DS5E          E DS                  OCCURS(10) INZ
014200990917     D** MONETA DI CONTO/MONETA CORRENTE
014300990917     D DGED          E DS
014400990922     D** IMPORTI STANDARD NELLE MONETE DI CONTO
014500990922     D DGEI          E DS
014600040505     D** PERCENTUALE SUPPLEMENTO CARBURANTE
014700040505     D DPSC          E DS
014800080617     D** FILIALI ADDEBITO LASCIATO AVVISO
014900080617     D DLAV          E DS
015000111102     D** FILIALI ADDEBITO CENTRO STORICO
015100111102     D DZTL          E DS
015200990917     D** CALL PGM RICERCA TABELLA
015300990917     D DSBS02        E DS                  EXTNAME(TIBS02DS)
015400990917     D** CAMBIO DIVISA
015500990917     D YEUR          E DS                  EXTNAME(YEURCODS)
015600990922     D** CALL PGM CALCOLO TOTALE IMPONIBILE
015700990922     D DSLV16        E DS                  EXTNAME(FNLV16DS)
015800020221     D** CALL PGM RECUPERO TARIFFE DI CARTELLO
015900160314     D TRUL27        E DS                  EXTNAME(TRUL27DS1)
015901160314     D** CALL PGM PER RECUPERO TARIFFA DI CARTELLO
015902160314     D TRULC7        E DS                  EXTNAME(TRULC7DS)
016000980526     D PARAMT          DS
016100980526     D  PARCA                256    256
016200101014
016300101014     d TrulISTds     e ds
016400040505
016500040505      * -------------- variabili ------------------------------------
016600040505
016700040507     D Keytbe          S              3                                         chiave TBE
016800080617     D indx            S              3  0                                      Indice schiera
016900040507     D KX              S              3  0                                      Indice schiera
017000040507     D IMP_SCA         S             15  3                                      Impon.sup.carb.
017100040507     D PER_SCA         S              5  2                                      Perc.sup.carb.
017200060414     d applicarevers   s              1
017201160314     D NtwTam          S              3                                         Network di TNTAM
017202160314     D TipTam          S              2                                         tipo tariffa Fedex
017300060906
017400060906     d Sca_pb          s              3  0                                      scag.prez.base gasol
017500060906     d Sca_spe         s              3  0                                      scag.prez.gasol sped
017600060906     d Sca_sca         s              3  0                                      scaglioni scattati
017700080909     d Pmg_sgl         s             13  3                                      prezzo medio gasolio
017800140227     d Variafuel_min   s             11  3                                      Fuel minimo
017900040505
018000061219     d Sav_dsp         s              8  0                                      Salv. dta spedizione
018100080220
018200080220     d Wrksv           s             20                                         salvataggio sigle va
018300090922     d WvariaMag       s              1                                         salvataggio sigle va
018400090924     d WTSP            s                   like(taditr)
018500090924     d IMPON           s                   like(taditr)
018600090924     d WimpoMag        s                   like(taditr)
018700130123     d Ritass          s              1
018800160114     d MancaTariffa    s               n   inz(*off)
018900160114
019000990916     C****************************************************************
019100990916     C*  RIEPILOGO INDICATORI
019200990916     C***************************************************************
019300000125     C* 05    - DI COMODO
019400990916     C* 07    - LETTURA GENERICO
019500990916     C* 08    - OFF MANCA TARIFFA CLIENTE
019600990916     C* 09    - LETTURA TITAD
019700000103     C* 10    - LOKUP
019800990923     C* 12    - MONETA CHE ACCETTA IMPORTI CON DECIMALI
019900990916     C* 15    - GENERICO
020000990916     C* 17    - TARIFFA SCADUTA
020100990916     C* 18    - TARIFFA NON TROVATA
020200990916     C* 28    - LETTURA TARIFFE PARTICOLARI
020300990916     C* 49    - TROVATA TARIFFA DEL CAP DI ARRIVO
020400001009     C* 56    - TROVATA TARIFFA  (TARIFFA REVERSIBILE)
020500001009     C* 57    - ASSEGNATO      (NON + IN USO *!!*)
020600020225     C* 90    - MANCA TARIFFA DI CARTELLO
020700990916     C* 94    - MANCA TARIFFA
020800990916     C* 96    - SCHIERE VARIE
020900990916     C*****************************************************************
021000000000     C     *ENTRY        PLIST
021100941111     C                   PARM                    KPJBA
021200990922     C                   PARM                    DTAS
021300990922     C                   PARM                    DTASV
021400130123     C                   PARM                    DTASDTA
021500060922
021600130124      * se ricevo anche il 4° parametro vuol dire che vengo richiamato dal confronto fatturazione
021700130123      * o ritassazione
021800130124      * nel 4° parametro  DTASDTA verranno messe in caso di ritassazione 2 date.
021900130123      * la 1° per tassare ISTAT
022000130123      * la 2° per tassare il FUEL
022100160114      * c'è una terza data che è la data fattura
022200160114      * e una quarta data che è la data di riferimento per il caricamento della tariffa di cartello
022300160114      *   e cioè la data di riferimento impostata nel lancio del confronto fatturazione
022400130124     C                   IF        SDSPRM > 3
022500130123     C                   EVAL      RITASS = 'S'
022600130123     c                   else
022700130123     c                   eval      ritass = ' '
022800130123     C                   ENDIF
022900160114
023000160114      /free
023100160114       //?Se sono in ritassazione
023200160114         IF  ritass = 'S';
023300160114       //?Se la data per il caricamento della cartello è vuota imposto la TASDCT
023400160114           IF  TASdtcar = 0;
023500160114             TASdtcar = TASdct;
023600160114           ENDIF;
023700160114         ENDIF;
023800160114      /end-free
023900130123
024000060922     C                   MOVEL     TASFLO        DTAS01
024100110624
024200151021      * pulisco i campi della DTAS che sono di solo output
024300151021     c                   clear                   tastap
024400151021     c                   clear                   tasepd
024500151209     c                   clear                   tasprg
024600060922
024700920225     C                   Z-ADD     1             CODUT             1 0
024800000000     C*---------------------------------------------------------------*
024900020226     C*
025000020226     C** ACCESSO TITPT01L
025100941107     C     KISOT         KLIST
025200941107     C                   KFLD                    TAMKSC
025300941107     C                   KFLD                    TAMCTR
025400941107     C                   KFLD                    TAMPRG
025500990722     C                   KFLD                    FISO              2
025600020226     C     KTPTC         KLIST
025700020226     C                   KFLD                    CARKSC
025800020226     C                   KFLD                    CARCTR
025900020226     C                   KFLD                    CARPRG
026000020226     C                   KFLD                    FISO
026100930419     C*
026200020222     C** ACCESSO TFTPD02L
026300941107     C     KISOD         KLIST
026400941107     C                   KFLD                    TPTKSC
026500941107     C                   KFLD                    TPTCTR
026600941107     C                   KFLD                    TPTPRG
026700941107     C                   KFLD                    TPTFTC
026800941107     C                   KFLD                    ISOCTS
026900020412     C** ACCESSO TFTPD02L
027000020412     C     KISOD2        KLIST
027100020412     C                   KFLD                    TPTKSC
027200020412     C                   KFLD                    TPTCTR
027300020412     C                   KFLD                    TPTPRG
027400020412     C                   KFLD                    TPTFTC
027500020222     C** ACCESSO TABEL X CODICE
027600941107     C     KTAB          KLIST
027700941107     C                   KFLD                    CODUT
027800941107     C                   KFLD                    COD               2
027900020222     C** ACCESSO TABEL X CODICE + CHIAVE
028000941107     C     KTABC         KLIST
028100941107     C                   KFLD                    CODUT
028200941107     C                   KFLD                    COD
028300941107     C                   KFLD                    KEY               8
028400020222     C** ACCESSO TNTAM  TARIFFA DI CARTELLO
028500020222     C     KTAMC         KLIST
028600020226     C                   KFLD                    CARKSC
028700020226     C                   KFLD                    CARCTR
028800020226     C                   KFLD                    CARPRG
028900020222     C** ACCESSO TNTAM TARIFFE SCADUTA
029000941107     C     KTAM2         KLIST
029100941107     C                   KFLD                    KSC2
029200941111     C                   KFLD                    CTR2
029300941107     C                   KFLD                    PRG2
029400020222     C** ACCESSO TNTAM X CLIENTE
029500941107     C     KTAM          KLIST
029600941107     C                   KFLD                    TMCLI
029700941107     C                   KFLD                    TMCTR
029800020222     C** ACCESO TITAD
029900941107     C     KTAD          KLIST
030000941107     C                   KFLD                    TDCLI
030100941107     C                   KFLD                    TDPRG
030200020618     C                   KFLD                    WLNP
030300941112     C                   KFLD                    COMCTS
030400990722     C                   KFLD                    COMNAZ
030500990722     C                   KFLD                    COMCAP
030600941107     C                   KFLD                    TDCTR
030700020222     C** ACCESSO TITAD PER TARIFFA REVERSIBILE
030800941112     C     KTDREV        KLIST
030900941112     C                   KFLD                    TDCLI
031000941112     C                   KFLD                    TDPRG
031100981027     C                   KFLD                    KFIL
031200941112     C                   KFLD                    COMCTS
031300990722     C                   KFLD                    COMNAZ
031400941112     C                   KFLD                    COMCAP
031500941112     C                   KFLD                    TDCTR
031600020826     C     *LIKE         DEFINE    TASPKF        USAPKF
031700020827     C     *LIKE         DEFINE    §qttpc        WTARA
031800020827     C     *LIKE         DEFINE    TASPKF        WNTARA
031900020826     C     *LIKE         DEFINE    KSCFIL        KFIL
032000020618     C     *LIKE         DEFINE    TASLNP        SAVLNP
032100020618     C     *LIKE         DEFINE    TASLNP        WLNP
032200020618     C     *LIKE         DEFINE    TASCTS        COMCTS
032300941107     C***********     -------------       ***********
032400930419     C                   MOVEL     ' '           RT
032500930419     C*
032600941125     C* TASTLA = ' ' ELABORO E CHIUDO PGM  CON RETRN
032700941125     C* TASTLA = 'L' ELABORO E CHIUDO PGM  CON LR
032800941125     C* TASTLA = 'C' NO ELAB.  CHIUDO PGM  CON LR PER CHIUSURA FILE
032900941116     C     TASTLA        IFEQ      ' '
033000941116     C     TASTLA        OREQ      'L'
033100941107     C*
033200941116     C     TASTLA        IFEQ      ' '
033300930419     C                   MOVEL     'R'           RT                1
033400930419     C                   END
033500980505     C* C A M P I   D I   O U T P U T
033600980505     C                   CLEAR                   TASERR
033700980505     C                   CLEAR                   TASMSG
033800980505     C                   CLEAR                   TASIAL
033900980526     C                   MOVEL     KPJBU         PARAMT
034000030902      * clearizzo indicatiori x manca tariffa e inoltro
034100030902     C                   SETOFF                                       9456
034200030211
034300151209      * verifico data spedizione con data attivazione varia t Diritto di chiamata prenotazione
034400151209      * ritiro e packing list
034500151209     c                   if        Tasdsp < 20160101
034600151209     c                   clear                   TASPRT
034700151209     c                   clear                   TASSPL
034800151209     c                   Endif
034900151209
035000030211     c                   Clear                   wFiso
035100990930     C** CONTROLLO LA DIVISA CHE MI VIENE PASSATA SE DIVERSA DA BLANK VERIFICO
035200990930     C** CHE SIA CORRETTA
035300990930     C     TASDIV        IFNE      *BLANKS
035400990930     C                   CLEAR                   DSLV16
035500990930     C                   MOVEL     'D'           D17FIM
035600990930     C                   MOVE      TASDIV        D17DIV
035700990930     C                   CALL      'FNLV16R'
035800990930     C                   PARM                    DSLV16
035900990930     C                   PARM                    DTASV
036000990930     C* CONTROLLO IL CODICE DI RITORNO
036100990930     C     O17ERR        IFNE      *BLANKS
036200990930     C                   MOVEL     'E'           TASERR
036300990930     C                   MOVEL     O17MSG        TASMSG
036400990930     C                   SETON                                        94
036500990930     C                   GOTO      FINE
036600990930     C                   ENDIF
036700990930     C                   ENDIF
036800941125     C*
036900980605     C** CARICA TABELLE SOLO LA 1° VOLTA
037000941125     C     CARTBL        IFEQ      *BLANK
037100941125     C                   MOVE      '1'           CARTBL            1
037200941125     C                   EXSR      CARTAB
037300011127      * RECUPERO UDATE SOLO LA PRIMA VOLTA
037400011127     C                   EXSR      RUDATE
037500020320     C     TASMSG        IFNE      *BLANKS
037600020320     C                   SETON                                        94
037700020320     C                   GOTO      FINE
037800020320     C                   ENDIF
037900941125     C                   END
038000991012     C*
038100991012     C** SALVO GLI IMPORTI CHE MI VENGONO PASSATI
038200991012     C                   EXSR      SAVDS
038300941125     C*
038400941110     C                   SETOFF                                       94
038500160114     c                   eval      MancaTariffa = *off
038600030925      *
038700030925      * se richiamato per il solo calcolo della varia D  VADO A FINE
038800030925     C                   IF        TASSVA = 'D'
038900030925     C                   GOTO      FINE
039000030925     C                   ENDIF
039100030925      *+
039200941124     C                   MOVEL     TASTBL        TIPBOL            1
039300941124     C                   MOVEL     TASKSC        KSCFIL            3 0
039400941124     C                   MOVE      TASKSC        KSCCOD            4 0
039500080617      * da giugno 2008 viene riattivato per alcune filiali l'addebito del lasciato avviso
039600080617      * se tasric diverso da blank .. quindi la spedizione ha un evento di lasciato avviso
039700080617      * verifico se la filiale del cliente è presente nella tabella LAV oppure nella tabella
039800080617      * LAV esiste la filiale 999
039900080617    1c                   If        TasRic <> ' '
040000080617     c                   clear                   indx
040100080617     c                   eval      indx = %lookup(kscfil:sfillav)
040200080617      * se indice =  a zero cerco con la filiale 999
040300080617    2c                   if        indx = *zeros
040400080617     c                   eval      indx = %lookup(999:sfillav)
040500080617    2c                   endif
040600080617      * pulisco il flag del calcolo dell'addebito del lasciato avviso perchè filiale cliente
040700080617      * non abilitato all'addebito
040800080617    2c                   If        indx = *zeros
040900080617     c                   clear                   tasric
041000080617      * altrimenti verifico la data attivazione tassazione
041100080617   x2c                   else
041200080617      * se data spedizione inferiore all'attivazione pulisco il flag
041300080618    3c                   if        tasdsp < sdtalav(indx)
041400080617     c                   clear                   tasric
041500080617    3c                   endif
041600080617    2c                   endif
041700080617    1c                   endif
041800080617     c
041900980429     C* VEDO SE TIPO INCASSO E' INTESTATO AL MITTENTE
042000980430     C                   CLEAR                   TASBM
042100061016      * se il primo carattere del campo TASTIC è uguale a blank
042200061016      * ci troviamo a tassare una spedizione in sede in quanto
042300061016      * in TNCSB (file contrassegni ) la combinazione di tpa (tipo assegno)
042400061018      * e tpi (tipo intestazione) o fus (tipo intestazione in bolla)
042500061018      * potrebbe non esistere in tabella  1A tipo incasso c/assegno
042600061016      * a questo punto prendo solo il tipo intestazione assegno che
042700061016      * è uguale a  M per mittente  e "B" o " " per Vettore
042800061016      * In Tasbm passo solo se è uguale a M
042900061016
043000061016     C                   If        %subst(tastic:1:1) = ' '
043100061016     C                   If        %subst(tastic:2:1) = 'M'
043200061016     c                   eval      TASBM = 'M'
043300061016     c                   endif
043400061016
043500061016     c                   else
043600980429     C     TASTIC        LOOKUP    TIC                                    96
043700980429     C   96              MOVEL     'M'           TASBM             1
043800061016     c                   endif
043900980605     C** TASSAZIONE
044000941107     C                   EXSR      TARIFF
044100941110     C     FINE          TAG
044200980605     C** 94 ON - MANCA TARIFFA
044300990922     C     *IN94         IFEQ      '1'
044400990929     C* E NON C'E' TOTALE IMPONIBILE  DO ERRORE
044500990929     C     TASIMV        IFEQ      *ZEROS
044600990922     C                   MOVEL     'E'           TASERR            1
044700990929     C                   ENDIF
044800160114      * se ho il flag MancaTariffa impostato, vuol dire che è un puro manca tariffa
044900160114      * msg 3 o msg 10, quindi imposto 'M' nel campo TASERR
045000160114     c                   IF        MancaTariffa
045100160114     c                   eval      TASerr = 'M'
045200160114     c                   ENDIF
045300990922     C* REIMPOSTO LA DS COME PRIMA SENZA CONVERSIONI IN MONETA
045400990922     C*    MI SEMBRA INUTILE ADESSO EVENTUALMENTE LO FACCIO DOPO
045500990922     C*
045600990922     C                   ELSE
045700990929     C* CALCOLO IL TOTALE IMPONIBILE SE E' UGUALE A ZEROS
045800990929     C*
045900990929     C     TASIMV        IFEQ      *ZEROS
046000990923     C                   CLEAR                   DSLV16
046100990923     C                   MOVEL     'T'           D17FIM
046200990923     C                   Z-ADD     TASPOR        D17POR
046300990923     C                   CALL      'FNLV16R'
046400990923     C                   PARM                    DSLV16
046500990923     C                   PARM                    DTASV
046600001205     C*
046700001205     C* SE SPEDIZIONI CON DATA MAGGIORE O UGUALE AL 01012001 CALCOLO L'ADDIZIONALE DI GESTIONE
046800001211     C* SUL TOTALE IMPONIBILE E DOPO CALCOLO SUL TOTALE IMPONIBILE + ADDIZ. GESTIONE L'ISTAT
046900001205     C*
047000001221     C     TASDSP        IFGE      20010101
047100020416     C* ESEGUO SOLO SE NON HO RICHIAMATO LA TASSAZIONE PER UNA VARIA
047200031120     C*  SOLA SE VARIA NON PRESENTE NELLA TABELLA T3A (TABELLA DELLE VARIE DELLE BOLLE CHE
047300031121     C* ACCETTANO UNA SINGOLA  VARIA ESEMPIO "O" "Y" "*".....)
047400031120     C*    TASSVA        ANDEQ     ' '
047500031120      *
047600031120     C                   IF        TASSVA <> ' '
047700031120     C                   SETOFF                                       11
047800031120     C     TASSVA        LOOKUP    T3A(1)                                 11
047900031120     C                   ENDIF
048000040113      *
048100040113      *
048200040113      * SE VARIA UGUALE A BLANK                                             OPPURE
048300040113      * VARIA PRESENTE NELLA TABELLA T3A                                    OPPURE
048400040113      * TASSAZIONE 2° BOLLA     TASSVA = 'G' E TASCVAG = 'S'                OPPURE
048500040113      * VARIA UGUALE A "Z"                                                  OPPURE
048600130124      * CALCOLO A.G.+ ISTAT                                                 OPPURE
048700130124      * CALCOLO ISTAT
048800130124      *
048900040113     C                   IF        TASSVA = ' ' OR
049000040113     C                             %FOUND       OR
049100040113     C                             (TASSVA = 'G' AND TASCVAG = 'S') OR
049200130124     C                             TASSVA = 'Z' OR
049300130124     C                             TASSVA = 'L'
049400001205     C                   EXSR      ADGIST
049500001205     C                   ENDIF
049600031120      *
049700031120     C                   ENDIF
049800001205     C*
049900990923     C                   Z-ADD     O17IMV        TASIMV
050000001205     C*
050100990929     C                   ENDIF
050200030924      *
050300030924      * DIRITTO DI FATTURAZIONE lo calcolo solo alla fine della TASSAZIONE senza aggiunte dell'addi-
050400030924      *                         zionale di gestione
050500030924      *                         se totale imponibile maggiore di zero
050600030924     C                   IF        TASIMV > 0 AND
050700030924     C                             (TASSVA = §$2VRD OR TASSVDF= §$2VRD)
050800030924      *
050900030924     C                   EXSR      DIRFAT
051000030924      *
051100030924     C                   ENDIF
051200990922     C* MUOVO LA DIVISA DELLA TARIFFA
051300020416     C*  SE NON PASSATA LA VARIA O SE PASSATA E IMPORTO >0
051400020416     C*
051500020416     C     TASSVA        IFEQ      *BLANKS
051600020416     C     O17IMV        ORGT      0
051700990929     C                   MOVE      §TADIV        TASDIV
051800020416     C                   ENDIF
051900020416     C*
052000020416     C* SE SI TRATTA DI UNA VARIA DA PASSARE ANCHE A 0 CONTROLLO
052100020416     C     TASSVA        IFNE      *BLANKS
052200020416     C     O17IMV        ANDEQ     0
052300020416     C     TASSVA        LOOKUP    SV0                                    05
052400020416     C     *IN05         IFEQ      *ON
052500020416     C                   MOVEL     TASSVA        SV(1)
052600020416     C                   ENDIF
052700020416     C                   ENDIF
052800080221
052900080221      * VERIFICO SE IMPONIBILE A ZERO SENZA SIGLE DI VARIE PERCHÈ QUESTO TIPO DI CONTROLLO
053000080221      * VIENE ESEGUITO DAI PGM CHIAMANTI
053100080221     c                   clear                   wrksv
053200080221     c                   movea     sv            wrksv
053300080221     C                   IF        TASIMV = 0 AND wrkSV = *BLANKS
053400080221     C                   MOVEL     'E'           TASERR            1
053500080221     C                   MOVEL     MSG(19)       TASMSG
053600080221     C                   SETON                                        94
053700080221     C                   GOTO      FINE
053800080221     C                   ENDIF
053900990922     C*
054000990922     C                   ENDIF
054100011127     C* CONTROLLO LA DIVISA IN BASE ALLA DATA TASSAZIONE SE DA CONVERTIRE OPPURE NO
054200011127     C* SE STO TASSANDO CON DATA > DEL 2001 UNA BOLLA CON DATA < DEL 2002 E LA DIVISA NON E'
054300011127     C* UGUALE ALLA MONETA DI CONTO AZIENDALE CONVERTO TUTTI GLI IMPORTI ANCHE LA QTA DA
054400011127     C* FATTURARE NELLA DIVISA DI CONTO
054500011127     C*
054600020826     C     TASDIV        IFNE      §GEDCN
054700011127     C     TASDSP        ANDLT     20020101
054800011127     C* CONVERTO
054900011127     C                   MOVE      TASDIV        DIVINP
055000011127     C                   MOVE      §GEDCN        DIVOUT
055100011127     C                   MOVE      'S'           CONQTA
055200011127     C                   MOVE      'S'           CONIAL
055300011127     C*
055400011127     C* RECUPERO LE CARATTERISTICHE DELLA MONETA DI CONTO
055500011127     C                   SETOFF                                       12
055600011127     C                   Z-ADD     1             K
055700011127     C     §GEDCN        LOOKUP    CV(K)                                  10
055800011127     C     *IN10         IFEQ      *ON
055900011127     C     K             OCCUR     DSCV
056000011127     C     §CVFDC        IFEQ      'S'
056100011127     C* DIVISA CHE ACCETTA DECIMALI
056200011127     C                   SETON                                        12
056300011127     C                   ENDIF
056400011127     C                   ENDIF
056500011127     C*
056600011127     C                   EXSR      CNVTAS
056700011127     C* VALORIZZO  LA DIVISA
056800011127     C                   MOVEL     §GEDCN        TASDIV
056900011127     C*
057000011127     C                   ENDIF
057100990922     C*
057200941107     C                   END
057300941107     C*
057400941107     C     RT            IFEQ      'R'
057500941107     C                   RETURN
057600941107     C                   ELSE
057700020319     C* CHIUDO PGM TRUL27R
057800020319     C                   CLEAR                   TRUL27
057900020319     C                   MOVE      'C'           I27TLA
058000160314     C                   CALL      'TRUL27R1'
058100020319     C                   PARM                    TRUL27
058101160314     C* CHIUDO PGM TRULC7R
058102160314     C                   CLEAR                   TRULC7
058103160314     C                   MOVE      'C'           IC7TLA
058104160314     C                   CALL      'TRULC7R'
058105160314     C                   PARM                    TRULC7
058200941107     C                   SETON                                        LR
058300941107     C                   END
058400941107     C************       -----------       *************
058500941107     C**
058600941112     C******************************************************
058700941110     C**  CALCOLA TASSAZIONE BOLLA
058800941112     C******************************************************
058900941110     C     TARIFF        BEGSR
059000980605     C                   MOVEL     'I'           WBOEST
059100000125     C                   CLEAR                   WBODPD
059200020318     C                   CLEAR                   WBOFED
059201160315     C                   CLEAR                   tasfie
059202160314     C                   CLEAR                   TRUL27
059203160314     C                   CLEAR                   TRULC7
059300991014     C                   SETOFF                                       12
063500001201     C*
063600001201     C* VERIFICO SE CODICE APPARTIENE ALLA SDI
063700001201     C*
063800001201     C                   CLEAR                   KSDIT
063900001201     C     KSCFIL        CHAIN     AZORG01L                           07
064000001201     C     *IN07         IFEQ      *OFF
064100001201     C                   MOVEL     ORGDIT        KSDIT             3
064200001201     C                   ENDIF
064300980605     C**
064400980605     C**
064500020222     C* PER 8888 E 9999 --> CERCO LA TARIFFA DI CARTELLO RELATIVA AL TIPO DI SPEDIZIONE
064600980605    1C     KSCCOD        IFEQ      8888
064700980527     C     KSCCOD        OREQ      9999
064701160314      * CERCO LA TARIFFA DI CARTELLO in base al network della spedizione
064702160314     C** PER VERIFICARE SE SONO BOLLA DPD FEDEX O ESTERA CHIAMO TRUL27
064703160314     C**  SENZA LA DATA
064704160314     C                   CLEAR                   TRUL27
064705160314     C                   MOVEL     TASTSP        I27TSP
064706160314     C                   MOVEL     TASLNA        I27LNA
064707160314     C                   MOVEL     TASLNP        I27LNP
064708160314     C                   MOVEL     TASKSC        I27CLI
064709160314     c                   Movel     Tasctr        I27Ctb
064710160314     C                   CALL      'TRUL27R1'
064711160314     C                   PARM                    TRUL27
064712160314      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
064713160314     C     O27ERR        IFEQ      *BLANKS
064714160314     C     O27FIE        IFEQ      'E'
064715160314     C                   MOVEL     'E'           WBOEST            1
064716160314     C                   ENDIF
064717160314     C*
064718160314     C     O27FIE        IFEQ      'D'
064719160314     C                   MOVEL     'S'           WBODPD            1
064720160314     C                   MOVEL     'I'           WBOEST            1
064721160314     C                   ENDIF
064722160314     C*
064723160314     C     O27FIE        IFEQ      'X'
064724160314     C     O27NTW        ANDEQ     'DPD'
064725160314     C                   MOVEL     'S'           WBODPD
064726160314     C                   MOVEL     'I'           WBOEST
064727160314     C                   ENDIF
064728160314     C*
064729160314     C     O27FIE        IFEQ      'X'
064730160314     C     O27NTW        ANDEQ     'EEX'
064731160314     C                   MOVEL     'E'           WBOEST
064732160314     C                   ENDIF
064733160314     C*
064734160314     C     O27FIE        IFEQ      'F'
064735160314     C     O27FIE        orEQ      'M'
064736160314     C     O27FIE        orEQ      'N'
064737160314     C                   MOVEL     'S'           WBOFED            1
064738160314     C                   MOVEL     'E'           WBOEST            1
064739160314     C                   ENDIF
064740160314     C                   ELSE
064741160314     C                   SETON                                        94
064742160314     C                   MOVEL     O27MSG        TASMSG
064743160314     C                   GOTO      FINE
064744160314     C                   ENDIF
064745160314     C**
064746160314     C* LO PASSO IN OUTPUT (ha senso solo per fedex)
064747160314     c                   MOVEL     O27FIE        TASFIE
064748160314      * Per tutte le tipologie di bolla FedEx imposto sempre 'F' xchè il
064749160314      * TNSF07 testa 'F' x definire che è una bolla FedEx (descrizione varie)
064750160314     c                   If        O27Fie = 'M' or O27Fie = 'N'
064751160314     c                   Movel     'F'           TAsFie
064752160314     c                   EndIf
064753160314     c
064754160314     c                   eval      NtwTam = O27ntw
064755160314     c                   clear                   tiptam
064756160314      * imposto il tipo tariffa fedex
064757160314     c                   if        NtwTam = 'FED' and
064758160314     c                             o27fie = 'N'
064759160314     c                   eval      TIPTam = 'FD'
064760160314     c                   endif
064761160314
064762160314     c                   if        NtwTam = 'FED' and
064763160314     c                             (o27fie = 'M' or o27fie = 'F')
064764160314     c                   eval      TIPTam = 'FM'
064765160314     c                   endif
064766160314
064800020225      * CERCO LA TARIFFA DI CARTELLO
064900020225     C                   EXSR      CERTCA
065000020319     C                   Z-ADD     CARKSC        KSC2
065100020319     C                   Z-ADD     CARCTR        CTR2
065200020319     C                   Z-ADD     CARCTR        CODCTR
065300020319     C                   Z-ADD     CARPRG        PRG2
065400020225      *
065500980605     C     KTAM2         CHAIN     TNTAM00L                           94
065600020222      *
065700980605     C** NON TROVATA (IMPOSSIBILE!!!)
065800980605    2C     *IN94         IFEQ      *ON
065900980605     C                   MOVEL     MSG(10)       TASMSG
066000980605     C                   GOTO      FINE
066100980605    2C                   ENDIF
066200980605     C* KSCCOD NON LO IMPOSTO PERCHE'PER I 9999 VOGLIO CHE RIMANGA TALE
066300980605     C**
066400980605   X1C                   ELSE
066500941112     C** ACCESSO TNTAM
066600980605     C                   Z-ADD     TASCTR        CODCTR            3 0
066700980605     C                   Z-ADD     TASKSC        CODCLI            7 0
066800980605     C**
066900941110     C                   Z-ADD     CODCLI        TMCLI             7 0
067000980515     C                   Z-ADD     CODCTR        TMCTR             3 0
067100941110     C                   EXSR      CERTAM
067200020227     C** SE 90 /94 SONO SPENTI NON HO TROVATO LA TARIFFA DI CARTELLO RELATIVA ALLA TARIFFA
067300020319     C** DEL CLIENTE   (QUESTO CASO L'HO ELIMINATO)
067400020227     C     *IN90         IFEQ      *OFF
067500020227     C     *IN94         ANDEQ     *OFF
067600020225     C*** NON SO COSA FARE ANCHE PERCHE' E' IMPOSSIBILE MA VADO A FINE LO STESSO
067700020225     C                   SETON                                        94
067800020225     C                   GOTO      FINE
067900020225     C                   ENDIF
068000980604     C** NON C'E' TARIFFA CLIENTE
068100980605    2C     *IN94         IFEQ      *ON
068200980604     C**
068300980605     C* SE NON HO CHIAMATO PER UNA VARIA  -- > ERRORE
068400020416     C* NON DO ERRORE SOLO PER LA VARIA "R"
068500020416    3C     TASSVA        IFNE      'R'
068600980604     C                   GOTO      FINE
068700980605   X3C                   ELSE
068800980604     C**
068900020416     C** SE HO CHIAMATO PER LA VARIA 'R' --> IMPOSTO LA TAR.CARTELLO
069000980605     C* IMPOSTO ANCHE KSCCOD perche' voglio che me lo tratti come
069100980605     C*  cliente generico da tariffa di cartello per l'assicurazione
069200980605     c*  Non ho problemi per i 9999 èerche' avendoli gia' impostati
069300980605     c*  la prima volta che fa  certam trova la tariffa (di cartello)
069400980605     C                   MOVE      8888          KSCCOD
069401160315     C                   CLEAR                   TRUL27
069402160315     C                   MOVEL     TASTSP        I27TSP
069403160315     C                   MOVEL     TASLNA        I27LNA
069404160315     C                   MOVEL     TASLNP        I27LNP
069405160315     C                   MOVEL     TASKSC        I27CLI
069406160315     c                   Movel     Tasctr        I27Ctb
069407160315     C                   CALL      'TRUL27R1'
069408160315     C                   PARM                    TRUL27
069409160315      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
069410160315     C     O27ERR        IFEQ      *BLANKS
069411160315     C     O27FIE        IFEQ      'E'
069412160315     C                   MOVEL     'E'           WBOEST            1
069413160315     C                   ENDIF
069414160315     C*
069415160315     C     O27FIE        IFEQ      'D'
069416160315     C                   MOVEL     'S'           WBODPD            1
069417160315     C                   MOVEL     'I'           WBOEST            1
069418160315     C                   ENDIF
069419160315     C*
069420160315     C     O27FIE        IFEQ      'X'
069421160315     C     O27NTW        ANDEQ     'DPD'
069422160315     C                   MOVEL     'S'           WBODPD
069423160315     C                   MOVEL     'I'           WBOEST
069424160315     C                   ENDIF
069425160315     C*
069426160315     C     O27FIE        IFEQ      'X'
069427160315     C     O27NTW        ANDEQ     'EEX'
069428160315     C                   MOVEL     'E'           WBOEST
069429160315     C                   ENDIF
069430160315     C*
069431160315     C     O27FIE        IFEQ      'F'
069432160315     C     O27FIE        orEQ      'M'
069433160315     C     O27FIE        orEQ      'N'
069434160315     C                   MOVEL     'S'           WBOFED            1
069435160315     C                   MOVEL     'E'           WBOEST            1
069436160315     C                   ENDIF
069437160315     C                   ELSE
069438160315     C                   SETON                                        94
069439160315     C                   MOVEL     O27MSG        TASMSG
069440160315     C                   GOTO      FINE
069441160315     C                   ENDIF
069442160315     C**
069443160315     C* LO PASSO IN OUTPUT
069444160315     c                   MOVEL     O27FIE        TASFIE
069445160315      * Per tutte le tipologie di bolla FedEx imposto sempre 'F' xchè il
069446160315      * TNSF02 testa 'F' x definire che è una bolla FedEx (descrizione varie)
069447160315     c                   If        O27Fie = 'M' or O27Fie = 'N'
069448160315     c                   Movel     'F'           TAsFie
069449160315     c                   EndIf
069450160315     c
069451160315     c                   eval      NtwTam = O27ntw
069452160315     c                   clear                   tiptam
069453160315      * imposto il tipo tariffa fedex
069454160315     c                   if        NtwTam = 'FED' and
069455160315     c                             o27fie = 'N'
069456160315     c                   eval      TIPTam = 'FD'
069457160315     c                   endif
069458160315
069459160315     c                   if        NtwTam = 'FED' and
069460160315     c                             (o27fie = 'M' or o27fie = 'F')
069461160315     c                   eval      TIPTam = 'FM'
069462160315     c                   endif
069463160315
069500020225     C* CERCO LA CARTELLO IN BASE ALLE CARATTERISTICHE DELLA SPEDIZIONE EW DEL CLIENTE
069600020225     C                   EXSR      CERTCA
069700020319     C                   Z-ADD     CARKSC        KSC2
069800020319     C                   Z-ADD     CARCTR        CTR2
069900020319     C                   Z-ADD     CARCTR        CODCTR
070000020319     C                   Z-ADD     CARPRG        PRG2
070100020225     C*
070200980605     C     KTAM2         CHAIN     TNTAM00L                           94
070300980605     C** NON TROVATA (IMPOSSIBILE!!!)
070400980605    4C     *IN94         IFEQ      *ON
070500980605     C                   MOVEL     MSG(10)       TASMSG
070600980605     C                   GOTO      FINE
070700980605    4C                   ENDIF
070800020225     C* MI CARICO LA DIVISA DELLA CARTELLO
070900020225     C                   MOVEL     TAMFLO        DSTA01
071000020225     C                   MOVEL     §TADIV        DIVCAR
071100020225     C*
071200980605    3C                   ENDIF
071300980605    2C                   ENDIF
071400980605    1C                   ENDIF
071500980604     C**
071600941110     C                   Z-ADD     TAMKSC        TDCLI             7 0
071700941110     C                   Z-ADD     TAMPRG        TDPRG             3 0
071800941110     C                   Z-ADD     TAMCTR        TDCTR             3 0
071900980609     C* CAMPO DI OUTPUT
072000000125     C                   MOVEL     TAMBAP        TASBAP
072100980609     C                   MOVEL     TAMFLO        DSTA01
072200000125     C*
072300000125     C* SE BOLLA DPD E TARIFFA DI CARTELLO --> ERRORE SE NON RICHIAMO
072400020319     C*  PER UNA VARIA E SE LA CARTELLO NON E' UNA CARTELLO DPD
072500000125     C     KSCCOD        IFEQ      8888
072600000125     C     KSCCOD        OREQ      9999
072700020416     C     TASSVA        IFNE      'R'
072800000125     C     WBODPD        ANDEQ     'S'
072900020225      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
073000020225     C     §TADPD        ANDNE     'S'
073100000125     C                   SETON                                        94
073200000125     C                   MOVEL     MSG(13)       TASMSG
073300000125     C                   GOTO      FINE
073400000125     C                   ENDIF
073500020416     C     TASSVA        IFNE      'R'
073600020319     C     WBOFED        ANDEQ     'S'
073700020319      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
073800020319     C     §TAFED        ANDNE     'S'
073900020319     C                   SETON                                        94
074000020319     C                   MOVEL     MSG(16)       TASMSG
074100020319     C                   GOTO      FINE
074200020319     C                   ENDIF
074300000125     C                   ENDIF
074400000125     C*
074500000125     C* SE TARIFFA O CLIENTE TARIFFA DPD E BOLLA NON LO E'--> MANCA TAR
074600000125     C     §TADPD        IFEQ      'S'
074700000125     C     WBODPD        IFEQ      ' '
074800000125     C                   SETON                                        94
074900000125     C                   MOVEL     MSG(12)       TASMSG
075000000125     C                   GOTO      FINE
075100000125     C                   ENDIF
075200000125     C                   ELSE
075300000125     C     WBODPD        IFEQ      'S'
075400000125     C                   SETON                                        94
075500000125     C                   MOVEL     MSG(13)       TASMSG
075600000125     C                   GOTO      FINE
075700000125     C                   ENDIF
075800000125     C                   ENDIF
075900020319     C* SE TARIFFA O CLIENTE TARIFFA FED E BOLLA NON LO E'--> MANCA TAR
076000020319     C     §TAFED        IFEQ      'S'
076100020319     C     WBOFED        IFEQ      ' '
076200020319     C                   SETON                                        94
076300020319     C                   MOVEL     MSG(15)       TASMSG
076400020319     C                   GOTO      FINE
076500020319     C                   ENDIF
076600020319     C                   ELSE
076700020319     C     WBOFED        IFEQ      'S'
076800020319     C                   SETON                                        94
076900020319     C                   MOVEL     MSG(16)       TASMSG
077000020319     C                   GOTO      FINE
077100020319     C                   ENDIF
077200020319     C                   ENDIF
077300000125     C**
077400990927     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
077500990927     C     §TADIV        IFEQ      *BLANK
077600990927     C                   MOVE      'ITL'         §TADIV
077700990927     C                   ENDIF
077800011127      * VERIFICO IN BASE ALLA DATA TASSAZIONE E DATA SPEDIZIONE SE LA DIVISA DELLA TARIFFA
077900011127      * E' CORRETTA
078000011127      *
078100011127      * SE STO TASSANDO con data > del 2001 un bolla con data > del 2001 E LA DIVISA NON E'
078200011127      * UGUALE ALLA MONETA DI CONTO AZIENDALE
078300020826     C     §TADIV        IFNE      §GEDCN
078400011127     C     TASDSP        ANDGT     20011231
078500011127      * MANCA TARIFFA
078600011127     C                   SETON                                        94
078700011127     C                   MOVEL     MSG(14)       TASMSG
078800011127     C                   GOTO      FINE
078900011127     C                   ENDIF
079000991014     C*
079100991014     C* RECUPERO LE CARATTERISTICHE DELLA DIVISA
079200991014     C                   Z-ADD     1             K
079300991014     C     §TADIV        LOOKUP    CV(K)                                  10
079400991014     C     *IN10         IFEQ      *ON
079500991014     C     K             OCCUR     DSCV
079600991014     C     §CVFDC        IFEQ      'S'
079700991014     C* DIVISA CHE ACCETTA DECIMALI
079800991014     C                   SETON                                        12
079900991014     C                   ENDIF
080000991014     C                   ENDIF
080100990921     C** RICERCA TNTAM
080200990921     C** VERIFICO SE LA MONETA PASSATA E'= A QUELLA  DELLA TARIFFA
080300990921     C     TASDIV        IFNE      §TADIV
080400990928     C* E LA TARIFFA PASSAT NON E' UGUALE A BLANK
080500990928     C     TASDIV        ANDNE     *BLANKS
080600990921     C** CONVERTO GLI IMPORTI NELLA MONETA DELLA TARIFFA CLIENTE
080700990921     C*  ANCHE LE VARIE
080800011127     C*
080900011127     C* PASSO LE MONETE PER LA CONVERSIONE ED IN PIÙ IL FLAG SE DEVO
081000011127     C* CONVERTIRE LA QUANTITA' DA FATTURARE OPPURE NO
081100011127     C* NEL CASO IN CUI CONVERTO NELLA MONETA DELLA TARIFFA
081200011127     C* NON E' NECESSARIO CONVERTIRE LA QTA DA FATTURARE E
081300011127     C* L'IMPORTO D'ASSICURARE CALCOLATO IN TASSAZIONE
081400011127     C                   MOVE      TASDIV        DIVINP            3
081500011127     C                   MOVE      §TADIV        DIVOUT            3
081600011127     C                   MOVE      'N'           CONQTA            1
081700011127     C                   MOVE      'N'           CONIAL            1
081800990921     C                   EXSR      CNVTAS
081900990921     C*
082000990921     C                   ENDIF
082100990929     C*------------------------------------------------------
082200990929     C* SE IMPONIBILE VALORIZZATO VADO A FINE
082300990929     C     TASIMV        IFNE      *ZEROS
082400000103     C* E NON E' STATA RICHIESTA NESSUNA VARIA
082500000103     C     TASSVA        ANDEQ     ' '
082600990929     C                   GOTO      FINE
082700990929     C                   ENDIF
082800980506     C*------------------------------------------------------
082900020826     C* VERIFICO SE APPLICARE IL PESO VDL O MENO
083000020826     C                   EXSR      CTRPES
083100020826     C*------------------------------------------------------
083200980506     C** PORTO O INOLTRO PRETASSATO
083300990721     C**  OPPURE DEVO CALCOLARE UNA VARIA SPECIFICA --> NON CERCO TITAD
083400941110     C     TASPOR        IFGT      0
083500980506     C     TASINL        ORGT      0
083600980506     C     TASSVA        ORNE      ' '
083700941110     C                   GOTO      TASSAT
083800941110     C                   END
083900941110     C*
084000941112     C                   MOVEL     CODCTR        UNOCTR            1 0
084100980506     C* QUANTITA' DA FATTURARE
084200950119     C     UNOCTR        IFEQ      5
084300950210     C     TASQFT        COMP      0                                      94
084400980512     C   94              MOVEL     MSG(6)        TASMSG
084500980512     C   94              GOTO      FINE
084600950119     C                   END
084700950119     C** TARIFFA A QUANTITA' - MANCA Q.TA' DA FATTURARE
084800950119     C     UNOCTR        IFEQ      4
084900950119     C     TAMFVD        IFEQ      *BLANKS
085000950119     C     TAMFVD        OREQ      '2'
085100961007     C     TAMFVD        OREQ      '4'
085200950210     C     TASQFT        COMP      0                                      94
085300980512     C   94              MOVEL     MSG(6)        TASMSG
085400950119     C   94              GOTO      FINE
085500950120     C                   GOTO      POIQTF
085600950119     C                   END
085700950119     C** TARIFFA A VALORE FLAG 2 E BLANK
085800950119     C     TAMFVD        IFEQ      '1'
085900950210     C     TASQFT        CABGT     0             POIQTF
086000950119     C                   END
086100950119     C*
086200950210     C                   Z-ADD     TASIAS        TASQFT
086300950119     C                   Z-ADD     1             K
086400950119     C     TASVAS        LOOKUP    CV(K)                                  05
086500980527     C     *IN05         IFEQ      *OFF
086600980527     C                   SETON                                        94
086700980527     C                   MOVEL     MSG(2)        TASMSG
086800980527     C                   GOTO      FINE
086900980527     C                   ENDIF
087000990917     C**
087100990917     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA
087200990917     C                   CLEAR                   YEUR
087300990917     C                   MOVEL     TASVAS        YECDVI
087400990917     C                   MOVEL     §TADIV        YECDVO
087500990917     C                   Z-ADD     TASQFT        YECIMI
087600991014     C   12              MOVE      '3'           YECDCO
087700991014     C  N12              MOVE      '0'           YECDCO
087800990917     C*
087900990917     C                   CALL      'YEURCO'
088000990917     C                   PARM                    YEUR
088100990917     C*
088200990917     C     YECESI        IFEQ      ' '
088300990917     C                   Z-ADD     YECIMO        TASQFT
088400990917     C                   END
088500950119     C* CAMBIO
088600950210     C     TASQFT        IFEQ      0
088700950119     C                   EXSR      CALVAS
088800950210     C                   Z-ADD     IMPVAS        TASQFT
088900950119     C                   END
089000950210     C     TASQFT        COMP      0                                      94
089100980527     C   94              MOVEL     MSG(6)        TASMSG
089200950119     C   94              GOTO      FINE
089300950119     C** TARIFFA A VALORE FLAG 1 E 3 - NON ESISTE VALORE ASSICURATIVO
089400950119     C                   END
089500950119     C     POIQTF        TAG
089600980518     C* MI SERVE MINTAS PER IL CARICAMENTO DELLO SCAGLIONE DEL
089700980518     C*  MINIMO TASSABILE
089800980518     C                   Z-ADD     1             A
089900980518     C                   MOVEL     UNOCTR        WTPG              1
090000980518     C     WTPG          LOOKUP    CTR(A)                                 05
090100980518     C   05A             OCCUR     DSTR
090200980518     C   05              Z-ADD     §TRATA        MINTAS
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
093600001009     C  N56              MOVEL     TASFIN        FLGINO            1
093700001009     C   56              MOVEL     TASFAP        FLGINO
093800970522     C**
093900980506     C** SE DEVO CALCOLARE UNA VARIA VADO SUBITO DA QUELLA
094000980506     C     TASSVA        IFNE      '  '
094100990916     C     *LIKE         DEFINE    TASQFT        WQFT
094200980506     C** VARIA 'G' --> CONTRASSEGNO
094300980506     C     TASSVA        IFEQ      §$2VRG
094400980506     C                   GOTO      VARIAG
094500980506     C                   ENDIF
094600980525     C** VARIA 'R' --> ASSICURAZIONE PER CONTO
094700980526     C     TASSVA        IFEQ      §$2VRR
094800980525     C                   GOTO      VARIAR
094900980525     C                   ENDIF
095000020412     C** VARIA 'O' --> RICERCA DOCUMENTI
095100020412     C     TASSVA        IFEQ      §$2VRO
095200020412     C                   GOTO      VARIAO
095300020412     C                   ENDIF
095400020516     C** VARIA 'Y' --> RITIRI ANNULLATI
095500020516     C     TASSVA        IFEQ      §$2VRY
095600020516     C                   GOTO      VARIAY
095700020516     C                   ENDIF
095800020516     C** VARIA '*' --> DIROTTAMENTI
095900020523     C     TASSVA        IFEQ      §$2AST
096000020523     C                   GOTO      VARAST
096100020516     C                   ENDIF
096200030403     C** VARIA 'W' --> RECAPITO C/ASSEGNI
096300030403     C     TASSVA        IFEQ      §$2VRW
096400030403     C                   GOTO      VARVRW
096500030403     C                   ENDIF
096600030527     C** VARIA 'M' --> ADDEBITO INSOLUTI INTERESSI DI MORA
096700030527     C     TASSVA        IFEQ      §$2VRM
096800030527     C                   GOTO      VARVRM
096900030527     C                   ENDIF
097000030626     C** VARIA 'T' --> ORM COMMISSIONATO
097100030626     C     TASSVA        IFEQ      §$2VRT
097200030626     C                   GOTO      VARVRT
097300030626     C                   ENDIF
097400040910     C** VARIA 'a' --> P.O.D IMMAGE
097500040910     C     TASSVA        IFEQ      §$2POD
097600040910     C                   GOTO      VARPOD
097700040910     C                   ENDIF
097800030604     C** VARIA 'D' --> DIRITTO DI FATTURAZIONE
097900030604     C     TASSVA        IFEQ      §$2VRD
098000030922     C                   GOTO      FINE
098100030604     C                   ENDIF
098200130124     C** VARIA 'Z' --> ADDIZIONALE DI GESTIONE + ISTAT
098300130124     C     TASSVA        IFEQ      §$2VRZ
098400130124     C                   GOTO      FINE
098500130124     C                   ENDIF
098600130124     C** VARIA 'L' --> SOLO ISTAT
098700130124     C     TASSVA        IFEQ      §$2VRV
098800040113     C                   GOTO      FINE
098900040113     C                   ENDIF
099000130124     C** VARIA 'f' --> SOLO FUEL
099100130124     C     TASSVA        IFEQ      §$2VFS
099200130124     C                   GOTO      VARVFS
099300130124     C                   ENDIF
099400980506     C                   ENDIF
099500970522     C**
099600941123     C** SE IL PORTO O L'INOLTRO SONO PRETASSATI
099700941110     C** DEBBO ESCLUDERE LA FASE DI CALCOLO TARIFFA
099800090924     C     TASPOR        CABGT     0             DOPOIN
099900090924     C     TASINL        CABGT     0             DOPOIN
100000980508     C*****
100100980508     C** CALCOLO    P O R T O     ED    I N O L T R O
100200980508     C*****
100300980506     C**
100400020218     C                   DO        200           A
100500941110     C     A             OCCUR     TARDS
100600941110     C     TSCG          IFGE      PESTAS
100700941110     C                   GOTO      ENDTAS
100800941110     C                   END
100900941110     C                   END
101000980508     C** NON TROVATO SCAGLIONE ESATTO
101100941110     C     TSCG          IFLT      PESTAS
101200941110     C                   SETON                                        94
101300980512     C                   MOVEL     MSG(5)        TASMSG
101400151209     c                   eval      %subst(tasmsg: 37)= %trim(%editc(PESTAS:'3'))
101500980512     C                   GOTO      FINE
101600941110     C                   END
101700941110     C     ENDTAS        TAG
101800980512     C** MI SALVO LA POSIZIONE DELLO SCAGLIO TROVATO CHE MI
101900980512     C*  SERVE PER IL CALCOLO TARIFFA CON GARANTITO MAX SCAGLIONE PREC
102000980512     C     *LIKE         DEFINE    A             WA
102100980512     C                   Z-ADD     A             WA
102200980508     C**
102300980508     C* CALCOLO PER QUALE VALORE MOLTIPLICARE LA TARIFFA
102400980508     C                   Z-ADD     PESTAS        WPTAS3
102500980508     C                   EXSR      CALQLI
102600980508     C**
102700980508     C* MODO APPLICAZIONE TARIFFA
102800980508     C                   MOVEL     TAMFLO        DSTA01
102900990927     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
103000990927     C     §TADIV        IFEQ      *BLANK
103100990927     C                   MOVE      'ITL'         §TADIV
103200990927     C                   ENDIF
103300980512     C**
103400980512     C* CALCOLO NORMALE ANCHE PER SCALARE AL DI SOTTO
103500980512     C*  DELL'APPLICAZIONE TARIFFA FINITA E PER MAX SCAGLIONE PREC
103600980512     C     §TAF02        IFNE      'S'
103700980512     C     §TAF02        OREQ      'S'
103800980512     C     WATA          ANDNE     ' '
103900980508     C** MOLTIPLICAZIONE TARIFFA
104000980508     C                   EXSR      MULTAR
104100980515     C* APPLICAZIONE MINIMO E MASSIMO
104200980515     C                   EXSR      MINMAX
104300980508     C                   Z-ADD     WPOR          TASPOR
104400980508     C                   Z-ADD     WINL          TASINL
104500980514     C                   ENDIF
104600980508     C**
104700980508     C** S C A L A R E
104800980512     C     §TAF02        IFEQ      'S'
104900980508     C                   EXSR      SCALAR
105000980512     C                   ENDIF
105100980508     C**
105200980508     C** M A X   S C A G L I O N E   P R E C E D E N TE
105300980512     C     §TAF02        IFEQ      'G'
105400980512     C                   EXSR      MAXSCA
105500980512     C                   ENDIF
105600990923     C**
105700990923     C** SE DIVISA DELLA TARIFFA NON ACCETTA I DECIMALI PREPARO IL
105800990923     C** DEL PORTO E DELL'INOLTRO SENZA DECIMALI
105900990923     C     *IN12         IFEQ      '0'
106000990923     C* PORTO
106100991112     C     TASPOR        ADD       0,5           CAMPOW           11 0
106200990923     C                   Z-ADD     CAMPOW        TASPOR
106300990923     C* INOLTRO
106400991112     C     TASINL        ADD       0,5           CAMPOW
106500990923     C                   Z-ADD     CAMPOW        TASINL
106600990923     C                   ENDIF
106700090924     c                   eval      WImpoMag=taspor+tasinl
106800090924     c
106900991012     C     DOPOIN        TAG
107000991012     C**
107100990923     C* VALORIZZO LA VARIA DELL'INOLTRO NELLA SCHIERA DELLE VARIE
107200991012     C     TASINL        IFGT      0
107300990923     C                   Z-ADD     1             X                 2 0
107400990923     C     §$2FIN        LOOKUP    SV(X)                                  30
107500990923     C* SE NON ESISTE CARICOLA SCHIERA
107600990923     C     *IN30         IFEQ      '0'
107700990923     C                   Z-ADD     1             X
107800990923     C     ' '           LOOKUP    SV(X)                                  30
107900990923     C   30              MOVEA     §$2FIN        SV(X)
108000990923     C                   ENDIF
108100990923     C* SE LO TROVO VALORIZZO TASINL
108200990923     C   30              Z-ADD     TASINL        VA(X)
108300991012     C                   END
108400990921     C****************************************
108500990921     C** ISOLA  LOCALITA' DISAGIATE C.STORICI
108600990921     C****************************************
108700970521     C** SE TROVATA TARIFFA DEL PAESE APPLICO LO STESSO L'ISOLA
108800970521     C**
108900970721    1C     FLGINO        IFEQ      'I'
109000970521     C     FLGINO        OREQ      'D'
109100110624      * sostituisco il controllo del centro storico con i nuovi flag T o Z
109200110624      * T = centro storico in città
109300110624      * Z = centro storico in provincia
109400110624     C     FLGINO        OREQ      'T'
109500110624     C     FLGINO        OREQ      'Z'
109600970521     C* IMPOSTO LA TARIFFA PARTICOLARE
109700970521     C                   SELECT
109800970521     C     FLGINO        WHENEQ    'I'                                          ISOLA
109900970521     C                   MOVEL     'I'           TASTC
110000970521     C     FLGINO        WHENEQ    'D'                                          LOCALITA' DI
110100970521     C                   MOVEL     'K'           TASTC
110200110624      * sostituisco il controllo del centro storico con i nuovi flag T o Z
110300110624      * T = centro storico in città
110400110624      * Z = centro storico in provincia
110500110624     C     FLGINO        WHENEQ    'T'                                          C.STORICO
110600970521     C                   MOVEL     'Q'           TASTC
110700110624     C     FLGINO        WHENEQ    'Z'                                          C.STORICO
110800110624     C                   MOVEL     'Q'           TASTC
110900970521     C                   ENDSL
111000110624      ** visto che l'attivazione del calcolo del centro storico dovrebbe essere
111100110909      ** il 3/10/11  attivo il flag "Q" in tastc confrontando la dta spedizione
111200110909     c                   if        tasdsp < 20111003 and tastc = 'Q'
111300110630     c                   clear                   tastc
111400110630     c                   endif
111500110628      ** Se TASTC è valorizzato calcolo la varia. Questa specifica si può togliere
111600110628      ** quando abilitiamo il centro storico
111700110628 bis1c                   If        tastc <> ' '
111800110628     c
111900970521     C**
112000060414     c* 14/4/06-ES--> il pgm per tassare questa tar.particolare usa
112100060414     c*           sempre il cod.tassazione mittente anche se non
112200060414     c*           applica la reversibilità. Da sempre lo fa.
112300060414     c*           Abbiamo valutato che è sbagliato ma deve seguire
112400060414     c*           la regola della reversibilità. Per cui non lo devo
112500060414     c*           più fare per tipbol='A' ma per 56 ON
112600060414     C**   TIPBOL        IFEQ      'A'
112700060414     C                   IF        *in56
112800981008     C                   MOVEL     TASCTS        WCTS              2
112900941123     C                   MOVEL     TASMCT        TASCTS
113000060414     C                   ENDif
113100970521     C*
113200970521     C                   EXSR      TASPAR
113300060414     C**   TIPBOL        IFEQ      'A'
113400060414     C                   IF        *in56
113500970721     C                   MOVEL     WCTS          TASCTS
113600970721     C                   END
113700970721     C**
113800970721     C* SOLO SE L'HO CALCOLATA FACCIO LE COSE SOTTOSTANTI
113900970721    2C     WNOEST        IFEQ      'S'
114000970721     C     TASCP         ANDGT     0
114100970522     C* MEMORIZZO PORTO E POSIZIONE VARIA DOVE HO MEMORIZZATO
114200970522     C                   Z-ADD     TASCP         TASISO
114300090924     c* imponibile per maggiorazione "E" o "H"
114400090924     c                   eval      wImpoMag=wImpoMag+TASISO
114500970522     C*
114600970522     C  N96              MOVEL     'S'           WPORTO            1
114700970522     C* SALVO L'INDICE DELLA SCHIERA DELLA VARIA
114800970522     C   96              Z-ADD     A             ISO               2 0
114900970522     C   96              MOVEL     ' '           WPORTO            1
115000970521     C*
115100970521     C** SE VA IN SOSTITUZIONE CLEARO L'INOLTRO
115200970521     C** FLAG=S INOLTRO+ISOLA
115300970521     C** FLAG=N SOLO ISOLA
115400110628     c** In caso di centro storico  non considero il flag FLGISO in quanto non
115500110628     c** non deve andare in sostituzione dell'inoltro ed in tariffa non viene
115600110628     c** richiesta la valorizzazione quindi è sempre diverso da "S"
115700970721    3C     FLGISO        IFNE      'S'
115800110628     c     TASTC         ANDNE     'Q'
115900090924     c* tolgo dall'imponibile per la maggiornazione l'Inoltro
116000090924     c                   eval      wImpoMag=wImpoMag-TASINL
116100090924     c*
116200090924     C                   CLEAR                   TASINL
116300990921     C** RICHIAMO LA VECCHIA POSIZIONE DELLA VARIA E LA SOSTIUISCO CON IL
116400990921     C**  VALORE DELL'ISOLA
116500990921     C** L'INDICE "X" PUNTA ALLA VARIA DELL'INOLTRO
116600990921     C     X             IFGT      *ZERO
116700990921     C* PULISCO L'INOLTRO
116800990921     C                   CLEAR                   VA(X)
116900990921     C                   CLEAR                   SV(X)
117000990921     C* UTILIZZO LA POSIZIONE VUOTA DELLA SCHIERA PER METTERE L'IMPORTO DELLA
117100990921     C* VARIA DELL'ISOLA
117200990921     C                   MOVEL     §1PVAR        SV(X)
117300990921     C                   Z-ADD     TASCP         VA(X)
117400990921     C* PULISCO LA POSIZIONE DELLA SCHIERA PRECEDENTE
117500990921     C   96              CLEAR                   VA(A)
117600990921     C   96              CLEAR                   SV(A)
117700990921     C* SALVO L'INDICE ATTUALE DELLA SCHIERA DELLA VARIA
117800990921     C   96              Z-ADD     X             ISO
117900990922     C                   END
118000970721    3C                   END
118100090924     c
118200970721    2C                   END
118300110628 bis1C                   ENDIF
118400970721    1C                   ENDIF
118500090924     c
118600090924     C***********************
118700090924     C** Maggiorazione per tipo servizio sul totale imponibile da maggiorare
118800110628     c** ovvero:  PORTO + INOLTRO + ISOLA/DISAGIATE/CENTRO STORICO
118900110628     C** Se porto o inoltro già presenti: solo su ISOLA/DISAGIATE/CENTRO STORICO
119000110628     c** Se isola/disagiate/centro storico già presente: solo su PORTO + INOLTRO
119100090924     C***********************
119200090924     C                   Z-ADD     1             A                 5 0
119300090924     C                   clear                   ESP
119400090924     c
119500090924    1c                   if        WimpoMag>0
119600090924     C*
119700090924     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
119800090924     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
119900090924     C*
120000090924    2C     KSDIT         IFEQ      'SDI'
120100090924     C     TASDSP        ANDLE     20001231
120200090924     C     TASTSP        ANDEQ     'C'
120300090924     C     'E'           LOOKUP    TS(A)                                  05
120400090924     C                   ELSE
120500090924     C     TASTSP        LOOKUP    TS(A)                                  05
120600090924    2C                   ENDIF
120700090924     C*
120800090924     C   05A             OCCUR     DS5E
120900090924    2C   05§5EFTC        IFNE      *BLANKS
121000090924     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
121100090924     c                   EXSR      CerESPRESSO
121200090924     c
121300090924    3c                   if        giaespr<>'S'
121400090924     C                   Z-ADD     0             WTSP
121500090924     c
121600090924     C                   Z-ADD     WImpoMag      IMPON
121700090924     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
121800090924     C                   IF        *in56
121900090924     c                   eval      applicarevers='S'
122000090924     c                   endif
122100090924     C                   EXSR      CALTS
122200090924     c                   clear                   applicarevers
122300090924     c
122400090924    4C     WTSP          IFGT      0
122500090924    5c                   if        esp<>999
122600090924     C                   MOVEL     WVariaMag     SV(ESP)
122700090924     C                   add       wtsp          VA(ESP)
122800090924   x5c                   else
122900090924     c* Se non c'e' una varia disponibile, sommo nel porto
123000090924     C                   ADD       WTSP          TASPOR
123100090924    5C                   END
123200090924    4C                   END
123300090924     c
123400090924    3C                   ENDif
123500090924    2C                   END
123600090924    1C                   END
123700990921     C****************
123800990921     C** DIRITTO FISSO
123900990921     C****************
124000980424     C     TASDIF        IFEQ      0
124100980424     C                   MOVEL     'H'           TASTC
124200990921     C* RICHIAMO LA ROUTINE DI CALCOLO DELLE TARIFFE PARTICOLARI
124300990921     C                   EXSR      TASPAR
124400990921     C   96              Z-ADD     TASCP         TASDIF
124500990921     C***
124600980424     C                   ENDIF
124700990921     C***********************
124800990921     C** PROVVIGIONE ASSEGNO
124900990921     C***********************
125000980506     C     VARIAG        TAG
125100980506     C     *LIKE         DEFINE    TASVA1        PROVAS
125200980506     C                   Z-ADD     0             PROVAS
125300980429     C** MANCA C/A
125400941112     C     TASCAS        CABEQ     0             POIT01
125500980429     C** PROVV. ASSEGNO GIA' PRESENTE
125600941112     C                   Z-ADD     1             A
125700941112     C     §$2VRG        LOOKUP    SV(A)                                  05
125800980429    1C  N05              DO
125900941116     C*
126000941116     C                   SETOFF                                       94
126100941116     C     *LIKE         DEFINE    TASCAS        CASLIT
126200941116     C                   Z-ADD     TASCAS        CASLIT
126300980429     C**
126400990921     C* CALCOLO IL VALORE DEL CONTRASSEGNO  IN BASE ALLA MONETA
126500020319     C*******
126600020319     C*******
126700020319     C* SE PRESENTE IL C/A ANNULLATO VEDO IN BASE ALLA TARIFFA CLIENTE
126800020319     C*  SE FATTURARE O MENO
126900020319     C*  §TAPCA='N' NON FATTURO GLI ANNULLATI
127000030131     C     TASSTA        IFEQ      '9'
127100030131     C     §TAPCA        ANDEQ     'N'
127200060922      * calcolo sempre la provvigione del C/A  annullato se si tratta di consegna anomala dirottata
127300060922     C     §ASCCA        ANDNE     '1'
127400060922      *
127500020319     C                   GOTO      POIT01
127600020319     C                   ENDIF
127700020319     C**
127800990921     C     TASVCA        IFNE      §TADIV
127900990921     C                   CLEAR                   YEUR
128000990921     C                   MOVEL     TASVCA        YECDVI
128100990921     C                   MOVEL     §TADIV        YECDVO
128200990921     C                   Z-ADD     TASCAS        YECIMI
128300991014     C   12              MOVE      '3'           YECDCO
128400991014     C  N12              MOVE      '0'           YECDCO
128500990921     C*
128600990921     C                   CALL      'YEURCO'
128700990921     C                   PARM                    YEUR
128800990921     C*
128900990921     C     YECESI        IFEQ      ' '
129000990921     C                   Z-ADD     YECIMO        CASLIT
129100990921     C                   ELSE
129200990921     C                   SETON                                        94
129300990921     C                   MOVEL     MSG(1)        TASMSG
129400990921     C                   GOTO      FINE
129500990923     C                   END
129600990921     C                   ENDIF
129700980429     C* PROVV ASSEGNO MITTENTE
129800980429    2C     TASBM         IFEQ      'M'
129900980429     C                   MOVEL     'W'           TASTC
130000980429     C                   ELSE
130100980429     C* PROVV ASSEGNO VETTORE
130200980429     C                   MOVEL     'V'           TASTC
130300980429    2C                   ENDIF
130400060712      * se spedizione EEX cerco sempre provvigione assegno mittente
130500160315     c****               If        o27fie = 'E'
130501160315     c                   if        WBOEST='E'and WBOFED=' '
130600060712     c                   movel     'W'           tastc
130700060712     c                   endif
130800980429     C**
130900980429     C                   MOVEL     TASTC         FISO
131000980506     C                   MOVEL     'S'           WCAR
131100980429     C                   EXSR      CERTPT
131200980429    2C     WEND          IFEQ      ' '
131300980429     C**
131400980429    3C     TPTTPG        IFEQ      'V'                                          VALORE
131500980429     C                   Z-ADD     CASLIT        TPDPES
131600980429     C                   ELSE                                                   ALTRO
131700980429     C                   Z-ADD     ISOPES        TPDPES
131800980429    3C                   ENDIF
131900980429     C**
132000980429     C                   EXSR      CALACD
132100941116     C*
132200980429    3C     VARIA         IFGT      0
132300941112     C                   Z-ADD     1             A
132400941112     C     ' '           LOOKUP    SV(A)                                  96
132500941112     C   96              MOVEL     §$2VRG        SV(A)
132600980429     C   96              Z-ADD     VARIA         VA(A)
132700980429     C  N96              ADD       VARIA         TASPOR
132800980429     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
132900980429    3C                   ENDIF
133000980429    2C                   ENDIF
133100980429    1C                   ENDDO
133200980506     C**
133300980506     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
133400980506     C     TASSVA        IFEQ      §$2VRG
133500980506     C                   GOTO      FINE
133600980506     C                   ENDIF
133700990921     C***********************
133800990921     C** DIRITTO ASSICURATIVO
133900990921     C***********************
134000941110     C     POIT01        TAG
134100941112     C                   Z-ADD     1             A
134200941112     C     §$2VRE        LOOKUP    SV(A)                                  05
134300941112     C  N05              DO
134400941112     C                   Z-ADD     0             DIRITT
134500941112     C     *LIKE         DEFINE    TASVA1        DIRITT
134600941110     C                   EXSR      CALVE
134700941112     C     DIRITT        IFGT      0
134800941112     C                   Z-ADD     1             A
134900941112     C     ' '           LOOKUP    SV(A)                                  96
135000941112     C   96              MOVEL     §$2VRE        SV(A)
135100941112     C   96              Z-ADD     DIRITT        VA(A)
135200941112     C  N96              ADD       DIRITT        TASPOR
135300941112     C                   END
135400941112     C                   END
135500941112     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
135600990921     C***********************
135700990921     C** ASS. X CONTO
135800990921     C***********************
135900980525     C     VARIAR        TAG
136000980525     C                   Z-ADD     0             WVARIR
136100060322     C                   Z-ADD     0             WVARIACB
136200980525     C     *LIKE         DEFINE    TASVA1        WVARIR
136300060321     C     *LIKE         DEFINE    TASVA1        WVARIACB
136400060320      * cerco la varia R sia in caso di bolla con importo d'assicurare e sia in caso di
136500060320      * ricerca di mandato
136600941112     C                   EXSR      CALVR
136700941112     C** MANCA TARIFFA:MANDATO VALORE = 0 E TASIAS=0
136800980526     C   94              GOTO      FINE
136900980526     C**
137000980525     C     WVARIR        IFGT      0
137100941112     C                   Z-ADD     1             A
137200941112     C     ' '           LOOKUP    SV(A)                                  96
137300941112     C   96              MOVEL     §$2VRR        SV(A)
137400980525     C   96              Z-ADD     WVARIR        VA(A)
137500060320      ** se non ci sono varie libere aggiungo al porto
137600980525     C  N96              ADD       WVARIR        TASPOR
137700060320
137800060320      * se varia uguale a zero e non ho trovato nessun mandato e non c'è importo d'assicurare in
137900060320      * bolla cerco la nuova AC base
138000060320
138100060320     c                   else
138200060320
138300060321     c                   If        task88 = 'X' and
138400060321      ** 88 = non si calcola
138500060322     c                             (Ksccod <> 8888 and Ksccod <> 9999) and
138600060322      ** data spedizione maggiore del 28 02 2006
138700060322     c                             tasdsp > 20060228
138800060320     c                   exsr      CALVACB
138900060320      ** carico la varia nella schiera
139000060321     c                   If        wvariacb > 0
139100060320     c                   z-add     1             a
139200060320     c     ' '           Lookup    sv(a)                                  96
139300060320     c   96              movel     §$2Acb        sv(a)
139400060321     c   96              z-add     Wvariacb      va(a)
139500060320      ** se non ci sono varie libere aggiungo al porto
139600060321     c  N96              add       Wvariacb      TASpor
139700060320     c                   endif
139800060320
139900060320     c                   endif
140000060320
140100060320     c                   END
140200980525     C**
140300980525     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
140400980525     C     TASSVA        IFEQ      §$2VRR
140500980525     C                   GOTO      FINE
140600980525     C                   ENDIF
140700990722     C***************
140800990722     C* T.C.P.
140900990722     C***************
141000990722     C*
141100990722     C                   MOVEL     §$2VRP        TASTC
141200990722     C                   EXSR      TASPAR
141300030624     C***************************************
141400030624     C* ANNULLAMENTO PORTO ASSEGNATO VARIA &
141500030624     C***************************************
141600030624     C*
141700030624     C* SE IL FLAG DI CALCOLO VARIA & È UGUALE A S
141800030624     C                   IF        TASFCAA = 'S'
141900030624     C                   MOVEL     §$2VPA        TASTC
142000030624     C                   EXSR      TASPAR
142100030624     C                   ENDIF
142200050928     C****************************
142300050928     C* LASCIATO AVVISO  VARIA 'c'
142400050928     C****************************
142500050929     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
142600050928     C                   If        tasric <> ' '
142700090305      * verifico se linea di arrivo o linea di partenza è estera non calcolo la varia "c"
142800051028     c     taslna        lookup    esttot                                 30
142900090305      * se linea di arrivo non è estero verifico la linea di partenza
143000090305     c  n30taslnp        lookup    esttot                                 30
143100090305      * se spedizione italiana calcolo
143200051028     c                   If        not *in30
143300050928     C                   Movel     §$2vla        Tastc
143400050928     C                   Exsr      TASpar
143500051005     c                   Endif
143600051005
143700050928     c                   Endif
143800110628     C****************************
143900110628     C* AVVISO AL DESTINATARIO AFFIDAMENTO SPEDIZIONE VARIA 'm'
144000110628     C****************************
144100110628     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
144200110628     C* in TASFLO §asemd
144300130923     C                   If        §asemd = 'S' or §asemd = 'X' or
144400130923     c                             §asemd = 'E'
144500110628     C                   Movel     §$2vad        Tastc
144600110628     C                   Exsr      TASpar
144700110628     c                   Endif
144800151209     C****************************
144900151209     C* PIN CODE  CONSEGNA CON VERIFICA PIN CODE VARIA "p"  (anche se non serve)
145000151209     C****************************
145100151209     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
145200151209     C* in TASFLO §aspin
145300151209     C                   If        §aspin = 'S'
145400151209     C                   Movel     §$2pin        Tastc
145500151209     C                   Exsr      TASpar
145600151209     c                   Endif
145700151209
145800151209     C*************************************************
145900151209     C* DIRITTO DI CHIAMATA PRENOTAZIONE ORM TELEFONICO
146000151209     C*************************************************
146100151209     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
146200151209     C* TASPRT
146300151209     C                   If        TASPRT = 'S'
146400151209     C                   Movel     §$2ORMT       Tastc
146500151209     C                   Exsr      TASpar
146600151209     c                   Endif
146700151209
146800151209     C****************************
146900151209     C* PACKING LIST
147000151209     C****************************
147100151209     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
147200151209     C* TASSPL
147300151209     C                   If        TASSPL = 'S'
147400151209     C                   Movel     §$2pkl        Tastc
147500151209     C                   Exsr      TASpar
147600151209     c                   Endif
147700151209
147800110628
147900020412     C***************
148000020412     C* RICERCA DOCUMENTI - SOLO SE RICHIESTA DA SOLA
148100020412     C***************
148200020412     C*
148300020412     C     VARIAO        TAG
148400020412     C     TASSVA        IFEQ      §$2VRO
148500020412     C                   MOVEL     §$2VRO        TASTC
148600020412     C                   EXSR      TASPAR
148700020412     C**
148800020412     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
148900020412     C     TASSVA        IFEQ      §$2VRO
149000020412     C                   GOTO      FINE
149100020412     C                   ENDIF
149200020412     C                   ENDIF
149300020516     C***************
149400020516     C* RITIRI ANNULLATI  - SOLO SE RICHIESTA DA SOLA
149500020516     C***************
149600020516     C*
149700020516     C     VARIAY        TAG
149800020516     C     TASSVA        IFEQ      §$2VRY
149900020516     C                   MOVEL     §$2VRY        TASTC
150000020516     C                   EXSR      TASPAR
150100020516     C**
150200020516     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
150300020516     C     TASSVA        IFEQ      §$2VRY
150400020516     C                   GOTO      FINE
150500020516     C                   ENDIF
150600020516     C                   ENDIF
150700020516     C***************
150800020516     C* DIROTTAMENTI      - SOLO SE RICHIESTA DA SOLA
150900020516     C***************
151000020516     C*
151100020523     C     VARAST        TAG
151200020523     C     TASSVA        IFEQ      §$2AST
151300020523     C                   MOVEL     §$2AST        TASTC
151400020516     C                   EXSR      TASPAR
151500020516     C**
151600020516     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
151700020523     C     TASSVA        IFEQ      §$2AST
151800020516     C                   GOTO      FINE
151900020516     C                   ENDIF
152000020516     C                   ENDIF
152100030403     C***************
152200030403     C* RECAPITO C/ASSEGNI- SOLO SE RICHIESTA DA SOLA
152300030403     C***************
152400030403     C*
152500030403     C     VARVRW        TAG
152600030403     C     TASSVA        IFEQ      §$2VRW
152700030403     C                   MOVEL     'G'           TASTC
152800030403     C                   EXSR      TASPAR
152900030403     C**
153000030403     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
153100030403     C     TASSVA        IFEQ      §$2VRW
153200030403     C                   GOTO      FINE
153300030403     C                   ENDIF
153400030403     C                   ENDIF
153500030527     C*
153600030527     C***************
153700030527     C* ADDEBITO INSOLUTI - INT.MORA  SOLO SE RICHIESTA DA SOLA
153800030527     C***************
153900030527     C     VARVRM        TAG
154000030527     C     TASSVA        IFEQ      §$2VRM
154100030527      * SE NON ESISTONO VARIE IMPOSTATE DO MANCA TARIFFA
154200030527     c                   MOVEA     SV            WRKSV            20
154300030527     C                   IF        WRKSV = *BLANKS
154400030527     C                   MOVEL     MSG(18)       TASMSG
154500030527     C                   MOVEL     'E'           TASERR            1
154600030527     C                   SETON                                        94
154700030527     C                   ENDIF
154800030527     C                   GOTO      FINE
154900030527     C                   ENDIF
155000030626     C***************
155100030626     C* ORM COMMISSIONATO  - SOLO SE RICHIESTA DA SOLA
155200030626     C***************
155300030626     C*
155400030626     C     VARVRT        TAG
155500030626     C     TASSVA        IFEQ      §$2VRT
155600030626     C                   MOVEL     §$2VRT        TASTC
155700030626     C                   EXSR      TASPAR
155800030626     C**
155900030626     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
156000030626     C     TASSVA        IFEQ      §$2VRT
156100030626     C                   GOTO      FINE
156200030626     C                   ENDIF
156300030626     C                   ENDIF
156400040910     C***************
156500040910     C* P.O.D IMMAGE      - SOLO SE RICHIESTA DA SOLA
156600040910     C***************
156700040910     C*
156800040910     C     VARPOD        TAG
156900040910     C     TASSVA        IFEQ      §$2POD
157000040910     C                   MOVEL     §$2POD        TASTC
157100040910     C                   EXSR      TASPAR
157200040910     C**
157300040910     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
157400040910     C     TASSVA        IFEQ      §$2POD
157500040910     C                   GOTO      FINE
157600040910     C                   ENDIF
157700040910     C                   ENDIF
157800990922     C************************
157900990922     C* CONSEGNE PARTICOLARI
158000990922     C************************
158100990722     C*
158200990922     C** CONSEGNE PARTICOLARI 1
158300941110     C     TASTC1        IFNE      *BLANKS
158400970521     C                   MOVEL     TASTC1        TASTC
158500970521     C                   EXSR      TASPAR
158600941112     C                   END
158700990922     C** CONSEGNE PARTICOLARI 2
158800941110     C     TASTC2        IFNE      *BLANKS
158900970521     C                   MOVEL     TASTC2        TASTC
159000970521     C                   EXSR      TASPAR
159100941110     C                   END
159200020826     C************************
159300020826     C* DITITTO DI PESATURA
159400020826     C************************
159500020826     C*
159600020826     C     §TATAP        IFEQ      'S'
159700020826     C                   MOVEL     '5'           TASTC
159800020826     C                   EXSR      TASPAR
159900020826     C                   END
160000990922     C************************
160100990922     C* RITIRO
160200990922     C************************
160300050504     C* LA VARIA "U" DI RITIRO  E'
160400050504     C** DA CALCOLARE: SUI PORTI ASSEGNATI
160500050504     C**               SUI PORTI FRANCHI NON CODIFICATI
160600050504     C**                   CON FLAG CONSEGNA A MAGAZZINO = A BLANK
160700050504     C**               SULLE BOLLE EXPORT ASSEGNATO NON DPD
160800050504     C**               SULLE BOLLE IMPORT ASSEGNATO TUTTE (ANCHE DPD)
160900990915     C*
161000990915     C* FRANCO CON CODICE CLIENTE UGUALE A 8888 OPPURE UGUALE ALLA TARIFFA DI
161100990915     C* CARTELLO
161200000919     C     TIPBOL        IFNE      'A'
161300000919     C     KSCCOD        ANDNE     8888
161400020417     C****       TASKSC    ANDNECARKSC
161500020417     C* SE NON SI TRATTA NEMMENO DI UNA TARIFFA DI CARTELLO NON TASSO
161600020417     C     TASKSC        LOOKUP    TAC                                    15
161700020417     C  N15              GOTO      ENDRIT
161800000919     C                   ENDIF
161900990930     C** DA CALCOLARE SOLO PER PORTI ASSEGNATI RITIRATI
162000000919     C*!!*       TIPBOL    CABNE'A'       ENDRIT
162100990930     C*
162200970213     C*
162300990914     C* SE BOLLA ASSEGNATA EXPORT --> NON CONSIDERO IL FLAG CONSEGNA A MAGAZZINO
162400990914     C*                               PERCHE' DEVE SEMPRE CALCOLARE LA VARIA "U"
162500970213     C     TASFDN        IFEQ      'S'
162600020319     C* PER ASSEGNATO EXPORT --> SEMPRE  (ANCHE DPD CHE FINO A OGGI
162700050504     C*                          ESCLUDEVA 4/4/02--> RIESCLUDO DPD)
162800050504     C* PER ASSEGNATO IMPORT --> SEMPRE  (ANCHE DPD 04/05/05)
162900050504     C*
163000001204     C* PER ALTRI ASSEGNATI  --> SOLO SE NON E' PORTATA A MAGAZZINO
163100001204     C     TIPBOL        IFEQ      'A'
163200001204     C*
163300980605     C     TASLNA        LOOKUP    EST                                    05
163400050504     c
163500050504     C  n05TASLNP        LOOKUP    ESTtot                                 05
163600050504     c
163700980605     C  N05              GOTO      ENDRIT
163800001204     C                   ELSE
163900001204     C* PER 8888 --> NON APPLICARE SE PORTATA A MAGAZZINO SIA PER EXP
164000001204     C*              CHE PER LE ALTRE SPEDIZIONI
164100001204     C                   GOTO      ENDRIT
164200970213     C                   ENDIF
164300001204     C                   ENDIF
164400060720      *----------------------------------------------------------------------------*
164500060720      * In caso di bolla di reso non si può NON CALCOLARE la varia U in quanto     *
164600060720      * questa serve per far pagare la tratta int.le della spedizione.             *
164700060720      * Infatti quando in Aprile 2006 è stata tolta la signora Villa ha reclamato. *
164800060720      * asterisco le seguenti specifiche                                           *
164900060720     c*** Per bolla import, non applico varia "U" se bolla di reso                 *
165000060720     C***     TASLNP        LOOKUP    ESTtot                                 05    *
165100060720     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
165200060720     C***                   GOTO      ENDRIT                                       *
165300060720     c***                   endif                                                  *
165400060720     c***                                                                          *
165500060720     c*** Per bolla EXport, non applico varia "U" se bolla di reso                 *
165600060720     C***     TASLNA        LOOKUP    EST                                    05    *
165700060720     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
165800060720     C***                   GOTO      ENDRIT                                       *
165900060720     c***                   endif                                                  *
166000060720      *----------------------------------------------------------------------------*
166100060424     c
166200970213     C**
166300970521     C* TARIFFA PARTICOLARE --> U
166400941121     C                   MOVEL     'U'           TASTC
166500970521     C**
166600110617     C** 17/06/2011 LB DN visto che sono state cambiate le tariffe della cartello
166700110617     c** per la tariffa particolare di ritiro per l'Estero e non essendo + uguale
166800110617     c** a quelle per l'italia in caso di prepagati (quindi non codificati) per la GB
166900110617     c** la tariffa di ritiro diventa di 50 euro circa rispetto i 5 euro circa di prima ?
167000110617     c** quindi risulta necessario correggere un errore che ci portiamo dietro da anni
167100110617     c** che tassiamo controllando il codice tassazione mittente solo per gli assegnati
167200110617     c** invece si deve fare anche per i poveri franchi (prepagati) per i quali viene
167300110617     c** calcolato il ritiro. Quindi sia per i franchi che assegnati il codice tassazione
167400110617     c** per la tariffa particolare di ritiro diventa il codice tassazione mittente
167500110617     c** come è giusto che venga tassata da dove viene ritirata e non dove viene
167600110617     c** consegnata la merce. A.G.
167700110617     C**   TIPBOL        IFEQ      'A'
167800970521     C                   MOVEL     TASCTS        WCTS
167900970521     C                   MOVEL     TASMCT        TASCTS
168000110617     C**                 END
168100970521     C*
168200970521     C                   EXSR      TASPAR
168300970521     C*
168400110617     C**   TIPBOL        IFEQ      'A'
168500970521     C                   MOVEL     WCTS          TASCTS
168600110617     C**                 END
168700941121     C     ENDRIT        TAG
168800990922     C************************
168900990922     C* SI DDT
169000031104      * Tutti i pgm che richiamano il TNSF20R x tassare le bolle arrivi
169100031104      * testano il flag DDT e passano 'S' nel caso di DDT SI, in questi pgm ho aggiunto anche i
169200031104      * nuovi flag 'P' e 'Q'
169300031104      * l'unico che non lo fa è FNLG20R così ho aggiunto anche qua il controllo con i 2 nuovi flag
169400031104      * 'P' è uguale a 'S' e viene impostato in arrivo quando si stampa il ddt si
169500031104      * 'Q' è uguale a 'C' e viene impostato in arrivo quando si stampa il ddt si
169600990922     C************************
169700970409     C     TASDDT        IFEQ      'S'
169800031104     C     TASDDT        orEQ      'P'
169900031104     C     TASDDT        orEQ      'Q'
170000970521     C* TARIFFA PARTICOLARE --> B
170100970521     C                   MOVEL     'B'           TASTC
170200970521     C                   EXSR      TASPAR
170300970409     C                   END
170400030203      ***************
170500030203      * BANCALI
170600030203      ***************
170700030203     C*
170800030203      * Non si tassano i bancali per le bolle in porto assegnato
170900030203     c                   If        TasBan > *Zeros and TipBol <> 'A'
171000030203     c                   Movel     §$2Vba        TASTC
171100030203     c                   ExSr      TasPar
171200030203     c                   EndIf
171300001205     C*
171400040506      ************************************************
171500040506      * SUPPLEMENTO CARBURANTE SPEDIZIONI FEDEX
171600040506      ************************************************
171700130124     C                   IF        WBOFED = 'S'
171800060905     C                   EXSR      SR_SUPCARB
171900130124     C                   ENDIF
172000060905      ************************************************
172100060905      * FUEL SURCHARGE  SPEDIZIONI NO FEDEX
172200060905      ************************************************
172300130124     C     VARVFS        TAG
172400130124     C                   IF        WBOFED = ' '
172500060905     C                   EXSR      SR_FUEL
172600130124     C**
172700130124     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
172800130124     C     TASSVA        IFEQ      §$2VFS
172900130124     C                   GOTO      FINE
173000130124     C                   ENDIF
173100130124     C                   ENDIF
173200040506      *
173300941110     C                   SETOFF                                       49
173400941110     C                   ENDSR
173500060414     C*****************************************************************
173600090922     C** Cerco la posizione per la varia della maggiorazione
173700060414     C*****************************************************************
173800060414     c     CerESPRESSO   BEGSR
173900060414     c                   if        esp=0
174000060414     c                   clear                   giaespr           1
174100090923     c                   clear                   wVariaMag         1
174200060414     C                   Z-ADD     1             A
174300090922
174400090922     c* Maggiorazione per tipo servizio E-priority
174500090922     c                   select
174600090922     c                   when      tastsp='E'
174700090922     c                   eval      wvariaMag=§$2tsp
174800090922
174900090922     c* Maggiorazione per tipo servizio H-H1030
175000090922     c                   when      tastsp='H'
175100090922     c                   eval      wvariaMag=§$2tsh
175200090922     c                   endsl
175300090922
175400090922     C     wvariaMag     LOOKUP    SV(a)                                  96
175500090922     c* gia esiste la varia della maggioraz --> non la calcolo
175600060414     c                   if        *in96
175700060414     c                   z-add     a             ESP               3 0
175800060414     c                   eval      giaespr='S'
175900060414     c                   else
176000060414     C                   Z-ADD     1             A
176100060414     C     ' '           LOOKUP    SV(A)                                  96
176200060414     c   96              z-add     a             ESP               3 0
176300060414     C  n96              ADD       999           ESP
176400060414     c                   endif
176500060414     c                   endif
176600060414     c                   ENDSR
176700980508     C*****************************************************************
176800980508     C** VALORE COL QUALE  MOLTIPLICARE LA TARIFFA
176900980508     C*****************************************************************
177000980508     C     CALQLI        BEGSR
177100991013     C                   Z-ADD     0             PESQLI           14 6
177200980508     C** SE LO SCAGLIONE SCELTO PER TASSARE E' INFERIORE AL MINIMO
177300980508     C** TASSABILE DA TARIFFA FORZO PESO TASSABILE = MINTAS
177400980512     C**
177500980512     C* WATA MI IDENTIFICA SE L'APPLICAZIONE TARIFFA FINITA E' DOVUTA
177600980512     C*  ALLO SCAGLIONE CONFRONTATO CON MINTAR (R)
177700980512     C*  O SE E' DOVUTA DAL PESO TASSABILE CONFRONTATO COL MINIMO
177800980515     C*  TASSABILE DI QUELLA TARIFFA (S) - differenza che ora non serve
177900980515     C*  + ma dal momento che c'e' la lascio
178000980508     C                   CLEAR                   WATA
178100980508    1C                   SELECT
178200980508     C     TSCG          WHENLE    MINTAR
178300980508     C                   Z-ADD     1             PESQLI
178400980514     C                   MOVEL     'R'           WATA              1
178500980508     C**
178600980515     C** SE PESO TASSABILE E' ANCORA INFERIORE
178700980514     C*  AL MINIMO TASSABILE DA TARIFFA IMPOSTO PESO TASS = 1
178800980515     C*  NON LO CONTROLLO PER LA TARIFFA A SCALARE PERCHE' ABBIAMO
178900980515     C*  SEMPRE INSERITO UNO SCAGLIONE=A MINIMO TASSABILE E
179000980515     C*  QUINDI E' GIA' CONTROLLATO NEL WHLE PRECEDENTE
179100980515     C     WPTAS3        WHENLE    MINTAS
179200980515     C     §TAF02        ANDNE     'S'
179300980508     C                   Z-ADD     1             PESQLI
179400980508     C                   MOVEL     'S'           WATA
179500980508   X1C                   OTHER
179600980508     C** CALCOLO : PESO IN Q.LI PER TARIFFA A PESO
179700980508     C** CALCOLO   VALORE/100   PER TARIFFA A VALORE (ESSENDO UNA %)
179800980508     C** PER LE ALTRE TARIFFE PESO TASSABILE INVARIATO
179900980508    2C     UNOCTR        IFEQ      0
180000980508     C     UNOCTR        OREQ      4
180100980508     C     WPTAS3        DIV       100           PESQLI
180200980508   X2C                   ELSE
180300980508     C                   Z-ADD     WPTAS3        PESQLI
180400980508    2C                   END
180500980508    1C                   ENDSL
180600980508     C                   ENDSR
180700980508     C*****************************************************************
180800980508     C* MOLTIPLICATO TARIFFA PER VALORE TROVATO (PESO TASSABILE)
180900980508     C*****************************************************************
181000980508     C     MULTAR        BEGSR
181100980508     C     *LIKE         DEFINE    TASPOR        WPOR
181200980508     C     *LIKE         DEFINE    TASINL        WINL
181300980518     C                   CLEAR                   WPOR
181400980518     C                   CLEAR                   WINL
181500980508     C**
181600980508     C     TITR          MULT(H)   PESQLI        WPOR                           ** PORTO
181700980508     C**
181800980508     C** SE TROVATA TARIFFA DEL PAESE NON SI APPLICA MAI INOLTRO
181900110624     C*****  N49FLGINO        IFNE      'C'
182000110624     C** Sostituisco il controllo del flag inoltro diverso da C=città ma
182100110624     c** anche da "T" Città centro storico
182200110624     C                   IF        NOT *IN49  AND
182300110624     C                             FLGINO <> 'C' AND FLGINO <> 'T'
182400980508     C     TINO          MULT(H)   PESQLI        WINL                           ** INOLTRO
182500980508     C                   END
182600980508     C                   ENDSR
182700980508     C*****************************************************************
182800980508     C* CALCOLO TARIFFA A SCALARE
182900980508     C*****************************************************************
183000980508     C     SCALAR        BEGSR
183100980508     C**
183200980514     C     *LIKE         DEFINE    PESTAS        WPTAS1
183300980508     C     *LIKE         DEFINE    PESTAS        WDIFSC
183400980508     C     *LIKE         DEFINE    PESTAS        WPTAS3
183500980511     C     *LIKE         DEFINE    PESQLI        WPESQ
183600980511     C     *LIKE         DEFINE    PESQLI        WPESSV
183700980511     C     *LIKE         DEFINE    TITR          WTITR
183800980511     C     *LIKE         DEFINE    TITR          WSTITR
183900980511     C     *LIKE         DEFINE    TINO          WTINO
184000980511     C     *LIKE         DEFINE    TINO          WSTINO
184100980508     C     *LIKE         DEFINE    TSCG          WPRESC
184200980508     C**
184300980508     C                   CLEAR                   TASPOR
184400980508     C                   CLEAR                   TASINL
184500980508     C                   CLEAR                   WTROVA
184600980508     C                   CLEAR                   WPTAS3
184700980508     C                   CLEAR                   WDIFSC
184800980508     C                   CLEAR                   WPRESC
184900980511     C                   CLEAR                   WPESSV
185000980511     C                   CLEAR                   WPESQ
185100980511     C                   CLEAR                   WTITR
185200980511     C                   CLEAR                   WSTITR
185300980511     C                   CLEAR                   WTINO
185400980511     C                   CLEAR                   WSTINO
185500980508     C                   Z-ADD     PESTAS        WPTAS1
185600980508     C                   Z-ADD     1             A
185700980508     C**
185800020218    1C     A             DOWLE     200
185900980508     C     WTROVA        ANDEQ     ' '
186000980508     C     TSCG          ANDGT     0
186100980508     C     A             OCCUR     TARDS
186200980512     C**
186300980508     C* FACCIO LA DIFFERENZA TRA LO SCAGLIONE LETTO ED IL PRECEDENTE
186400980508     C     TSCG          SUB       WPRESC        WDIFSC
186500980508     C**
186600980508     C* SOTRAGGO DAL PESO TASSABILE RIMASTO LA PARTE DA TASSARE ORA
186700980508     C* IN WPTAS1 HO SEMPRE LA PARTE DI PESO TASSABILE ANCORA DA TASSAR
186800980508     C                   SUB       WDIFSC        WPTAS1
186900980508     C**
187000980508     C** SE IL VALORE E' DIVENTATO < DI 0 SIGNIFICA CHE HO TERMINATO LA
187100980508     C**  TASSAZIONE PER CUI USO SOLO LA PARTE RIMASTA DI WPTAS1
187200980508    2C     WPTAS1        IFLE      0
187300980511     C     WDIFSC        ADD       WPTAS1        WPTAS3
187400980514     C* PESO TASSABILE GLOBALE DELLA SPEDIZIONE
187500980508     C                   MOVEL     'S'           WTROVA            1
187600980508     C                   ELSE
187700980508     C**
187800980508     C* PARTE DI PESO DA TASSARE CON QUESTO SCAGLIONE
187900980508     C                   Z-ADD     WDIFSC        WPTAS3
188000980508     C* SALVO LO SCAGLIONE PRECEDENTE
188100980508     C                   Z-ADD     TSCG          WPRESC
188200980508    2C                   ENDIF
188300980508     C**
188400980508     C* IMPOSTO IL VALORE DA MOLTIPLICARE PER LA TARIFFA
188500980508     C                   EXSR      CALQLI
188600980512     C**
188700980511     C* SE LO SCAGLIONE E' APPLICAZIONE TARIFFA FINITA DEVO CONSIDERARE
188800980512     C*  SOLO L'ULTIMO SCAGLIONE COSI' E NON I PRECEDENTI
188900980514    2C     WATA          IFNE      ' '
189000980511     C     WTROVA        ANDEQ     ' '
189100980511     C* MI SALVO IL VALORE MA PER ORA NON LO MOLTIPLICO
189200980511     C                   Z-ADD     PESQLI        WPESQ
189300980511     C                   Z-ADD     TITR          WTITR
189400980511     C                   Z-ADD     TINO          WTINO
189500980511   X2C                   ELSE
189600980511     C**
189700980511     C* SE LO SCAGLIONE TROVATO E' L'ULTIMO MA COMUNQUE TARIFFA FINITA
189800980511     C* DEVO MOLTIPLICARE SOLO QUESTO --> AZZERO UN EVENTUALE PRECEDENT
189900980511     C*  CHE AVEVA LA TASSAZIONE TARIFFA FINITA
190000980511    3C     WTROVA        IFEQ      'S'
190100980514     C     WATA          ANDNE     ' '
190200980511     C                   CLEAR                   WPESQ
190300980511     C                   CLEAR                   WTITR
190400980511     C                   CLEAR                   WTINO
190500980511    3C                   ENDIF
190600980511     C*
190700980511     C* PRIMA DI MOLTIPLICARE LA TARIFFA TROVATA VEDO SE CE NE E'
190800980511     C*  UNA PRECEDENTE TARIFFA FINITA DA CONSIDERARE
190900980511    3C     WPESQ         IFGT      0
191000980511     C* SALVO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
191100980511     C*  APPENA LETTO
191200980511     C                   Z-ADD     PESQLI        WPESSV
191300980511     C                   Z-ADD     TITR          WSTITR
191400980511     C                   Z-ADD     TINO          WSTINO
191500980511     C* IMPOSTO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
191600980511     C*  PRECEDENTE PER IL CALCOLO
191700980511     C                   Z-ADD     WPESQ         PESQLI
191800980511     C                   Z-ADD     WTITR         TITR
191900980511     C                   Z-ADD     WTINO         TINO
192000980511     C                   EXSR      MULTAR
192100980511     C                   ADD       WPOR          TASPOR
192200980511     C                   ADD       WINL          TASINL
192300980511     C                   CLEAR                   WPESQ
192400980511     C                   CLEAR                   WTITR
192500980511     C                   CLEAR                   WTINO
192600980511     C* REIMPOSTO I DATI DELLO SCAGLIONE APPENA LETTO
192700980511     C                   Z-ADD     WPESSV        PESQLI
192800980511     C                   Z-ADD     WSTITR        TITR
192900980511     C                   Z-ADD     WSTINO        TINO
193000980511    3C                   ENDIF
193100980511     C**
193200980508     C* MOLTIPLICAZIONE TARIFFA
193300980508     C                   EXSR      MULTAR
193400980508     C                   ADD       WPOR          TASPOR
193500980508     C                   ADD       WINL          TASINL
193600980511    2C                   ENDIF
193700980515     C                   ADD       1             A
193800980508     C**
193900980508    1C                   ENDDO
194000980515     C**
194100980515     C* APPLICO MINIMO E MASSIMO
194200980515     C                   Z-ADD     TASPOR        WPOR
194300980515     C                   Z-ADD     TASINL        WINL
194400980515     C                   EXSR      MINMAX
194500980515     C                   Z-ADD     WPOR          TASPOR
194600980515     C                   Z-ADD     WINL          TASINL
194700980508     C                   ENDSR
194800980512     C*****************************************************************
194900980512     C* CALCOLO MAX SCAGLIONE PRECEDENTE
195000980512     C*****************************************************************
195100980512     C     MAXSCA        BEGSR
195200980513     C* SOLO SE LO SCAGLIONE APPLICATO NON ERA IL PRIMO
195300980513     C     WA            IFGT      1
195400980512     C* PER CONFRONTARLO CON IL VALORE NORMALE CALCOLATO
195500980512     C* IL CONFRONTO E' DATO DALLA SOMMA TRA PORT E INOLTRO
195600980512     C                   SUB       1             WA
195700980512     C     WA            OCCUR     TARDS
195800980512     C* CALCOLO PESO DA MOLTIPLICARE
195900980512     C                   Z-ADD     TSCG          WPTAS3
196000980512     C                   EXSR      CALQLI
196100980515     C** MOLTIPLICO LA TARIFFA
196200980512     C                   EXSR      MULTAR
196300980515     C** MINIMO E MASSIMO
196400980515     C                   EXSR      MINMAX
196500980515     C**
196600990923     C     TASPOR        ADD       TASINL        T0100            15 3
196700990923     C     WPOR          ADD       WINL          W0100            15 3
196800980515     C** PRENDO DELLE 2 LA TARIFFA PIU' ALTA
196900980512     C     W0100         IFGT      T0100
197000980512     C                   Z-ADD     WPOR          TASPOR
197100980512     C                   Z-ADD     WINL          TASINL
197200980512     C                   ENDIF
197300980513     C                   ENDIF
197400980512     C**
197500980512     C                   ENDSR
197600980515     C*****************************************************************
197700980515     C** MINIMO E MASSIMO DA APPLICARE -SE APPLICATO TUTTO NEL PORTO
197800980515     C*****************************************************************
197900980515     C     MINMAX        BEGSR
198000980515     C** NON ESISTE MINIMO E MASSIMO
198100980515     C     TMIN          IFEQ      0
198200980515     C     TMAX          ANDEQ     0
198300980515     C                   GOTO      DOPOMM
198400980515     C                   END
198500980515     C** MINIMO
198600990923     C     WPOR          ADD       WINL          WMAX             15 3
198700980515     C     TMIN          IFGT      WMAX
198800980515     C                   Z-ADD     TMIN          WPOR
198900980515     C                   Z-ADD     0             WINL
199000980515     C                   Z-ADD     TMIN          WMAX
199100980515     C                   END
199200980515     C** MASSIMO
199300980515     C     TMAX          IFGT      0
199400980515     C     WMAX          IFGT      TMAX
199500980515     C                   Z-ADD     TMAX          WPOR
199600980515     C                   Z-ADD     0             WINL
199700980515     C                   END
199800980515     C                   END
199900980515     C     DOPOMM        ENDSR
200000980429     C*****************************************************************
200100980429     C* CERCO VARIA ISOLA LOCALITA' DISAGIATA CENTRI STORICI
200200090922     c* cerco anche la maggiorazione
200300980429     C*****************************************************************
200400980429     C     CERISO        BEGSR
200500980429     C                   CLEAR                   VAISO
200600060414     C                   CLEAR                   VATSP
200700140204      * vaaggfuel varie che vengono sommate solo all'imponibile fuel e supplemento carburante
200800140204     C                   CLEAR                   VAAGGFUEL
200900980429     C     *LIKE         DEFINE    TASVA1        VAISO
201000060414     C     *LIKE         DEFINE    TASVA1        VATSP
201100140204     C     *LIKE         DEFINE    TASVA1        VAAGGFUEL
201200981020     C                   Z-ADD     1             CC                3 0
201300980429     C     §$2VRJ        LOOKUP    SV(CC)                                 96     ISOLA
201400980429     C   96              ADD       VA(CC)        VAISO
201500981008     C                   Z-ADD     1             CC
201600980429     C     §$2VRK        LOOKUP    SV(CC)                                 96     DISAGIATE
201700980429     C   96              ADD       VA(CC)        VAISO
201800981008     C                   Z-ADD     1             CC
201900980429     C     §$2VRQ        LOOKUP    SV(CC)                                 96     C.STORICI
202000980429     C   96              ADD       VA(CC)        VAISO
202100060414     C                   Z-ADD     1             CC                3 0
202200090923     c* "E" o "H"
202300090923     c                   if        WvariaMag<>' '
202400140204     C     WVariaMag     LOOKUP    SV(CC)                                 96     maggiorazione e o h
202500060414     C   96              z-add     VA(CC)        VAtsp
202600090923     c                   endif
202700140204      * CALCOLO LE VARIE  AGGIUNTIVE AL SOLO IMPONIBILE FUEL
202800140204     C                   Z-ADD     1             CC                3 0
202900140204     C     '='           LOOKUP    SV(CC)                                 96     RESO BANCALI
203000140204     C   96              ADD       VA(CC)        VAAGGFUEL
203100140204     C                   Z-ADD     1             CC                3 0
203200140204     C     'c'           LOOKUP    SV(CC)                                 96     LASCIATO AVVISO
203300140204     C   96              ADD       VA(CC)        VAAGGFUEL
203400140204     C                   Z-ADD     1             CC                3 0
203500140204     C     'A'           LOOKUP    SV(CC)                                 96     APPUNTAMENTO
203600140204     C   96              ADD       VA(CC)        VAAGGFUEL
203700140204     C                   Z-ADD     1             CC                3 0
203800140204     C     'B'           LOOKUP    SV(CC)                                 96     CONSEGNA DDT
203900140204     C   96              ADD       VA(CC)        VAAGGFUEL
204000140204     C                   Z-ADD     1             CC                3 0
204100140204     C     'C'           LOOKUP    SV(CC)                                 96     FACCH. ARR.
204200140204     C   96              ADD       VA(CC)        VAAGGFUEL
204300140204     C                   Z-ADD     1             CC                3 0
204400140204     C     'F'           LOOKUP    SV(CC)                                 96     FUORI MISURA
204500140204     C   96              ADD       VA(CC)        VAAGGFUEL
204600140204     C                   Z-ADD     1             CC                3 0
204700140204     C     'H'           LOOKUP    SV(CC)                                 96     DIRITTO FISSO
204800140204     C   96              ADD       VA(CC)        VAAGGFUEL
204900140204     C                   Z-ADD     1             CC                3 0
205000140204     C     'I'           LOOKUP    SV(CC)                                 96     SPESE DI GIACENZA
205100140204     C   96              ADD       VA(CC)        VAAGGFUEL
205200140204     C                   Z-ADD     1             CC                3 0
205300140204     C     'N'           LOOKUP    SV(CC)                                 96     ANTEPORTO
205400140204     C   96              ADD       VA(CC)        VAAGGFUEL
205500140204     C                   Z-ADD     1             CC                3 0
205600140204     C     'P'           LOOKUP    SV(CC)                                 96     AI PIANI
205700140204     C   96              ADD       VA(CC)        VAAGGFUEL
205800140204     C                   Z-ADD     1             CC                3 0
205900140204     C     'S'           LOOKUP    SV(CC)                                 96     SUPERMERCATO
206000140204     C   96              ADD       VA(CC)        VAAGGFUEL
206100140204     C                   Z-ADD     1             CC                3 0
206200140204     C     'U'           LOOKUP    SV(CC)                                 96     RITIRO
206300140204     C   96              ADD       VA(CC)        VAAGGFUEL
206400140204     C                   Z-ADD     1             CC                3 0
206500140204     C     'X'           LOOKUP    SV(CC)                                 96     CONS.DISAG.
206600140204     C   96              ADD       VA(CC)        VAAGGFUEL
206700151209     C                   Z-ADD     1             CC                3 0
206800151209     C     'p'           LOOKUP    SV(CC)                                 96     PIN CODE
206900151209     C   96              ADD       VA(CC)        VAAGGFUEL
206901170227     C                   Z-ADD     1             CC                3 0
207100151209     C     'z'           LOOKUP    SV(CC)                                 96     EXPO
207200151209     C   96              ADD       VA(CC)        VAAGGFUEL
207301170227     C                   Z-ADD     1             CC                3 0
207400151209     C     't'           LOOKUP    SV(CC)                                 96     Diritto Pren.ORM
207500151209     C   96              ADD       VA(CC)        VAAGGFUEL
207601170227     C                   Z-ADD     1             CC                3 0
207700151209     C     'k'           LOOKUP    SV(CC)                                 96     Packing List
207800151209     C   96              ADD       VA(CC)        VAAGGFUEL
207900151209     c
208000140204     c
208100980429     C                   ENDSR
208200130124     C*****************************************************************
208300130124     C* CERCO VARIA ISOLA LOCALITA' DISAGIATA CENTRI STORICI INOLTRO
208400130124     c* MAGGIORAZIONI PER CALCOLO DIRETTO DEL SOLO FUEL
208500130124     C*****************************************************************
208600130124     C     CER_IMPFUEL   BEGSR
208700130124     C                   CLEAR                   IMP_SCA
208800130124     C                   Z-ADD     1             CC                3 0
208900130124     C     §$2VRJ        LOOKUP    SV(CC)                                 96     ISOLA
209000130124     C   96              ADD       VA(CC)        IMP_SCA
209100130124     C                   Z-ADD     1             CC
209200130124     C     §$2VRK        LOOKUP    SV(CC)                                 96     DISAGIATE
209300130124     C   96              ADD       VA(CC)        IMP_SCA
209400130124     C                   Z-ADD     1             CC
209500130124     C     §$2VRQ        LOOKUP    SV(CC)                                 96     C.STORICI
209600130124     C   96              ADD       VA(CC)        IMP_SCA
209700130124     C                   Z-ADD     1             CC
209800130124     C     §$2FIN        LOOKUP    SV(CC)                                 96     INOLTRO
209900130124     C   96              ADD       VA(CC)        IMP_SCA
210000130124     C                   Z-ADD     1             CC                3 0
210100130124     C     §$2TSP        LOOKUP    SV(CC)                                 96     ESPRESSO
210200130124     C   96              ADD       VA(CC)        IMP_SCA
210300130124     C                   Z-ADD     1             CC                3 0
210400130124     C     §$2TSH        LOOKUP    SV(CC)                                 96     H 10,30
210500130124     C   96              ADD       VA(CC)        IMP_SCA
210600130125      *
210700130125     c                   add       taspor        imp_sca
210800140204      * se data fattura > 20140130 aggiungo nuove varia all'imponibile fuel
210900140204     c                   If        tasdtfat > 20140130
211000140204     C                   Z-ADD     1             CC                3 0
211100140204     C     '='           LOOKUP    SV(CC)                                 96     RESO BANCALI
211200140204     C   96              ADD       VA(CC)        imp_sca
211300140204     C                   Z-ADD     1             CC                3 0
211400140204     C     'c'           LOOKUP    SV(CC)                                 96     LASCIATO AVVISO
211500140204     C   96              ADD       VA(CC)        imp_sca
211600140204     C                   Z-ADD     1             CC                3 0
211700140204     C     'A'           LOOKUP    SV(CC)                                 96     APPUNTAMENTO
211800140204     C   96              ADD       VA(CC)        imp_sca
211900140204     C                   Z-ADD     1             CC                3 0
212000140204     C     'B'           LOOKUP    SV(CC)                                 96     CONSEGNA DDT
212100140204     C   96              ADD       VA(CC)        imp_sca
212200140204     C                   Z-ADD     1             CC                3 0
212300140204     C     'C'           LOOKUP    SV(CC)                                 96     FACCH. ARR.
212400140204     C   96              ADD       VA(CC)        imp_sca
212500140204     C                   Z-ADD     1             CC                3 0
212600140204     C     'F'           LOOKUP    SV(CC)                                 96     FUORI MISURA
212700140204     C   96              ADD       VA(CC)        imp_sca
212800140204     C                   Z-ADD     1             CC                3 0
212900140204     C     'H'           LOOKUP    SV(CC)                                 96     DIRITTO FISSO
213000140204     C   96              ADD       VA(CC)        imp_sca
213100140204     C                   Z-ADD     1             CC                3 0
213200140204     C     'I'           LOOKUP    SV(CC)                                 96     SPESE DI GIACENZA
213300140204     C   96              ADD       VA(CC)        imp_sca
213400140204     C                   Z-ADD     1             CC                3 0
213500140204     C     'N'           LOOKUP    SV(CC)                                 96     ANTEPORTO
213600140204     C   96              ADD       VA(CC)        imp_sca
213700140204     C                   Z-ADD     1             CC                3 0
213800140204     C     'P'           LOOKUP    SV(CC)                                 96     AI PIANI
213900140204     C   96              ADD       VA(CC)        imp_sca
214000140204     C                   Z-ADD     1             CC                3 0
214100140204     C     'S'           LOOKUP    SV(CC)                                 96     SUPERMERCATO
214200140204     C   96              ADD       VA(CC)        imp_sca
214300140204     C                   Z-ADD     1             CC                3 0
214400140204     C     'U'           LOOKUP    SV(CC)                                 96     RITIRO
214500140204     C   96              ADD       VA(CC)        imp_sca
214600140204     C                   Z-ADD     1             CC                3 0
214700140204     C     'X'           LOOKUP    SV(CC)                                 96     CONS.DISAG.
214800140204     C   96              ADD       VA(CC)        imp_sca
214801170303     C                   Z-ADD     1             CC                3 0
214802170303     C     'p'           LOOKUP    SV(CC)                                 96     PIN CODE
214803170303     C   96              ADD       VA(CC)        imp_sca
214804170303     C                   Z-ADD     1             CC                3 0
214805170303     C     'z'           LOOKUP    SV(CC)                                 96     EXPO
214806170303     C   96              ADD       VA(CC)        imp_sca
214807170303     C                   Z-ADD     1             CC                3 0
214808170303     C     't'           LOOKUP    SV(CC)                                 96     Diritto Pren.ORM
214809170303     C   96              ADD       VA(CC)        imp_sca
214810170303     C                   Z-ADD     1             CC                3 0
214811170303     C     'k'           LOOKUP    SV(CC)                                 96     Packing List
214812170303     C   96              ADD       VA(CC)        imp_sca
214813170303     c
214900140204     c
215000140204     c                   endif
215100130124     c
215200130124     C                   ENDSR
215300060905     C*****************************************************************
215400060905     C* Fuel Surcharge
215500060905     C*****************************************************************
215600060905     c     SR_fuel       BEGSR
215700060906     C                   CLEAR                   SCA_PB
215800060906     C                   CLEAR                   SCA_SPE
215900060906     C                   CLEAR                   SCA_SCA
216000130124      * Se al programma vengono passate le date aggiuntive per la ritassazione
216100130124      * imposto come data di spedizione quella passata se maggiore di zero
216200130124     c                   clear                   Sav_dsp
216300130124     c                   If        Ritass = 'S' and tasdtfue > 0
216400130124     c                   eval      Sav_dsp = Tasdsp
216500130124     c                   movel     tasdtfue      tasdsp
216600130124     c                   endif
216700060905      *
216800060907      * verifico se esiste già la varia non la calcolo
216900060905      *
217000060905     c     §$2VFS        lookup    sv                                     96
217100060907     c                   IF        %found
217200060907     c                   goto      Endfuel
217300060907     c                   endif
217400060905
217500060905      * verifico se cliente ha in tariffa il calcolo del FUEL Surcharge
217600060907      * altrimenti non vado a cercare nella cartello e vado a fine
217700060906     C                   MOVEL     'f'           FISO
217800060906     C                   MOVEL     'f'           WFISO
217900060905
218000060906     c                   z-add     1             a
218100060906     c     Wfiso         Lookup    Cp(a)                                  05
218200060906     C  N05              GOTO      ENDFUEL
218300060906      * aggancio la ds della tabella 1P
218400060906     c     A             Occur     DS1P
218500060906
218600060905     C     KISOT         CHAIN     TITPT01L
218700060907      * cliente non ha la tariffa  non faccio calcolo
218800060907     C                   IF        not %FOUND(TITPT01L)
218900060907     c                   goto      Endfuel
219000060907     c                   endif
219100140227      * ds x recupero % minima applicabile
219200140227     c                   eval      DTPT01 = TPTflo
219300140227      *
219400140228      * data prezzo base maggiore della data spedizone non faccio calcolo
219500060907     C                   IF        TPTDPB > TASDSP
219600060907     c                   goto      Endfuel
219700060907     c                   endif
219800140227      * Ricerco il valore minimo applicabile e lo scaglione corrispondente in vigore
219900140227      * alla data spedizione
220000150217      * solo se in tariffa non è stato specificato che non si deve applicare il VMA
220100150217
220200150217     c                   If        §TPTvma = 'N'
220300150217     c                   clear                   dpbvma
220400150217     c                   clear                   dpbsvm
220500150217     c                   else
220600140227
220700150217      * si applicazione VMA
220800140227     C     TASDSP        SETLL     TIDPB01L
220900140227     C                   READ      TIDPB01L
221000140227     c                   if        not %found(tidpb01l)
221100140227     c                   clear                   dpbvma
221200140227     c                   clear                   dpbsvm
221300140227     c                   endif
221400150217     c                   endif
221500060906
221600060905      * Ricerco scaglione di appartenenza del prezzo base
221700060905
221800060911     C     TPTDPB        SETLL     TIPMG01L
221900060906     C                   READ      TIPMG01L
222000060906     C                   IF        NOT %EOF(TIPMG01L)
222100060906      *
222200060906     C                   Z-ADD     PMGSCA        SCA_PB
222300060906     C                   ENDIF
222400060906
222500060906      * Ricerco scaglione di appartenenza del prezzo del gasolio al momento della spedizione
222600060906
222700060911     C     TASDSP        SETLL     TIPMG01L
222800060906     C                   READ      TIPMG01L
222900060906     C                   IF        NOT %EOF(TIPMG01L)
223000060906      *
223100080909      * recupero il prezzo per ricercare il F.D. in tariffa
223200080909      *
223300080909     c                   z-add     pmgpmg        PMG_SGL
223400080909
223500060906     C                   Z-ADD     PMGSCA        SCA_SPE
223600060906     C                   ENDIF
223700140227      * verifico se il prezzo del gasolio al momento della spedizione è minore del valore minimo
223800140227      * applicabile sostituisco il prezzo del gasolio e il suo scaglione con il valore minimo
223900140227      * applicabile e lo scaglione corrispondente
224000140227
224100140227     c                   if        pmgpmg < dpbvma
224200140227     c                   z-add     dpbvma        PMG_SGL
224300140227     c                   z-add     dpbsvm        SCA_SPE
224400140227     c                   Endif
224500140227
224600060906
224700060906      * calcolo gli scaglioni di aumento
224800060906
224900060906     C                   EVAL      SCA_SCA = SCA_SPE - SCA_PB
225000060906     C                   IF        SCA_SCA < 0
225100060906     C                   CLEAR                   SCA_SCA
225200060906     C                   ENDIF
225300140228      * calcolo dell'importo da addebitare al cliente
225400140228
225500140228     C                   CLEAR                   VARIA
225600140228     C                   CLEAR                   IMP_SCA
225700140228     C                   CLEAR                   VARIAfuel_min
225800140228      * calcolo imponibile FUEL
225900140228
226000140228      * se sono diretta per il solo calcolo del Fuel(principalmente da ritassazione)
226100140228      * mi calcolo imponibile in maniera diversa dalla normale tassazione
226200140228     C                   If        tassva = §$2VFS
226300140228     c                   exsr      Cer_impfuel
226400140228     c                   else
226500140228     C                   EXSR      CERISO
226600140228     c                   If        tasdtfat <= 20140130
226700140228     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP
226800140228     c                   else
226900140228      * recupero importi di isola e località disagiate ed espresso + nuove varie introdotte
227000140228      * dal 3/2/2014 dv 2473
227100140228     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
227200140228     c                                       VAAGGFUEL
227300140228     c                   endif
227400140228     c                   endif
227500140228      *
227600060906
227700060906      * se gli scaglioni scattati sono maggiore di zero cerco l'incidenza percentuale di
227800060906      * aumento nel dettaglio tariffario
227900060906     C                   IF        SCA_SCA > 0
228000080909      * cambia con il progetto 666 aumento supplemento carburante in tariffa
228100080909      * per cercare il Fattore demoltiplicatore non usiamo più il numero di scaglioni
228200080909      * di aumento ma il prezzo medio del gasolio della settimana della data di spedizione
228300080909     C****               Z-ADD     SCA_SCA       TPDPES
228400080909     C                   Z-ADD     PMG_SGL       TPDPES
228500060906     C                   EXSR      CALACD
228600060907      * Se la percentuale di incidenza è maggiore di zero
228700060907    1C                   IF        AITR > 0
228800060905      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
228900060905      *                     per abbandonare il concetto di monete diverse dall'EURO)
229000060907     C                   EVAL(H)   VARIA = (IMP_SCA * aitr * sca_sca) / 100
229100140227
229200140227     C                   ENDIF
229300140228     C                   ENDIF
229400140227      * anche se il cliente ha negato la tariffa verifico
229500140227      * se nella tariffa particolare del cliente è stato espressa la % minima di apllicazione
229600140227      * calcolo l'importo del fuel utilizzando questa percentuale
229700140227
229800140227     c                   if        §tptpma > 0
229900140227     c                   eval(H)   variafuel_min =  (IMP_SCA * §TPTpma)/ 100
230000140227     c                   Endif
230100140227
230200140227      * prendo il valore della varia maggiore tra quella calcolata con la % minima applicabile
230300140227      * e quella calcolata con il FD della tariffa
230400140227
230500140227     c                   if        variafuel_min > Varia
230600140227     c                   eval      Varia =  variafuel_min
230700140227     c                   endif
230800140227
230900060905      **
231000140227    1c                   If        Varia > 0
231100060905     C                   Z-ADD     1             A
231200060905     C     ' '           LOOKUP    SV(A)                                  96
231300060907     C   96              MOVEL     §$2VFS        SV(A)
231400060905     C   96              Z-ADD     VARIA         VA(A)
231500060905     C  N96              ADD       VARIA         TASPOR
231600060905    1C                   ENDIF
231700060905      *
231800140228
231900130124     c     endfuel       TAG
232000060905      *
232100130124      * se impostato il campo del salvataggio della data spedizione
232200130124     c                   if        Sav_dsp <> *zeros
232300130124     c                   eval      tasdsp = Sav_dsp
232400130124     c                   clear                   Sav_dsp
232500130124     c                   endif
232600130124
232700130124     C                   ENDSR
232800040506     C*****************************************************************
232900060905     C* SUPPLEMENTO CARBURANTE FEDEX
233000040506     C*****************************************************************
233100060905     C     SR_SUPCARB    BEGSR
233200040507      *
233300040507      * verifico se esiste già la varia
233400040507      *
233500040507     c     §$2VSC        lookup    sv                                     96
233600040507     c                   IF        not %found
233700040506
233800040506      * RICERCO IN BASE ALLA DATA SPEDIZIONE LA GIUSTA PERCENTUALE DI
233900040506      * SUPPLEMNENTO
234000130124      * Se al programma vengono passate le date aggiuntive per la ritassazione
234100130124      * imposto come data di spedizione quella passata se maggiore di zero
234200130124     c                   clear                   Sav_dsp
234300130124     c                   If        Ritass = 'S' and tasdtfue > 0
234400130124     c                   eval      Sav_dsp = Tasdsp
234500130124     c                   movel     tasdtfue      tasdsp
234600130124     c                   endif
234700130124      *
234800040506
234900040507     C                   CLEAR                   VARIA
235000040507     C                   CLEAR                   IMP_SCA
235100040507     C                   CLEAR                   PER_SCA
235200040507     C                   Z-ADD     1             KX
235300040507    1c                   DO        *HIVAL
235400040507      * SCHIERA IN ORDINE DISCENDENTE
235500040507     C                   MOVE      DSCA(KX)      WDSPSC
235600040507      * SE DATA UGUALE A ZERO VADO A FINE
235700040507    2C                   IF        DS_DSC = *ZEROS
235800040507     C                   LEAVE
235900040507    2C                   ENDIF
236000040507      * SE DATA SPEDIZIONE MAGGIORE DI QUELLA IN SCHIERA RECUPERO LA PERCENTUALE DI CALCOLO
236100040510    2C                   IF        TASDSP >=  DS_DSC
236200040507     c                   z-add     DS_psc        PER_SCA
236300040507     C                   LEAVE
236400040507     C                   ELSE
236500040507      * SE DATA MINORE LEGGO LA DATA SUCCESSIVA
236600040507    3C                   IF        KX < 999
236700040507     C                   ADD       1             KX
236800040507     C                   ELSE
236900040507     C                   LEAVE
237000040507    3C                   ENDIF
237100040507    2C                   ENDIF
237200040507
237300040507    1C                   ENDDO
237400040507      * SE PERCENTUALE MAGGIORE DI ZERO ESEGUO IL CALCOLO
237500040507    1C                   IF        PER_SCA > 0
237600040507      * CALCOLO L'IMPONIBILE
237700060414      * recupero importi di isola e località disagiate ed espresso
237800040507     C                   EXSR      CERISO
237900040507      *
238000060414     c* Di fatto qui l'espresso non ci sarà mai!!! bolla estera
238100140204      *
238200140204     c                   If        tasdtfat <= 20140130
238300060414     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP
238400140204     c                   else
238500140204     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
238600140204     c                             vaaggfuel
238700140204     c                   Endif
238800040507      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
238900040507      *                     per abbandonare il concetto di monete diverse dall'EURO)
239000040507     C                   EVAL(H)   VARIA = (IMP_SCA * PER_SCA) / 100
239100040507      **
239200040507     C                   Z-ADD     1             A
239300040507     C     ' '           LOOKUP    SV(A)                                  96
239400040507     C   96              MOVEL     §$2VSC        SV(A)
239500040507     C   96              Z-ADD     VARIA         VA(A)
239600040507     C  N96              ADD       VARIA         TASPOR
239700040507    1C                   ENDIF
239800130124      *
239900130124      * se impostato il campo del salvataggio della data spedizione
240000130124     c                   if        Sav_dsp <> *zeros
240100130124     c                   eval      tasdsp = Sav_dsp
240200130124     c                   clear                   Sav_dsp
240300130124     c                   endif
240400130124      *
240500130124     C                   ENDIF
240600040507      *
240700040507     C                   ENDSR
240800970521     C*****************************************************************
240900970521     C* TASSAZIONE TARIFFE PARTICOLARI
241000970521     C*****************************************************************
241100970521     C     TASPAR        BEGSR
241200970521     C     *LIKE         DEFINE    TASVA1        TASCP
241300970522     C     *LIKE         DEFINE    TASVA1        TASISO
241400970521     C     *LIKE         DEFINE    TASTC1        TASTC
241500970721     C                   CLEAR                   TASCP
241600970721     C                   CLEAR                   WNOEST
241700970721     C* SE NON ESISTE LA SIGLA DELLA VARIA O E' GIA' IN TASSAZIONE
241800970721     C*  NON RITASSO NE APPLICO L'ESPRESSO
241900970521     C**
242000970521     C                   Z-ADD     1             A
242100970521     C     TASTC         LOOKUP    CP(A)                                  05
242200970521     C  N05              GOTO      ENDPAR
242300970521     C**
242400970521     C     A             OCCUR     DS1P
242500970521     C                   Z-ADD     1             A
242600970521     C     §1PVAR        LOOKUP    SV(A)                                  05
242700970521     C* SIGLA VARIA NON PRESENTE --> CALCOLO
242800970521     C     *IN05         IFEQ      *OFF
242900970721     C                   MOVEL     'S'           WNOEST            1
243000970521     C                   EXSR      CALCP
243100970521     C**
243200970521     C     TASCP         IFGT      0
243300130927      * se sto tassando la tariffa particolare m = avviso affidamento spedizione
243400130927      * ed il flag EMD è uguale a "E" (Sia sms che mail) raddoppio la varia
243500130927     c                   if        §1pvar = §$2VAD and
243600130927     c                             §asemd = 'E'
243700130927     c                   eval      tascp = tascp * 2
243800130927     c                   endif
243900130927      *
244000970521     C                   Z-ADD     1             A
244100970521     C     ' '           LOOKUP    SV(A)                                  96
244200970521     C   96              MOVEL     §1PVAR        SV(A)
244300970521     C   96              Z-ADD     TASCP         VA(A)
244400970521     C  N96              ADD       TASCP         TASPOR
244500970521     C                   END
244600970521     C                   END
244700970521     C     ENDPAR        ENDSR
244800941112     C******************************************************
244900941107     C** CALCOLO PESO TASSABILE PER TARIFFA A PESO
245000941112     C******************************************************
245100941107     C     CALPT         BEGSR
245200980313     C** MI MEMORIZZO SU BLTAS IL PESO TASSABILE SE
245300980313     C** IL PESO E' SUPERIORE AL MINIMO TASSABILE
245400980313     C** SE ESISTE IL RAPPORTO PESO VOLUME IN TARIFFA  E VOL IN BOLLA
245500980313     C** IL PESO TASSABILE SU BLTAS E' SEMPRE ARROTONDATO AL KG SUPERIO
245600980311     C                   CLEAR                   TASPVL
245700050912     C***  UNOCTR        IFEQ      0
245800050912     C**** UNOCTR        OREQ      3
245900050912      * modifico --- il peso tassabile si stampa e si calcola nei seguenti casi :
246000050912      * Tariffa a quintale "0" il peso da fatturare maggiore del  minimo tassabile
246100050912      *                        (definito nella tabella TR = 100) oppure volume da fatturare
246200050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
246300050912      *                        tariffa maggiore di zero
246400050912      * Tariffa a kg       "3" non controllo il peso da fatturare maggiore del minimo tassabile
246500050912      *                        (definito nella tabella TR = 0,1) ma solo volume da fatturare
246600050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
246700050912      *                        tariffa maggiore di zero
246800050912     C***  TARIFFA A QUINTALE
246900050912
247000050912     C     UNOCTR        IFEQ      0
247100050912
247200020826     C     USAPKF        IFGT      MINTAS
247300950901     C     PESTAS        ADD       0,9           TASPVL
247400941107     C                   ELSE
247500941107     C     TASVLF        IFNE      0
247600980311     C     RAPPV         ANDNE     0
247700950901     C     PESTAS        ADD       0,9           TASPVL
247800980313     C                   ENDIF
247900980313     C                   ENDIF
248000980313     C                   ENDIF
248100050912
248200050912     C***  TARIFFA A KG
248300050912
248400050912     C     UNOCTR        IFEQ      3
248500050912
248600050912     C     TASVLF        IFNE      0
248700050912     C     RAPPV         ANDNE     0
248800050912     C     PESTAS        ADD       0,9           TASPVL
248900050912     C                   ENDIF
249000050912     C                   ENDIF
249100050912
249200980313     C                   ENDSR
249300950119     C******************************************************
249400950119     C** CALCOLA VALORE DA ASSICURARE PER TARIFFA A VALORE
249500950119     C******************************************************
249600950119     C     CALVAS        BEGSR
249700950119     C     *LIKE         DEFINE    TASIAS        IMPVAS
249800950119     C                   Z-ADD     0             IMPVAS
249900950119     C                   MOVEL     'C'           FISO
250000990722     C                   MOVEL     'C'           WFISO             1
250100950119     C**FLAG ASSICURAZIONE X CONTO = C
250200950119     C                   Z-ADD     1             A
250300990722     C     WFISO         LOOKUP    CP(A)                                  05
250400950119     C  N05              GOTO      DOPVA1
250500950119     C**----------------------------------------
250600990721     C     KISOT         CHAIN     TITPT01L                           28
250700980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
250800980528     C     *IN28         IFEQ      *ON
250900980528     C     TPTATB        ORNE      ' '
251000980528     C                   GOTO      DOPVA1
251100980528     C                   ENDIF
251200980528     C**
251300950119     C** ESCLUDO GLI ANNULLATI
251400950119     C     TPTFVM        IFEQ      '3'
251500020826     C     TPTVLM        MULT      USAPKF        IMPVAS
251600950119     C                   END
251700950119     C*PESO
251800950119     C     TPTFVM        IFEQ      '1'
251900950119     C     TPTVLM        MULT      TASNCL        IMPVAS
252000950119     C                   END
252100950119     C* COLLI
252200950119     C     TPTFVM        IFEQ      '2'
252300950119     C                   Z-ADD     TPTVLM        IMPVAS
252400950119     C                   END
252500950119     C*SPEDIZIONE
252600950119     C     DOPVA1        ENDSR
252700941112     C******************************************************
252800941107     C** CALCOLA VARIA R  --- ASSICURAZIONE PER CONTO
252900941112     C******************************************************
253000941107     C     CALVR         BEGSR
253100050525     c* Il falg task88 cambia significato:indica se bolla con mandato o no
253200050525     c*
253300050525     c* con mandato significa: x mandato a val variabile--> tutte le bolle
253400050525     c*                        x mandato a val.indicato --> solo bolle con
253500050525     c*                        tasias=0 o con tasias <=al valore del mandat
253600050525     c*
253700050525     c* Imposto task88=N se mandato
253800050525     c* Imposto task88=S se no mandato ma tasias>0
253900050525     c* Imposto task88=X per bolle senza mandato e senza importo
254000050525     c
254100050525     C                   MOVEL     'X'           TASK88
254200980603     C** 88 = SI CALCOLA SOLO SE ESISTE IL VALORE MERCE IN BOLLA
254300941112     C     KSCCOD        IFEQ      8888
254400980527     C     KSCCOD        OREQ      9999
254500980709     C     TASIAS        IFEQ      0
254600050525     C**                 MOVEL     'S'           TASK88
254700980709     C                   GOTO      ENDVR
254800941112     C                   END
254900980709     C                   END
255000980603     C**
255100941116     C                   SETOFF                                       94
255200941116     C     *LIKE         DEFINE    TASIAS        IASLIT
255300941116     C                   Z-ADD     TASIAS        IASLIT
255400941116     C     IASLIT        IFNE      0
255500941116     C                   Z-ADD     1             K
255600941116     C     TASVAS        LOOKUP    CV(K)                                  05
255700980504     C     *IN05         IFEQ      *OFF
255800980504     C                   SETON                                        94
255900980504     C                   MOVEL     MSG(2)        TASMSG
256000980504     C                   GOTO      FINE
256100980504     C                   ENDIF
256200941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
256300990922     C**
256400990922     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA SE E' DIVERSA
256500990922     C* DALLA VALUTA DELLA TARIFFA
256600990922     C     §TADIV        IFNE      TASVAS
256700990922     C                   CLEAR                   YEUR
256800990922     C                   MOVEL     TASVAS        YECDVI
256900990922     C                   MOVEL     §TADIV        YECDVO
257000990922     C                   Z-ADD     IASLIT        YECIMI
257100011129     C   12              MOVE      '2'           YECDCO
257200991014     C  N12              MOVE      '0'           YECDCO
257300990922     C*
257400990922     C                   CALL      'YEURCO'
257500990922     C                   PARM                    YEUR
257600990922     C*
257700990922     C     YECESI        IFEQ      ' '
257800990922     C                   Z-ADD     YECIMO        IASLIT
257900990922     C                   END
258000990922     C                   ENDIF
258100990922     C                   ENDIF
258200941123     C**----------------------------------------
258300941107     C                   MOVEL     'C'           FISO
258400990722     C                   MOVEL     'C'           WFISO
258500941107     C**FLAG ASSICURAZIONE X CONTO = C
258600941123     C                   Z-ADD     1             A
258700990722     C     WFISO         LOOKUP    CP(A)                                  05
258800941123     C  N05              GOTO      ENDVR
258900020226      * AGGANCIO LA DS DELA TABELLA 1P
259000020226     C     A             OCCUR     DS1P
259100941123     C**----------------------------------------
259200990721     C     KISOT         CHAIN     TITPT01L                           28
259300980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
259400980528     C     *IN28         IFEQ      *ON
259500980528     C     TPTATB        ORNE      *BLANKS
259600980528     C                   GOTO      DOPAC1
259700980528     C                   ENDIF
259800980505     C**
259900980505     C** CONTROLLO IL TIPO APPLICAZIONE DELLA TARIFFA ASSIC X CONTO
260000980505     C**  SE INCONGRUENTE CON TIPO BOLLA PRENDO LA CARTELLO
260100980505     C     TPTAPL        IFEQ      'A'
260200980505     C     TIPBOL        ANDEQ     'F'
260300980505     C     TPTAPL        OREQ      'P'
260400980505     C     TIPBOL        ANDEQ     'A'
260500980505     C                   GOTO      DOPAC1
260600980505     C                   ENDIF
260700980505     C**
260800980504     C* VALORE MERCE = 0 --> ESCLUDO SE NON E' UN VERO MANDATO
260900980505     C                   SETOFF                                       94
261000980504    1C     TPTVLM        IFEQ      0
261100980709     C* CHE SIA UN MANDATO FITTIZIO O UN MANDATO A VALORE VARIABILE
261200980709     C*  DEVO SEMPRE SCRIVERE LA BOLLA COME 8888 PERCHE L'ASSICURAZIONE
261300980709     C*  VUOLE VEDERE BOLLA PER BOLLA QUANTO SONO ASSICURATE
261400050525     C****               MOVEL     'S'           TASK88
261500980709     C**
261600980504    2C     IASLIT        IFEQ      0
261700980504     c* MANDATO FITTIZIO
261800990922    3C     TPTTMA        IFEQ      'F'
261900980504     C                   GOTO      ENDVR
262000980504    3C                   ENDIF
262100050525     c
262200050525     c
262300050525     c* anche se manca tariffa è comunque con mandato
262400050525     c                   eval      task88='N'
262500050525     c*
262600050525     c* non devo dare manca tariffa se tipo bolla che non prevede
262700050525     c* Assicurazione
262800050525     C     TASTBL        LOOKUP    TBN                                    05
262900050525     c                   if        not *in05
263000050526     c                   eval      *in94='1'
263100050525     C                   MOVEL     MSG(4)        TASMSG
263200050525     C                   GOTO      FINE
263300050525     c                   endif
263400050525     c
263500050525   x2c                   else
263600050525     c* Mandato a valore variabile con importo: se fittizio 8888 altriment
263700050525     c* con mandato
263800050525    3C     TPTTMA        IFEQ      'F'
263900050525     c                   eval      task88='S'
264000050525     c                   else
264100050525     c                   eval      task88='N'
264200050525    3C                   END
264300050525    2C                   END
264400050525     c
264500941107     C** MANCA TARIFFA: MANDATO CON VALORE = 0
264600941107     C**                E NON ESISTE VALORE IN BOLLA (SOLO COD.)
264700980504   X1C                   ELSE
264800980504    2C     IASLIT        IFGT      0
264900941112     C                   Z-ADD     0             VALSPE           15 3
265000980504    3C     TPTFVM        IFEQ      '3'
265100020826     C     TPTVLM        MULT      USAPKF        VALSPE
265200980504    3C                   END
265300941107     C*PESO
265400980504    3C     TPTFVM        IFEQ      '1'
265500941107     C     TPTVLM        MULT      TASNCL        VALSPE
265600980504    3C                   END
265700941107     C* COLLI
265800980504    3C     TPTFVM        IFEQ      '2'
265900941107     C                   Z-ADD     TPTVLM        VALSPE
266000980504    3C                   END
266100030908      *
266200030908      * ARROTONDO AL DECIMALE DELLA TARIFFA
266300030908     C                   CLEAR                   YEUR
266400030908     C                   MOVEL     §TADIV        YECDVI
266500030908     C                   MOVEL     §TADIV        YECDVO
266600030908     C                   Z-ADD     VALSPE        YECIMI
266700030908     C                   CALL      'YEURCO'
266800030908     C                   PARM                    YEUR
266900030908     C*
267000030908     C     YECESI        IFEQ      ' '
267100030908     C                   Z-ADD     YECIMO        VALSPE
267200030908     C                   END
267300941107     C*SPEDIZIONE
267400980504    3C     IASLIT        IFGT      VALSPE
267500941107     C                   GOTO      DOPAC1
267600050526     c                   else
267700050526     c
267800050526     c* Se valore rientra nel mandato, imposto che si tratta di mandato
267900050526     C                   MOVEL     'N'           TASK88
268000980504    3C                   END
268100050526     c
268200050526   x2c                   else
268300050526     c* se senza valore in bolla ma madanto con valore -->ok da mandato
268400050526     c                   eval      task88='N'
268500980504    2C                   END
268600980504    1C                   END
268700050526     c
268800980525     C** SE IL CLIENTE E' 8888 HO IMPOSTATO PRIMA LA TARIFFA DI CARTELL
268900980525     C     KSCCOD        IFEQ      8888
269000050525     C                   MOVEL     'S'           TASK88
269100980525     C                   ENDIF
269200050525     c
269300050525     c** Anche se lo considero con mandato, se non c'e' importo e tipo
269400050525     c**  bolla senza assicurazione, non calcolo varia R
269500050525     C     TASIAS        IFEQ      0
269600050525     C     TASTBL        LOOKUP    TBN                                    05
269700050525     C   05              GOTO      ENDVR
269800050525     C                   ENDIF
269900980505     C**
270000941107     C                   GOTO      DOPAC2
270100941107     C     DOPAC1        TAG
270200941107     C** NON ESISTE TARIFFA AXC DEL CLIENTE
270300941121     C*-----------------------------------------------------------------
270400980505     C* SE USATA SI TRATTA DI TARIFFA DI CARTELLO
270500050525     C***                MOVEL     'S'           TASK88
270600941112     C     IASLIT        CABEQ     0             ENDVR
270700050525     c                   eval      task88='S'
270800020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
270900020226     C     KTPTC         CHAIN     TITPT01L                           28
271000020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
271100020226     C     *IN28         IFEQ      *ON
271200020226     C     TPTATB        OREQ      'A'
271300020226     C                   GOTO      ENDVR
271400020226     C                   ENDIF
271500941107     C*-----------------
271600941107     C     DOPAC2        TAG
271700980504     C** SE NON SERVE --> ESCO DAL CALCOLO
271800980504     C** TIPO APPLICAZIONE
271900980504     C** P - SOLO PER FRANCHI
272000980504     C** A - SOLO PER ASSEGNATI
272100980504    1C     TPTAPL        IFNE      *BLANKS
272200980504     C     IASLIT        ANDEQ     0
272300980504    2C     TIPBOL        IFEQ      'F'
272400980504    3C     TPTAPL        IFEQ      'A'
272500980504     C                   GOTO      ENDVR
272600980504    3C                   ENDIF
272700980504   X2C                   ELSE
272800980504    3C     TPTAPL        IFEQ      'P'
272900980504     C                   GOTO      ENDVR
273000980504    3C                   ENDIF
273100980504    2C                   END
273200980504    1C                   END
273300980504     C**
273400941112     C     *LIKE         DEFINE    TASIAS        VALVR
273500941112     C     *LIKE         DEFINE    TASIAS        ASCPES
273600941112     C     *LIKE         DEFINE    TASIAS        ASCPE2
273700941112     C                   Z-ADD     0             VALVR
273800980526    1C     IASLIT        IFEQ      0
273900980526    2C     TPTFVM        IFEQ      '3'
274000020826     C     USAPKF        MULT      TPTVLM        VALVR
274100980526    2C                   END
274200980526    2C     TPTFVM        IFEQ      '1'
274300941107     C     TASNCL        MULT      TPTVLM        VALVR
274400980526    2C                   END
274500980526    2C     TPTFVM        IFEQ      '2'
274600941107     C                   Z-ADD     TPTVLM        VALVR
274700980526    2C                   END
274800980526   X1C                   ELSE
274900941112     C                   Z-ADD     IASLIT        VALVR
275000980526    1C                   END
275100011119     C***** OBSOLETO NON RESTITUISCO + IL VALORE IN LIRE MA NELLA DIVISA DELLA TARIFFA
275200011129     C***** PERO' ARROTONDO AL DECIMALE DELLA TARIFFA
275300011129     C                   CLEAR                   YEUR
275400011129     C                   MOVEL     §TADIV        YECDVI
275500011129     C                   MOVEL     §TADIV        YECDVO
275600011129     C                   Z-ADD     VALVR         YECIMI
275700011129     C                   CALL      'YEURCO'
275800011129     C                   PARM                    YEUR
275900011129     C*
276000011129     C     YECESI        IFEQ      ' '
276100011129     C                   Z-ADD     YECIMO        VALVR
276200011129     C                   END
276300011129     C* RESTITUISCO IN OUTPUT L'IMPORTO DA ASSICURARE
276400011129     C                   Z-ADD     VALVR         TASIAL
276500941107     C** VALORE SPEDIZIONE PER CALCOLO ASSICURAZIONE
276600980312     C**
276700980526     C** CALCOLO SOLO SE NON HO IMPOSTATO IL FLAG
276800980526    1C     PARCA         IFEQ      ' '
276900980505     C**
277000980505     C** SE C'E' GIA' LA VARIA R NON LA RICALCOLO
277100980505     C                   Z-ADD     1             A
277200980505     C     §$2VRR        LOOKUP    SV(A)                                  05
277300980526    2C     *IN05         IFEQ      *OFF
277400980312     C                   MOVEL     TPTTPG        WTPG
277500980312     C                   MOVEL     TPTRPV        WRPV
277600980313     C                   CLEAR                   WDET
277700980313     C                   Z-ADD     TPTARL        WARL
277800980313     C                   Z-ADD     TPTARF        WARF
277900980313     C                   Z-ADD     TPTARO        WARO
278000980312     C                   EXSR      CALCOL
278100980312     C**
278200980312     C** A VALORE
278300980526    3C     TPTTPG        IFEQ      'V'
278400980312     C                   Z-ADD     VALVR         ISOPES
278500980526    3C                   END
278600980312     C     *LIKE         DEFINE    TASIAS        TPDPES
278700980504     C**
278800980504     C**CALCOLO IMPORTO ASSICURAZIONE PER CONT
278900980504     C                   Z-ADD     ISOPES        TPDPES
279000941107     C                   EXSR      CALACD
279100980525     C                   Z-ADD     VARIA         WVARIR
279200980526    2C                   ENDIF
279300980526    1C                   ENDIF
279400980504     C     ENDVR         ENDSR
279500060320     C******************************************************
279600060320     C** CALCOLA VARIA d  --- A C BASE
279700060320     C******************************************************
279800060321     c     CALVACB       BEGSR
279900060320     c* Il falg task88 cambia significato:indica se bolla con mandato o no
280000060320     c* Imposto task88=N se mandato
280100060321     c* Lascio  task88=X per bolle senza mandato e senza importo
280200060321
280300060321      **----------------------------------------
280400060321     c                   movel     'd'           Fiso
280500060321     c                   movel     'd'           WFiso
280600060321      ** Flag A C BASE  = d
280700060321     c                   z-add     1             a
280800060321     c     Wfiso         Lookup    Cp(a)                                  05
280900060321     c  N05              Goto      EndVacb
281000060321      * aggancio la ds della tabella 1P
281100060321     c     A             Occur     DS1P
281200060321      **----------------------------------------
281300060321     c     Kisot         Chain     TITPT01L
281400060321      ** esiste Tariffa A C base
281500060321    1c                   If        %found(titpt01l)
281600060321      **
281700060321      ** Controllo il tipo applicazione della tariffa A C Base
281800060321      **
281900060321    2c                   If        (tptapl = 'A' and tipbol = 'A') or
282000060321     c                             (tptapl = 'P' and tipbol = 'F') or
282100060321     c                              tptapl = ' '
282200060321      *
282300060321     c                   eval      task88='N'
282400060321      ** Anche se lo considero con mandato,  tipo
282500060321      **  bolla senza assicurazione, non calcolo varia d
282600060321     c     TAStbl        Lookup    Tbn                                    05
282700060321     c   05              Goto      ENDVacb
282800060321
282900060321      * calcolo valore assicurato
283000060321
283100060321     c     *Like         Define    TASias        VALVacb
283200060321
283300060321     c                   clear                   VALVacb
283400060321
283500060321      * Peso
283600060321    3c                   If        TPTfvm = '3'
283700060321     c     TPTvlm        Mult      USAPkf        Valvacb
283800060321    3c                   Endif
283900060321      * Colli
284000060321    3c                   If        TPTfvm = '1'
284100060321     c     TPTvlm        Mult      TASncl        Valvacb
284200060321    3c                   Endif
284300060321      * Spedizione
284400060321    3c                   If        TPTfvm = '2'
284500060321     c                   Z-add     TPTvlm        Valvacb
284600060321    3c                   Endif
284700060321      *
284800060321      * Arrotondo al decimale della tariffa
284900060321     c                   Clear                   YEUR
285000060321     c                   Movel     §TAdiv        YECdvi
285100060321     c                   Movel     §TAdiv        YECdvo
285200060321     c                   Z-add     VALvacb       YECimi
285300060321     c                   call      'YEURCO'
285400060321     c                   Parm                    YEUR
285500060321      *
285600060321     c                   If        YECesi = ' '
285700060321     c                   Z-add     YECimo        VALvacb
285800060321     c                   Endif
285900060321
286000060321      ** Calcolo solo se non ho impostato il flag
286100060321    3c                   IF        Parca = ' '
286200060321
286300060321      ** Se c'è' già la varia non la ricalcolo
286400060321     c                   Z-add     1             a
286500060321     c     §$2acb        Lookup    sv(a)                                  05
286600060321    4c     *IN05         Ifeq      *off
286700060321     c                   movel     TPTtpg        Wtpg
286800060321     c                   Movel     TPTrpv        Wrpv
286900060321     c                   clear                   Wdet
287000060321     c                   Z-add     TPTarl        Warl
287100060321     c                   Z-add     TPTarf        Warf
287200060321     c                   Z-add     TPTaro        Waro
287300060321     c                   Exsr      Calcol
287400060321
287500060321      ** A Valore
287600060321    5c                   IF        tpttpg =  'V'
287700060321     c                   z-add     valvacb       ISOpes
287800060321    5c                   Endif
287900060321
288000060321     c                   Z-add     ISOpes        TPDpes
288100060321     c                   Exsr      Calacd
288200060321     c                   Z-add     varia         Wvariacb
288300060321    4c                   endif
288400060321
288500060321    3c                   endif
288600060321
288700060321    2c                   endif
288800060321    1c                   Endif
288900060321
289000060321     c     EndVacb       ENDSR
289100941112     C******************************************************
289200980312     C** CALCOLA TARIFFA PARTICOLARE
289300941112     C******************************************************
289400941107     C     CALACD        BEGSR
289500990923     C* CAMPO VARIA CON  DECIMALI
289600990923     C                   Z-ADD     0             VARIA            15 3
289700990923     C* CAMPO VARIA SENZA DECIMALI ALTRIMENTI NON FUNZIONA L'ARROTONDAMENTO
289800990923     C                   Z-ADD     0             VARIAD           15 0
289900990923     C*
290000941107     C                   DO        100           A
290100941122     C     A             OCCUR     ACDS
290200941107     C                   CLEAR                   ACDS
290300941107     C                   END
290400941112     C** PULIZIA DS TARIFFA
290500020412     C*
290600020412     C* SE ESISTE ALMENO UNA RIGA DI DETTAGLIO E NON RIESCO AD APPLICAR
290700020412     C*  QUESTA TAR.PARTICOLARE --> MANCA TARIFFA
290800980605     C* BOLLA IMPORT / ESXPORT
290900980605     C     WBOEST        IFEQ      'E'
291000020226     C     §1PCTE        IFNE      *BLANKS
291100020226     C                   MOVEL     §1PCTE        ISOCTS
291200941112     C                   GOTO      POIAC
291300941112     C                   END
291400941112     C                   ELSE
291500020226     C     §1PCTS        IFNE      *BLANKS
291600020226     C                   MOVEL     §1PCTS        ISOCTS
291700941112     C                   GOTO      POIAC
291800941112     C                   END
291900941112     C                   END
292000941112     C** CODICI TASSAZIONE FISSI DA TABELLA TARIFFE PARTICOLARI
292100941110     C                   MOVEL     TASCTS        ISOCTS            2
292200060414     c
292300060414      * Se assegnato e tariffa FEDEX metto in ISOCTS  quello mittente
292400050124     c                   If        TIPBOL = 'A' and WBOFED = 'S'
292500050124     c                   movel     tasmct        isocts
292600050124     c                   endif
292700060414     c* Se devo utilizzare quello del mittente per via della
292800060414     c*  reversibilità --> lo uso
292900060414     c                   if        applicarevers='S'
293000060414     c                   movel     tasmct        isocts
293100060414     c                   endif
293200050124      *
293300990721     C     KISOD         SETLL     TITPD02L                               28
293400941107     C   28              GOTO      DOPAC3
293500941107     C                   Z-ADD     1             A
293600941107     C     ISOCTS        LOOKUP    CTS(A)                                 05
293700941107     C   05A             OCCUR     DSCT
293800941107     C   05              MOVEL     §CTRAP        ISOCTS
293900941107     C  N05              MOVEL     *BLANKS       ISOCTS
294000941107     C** REGIONE
294100990721     C     KISOD         SETLL     TITPD02L                               28
294200941107     C   28              GOTO      DOPAC3
294300980605     C     WBOEST        IFEQ      'E'
294400941112     C                   MOVEL     'EE'          ISOCTS
294500941112     C                   ELSE
294600941112     C                   MOVEL     'IT'          ISOCTS
294700941112     C                   END
294800941112     C     POIAC         TAG
294900990721     C     KISOD         SETLL     TITPD02L                               28
295000020412     C**N28                GOTO ENDACD
295100020412     C** N 28 --> MANCA TARIFFA
295200020412     C     *IN28         IFEQ      *OFF
295300020412     C* SE NON È PRESENTE NESSUNA RIGA DI DETTAGLIO, NON DO
295400020412     C*  MANCA TARIFFA: SIGNIFICA CHE È INSERITA UNA TASSAZIONE =0
295500020412     C     KISOD2        SETLL     TITPD02L                               28
295600020412     C  N28              GOTO      ENDACD
295700020412     C*
295800020412     C                   SETON                                        94
295900020412     C                   MOVEL     MSG(17)       TASMSG
296000020412     C                   GOTO      FINE
296100020412     C                   ENDIF
296200020412     C**
296300020412     C** NON TROVATA TARIFFA
296400941107     C     DOPAC3        TAG
296500941107     C                   DO        *HIVAL        A
296600990721     C     KISOD         READE     TITPD02L                               28
296700941107     C   28              GOTO      DOPAC4
296800941107     C     A             OCCUR     ACDS
296900990923     C* VERIFICO SE STO CARICANDO LA TARIFFA DI CARTELLO
297000990923     C     TPTKSC        IFEQ      CARKSC
297100990923     C     TPTCTR        ANDEQ     CARCTR
297200990923     C     TPTPRG        ANDEQ     CARPRG
297300990923     C* E LA DIVISA DELLA TARIFFA DI CARTELLO E' DIVERSA DALLA DIVISA DELLA
297400990923     C* TARIFFA DEL CLIENTE  CONVERTO I VALORI O SCAGLIONI NELLA DIVISA
297500990923     C* DELLA TARIFFA DEL CLIENTE
297600990923     C     §TADIV        ANDNE     DIVCAR
297700990923     C*
297800990923     C                   EXSR      CONTPD
297900990923     C*
298000990923     C                   ENDIF
298100990923     C*
298200941112     C                   Z-ADD     TPDSGL        ASGL
298300941112     C                   Z-ADD     TPDITR        AITR
298400941112     C                   Z-ADD     TPDMIN        AMIN
298500941112     C                   Z-ADD     TPDMAX        AMAX
298600941112     C                   MOVEL     TPDAIN        AAIN
298700941107     C                   END
298800941107     C     DOPAC4        TAG
298900941107     C                   DO        100           A
299000941107     C     A             OCCUR     ACDS
299100941112     C     ASGL          IFGE      TPDPES
299200941107     C                   GOTO      DOPAC5
299300941107     C                   END
299400941107     C                   END
299500941123     C     ASGL          IFLT      TPDPES
299600941122     C                   SETON                                        94
299700980527     C                   MOVEL     MSG(7)        TASMSG
299800980527     C                   GOTO      FINE
299900980527     C                   ENDIF
300000941122     C** NON TROVATO SCAGLIONE ESATTO
300100941107     C     DOPAC5        TAG
300200941112     C*-----------
300300941122     C     ASGL          IFLE      MINTAS
300400941122     C                   Z-ADD     MINTAS        TPDPES
300500941112     C                   END
300600941122     C     TPDPES        IFLE      MINTAS
300700941122     C                   Z-ADD     MINTAS        TPDPES
300800941122     C                   END
300900991013     C                   Z-ADD     0             TPDQLI           18 6
301000980429     C* DIVIDO PER 100 TARIFFA 0 PER AVERE QUINTALI
301100980429     C*                TARIFFA 4 PERCHE' E' TARIFFA %
301200980429     C     WTPG          IFEQ      '0'
301300980429     C     WTPG          OREQ      '4'
301400941112     C     TPDPES        DIV       100           TPDQLI
301500941112     C                   ELSE
301600941112     C                   Z-ADD     TPDPES        TPDQLI
301700941112     C                   END
301800980429     C** PESO TASSABILE
301900060906      * se trovato dettaglio tariffario della tariffa "f" vado a fine routine in quanto il calcolo
302000060906      * viene eseguito nella routine FUEL
302100060906     c                   If        wfiso = 'f'
302200060906     c                   goto      endacd
302300060906     c                   endif
302400060906
302500990923     C   12AITR          MULT(H)   TPDQLI        VARIA
302600990923     C  N12AITR          MULT(H)   TPDQLI        VARIAD
302700941112     C                   MOVEL     AAIN          FLGISO
302800980429     C*
302900980430     C     VARIA         IFGT      0
303000991012     C     *IN12         ANDEQ     *ON
303100991012     C     VARIAD        ORGT      0
303200991012     C     *IN12         ANDEQ     *OFF
303300991012     C*
303400980430     C     WTPG          IFEQ      'V'
303500980430     C     WTPG          OREQ      'T'
303600990923     C   12VARIA         DIV(H)    100           VARIA
303700990923     C  N12VARIAD        DIV(H)    100           VARIAD
303800980430     C                   ENDIF
303900980430     C                   ENDIF
304000980429     C**
304100991012     C* SPOSTO L'IMPORTO DEL CAMPO VARIAD NEL CAMPO VARIA SOLO SE E' STATO USATO
304200991012     C  N12              Z-ADD     VARIAD        VARIA
304300991012     C*
304400991012     C** MINIMO
304500941112     C     AMIN          IFGT      VARIA
304600991012     C                   Z-ADD     AMIN          VARIA
304700941107     C                   END
304800991012     C** MASSIMO
304900941115     C     AMAX          IFGT      0
305000941112     C     AMAX          IFLT      VARIA
305100991012     C                   Z-ADD     AMAX          VARIA
305200941112     C                   END
305300941115     C                   END
305400000103     C** SE VARIA CON ESENZIONE IVA DEVE AVERE SOLO 2 DCEIMALI
305500000103     C* CERCO LA SIGLA VARIA CORRISPONDENTE
305600000103     C                   MOVEL     TPTFTC        WFTC              1
305700000103     C                   Z-ADD     1             A
305800000103     C     WFTC          LOOKUP    CP(A)                                  10
305900000103     C     *IN10         IFEQ      *ON
306000000103     C     A             OCCUR     DS1P
306100000103     C     §1PVAR        LOOKUP    VES                                    10
306200000103     C     *IN10         IFEQ      *ON
306300000103     C                   MOVE      VARIA         W001A             1
306400000103     C     W001A         IFNE      '0'
306500000103     C     VARIA         ADD       0,001         COMOD2           15 2
306600000103     C                   Z-ADD     COMOD2        VARIA
306700000103     C                   ENDIF
306800000103     C                   ENDIF
306900000103     C                   ENDIF
307000000103     C**
307100941107     C     ENDACD        TAG
307200990923     C*
307300941107     C                   ENDSR
307400941107     C/SPACE
307500941112     C******************************************************
307600980424     C** CALCOLO TARIFFE  PARTICOLARI
307700970521     C******************************************************
307800980428     C     CALCP         BEGSR
307900990721     C** CERO LA TARIFFA PARTICOLARE IN TITPT O CARTELLO
308000980430     C                   MOVEL     TASTC         FISO
308100980506     C                   MOVEL     'S'           WCAR
308200110908      ** se sto calcolando la varia "Q" = Centro storico o ZTL
308300110908      ** non devo cercare la tariffa nella Cartello in mancanza
308400110908      ** di quella del cliente
308500111102      ** E' stata aggiuna una tabella che pilota il recupero della
308600111102      ** tariffa di cartello se la filiale del cliente di tassazione
308700111102      ** è presente nella tabella ztl e la data spedizione è maggiore
308800111102      ** uguale alla data attivazione della tassazione
308900111102     c                   clear                   indztl            3 0
309000110908     c                   If        tastc = 'Q'
309100111102     c                   movel     'N'           WCAR
309200111102      * cerco la filiale del cliente nella tabella ZTL
309300111102     c                   eval      indztl = %lookup(kscfil:sfilztl)
309400111102      * se indice =  a zero cerco con la filiale 999
309500111102    2c                   if        indztl = *zeros
309600111102     c                   eval      indztl = %lookup(999:sfilztl)
309700111102    2c                   endif
309800111102      * imposto il flag della ricerca tariffa di cartello = 'S'  se filiale cliente
309900111102      * abilitata e la data spedizione > = data attivazione tassazione
310000111102    2c                   If        indztl > *zeros and
310100111102    3c                             tasdsp >= sdtaztl(indztl)
310200111102     c                   movel     'S'           WCAR
310300111102    3c                   endif
310400110908     c                   endif
310500110908      *
310600980428     C                   EXSR      CERTPT
310700980428     C** SOLO SE L'HO TROVATA
310800980428    1C     WEND          IFEQ      ' '
310900970521     C                   Z-ADD     ISOPES        TPDPES
311000970521     C                   EXSR      CALACD
311100970521     C                   Z-ADD     VARIA         TASCP
311200980428    1C                   ENDIF
311300970521     C*
311400970521     C                   ENDSR
311500001205     C/SPACE
311600001205     C******************************************************
311700001205     C** CALCOLO ADDIZIONALE DI GESTIONE + ISTAT
311800001205     C******************************************************
311900001205     C     ADGIST        BEGSR
312000130124
312100130124      * verifico se devo calcolare solo l'ISTAT
312200130124     c                   If        tassva = §$2VRV
312300130124     c                   goto      Tag_istat
312400130124     c                   endif
312500001205     C**
312600001205     C*
312700001205     C****************
312800041129     C** ADDIZIONALE DI GESTIONE  anni precedenti       varia "Z"
312900001205     C****************
313000001205     C*
313100031121     C     *LIKE         DEFINE    TASVA1        TASADG
313200001205     C     TAMPAG        IFNE      0
313300001205     C                   Z-ADD     1             A
313400001205     C     §$2VRZ        LOOKUP    SV(A)                                  05
313500001205     C  N05              DO
313600001205     C                   Z-ADD     0             TASADG
313700001205     C* TOTALE IMPONIBILE
313800001205     C                   Z-ADD     O17IMV        IMPADG           15 3
313900001205     C**
314000001205     C                   Z-ADD     0             COMADG           15 3
314100001205     C     IMPADG        MULT      TAMPAG        COMADG
314200001205     C     COMADG        IFGT      0
314300001205     C     COMADG        DIV(H)    100           TASADG
314400001205     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
314500001205     C     *IN12         IFEQ      '0'
314600001205     C     TASADG        ADD       0,5           CAMPOW           11 0
314700001205     C                   Z-ADD     CAMPOW        TASADG
314800001205     C                   END
314900001205     C                   END
315000001205     C*
315100040128     c     tasadg        ifgt      0
315200040128      *
315300001205     C                   Z-ADD     1             A
315400001205     C     ' '           LOOKUP    SV(A)                                  96
315500001205     C   96              MOVEL     §$2VRZ        SV(A)
315600001205     C   96              Z-ADD     TASADG        VA(A)
315700001205     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
315800001205     C  N96              ADD       TASADG        TASPOR
315900001205     C*
316000001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
316100001205     C*
316200001205     C                   CLEAR                   DSLV16
316300001205     C                   MOVEL     'T'           D17FIM
316400001205     C                   Z-ADD     TASPOR        D17POR
316500001205     C                   CALL      'FNLV16R'
316600001205     C                   PARM                    DSLV16
316700001205     C                   PARM                    DTASV
316800040128      *
316900040128     c                   endif
317000001205     C*
317100001205     C                   END
317200001205     C                   END
317300041129     C*
317400041129     C****************
317500041129     C** ADDIZIONALE DI GESTIONE    anno corrente       varia "b"
317600041129     C****************
317700041129     C*
317800041129     c                   clear                   tasadg
317900041129     c                   clear                   impadg
318000041129     c                   clear                   comadg
318100041129
318200041129     C     TAMPPA        IFNE      0
318300041129     C                   Z-ADD     1             A
318400041129     C     §$2VAG        LOOKUP    SV(A)                                  05
318500041129     C  N05              DO
318600041129     C                   Z-ADD     0             TASADG
318700041129     C* TOTALE IMPONIBILE
318800041129     C                   Z-ADD     O17IMV        IMPADG
318900041129     C**
319000041129     C                   Z-ADD     0             COMADG
319100041129     C     IMPADG        MULT      TAMPPA        COMADG
319200041129     C     COMADG        IFGT      0
319300041129     C     COMADG        DIV(H)    100           TASADG
319400041129     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
319500041129     C     *IN12         IFEQ      '0'
319600041129     C     TASADG        ADD       0,5           CAMPOW           11 0
319700041129     C                   Z-ADD     CAMPOW        TASADG
319800041129     C                   END
319900041129     C                   END
320000041129     C*
320100041129     c     tasadg        ifgt      0
320200041129      *
320300041129     C                   Z-ADD     1             A
320400041129     C     ' '           LOOKUP    SV(A)                                  96
320500041129     C   96              MOVEL     §$2VAG        SV(A)
320600041129     C   96              Z-ADD     TASADG        VA(A)
320700041129     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
320800041129     C  N96              ADD       TASADG        TASPOR
320900041129     C*
321000041129     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
321100041129     C*
321200041129     C                   CLEAR                   DSLV16
321300041129     C                   MOVEL     'T'           D17FIM
321400041129     C                   Z-ADD     TASPOR        D17POR
321500041129     C                   CALL      'FNLV16R'
321600041129     C                   PARM                    DSLV16
321700041129     C                   PARM                    DTASV
321800041129      *
321900041129     c                   endif
322000041129     C*
322100041129     C                   END
322200041129     C                   END
322300001205     C**
322400001205     C***************
322500001205     C** ISTAT
322600001205     C***************
322700001205     C*
322800130124     c     Tag_istat     tag
322900130124
323000130124      * Se al programma vengono passate le date aggiuntive per la ritassazione
323100130124      * imposto come data di spedizione quella passata se maggiore di zero
323200130124     c                   clear                   Sav_dsp
323300130124     c                   If        Ritass = 'S' and tasdtist > 0
323400130124     c                   eval      Sav_dsp = Tasdsp
323500130124     c                   movel     tasdtist      tasdsp
323600130124     c                   endif
323700130124      *
323800031121     C     *LIKE         DEFINE    TASVA1        TASIAC
323900001205     C                   Z-ADD     1             A
324000001205     C     §$2VRV        LOOKUP    SV(A)                                  05
324100101014    1C  N05TAMrct        IFGT      0
324200001205     C                   Z-ADD     0             TASIAC
324300001205     C*
324400001205     C* CALCOLO ISTAT SU TOTALE IMPONIBILE
324500001205     C*
324600001205     C                   Z-ADD     O17IMV        IMPIAC           15 3
324700101014     C                   Z-ADD     0             PUNIAC            7 4
324800101014
324900101014      * richiamo nuovo pgm per calcolo % istat da aggiungere all'imponibile
325000101014     c
325100101014     c                   clear                   trulistds
325200101014     c                   eval      IISTsca = tamrct
325300101014     c                   eval      IISTdsp = tasdsp
325400101014     c                   call      'TRULISTR'
325500101014     c                   parm                    trulistds
325600101014     c                   IF        OISTerr = *blanks
325700101014     c                   eval      puniac = OISTiac
325800101014     c                   ENDIF
325900101014     c
326000050629     C                   Z-ADD     0             PERIAC           15 6
326100050629     C                   Z-ADD     0             PERIAX           15 4
326200001205     C     PUNIAC        MULT      TAMPRI        PERIAX
326300001205    3C     PERIAX        IFNE      0
326400031121     C     PERIAX        DIV       100           PERIAC
326500001205    3C                   END
326600001205     C** PERCENTUALE DI ISTAT DA APPLICARE
326700001205     C** APPLICO I PUNTI X TRIMESTRE SOLO SE
326800001205     C** LA DATA DI SPEDIZIONE E'
326900001205     C** MAGGIORE O UGUALE ALLA DATA DI SCATTO
327000001205     C** DEI PUNTI NEL TRIMESTRE
327100001205    3C     PERIAC        IFNE      0
327200031121     C                   DIV       100           PERIAC
327300001205     C     IMPIAC        MULT(H)   PERIAC        TASIAC
327400001205    3C                   END
327500001205     C*
327600001205    3C     TASIAC        IFGT      0
327700001205     C*  SENZA DECIMALI
327800001205    4C     *IN12         IFEQ      *OFF
327900001205     C     TASIAC        ADD       0,5           CAMPOW           11 0
328000001205     C                   Z-ADD     CAMPOW        TASIAC
328100001205    4C                   END
328200001205     C*
328300001205     C                   Z-ADD     1             A
328400001205     C     ' '           LOOKUP    SV(A)                                  96
328500001205     C   96              MOVEL     §$2VRV        SV(A)
328600001205     C   96              Z-ADD     TASIAC        VA(A)
328700001205     C  N96              ADD       TASIAC        TASPOR
328800001205     C*
328900001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
329000001205     C*
329100001205     C                   CLEAR                   DSLV16
329200001205     C                   MOVEL     'T'           D17FIM
329300001205     C                   Z-ADD     TASPOR        D17POR
329400001205     C                   CALL      'FNLV16R'
329500001205     C                   PARM                    DSLV16
329600001205     C                   PARM                    DTASV
329700001205     C*
329800001205    3C                   END
329900001205    1C                   ENDIF
330000130124      * se impostato il campo del salvataggio della data spedizione
330100130124     c                   if        Sav_dsp <> *zeros
330200130124     c                   eval      tasdsp = Sav_dsp
330300130124     c                   clear                   Sav_dsp
330400130124     c                   endif
330500001205     C*
330600001205     C                   ENDSR
330700030922     C*******************************************************
330800030922     C** CALCOLO DIRITTO DI FATTURAZIONE  + TOTALE IMPONIBILE
330900030922     C******************************************************
331000030922     C     DIRFAT        BEGSR
331100030922     C**
331200030922      * prima verifico se esiste già
331300030922     C                   Z-ADD     1             A
331400030922     C     §$2VRD        LOOKUP    SV(A)                                  05
331500030922     C* SIGLA VARIA NON PRESENTE --> CALCOLO
331600030922     C     *IN05         IFEQ      *OFF
331700030922     C                   Z-ADD     1             A
331800030922     C     ' '           LOOKUP    SV(A)                                  96
331900030922     C   96              MOVEL     §$2VRD        SV(A)
332000030922     C   96              Z-ADD     TASIVDF       VA(A)
332100030922     C  N96              ADD       TASIVDF       TASPOR
332200030922     C*
332300030922     C* RICALCOLO L'IMPONIBILE SOMMANDO IL DIRITTO DI FATTURAZIONE
332400030922     C*
332500030922     C                   CLEAR                   DSLV16
332600030922     C                   MOVEL     'T'           D17FIM
332700030922     C                   Z-ADD     TASPOR        D17POR
332800030922     C                   CALL      'FNLV16R'
332900030922     C                   PARM                    DSLV16
333000030922     C                   PARM                    DTASV
333100030924     C*
333200030924     C                   Z-ADD     O17IMV        TASIMV
333300030924     C*
333400030922     C*
333500030922     C                   ENDIF
333600030922     C*
333700030922     C                   ENDSR
333800980428     C******************************************************
333900980428     C* CERCO TARIFFA PARTICOLARE
334000980428     C******************************************************
334100980428     C     CERTPT        BEGSR
334200980428     C                   CLEAR                   TASCP
334300980428     C                   CLEAR                   WEND
334400980428     C                   MOVEL     *BLANKS       FLGISO            1            FLAG.APPLICAZI.
334500990722     C                   MOVEL     FISO          WFISO
334600980428     C                   Z-ADD     1             A
334700990722     C     WFISO         LOOKUP    CP(A)                                  05
334800980428     C**
334900980428    1C     *IN05         IFEQ      *ON
335000020226     C     A             OCCUR     DS1P
335100980428     C*-----------------------------------------
335200990721     C     KISOT         CHAIN     TITPT01L                           28
335300980528     C** ESISTE TARIFFA DEL CLIENTE
335400980528     C     *IN28         IFEQ      *OFF
335500980528     C     TPTATB        ANDEQ     *BLANKS
335600980428     C                   GOTO      DOPIS2
335700980528     C                   ENDIF
335800980428     C*-----------------------------------------
335900980428     C** MEMORIZZO DATI DA TARIFFA DI CARTELLO
336000980506     C**  SOLO SE HO DETTO CHE LA POSSO USARE E SE C'E'
336100980428     C*-----------------------------------------
336200020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
336300020226     C     WCAR          IFEQ      'S'
336400020226     C     KTPTC         CHAIN     TITPT01L                           28
336500020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
336600020226     C     *IN28         IFEQ      *ON
336700020226     C     TPTATB        OREQ      'A'
336800020226     C                   MOVEL     'S'           WEND
336900020226     C                   GOTO      DOPIS3
337000020226     C                   ENDIF
337100020226      * E SE NON DEVO CARICARE LA CARTELLO ERRORE NON TROVATA TARIFFA PARTICOLARE
337200020226     C                   ELSE
337300020226     C                   MOVEL     'S'           WEND
337400020226     C                   GOTO      DOPIS3
337500020226     C                   ENDIF
337600980430     C     DOPIS2        TAG
337700980430     C* ARROTONDAMENTI E RPV
337800980430     C                   MOVEL     TPTTPG        WTPG
337900090310      * in caso di pod-immage varia "a" imposto come tipo pagamento a colli
338000090310
338100090310     c                   if        fiso = §$2POD
338200090310     C                   MOVEL     '1'           WTPG
338300090310     c                   endif
338400090310
338500980430     C                   MOVEL     TPTRPV        WRPV
338600980430     C                   CLEAR                   WDET
338700980430     C                   Z-ADD     TPTARL        WARL
338800980430     C                   Z-ADD     TPTARF        WARF
338900980430     C                   Z-ADD     TPTARO        WARO
339000980430     C                   EXSR      CALCOL
339100980428     C** NON TROVATA TARIFFA PARTICOLARE
339200980428   X1C                   ELSE
339300980428     C                   MOVEL     'S'           WEND              1
339400980428    1C                   ENDIF
339500020226     C     DOPIS3        ENDSR
339600980312     C******************************************************
339700980312     C** CALCOLO IL VALORE DA MOLTIPLICARE/RPV/ARROTONDAMENTI
339800980312     C******************************************************
339900980312     C     CALCOL        BEGSR
340000980313     C     *LIKE         DEFINE    TAMARL        ARR
340100980313     C     *LIKE         DEFINE    TASIAS        PESTAS
340200980313     C     *LIKE         DEFINE    TASIAS        ISOPES
340300980312     C     *LIKE         DEFINE    TASIAS        ISOPE2
340400980312     C     *LIKE         DEFINE    TPTRPV        WRPV
340500980313     C     *LIKE         DEFINE    TPTARL        WARL
340600980313     C     *LIKE         DEFINE    TPTARF        WARF
340700980313     C     *LIKE         DEFINE    TPTARO        WARO
340800980312     C                   Z-ADD     0             ISOPES
340900980316     C                   SELECT
341000030203
341100030204     c                   When      wFiso = '='
341200030203     c                   Z-Add     TasBan        Isopes
341300030203
341400980429     C     WTPG          WHENEQ    '0'
341500980429     C     WTPG          OREQ      '3'                                          KG
341600020826     C                   Z-ADD     USAPKF        ISOPES
341700980316     C*
341800980429     C     WTPG          WHENEQ    '1'                                          COLLI
341900980312     C                   Z-ADD     TASNCL        ISOPES
342000980316     C*
342100980429     C     WTPG          WHENEQ    '2'                                          SPEDIZ
342200980312     C                   Z-ADD     1             ISOPES
342300980316     C*
342400980429     C     WTPG          WHENEQ    '4'                                          VALORE
342500980429     C     WTPG          OREQ      '5'                                          Q.TA'
342600980312     C                   Z-ADD     TASQFT        ISOPES
342700980429     C*
342800980429     C     WTPG          WHENEQ    'T'                                          TRASPORTO
342900980429     C                   Z-ADD     TASPOR        ISOPES
343000980429     C                   ADD       TASINL        ISOPES
343100980429     C                   EXSR      CERISO
343200980429     C                   ADD       VAISO         ISOPES
343300060414     C                   ADD       VAtsp         ISOPES
343400980316     C                   ENDSL
343500980312     C** MINIMO TASSABILE STD
343600980312     C                   Z-ADD     0             MINTAS
343700980312     C**
343800980312     C                   Z-ADD     1             A
343900980312     C     WTPG          LOOKUP    CTR(A)                                 05
344000980312     C   05A             OCCUR     DSTR
344100980312     C   05              Z-ADD     §TRATA        MINTAS
344200980312     C**
344300980312     C** SOLO PER TAD (PER TPD NON MI SERVE)
344400980428    1C     WDET          IFEQ      'TAD'
344500980312     C                   CLEAR                   MINTAR
344600980312     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
344700980312     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
344800980312     C   05              Z-ADD     §TRATA        MINTAR
344900980428    2C     TAMATA        IFGT      0
345000980312     C                   Z-ADD     TAMATA        MINTAR
345100980428    2C                   ENDIF
345200980428    1C                   ENDIF
345300980312     C**
345400980428     C** RPV SOLO PER TARIFFA 0 E 3
345500980428    1C     §TRRPV        IFEQ      'S'
345600980428     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
345700980428    2C     WDET          IFEQ      'TAD'
345800980428     C     TASVLF        ANDGT     0
345900020218    3C                   DO        200           A
346000980428     C     A             OCCUR     TARDS
346100980428    4C     TSCG          IFGE      ISOPES
346200980428     C                   GOTO      POITAS
346300980428    4C                   END
346400980428    3C                   END
346500980428     C** RICERCO RPV CON PESO REALE
346600980428    3C     TSCG          IFLT      ISOPES
346700980428     C                   SETON                                        94
346800980527     C                   MOVEL     MSG(5)        TASMSG
346900151209     c                   eval      %subst(tasmsg: 37)= %trim(%editc(ISOPES:'3'))
347000980527     C                   GOTO      FINE
347100980428    3C                   END
347200980428     C     POITAS        TAG
347300980428     C                   Z-ADD     TRPV          WRPV
347400040603     C                   Z-ADD     TRPV          rappv
347500980428    2C                   ENDIF
347600980428     C**
347700980428    2C     TASVLF        IFNE      0
347800980428     C     WRPV          ANDNE     0
347900980428     C                   Z-ADD     0             COMPRV            9 3
348000980428     C     TASVLF        MULT      WRPV          COMPRV
348100980428     C     COMPRV        MULT      100           ISOPE2
348200980428    3C     ISOPE2        IFGT      ISOPES
348300980428     C                   Z-ADD     ISOPE2        ISOPES
348400980428    3C                   ENDIF
348500980428    2C                   ENDIF
348600980428    1C                   ENDIF
348700990923     C**
348800990923     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
348900990923    0C     §TRARR        IFEQ      'S'
349000980313     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
349100980312     C     ISOPES        IFGE      MINTAS
349200980312     C                   Z-ADD     0             ARR
349300980313     C     ISOPES        IFGT      WARL
349400980313     C                   Z-ADD     WARO          ARR
349500980312     C                   ELSE
349600980313     C                   Z-ADD     WARF          ARR
349700980312     C                   END
349800980313     C**
349900980313     C                   Z-ADD     0             RESTO            15 5
350000980312     C                   Z-ADD     0             ISOPEX           13 0
350100980312     C     ARR           IFNE      0
350200980312     C     ISOPES        ANDNE     0
350300980312     C     ISOPES        DIV       ARR           ISOPEX
350400980312     C                   MVR                     RESTO
350500980312     C     RESTO         IFGT      0
350600980312     C     ISOPEX        MULT      ARR           ISOPES
350700980312     C                   ADD       ARR           ISOPES
350800980312     C                   ENDIF
350900980312     C                   ENDIF
351000980312     C                   ENDIF
351100980312     C**
351200980312     C***        GIUARR    TAG
351300980312    0C                   ENDIF
351400980312     C                   ENDSR
351500970521     C******************************************************
351600941107     C** CALCOLA DIRITTO ASSICURATIVO - VARIA E
351700941112     C******************************************************
351800941107     C     CALVE         BEGSR
351900941107     C                   MOVEL     'R'           FISO
352000980506     C                   MOVEL     'N'           WCAR
352100980430     C                   EXSR      CERTPT
352200980430     C     WEND          IFEQ      ' '
352300990922     C** TARIFFA A VALORE TOLGO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DI CONTO
352400990922     C*  E CALCOLO VALORE AL KG (FISSO) PER LA PARTE RESTANTE
352500980428    1C     TPTTPG        IFEQ      'V'
352600980312     C                   CLEAR                   ISOPES
352700990922     C* CONVERTO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DELLA TARIFFA
352800990922     C*
352900990922     C     *LIKE         DEFINE    §GELRP        WLRP
353000990922     C*
353100990922     C     §TADIV        IFNE      §GEDCN
353200990922     C                   CLEAR                   YEUR
353300990922     C                   MOVEL     §GEDCN        YECDVI
353400990922     C                   MOVEL     §TADIV        YECDVO
353500990922     C                   Z-ADD     §GELRP        YECIMI
353600991014     C   12              MOVE      '3'           YECDCO
353700991014     C  N12              MOVE      '0'           YECDCO
353800990922     C*
353900990922     C                   CALL      'YEURCO'
354000990922     C                   PARM                    YEUR
354100990922     C*
354200990922     C     YECESI        IFEQ      ' '
354300990922     C                   Z-ADD     YECIMO        WLRP
354400990922     C                   END
354500991112     C* SE TARIFFE UGUALI VALORIZZO WLRP
354600991112     C                   ELSE
354700991118     C                   Z-ADD     §GELRP        WLRP
354800990922     C                   ENDIF
354900990922     C*
355000990922    2C     TPTVLM        IFGT      WLRP
355100990922     C     TPTVLM        SUB       WLRP          ISOPES
355200020826     C     ISOPES        MULT      USAPKF        ISOPES
355300980312    2C                   ENDIF
355400980312    1C                   ENDIF
355500980430     C**
355600980312     C                   Z-ADD     ISOPES        TPDPES
355700941112     C                   EXSR      CALACD
355800941112     C                   Z-ADD     VARIA         DIRITT
355900941112     C**
356000941112     C     TPTAPL        IFNE      *BLANKS
356100941112     C     TIPBOL        IFEQ      'F'
356200941112     C     TPTAPL        IFEQ      'A'
356300941112     C                   Z-ADD     0             DIRITT
356400941112     C                   END
356500941112     C                   ELSE
356600941112     C     TPTAPL        IFEQ      'P'
356700941112     C                   Z-ADD     0             DIRITT
356800941112     C                   END
356900941112     C                   END
357000941112     C                   END
357100941112     C** TIPO APPLICAZIONE
357200941112     C** P - SOLO PER FRANCHI
357300941112     C** A - SOLO PER ASSEGNATI
357400980430     C                   ENDIF
357500941107     C                   ENDSR
357600941107     C/SPACE
357700941112     C******************************************************
357800941112     C** CALCOLA MAGGIORAZIONE TIPO SERVIZIO
357900941112     C******************************************************
358000941112     C     CALTS         BEGSR
358100980506     C                   MOVEL     'S'           WCAR              1
358200090924     c
358300090924     C* 24/09/09 --> Ora applichiamo sempre la maggiorazine espresso
358400090924     c
358500980506     C* SE TIPO SERVIZIO DELLA TARIFFA = A TIPO SERVIZIO BOLLA
358600980506     C*  NON PRENDO LA TARIFFA DI CARTELLO
358700090922     C***  TAMTSP        IFEQ      TASTSP
358800090922     C***                MOVEL     'N'           WCAR              1
358900090922     C***                ENDIF
359000980506     C* SIGLA VARIA CORRISPONDENTE
359100980506     C                   MOVEL     §5EFTC        FISO
359200980506     C                   EXSR      CERTPT
359300980506     C     WEND          IFEQ      ' '
359400980506     C**
359500980506     C** A TRASPORTO
359600980430     C     TPTTPG        IFEQ      'T'
359700980312     C                   Z-ADD     IMPON         ISOPES
359800941112     C                   END
359900980312     C**
360000980312     C                   Z-ADD     ISOPES        TPDPES
360100941112     C                   EXSR      CALACD
360200941112     C                   Z-ADD     VARIA         WTSP
360300980506     C                   ENDIF
360400941112     C**
360500941112     C                   ENDSR
360600941112     C******************************************************
360700020225     C** CERCA TARIFFA TNTAM PIU' RELATIVA CARTELLO
360800941112     C******************************************************
360900941107     C     CERTAM        BEGSR
361000020225      *
361100941111     C     *LIKE         DEFINE    TAMPRG        PRG2
361200941111     C     *LIKE         DEFINE    TAMKSC        KSC2
361300941111     C     *LIKE         DEFINE    TAMCTR        CTR2
361400061219      * se in tasflo c'è una data riferimento per il recupero della tariffa la imposto
361500061219      * solo temporaneamente al posto della data spedizione
361600061219     c                   clear                   Sav_dsp
361700070108     c                   if        §asdrt  > *zeros
361800061219     c                   eval      Sav_dsp = Tasdsp
361900070108     c***                eval      tasdsp = §asdrt
362000070108     c                   movel     §asdrt        tasdsp
362100061219     c                   endif
362200020225      *
362300941112     C                   SETOFF                                       180717
362400020227     C                   SETOFF                                       0890
362500941111     C                   Z-ADD     0             PRG2
362600941111     C                   Z-ADD     0             KSC2
362700941111     C                   Z-ADD     0             CTR2
362800941107     C                   Z-ADD     0             TAMKSC
362900941107     C                   Z-ADD     0             TAMPRG
363000941107     C                   Z-ADD     0             TAMCTR
363100980605     C     KTAM          SETLL     TNTAM000                               08
363200980605     C* MANCA TARIFFA CLIENTE
363300980605    1C     *IN08         IFEQ      *OFF
363400980605     C                   MOVEL     MSG(3)        TASMSG
363500980527     C                   SETON                                        94
363600160114     c                   eval      MancaTariffa = *on
363700980527     C                   GOTO      ENDTAM
363800980605    1C                   ENDIF
363900980605     C**
364000941107     C     SUTAM         TAG
364100980605     C**
364200941107     C     KTAM          READE     TNTAM000                               07
364300980605     c**
364400941107     C** SE HO LETTO UNA TARIFFA SCADUTA (17) E IL
364500980605     C** RECORD SUCCESSIVO MI ACCENDE FINE FILE
364600980605     C** chaino vecchio progressivo per tassare con tariffa scaduTa
364700980605    1C     *IN07         IFEQ      *ON
364800980605     C   17KTAM2         CHAIN     TNTAM000                           18
364900980605     c**
365000980605     C     *IN17         IFEQ      *OFF
365100980605    2C     *IN18         OREQ      *ON
365200980605     C                   MOVEL     MSG(3)        TASMSG
365300160114     c                   eval      MancaTariffa = *on
365400980605     C                   SETON                                        94
365500980605    2C                   ENDIF
365600980605     C**
365700980605     C                   GOTO      ENDTAM
365800980605     C**
365900980605     C                   ELSE
366000980605     C**
366100980605     C** SE NON E' TARIFFA ADATTA PER LA BOLLA (ITALIA INVECE CHE ESTER
366200980605     C**  O VICEVERSA) --> RILEGGO COME SE NON AVESSI LETTO
366300160315     C***  TAMFIE        IFNE      WBOEST
366400160315     C***  TAMFIE        ANDNE     ' '
366500160315     C***                GOTO      SUTAM
366600160315     C***                ENDIF
366700980605    1C                   ENDIF
366800980605     C**
366900980605     C** TARIFFA DA DECORRERE : SEGNALO SE NON C'E' UNA TARIFFA SCADUTA
367000980605     C**    (di fatto non e' possibile perche' se c'e' una da decorrere
367100980605     C**    e ce n'e' una prima e' la tariffa valida dal momento che
367200980605     C**    non e' possibile che esista una buco di decorrenze scadenze
367300980605     C**    tra i vari progressivi)
367400980605    1C     TASDSP        IFLT      TAMDDT
367500980605     C   17KTAM2         CHAIN     TNTAM000                           18
367600980605     C* NON C'E' TARIFFA SCADUTA O NON TROVATA DI NUOVO
367700980605    2C     *IN17         IFEQ      *OFF
367800980605     C     *IN18         OREQ      *ON
367900980605     C                   MOVEL     MSG(10)       TASMSG
368000980605     C                   SETON                                        94
368100160114     c                   eval      MancaTariffa = *on
368200980605    2C                   ENDIF
368300980605     C**
368400941107     C                   GOTO      ENDTAM
368500980605    1C                   END
368600980605     C**
368700980605     C** TARIFFA SCADUTA: DATA SPEDIZIONE MAGGIORE DELLA DATA  SCADENZA
368800980605    1C     TASDSP        IFGT      TAMDST
368900941107     C                   SETON                                        17
369000980605     C                   Z-ADD     TAMKSC        KSC2
369100980605     C                   Z-ADD     TAMCTR        CTR2
369200980605     C                   Z-ADD     TAMPRG        PRG2
369300941107     C                   GOTO      SUTAM
369400980605    1C                   ENDIF
369700160317     C*
369800020227     C     ENDTAM        TAG
369900020227     C*
370000020225     C*
370100020227    0C     *IN94         IFEQ      *OFF
370101160317     C                   MOVEL     TAMFLO        DSTA01
370102160317     c                   clear                   tasfie
370103160317      * stabilisco il network della tariffa per la ricerca della cartello
370104160317     c                   select
370105160317      * ITALIA
370106160317     c                   when      tamfie = 'I' and §tadpd = ' '
370107160317     c                   eval      NTWTAM = 'COR'
370108160317     C                   MOVEL     'I'           WBOEST
370109160317      * DPD
370110160317     c                   when      tamfie = 'I' and §tadpd = 'S'
370111160317     c                   eval      NTWTAM = 'DPD'
370112160317     C                   movel     'S'           WBODPD
370113160317     C                   MOVEL     'I'           WBOEST
370114160317      * FEDEX
370115160317     c                   when      tamfie = 'E' and §tafed = 'S'
370116160317     c                   eval      NTWTAM = 'FED'
370117160317     c                   eval      tasfie = 'F'
370118160317     C                   movel     'S'           WBOFED
370119160317     C                   MOVEL     'E'           WBOEST
370120160317      * da aggiungere la differenzazione tariffa merce e documenti
370121160317      *
370122160317      * EEX
370123160317     c                   when      tamfie = 'E' and §tafed = ' '
370124160317     c                   eval      NTWTAM = 'EEX'
370125160317     C                   MOVEL     'E'           WBOEST
370126160317
370127160317     c                   Endsl
370128160317      * verifico se tariffa adatta alla bolla solo per quanto riguarda italia e estero e non
370129160317      * network
370130160317     C                   CLEAR                   TRUL27
370131160317     C                   MOVEL     TASTSP        I27TSP
370132160317     C                   MOVEL     TASLNA        I27LNA
370133160317     C                   MOVEL     TASLNP        I27LNP
370134160317     C                   MOVEL     TASKSC        I27CLI
370135160317     c                   Movel     Tasctr        I27Ctb
370136160317     C                   CALL      'TRUL27R1'
370137160317     C                   PARM                    TRUL27
370138160317      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
370139160317     C     O27ERR        IFEQ      *BLANKS
370140160317      * se  o27fie diverso da I e tamfie = 'E' o §tadpd = 'S' tariffa ok
370141160317      * se  o27fie uguale   a I e tamfie = 'I' e §tadpd = ' ' tariffa ok
370142160317      * Bolla italia e tariffa Estera
370143160317     c                   if        o27fie = 'I' and tamfie = 'E'
370144160317     C                   MOVEL     MSG(3)        TASMSG
370145160317     C                   SETON                                        94
370146160317     C                   ENDIF
370147160317      *Bolla italia e tariffa DPD
370148160317     c                   if        o27fie='I' and tamfie='I' and §tadpd='S'
370149160317     C                   MOVEL     MSG(3)        TASMSG
370150160317     C                   SETON                                        94
370151160317     C                   ENDIF
370152160317      *Bolla Estera e tariffa Italia
370153160317     c                   if        o27fie <> 'I' and tamfie='I' and §tadpd=' '
370154160317     C                   MOVEL     MSG(3)        TASMSG
370155160317     C                   SETON                                        94
370156160317     C                   ENDIF
370157160317     c
370158160317     C                   ELSE
370159160317     C                   SETON                                        94
370160160317     C                   MOVEL     O27MSG        TASMSG
370161160317     C                   GOTO      FINE
370162160317     C                   ENDIF
370200020227      *
370201160317      *
370202160317     C** CERCO LA CARTELLO RELATIVA ALLA TARIFFA DEL CLIENTE
370203160317     C*  SE NON HO GIA' UNA TARIFFA DI CARTELLO E SE 94 SPENTO
370300020227     C                   SETOFF                                       90
370400020227     C                   Z-ADD     1             T
370500020227     C     TAMKSC        LOOKUP    TAC(T)                                 90
370600020227      *
370700020227      * PER 90 SPENTO VUOL DIRE CHE NON HO TROVATO NESSUNA CARTELLO
370800020227    1C     *IN90         IFEQ      *OFF
370900020225     C                   MOVEL     TAMFLO        DSTA01
371000020225     C* AGGANCIO LA TABELLA DELLE TARIFFE DI CARTELLO
371001160317      * imposto il tipo tariffa fedex
371002160317     c                   clear                   tiptam
371003160317     c                   if        NtwTam = 'FED' and
371004160317     c                             o27fie = 'N'
371005160317     c                   eval      TIPTam = 'FD'
371006160317     c                   endif
371007160317
371008160317     c                   if        NtwTam = 'FED' and
371009160317     c                             (o27fie = 'M' or o27fie = 'F')
371010160317     c                   eval      TIPTam = 'FM'
371011160317     c                   endif
371012160317
371013160317     C*
371100020319     C                   EXSR      CERTCA
371200020319    1C                   ENDIF
371300020227     C*
371400020227    0C                   ENDIF
371500061220      * se impostato il campo del salvataggio della data spedizione
371600061220     c                   if        Sav_dsp <> *zeros
371700061220     c                   eval      tasdsp = Sav_dsp
371800061220     c                   clear                   Sav_dsp
371900061220     c                   endif
372000020225     C*
372100941107     C*
372200020227     C     ENDTA2        ENDSR
372300020618     C******************************************************
372400020618     C* RICERCA DETTAGLIO SU TUTAD
372500020618     C******************************************************
372600020618     C     RICTAD        BEGSR
372700020618     C     KTAD          SETLL     TITAD000                               09
372800020618     C** 49  TROVATA TARIFFA DEL CAP   DI ARRIVO
372900020618     C   09              SETON                                        49
373000020618     C**
373100020618    1C     *IN09         IFEQ      *OFF
373200020618     C                   MOVEL     *BLANKS       COMNAZ
373300020618     C                   MOVEL     *BLANKS       COMCAP
373400020618     C     KTAD          SETLL     TITAD000                               09
373500020618    2C     *IN09         IFEQ      *OFF
373600020618     C*
373700020618     C                   Z-ADD     1             A
373800020618     C     COMCTS        LOOKUP    CTS(A)                                 05
373900020618     C**
374000020618     C* SE TROVATO COD.TASSAZIONE
374100020618    3C     *IN05         IFEQ      *ON
374200020618     C     A             OCCUR     DSCT
374300020618     C                   MOVEL     §CTRAP        COMCTS
374400020618     C** CARICO REGIONE AL POSTO DEL CODICE DI TASSAZIONE
374500020618     C     KTAD          SETLL     TITAD000                               09
374600020618    3C                   ENDIF
374700020618     C**
374800020618    3C     *IN09         IFEQ      *OFF
374900020618     C*
375000020618     C     WBOEST        IFEQ      'E'
375100020618     C                   MOVEL     'EE'          COMCTS
375200020618     C                   ELSE
375300020618     C                   MOVEL     'IT'          COMCTS
375400020618     C                   END
375500020618     C     KTAD          SETLL     TITAD000                               09
375600020618    3C                   ENDIF
375700020618    2C                   ENDIF
375800020618    1C                   ENDIF
375900020618     C                   ENDSR
376000941107     C/SPACE
376100941112     C******************************************************
376200990721     C** CERCA TARIFFA TITAD DETTAGLIO E CARICA DS
376300941112     C******************************************************
376400941107     C     CARTAR        BEGSR
376500941112C    C                   SETOFF                                       495694
376600941112     C                   MOVEL     TASCAD        COMCAP            9
376700990923     C                   MOVEL     TASNZD        COMNAZ            3
376800941112     C                   MOVEL     TASCTS        COMCTS
376900020618     C                   MOVEL     TASLNP        WLNP
377000020618     C                   EXSR      RICTAD
377100020618     C   09              GOTO      LATAR
377200020618     C**
377300020618     C** SE NON TROVATO TITAD CON LA LINEA DI PARTENZA, CERCO CON LA
377400020618     C**  REGIONE TROVATA DA ORGANIGRANNA DI TASLNP
377500020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
377600020618     C     TASLNP        IFNE      SAVLNP
377700020618     C     TASLNP        CHAIN     AZORG01L                           31
377800020618     C   31              CLEAR                   ORGDE7
377900020618     C                   MOVEL     ORGDE7        OG147
378000020618     C                   MOVEL     TASLNP        SAVLNP
378100020618     C                   ENDIF
378200020618     C**
378300020618     C                   MOVEL     TASCAD        COMCAP            9
378400020618     C                   MOVEL     TASNZD        COMNAZ            3
378500020618     C                   MOVEL     TASCTS        COMCTS
378600020618     C                   MOVEL     §OGRET        WLNP
378700020618     C                   EXSR      RICTAD
378800020618     C   09              GOTO      LATAR
378900020618     C**
379000020618     C** SE NON TROVATO TITAD CON LA REGIONE DI TASLNP, CERCO CON LA
379100020618     C**  LINEA ITALIA 950
379200020618     C                   MOVEL     TASCAD        COMCAP            9
379300020618     C                   MOVEL     TASNZD        COMNAZ            3
379400020618     C                   MOVEL     TASCTS        COMCTS
379500020618     C                   MOVEL     950           WLNP
379600020618     C                   EXSR      RICTAD
379700020618     C   09              GOTO      LATAR
379800941112     C*
379900941112     C*--------------------TARIFFA REVERSIBILE----------------------------------
380000941107     C     TIPBOL        CABNE     'A'           MANTAR
380100941107     C** TARIFFA REVERSIBILE SOLO PER RITORNO(TBL=A)
380200941107     C     TASLNP        CABEQ     68            MANTAR
380300941107     C** NO TARIFFA REVERSIBILE PER 68
380400981027     C     KSCFIL        IFEQ      888
380500981027     C                   Z-ADD     TASLNA        KFIL
380600981027     C                   ELSE
380700981027     C                   MOVEL     KSCFIL        KFIL
380800981027     C                   ENDIF
380900981008     C**
381000941107     C                   SETON                                        56
381100941112     C                   MOVEL     TASCAM        COMCAP
381200990923     C                   MOVEL     TASNZM        COMNAZ
381300941123     C                   MOVEL     TASMCT        COMCTS
381400020618     C                   MOVEL     KFIL          WLNP
381500020618     C                   EXSR      RICTAD
381600020618     C   09              GOTO      LATAR
381700020618     C*
381800020618     C** SE NON TROVATO TITAD CON LA LINEA CLIENTE, CERCO CON LA
381900020618     C**  REGIONE TROVATA DA ORGANIGRANNA DELLA LIENA CLIENTE
382000020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
382100020618     C     KFIL          IFNE      SAVLNP
382200020618     C     KFIL          CHAIN     AZORG01L                           31
382300020618     C   31              CLEAR                   ORGDE7
382400020618     C                   MOVEL     ORGDE7        OG147
382500020618     C                   MOVEL     KFIL          SAVLNP
382600020618     C                   ENDIF
382700020618     C*
382800020618     C                   MOVEL     TASCAM        COMCAP
382900020618     C                   MOVEL     TASNZM        COMNAZ
383000020618     C                   MOVEL     TASMCT        COMCTS
383100020618     C                   MOVEL     §OGRET        WLNP
383200020618     C                   EXSR      RICTAD
383300020618     C   09              GOTO      LATAR
383400020618     C**
383500020618     C** SE NON TROVATO TITAD CON LA REGIONE DI KFIL, CERCO CON LA
383600020618     C**  LINEA ITALIA 950
383700020618     C                   MOVEL     TASCAM        COMCAP
383800020618     C                   MOVEL     TASNZM        COMNAZ
383900020618     C                   MOVEL     TASMCT        COMCTS
384000020618     C                   MOVEL     950           WLNP
384100020618     C                   EXSR      RICTAD
384200020618     C   09              GOTO      LATAR
384300941107     C     MANTAR        TAG
384400980527     C     *IN09         IFEQ      *OFF
384500980527     C                   SETON                                        94
384600980527     C                   MOVEL     MSG(8)        TASMSG
384700980527     C                   GOTO      FINE
384800980527     C                   ENDIF
384900941107     C** MANCA TARIFFA
385000941107     C     LATAR         TAG
385100030902     C                   EXSR      CARDST
385200941112     C** CARICA DS DELLA TARIFFA
385300941107     C                   ENDSR
385400941107     C/SPACE
385500941112     C******************************************************
385600941107     C** CARICA DS TARIFFA
385700941112     C******************************************************
385800941107     C     CARDST        BEGSR
385900980514     C     *LIKE         DEFINE    TADRPV        RAPPV
386000980514     C**
386100020218     C                   DO        200           A
386200941112     C     A             OCCUR     TARDS
386300941112     C                   CLEAR                   TARDS
386400941112     C                   END
386500941112     C                   Z-ADD     0             RAPPV
386600980514     C                   CLEAR                   WELAMT
386700941112     C**
386800980514    1C                   DO        *HIVAL        A
386900980514     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
387000980514    2C     WELAMT        IFNE      'S'
387100020618     C     KTAD          READE     TITAD000                               15
387200941107     C   15              GOTO      ENDDST
387300980514    2C                   ENDIF
387400980514     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
387500980514    2C     WELAMT        IFEQ      'S'
387600980515     C                   MOVEL     'E'           WELAMT            1
387700980514    2C                   ENDIF
387800980514     C**
387900941107     C     A             OCCUR     TARDS
388000980514     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
388100980514     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
388200980514     C**
388300941120     C                   Z-ADD     TADLNP        TLNP
388400941107     C                   MOVEL     TADCTS        TCTS
388500990721     C                   MOVEL     TADNAZ        TNAZ
388600941110     C                   MOVEL     TADCAP        TCAP
388700941107     C                   Z-ADD     TADCTR        TCTR
388800941107     C                   Z-ADD     TADRPV        TRPV
388900941122     C                   Z-ADD     TADMIN        TMIN
389000941122     C                   Z-ADD     TADMAX        TMAX
389100980514     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
389200980514    2C     WELAMT        IFNE      'E'
389300980514     C**
389400980514    3C     TADSGL        IFEQ      MINTAS
389500980514     C                   MOVEL     'E'           WELAMT
389600980514   X3C                   ELSE
389700980514     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
389800980514     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
389900980514     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
390000980514     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
390100980515    4C     TADSGL        IFGT      MINTAS
390200980514     C                   MOVEL     'S'           WELAMT
390300980514     C                   Z-ADD     MINTAS        TSCG
390400980514     C**
390500980522     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
390600980522    5C     TAMATA        IFEQ      0
390700980522     C     TAMATA        ORLE      MINTAS
390800980522     C     TADSGL        ORGT      TAMATA
390900980522     C**
391000980522    6C     UNOCTR        IFEQ      4
391100980514     C     UNOCTR        OREQ      0
391200980514     C     MINTAS        DIV(H)    100           WMTAS            15 5
391300980514     C                   ELSE
391400980514     C                   Z-ADD     MINTAS        WMTAS
391500980522    6C                   ENDIF
391600980514     C* TARIFFA
391700980514     C     WMTAS         MULT(H)   TADITR        TITR
391800980514     C     WMTAS         MULT(H)   TADINO        TINO
391900980522     C                   ELSE
392000980522     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
392100980522     C                   Z-ADD     TADITR        TITR
392200980522     C                   Z-ADD     TADINO        TINO
392300980522    5C                   ENDIF
392400980522    4C                   ENDIF
392500980514    3C                   ENDIF
392600980514    2C                   ENDIF
392700980514     C*
392800980514     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
392900980514    2C     WELAMT        IFNE      'S'
393000980514     C                   Z-ADD     TADSGL        TSCG
393100980514     C                   Z-ADD     TADITR        TITR
393200980514     C                   Z-ADD     TADINO        TINO
393300980514    2C                   ENDIF
393400980514    1C                   ENDDO
393500941107     C     ENDDST        ENDSR
393600941107     C/SPACE
393700941112     C******************************************************
393800941110     C** CARICAMENTO TABELLE SOLO LA 1 VOLTA
393900941112     C******************************************************
394000941110     C     CARTAB        BEGSR
394100980605     C     *LIKE         DEFINE    TAMATA        MINTAS
394200980605     C     *LIKE         DEFINE    TAMATA        MINTAR
394300990923     C     *LIKE         DEFINE    §TADIV        DIVCAR
394400020618     C                   CLEAR                   SAVLNP
394500980605     C**
394600980605     C                   OPEN      TABEL00F
394700980605     C**
394800941110     C                   EXSR      CARQT
394900941110     C** CARICAMENTO IAC
395000941110     C                   EXSR      CARTR
395100941110     C** CARICAMENTO TIPI TARIFFA
395200941110     C                   EXSR      CAR$2
395300941110     C** CARICAMENTO SIGLE VARIE
395400000103     C                   EXSR      CARCC
395500000103     C** CARICAMENTO SIGLE VARIE ESENTI
395600941112     C                   EXSR      CARCV
395700941112     C** CARICAMENTO CAMBI
395800941110     C                   EXSR      CARCT
395900941110     C** CARICAMENTO CODIFICI TASSAZIONE
396000941112     C                   EXSR      CAR5E
396100941112     C** CARICA TIPI SERVIZIO
396200980429     C                   EXSR      CAR1A
396300980429     C** CARICA TIPI INCASSO C/A MITTENTI
396400980603     C                   EXSR      CARTB
396500980603     C** CARICA DS TIPI BOLLA CHE NON PREVEDONO ASSICURAZIONE
396600020319     C                   EXSR      CAREST
396700980605     C** CARICA P.O. ESTERI
396800990917     C                   EXSR      CARDIV
396900990917     C** CARICAMENTO MONETA DI CONTO
397000020320     C                   EXSR      CARPAR
397100020320     C** CARICA TARIFFE DI CARTELLO
397200031120     C                   EXSR      CAR3A
397300031120     C** CARICO VARIE DELLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
397400980605     C                   CLOSE     TABEL00F
397500040505     C**
397600040505     C                   OPEN      TNTBE01L
397700040505     C** caricamento percentuale carburante
397800040505     C                   EXSR      CARPSC
397900080617     C** caricamento filiali addebito lasciato avviso
398000080617     C                   EXSR      CARLAV
398100111102     C** caricamento filiali addebito centro storico da cartello
398200111102     C                   EXSR      CARZTL
398300040505     C**
398400040505     C                   CLOSE     TNTBE01L
398500040505     C**
398600941110     C                   ENDSR
398700941110     C**********************************************************
398800941110     C** CARICAMENTO DS MULTIPLA  CODICI TASSAZIONE
398900941110     C**********************************************************
399000941110     C     CARCT         BEGSR
399100041220     C                   DO        600           A
399200941110     C     A             OCCUR     DSCT
399300941110     C                   CLEAR                   DSCT
399400941110     C                   END
399500941110     C** PULIZIA DS
399600941110     C                   MOVEL     'CT'          COD
399700941110     C     KTAB          SETLL     TABEL                                  07
399800941110     C  N07              SETON                                          H7
399900941110     C   H7              GOTO      FINE
400000941110     C                   Z-ADD     0             K
400100941110     C                   DO        *HIVAL        A
400200941110     C     KTAB          READE     TABEL                                  07
400300941110     C   07              GOTO      ENDCTS
400400941110     C     TBLKEY        IFNE      *BLANKS
400500941110     C                   ADD       1             K
400600941110     C                   MOVEL     TBLKEY        CTS(K)
400700941110     C     K             OCCUR     DSCT
400800941110     C                   MOVEL     TBLUNI        DSCT
400900941110     C                   END
401000941110     C                   END
401100941110     C     ENDCTS        TAG
401200941110     C                   ENDSR
401300941110     C/SPACE
401400941112     C**********************************************************
401500941112     C** CARICAMENTO TIPI SERVIZIO
401600941112     C**********************************************************
401700941112     C     CAR5E         BEGSR
401800941112     C                   DO        10            A
401900941112     C     A             OCCUR     DS5E
402000941112     C                   CLEAR                   DS5E
402100941112     C                   END
402200941112     C** PULIZIA DS
402300941112     C                   MOVEL     '5E'          COD
402400941112     C     KTAB          SETLL     TABEL                                  07
402500941112     C  N07              SETON                                          H7
402600941112     C   H7              GOTO      FINE
402700941112     C                   Z-ADD     0             K
402800941112     C                   DO        *HIVAL        A
402900941112     C     KTAB          READE     TABEL                                  07
403000941112     C   07              GOTO      END5E
403100941112     C     TBLKEY        IFNE      *BLANKS
403200941112     C                   ADD       1             K
403300941112     C                   MOVEL     TBLKEY        TS(K)
403400941112     C     K             OCCUR     DS5E
403500941112     C                   MOVEL     TBLUNI        DS5E
403600941112     C                   END
403700941112     C                   END
403800941112     C     END5E         TAG
403900941112     C                   ENDSR
404000980429     C**********************************************************
404100980429     C** CARICAMENTO TIPI INCASSO C/A
404200980429     C**********************************************************
404300980429     C     CAR1A         BEGSR
404400980430     C                   MOVEA     *ALL'9'       TIC
404500980429     C                   MOVEL     '1A'          COD
404600980429     C                   Z-ADD     0             K
404700980429     C     KTAB          SETLL     TABEL                                  07
404800980429     C     KTAB          READE     TABEL                                  07
404900980430    1C     *IN07         DOWEQ     *OFF
405000980430    2C     TBLKEY        IFNE      *BLANKS
405100980430     C     TBLFLG        ANDEQ     ' '
405200980429     C                   MOVEL     TBLUNI        DS1A
405300980430    3C     §1AFMB        IFEQ      'M'
405400980429     C                   ADD       1             K
405500980429     C                   MOVEL     TBLKEY        TIC(K)
405600980430    3C                   ENDIF
405700980430    2C                   ENDIF
405800980429     C     KTAB          READE     TABEL                                  07
405900980430    1C                   ENDDO
406000980429     C                   ENDSR
406100980603     C**********************************************************
406200980603     C** CARICAMENTO TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
406300980603     C**********************************************************
406400980603     C     CARTB         BEGSR
406500980603     C                   MOVEL     'TB'          COD
406600980603     C                   Z-ADD     0             K
406700980603     C     KTAB          SETLL     TABEL                                  07
406800980603     C     KTAB          READE     TABEL                                  07
406900980603    1C     *IN07         DOWEQ     *OFF
407000980603    2C     TBLKEY        IFNE      *BLANKS
407100980603     C     TBLFLG        ANDEQ     ' '
407200980603     C                   MOVEL     TBLUNI        DSTB
407300980603    3C     §TBFAS        IFEQ      '0'
407400980603     C                   ADD       1             K
407500980603     C                   MOVEL     TBLKEY        TBN(K)
407600980603    3C                   ENDIF
407700980603    2C                   ENDIF
407800980603     C     KTAB          READE     TABEL                                  07
407900980603    1C                   ENDDO
408000980603     C                   ENDSR
408100031120     C**********************************************************
408200031120     C** CARICAMENTO VARIE PER TIPI BOLLE CHE ACCETTANO UNA SINGOLA VARIA
408300031120     C**********************************************************
408400031120     C     CAR3A         BEGSR
408500031120     C                   MOVEL     '3A'          COD
408600031120     C                   Z-ADD     0             K
408700031120     C     KTAB          SETLL     TABEL                                  07
408800031120     C     KTAB          READE     TABEL                                  07
408900031120    1C     *IN07         DOWEQ     *OFF
409000031120    2C     TBLKEY        IFNE      *BLANKS
409100031120     C     TBLFLG        ANDEQ     ' '
409200031120     C                   MOVEL     TBLUNI        DS3A
409300031120    3C     §3ASVA        IFNE      ' '
409400031120     C                   ADD       1             K
409500031120     C                   MOVEL     §3ASVA        T3A(K)
409600031120    3C                   ENDIF
409700031120    2C                   ENDIF
409800031120     C     KTAB          READE     TABEL                                  07
409900031120    1C                   ENDDO
410000031120     C                   ENDSR
410100020319     C**********************************************************
410200020319     C** CARICAMENTO PO ESTERI
410300020319     C**********************************************************
410400020319     C     CAREST        BEGSR
410500020319     C*
410600020319     C                   CLEAR                   K
410700050504     C                   CLEAR                   KK                4 0
410800020319     C**
410900020319     C                   CLEAR                   CC
411000020319     C     *LOVAL        SETLL     AZORG01L
411100020319     C                   READ      AZORG01L                               07
411200020319    1C     *IN07         DOWEQ     *OFF
411300020319     C** FIL O AGENZIA
411400020319    2C     ORGFAG        IFEQ      'F'
411500020319     C     ORGFAG        OREQ      'A'
411600020319     C* NON ANNULLATO
411700020319    3C     ORGFVA        IFEQ      ' '
411800020319    4C                   MOVEL     ORGDE3        OG143
411900050504     c* p.o. esteri senza dpd
412000020319     C     §OGNTW        IFEQ      'EEX'
412100020319     C     §OGNTW        OREQ      'EUP'
412200020319     C     §OGNTW        OREQ      'FED'
412300020319     C                   ADD       1             K
412400020319     C                   MOVEL     ORGFIL        EST(K)
412500020319    4C                   ENDIF
412600050504     c* p.o. estero totale, anche con DPD
412700050504    4C     §OGNTW        IFEQ      'EEX'
412800050504     C     §OGNTW        OREQ      'EUP'
412900050504     C     §OGNTW        OREQ      'DPD'
413000050504     C     §OGNTW        OREQ      'FED'
413100050504     C                   ADD       1             KK
413200050504     C                   MOVEL     ORGFIL        ESTtot(KK)
413300050504    4C                   ENDIF
413400020319    3C                   ENDIF
413500020319    2C                   ENDIF
413600020319     C**
413700020319     C                   READ      AZORG01L                               07
413800020319    1C                   ENDDO
413900020319     C                   ENDSR
414000941110     C********************************************************
414100941110     C** CARICAMENTO SIGLE VARIE
414200941110     C********************************************************
414300941110     C     CAR$2         BEGSR
414400941110     C                   CLEAR                   DS$2
414500941110     C                   MOVEL     '$2'          COD
414600941110     C                   MOVEL     *BLANKS       KEY
414700941110     C                   MOVEL     '1'           KEY
414800941110     C     KTABC         CHAIN     TABEL                              07
414900941110     C   07              SETON                                          H7
415000941110     C   H7              GOTO      FINE
415100941110     C                   MOVEL     TBLUNI        DS$2
415200941110     C                   ENDSR
415300000103     C********************************************************
415400000103     C** CARICAMENTO SIGLE VARIE  ESENTI IVA
415500000103     C********************************************************
415600000103     C     CARCC         BEGSR
415700000103     C                   CLEAR                   DSCC
415800020416     C                   CLEAR                   KK
415900000103     C                   MOVEL     'CC'          COD
416000000103     C                   MOVEL     *BLANKS       KEY
416100000103     C                   MOVEL     'VARIE'       KEY
416200000103     C     KTABC         SETLL     TABEL                                  07
416300020416     C**N07                GOTO ENDCC
416400000103     C                   DO        *HIVAL
416500000103     C     KTAB          READE     TABEL                                  07
416600000103     C   07              LEAVE
416700000103     C                   MOVEL     TBLUNI        DSCC
416800000103     C* CONTROLLO SE VARIA ESENTE CARICO LA SIGLA IN TABELLA
416900000103     C     §CCJEI        IFNE      *BLANKS
417000000103     C                   ADD       1             K
417100000103     C                   MOVE      TBLKEY        VES(K)
417200000103     C                   ENDIF
417300020416     C* CONTROLLO SE DA RESTITUIRE CON SIGAL VARIA E IMPORTO =0
417400020416     C     §CCSV0        IFEQ      'S'
417500050504     C                   ADD       1             KK                4 0
417600020416     C                   MOVE      TBLKEY        SV0(KK)
417700020416     C                   ENDIF
417800000103     C                   ENDDO
417900000103     C                   Z-ADD     0             K
418000020416     C                   Z-ADD     0             KK
418100000103     C     ENDCC         ENDSR
418200941112     C********************************************************
418300941112     C** CARICAMENTO CAMBI
418400941112     C********************************************************
418500941112     C     CARCV         BEGSR
418600941116     C                   Z-ADD     0             K
418700941116     C                   DO        50            A
418800941116     C     A             OCCUR     DSCV
418900941112     C                   CLEAR                   DSCV
419000941116     C                   END
419100941116     C                   MOVEL     *BLANKS       CV
419200941116     C*
419300941112     C                   MOVEL     'CV'          COD
419400941116     C     KTAB          SETLL     TABEL                                  07
419500941116     C  N07              GOTO      ENDCV
419600941116     C                   DO        *HIVAL        A
419700941116     C     KTAB          READE     TABEL                                  07
419800941116     C   07              GOTO      ENDCV
419900941116     C     TBLKEY        IFNE      *BLANKS
420000941116     C                   ADD       1             K
420100941116     C                   MOVEL     TBLKEY        CV(K)
420200941116     C     K             OCCUR     DSCV
420300941116     C                   MOVEL     TBLUNI        DSCV
420400941116     C                   END
420500941116     C                   END
420600941112     C     ENDCV         ENDSR
420700941110     C******************************************************
420800941110     C** CARICAMENTO TIPI TARIFFA
420900941110     C******************************************************
421000941110     C     CARTR         BEGSR
421100030205     C                   DO        50            A
421200941110     C     A             OCCUR     DSTR
421300941110     C                   CLEAR                   DSTR
421400941110     C                   END
421500941110     C**
421600941110     C                   MOVEL     'TR'          COD
421700941112     C                   Z-ADD     0             K
421800941110     C     KTAB          SETLL     TABEL                                  07
421900941110     C  N07              SETON                                          H7
422000941110     C   H7              GOTO      FINE
422100941110     C                   DO        *HIVAL        A
422200941110     C     KTAB          READE     TABEL                                  07
422300941110     C   07              GOTO      TAB1
422400941110     C     TBLKEY        IFNE      *BLANKS
422500941110     C                   ADD       1             K
422600941110     C                   MOVEL     TBLKEY        CTR(K)
422700941110     C     K             OCCUR     DSTR
422800941110     C                   MOVEL     TBLUNI        DSTR
422900941110     C                   END
423000941110     C                   END
423100941110     C     TAB1          ENDSR
423200941110     C**********************************************************
423300020221     C** MEMORIZZA
423400941110     C** TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
423500941110     C**********************************************************
423600941110     C     CARPAR        BEGSR
423700020225      *
423800020225     C     *LIKE         DEFINE    TAMKSC        CARKSC
423900020225     C     *LIKE         DEFINE    TAMCTR        CARCTR
424000020225     C     *LIKE         DEFINE    TAMPRG        CARPRG
424100020221      *
424200020222      * RICHIAMO IL TRUL27R PER OGNI NETWORK PER CARICARMI LE TARIFFE DI CARTELLO IN SCHIERA
424300020221      *
424400020221     C* CORRIERE BARTOLINI
424500160317     C                   CLEAR                   TRULC7
424600160317     C                   MOVEL     'COR'         IC7TNTW
424700020222     C                   EXSR      SRTRUL
425200020222      * DPD
425300160317     C                   CLEAR                   TRULC7
425400160317     C                   MOVEL     'DPD'         IC7TNTW
425500020222     C                   EXSR      SRTRUL
425600020222      * FEDEX
425700160317     C                   CLEAR                   TRULC7
425701160317     C                   EVAL      IC7TIP = 'FM'
425800160317     C                   MOVEL     'FED'         IC7TNTW
425900020222     C                   EXSR      SRTRUL
425901160317      * FEDEX DOCUMENTI
425902160317     C                   CLEAR                   TRULC7
425903160317     C                   EVAL      IC7TIP = 'FD'
425904160317     C                   MOVEL     'FED'         IC7TNTW
425905160317     C                   EXSR      SRTRUL
426000020222      * EUROEXPRESS
426100160321     C                   CLEAR                   TRULC7
426200160321     C                   MOVE      'L'           IC7TLA
426300160321     C                   MOVEL     'EEX'         IC7TNTW
426400020222     C                   EXSR      SRTRUL
426500020226      *
426600020226     C                   CLEAR                   CARKSC
426700020226     C                   CLEAR                   CARCTR
426800020226     C                   CLEAR                   CARPRG
426900020222      *
427000020222      ** CARICO LE TARIFFE PARTICOLARI
427100941111     C                   EXSR      CAR1P
427200020222      *
427300941110     C                   ENDSR
427400040505     C**********************************************************
427500040505     C** CARICAMENTO SCHIERA SUPPLEMEMNTO CARBURANTE
427600040505     C**********************************************************
427700040505     C     CARPSC        BEGSR
427800040505
427900040507     C                   CLEAR                   KX
428000040505
428100040505     C                   MOVEL     'PSC'         KEYTBE
428200040505     C     KEYTBE        SETLL     TNTBE01L
428300040505     C                   DO        *HIVAL
428400040505     C     KEYTBE        READE     TNTBE01L
428500040505
428600040505     C                   IF        %EOF(TNTBE01L)
428700040505     C                   LEAVE
428800040505     C                   ENDIF
428900040505
429000040505     C                   IF        TBEATB = ' '
429100040507     C                   ADD       1             KX
429200040507     C                   IF        KX <= 999
429300040505     C                   MOVEL     TBEUNI        DPSC
429400040507     C                   Z-ADD     §PSCDAD       DS_DSC
429500040507     C                   Z-ADD     §PSCPER       DS_PSC
429600040507     C                   MOVE      WDSPSC        DSCA(KX)
429700040507     C                   ENDIF
429800040505     C                   ENDIF
429900040505
430000040505     C                   ENDDO
430100040507
430200040507     C                   SORTA     DSCA
430300040505
430400040505     C                   ENDSR
430500080617      *****************************************************************
430600080617      ** CARICAMENTO SCHIERA FILIALI ABILITATE ADDEBITO LASCIATO AVVISO
430700080617      *****************************************************************
430800080617     C     CARLAV        BEGSR
430900080617
431000080617     C                   CLEAR                   KX
431100080617
431200080617     C                   MOVEL     'LAV'         KEYTBE
431300080617     C     KEYTBE        SETLL     TNTBE01L
431400080617     C                   DO        *HIVAL
431500080617     C     KEYTBE        READE     TNTBE01L
431600080617
431700080617     C                   IF        %EOF(TNTBE01L)
431800080617     C                   LEAVE
431900080617     C                   ENDIF
432000080617
432100080617     C                   IF        TBEATB = ' '
432200080617     C                   ADD       1             KX
432300080617     C                   IF        KX <= 999
432400080617     C                   MOVEL     TBEUNI        DLAV
432500080617     C                   EVAL      SFILLAV(KX) = %dec(tbeke1:3:0)
432600080618     C                   EVAL      SDTALAV(KX) = D§LAVTAS
432700080617     C                   ENDIF
432800080617     C                   ENDIF
432900080617
433000080617     C                   ENDDO
433100080617
433200080617
433300080617     C                   ENDSR
433400111102      ******************************************************************
433500111102      ** CARICAMENTO SCHIERA FILIALI ADDEBITO DA CARTELLO CENTRO STORICO
433600111102      ******************************************************************
433700111102     C     CARZTL        BEGSR
433800111102
433900111102     C                   CLEAR                   KX
434000111102
434100111102     C                   MOVEL     'ZTL'         KEYTBE
434200111102     C     KEYTBE        SETLL     TNTBE01L
434300111102     C                   DO        *HIVAL
434400111102     C     KEYTBE        READE     TNTBE01L
434500111102
434600111102     C                   IF        %EOF(TNTBE01L)
434700111102     C                   LEAVE
434800111102     C                   ENDIF
434900111102
435000111102     C                   IF        TBEATB = ' '
435100111102     C                   ADD       1             KX
435200111102     C                   IF        KX <= 999
435300111102     C                   MOVEL     TBEUNI        DZTL
435400111102     C                   EVAL      SFILZTL(KX) = %dec(tbeke1:3:0)
435500111102     C                   EVAL      SDTAZTL(KX) = D§ZTLTAS
435600111102     C                   ENDIF
435700111102     C                   ENDIF
435800111102
435900111102     C                   ENDDO
436000111102
436100111102
436200111102     C                   ENDSR
436300020222     C*************************************************************
436400020222     C** RICHIAMO DEL TRUL27R
436500020222     C*************************************************************
436600020222     C     SRTRUL        BEGSR
436700020222      *
436800160114     C***                Z-ADD     TASDCT        I27DTA
436900160114      /free
437000160114       //?Passo la data giusta al TRUL27R
437100160114        IF  Ritass = 'S';
437200160317           IC7dta = TASdtcar;
437300160114         ELSE;
437400160317           IC7dta = TASdct;
437500160114         ENDIF;
437600160114      /end-free
437700160317     c                   Movel     TasCtr        IC7Ctb
437800160317     C                   CALL      'TRULC7R'
437900160317     C                   PARM                    TRULC7
438000020222      *
438100160317     C     OC7ERR        IFEQ      *BLANKS
438200160317     C     OC7PRGC       ANDNE     *BLANKS
438300020222     C                   ADD       1             T                 2 0
438400160317     C                   MOVE      IC7TNTW       NTW(T)
438500160317     C                   Z-ADD     OC7KSCC       TAC(T)
438600160317     C                   Z-ADD     OC7CTRC       CTC(T)
438700160317     C                   MOVE      OC7PRGC       PRC(T)
438701160317     C                   MOVE      IC7TIP        TIP(T)
438800020222      *
438900020222      * AGGANCIO LA TARIFFA PER RECUPERARE LA DIVISA
439000020222      *
439100160317     C                   Z-ADD     OC7KSCC       CARKSC
439200160317     C                   Z-ADD     OC7CTRC       CARCTR
439300160317     C                   MOVE      OC7PRGC       CARPRG
439400020222     C     KTAMC         CHAIN     TNTAM00L                           94
439500020222      *
439600020222     C     *IN94         IFEQ      *OFF
439700020222     C                   MOVEL     TAMFLO        DSTA01
439800020222     C                   MOVE      §TADIV        DIC(T)
439900020226      * SE C'E ERRORE VADO A FINE PROGRAMMA
440000020226     C                   ELSE
440100020226     C                   MOVEL     MSG(9)        TASMSG
440200020226     C                   ENDIF
440300020226      *
440400020222      * SE C'E ERRORE VADO A FINE PROGRAMMA
440500020222     C                   ELSE
440600020222     C                   SETON                                        94
440700020222     C                   MOVEL     MSG(9)        TASMSG
440800020222     C                   ENDIF
440900020222      *
441000020222     C                   ENDSR
441100020225     C*************************************************************
441200020225     C** CERCO LA TARIFFA DI CARTELLO
441300020225     C*************************************************************
441400020225     C     CERTCA        BEGSR
441500020225      *
441600020318     C** DALLE SCHIERE CARICO LA TARIFFA DI CARTELLO
441700020318     C                   SETOFF                                       90
441800020319     C                   Z-ADD     1             T
441900160317     C**** O27NTW        LOOKUP    NTW(T)                                 90
441901160317      * se ho il tipo tariffa fedex valorizzato vado con questo altrimenti vado
441902160317      * con il network della tariffa
441903160317     c     Tiptam        ifne      '  '
441904160317     c     tiptam        lookup    TIP(T)                                 90
441905160317     c                   else
441906160317     C     NtwTam        LOOKUP    NTW(T)                                 90
441907160317     c                   endif
442000020318     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
442100020318     C     *IN90         IFEQ      *OFF
442200020318     C                   MOVEL     MSG(9)        TASMSG
442300020319     C                   SETON                                        94
442400020318     C                   GOTO      FINE
442500020318     C                   ENDIF
442600020318     C*
442700020318     C                   Z-ADD     TAC(T)        CARKSC
442800020318     C                   Z-ADD     CTC(T)        CARCTR
442900020318     C                   Z-ADD     PRC(T)        CARPRG
443000020318     C                   MOVE      DIC(T)        DIVCAR
443100160129      /free
443200160129       //?Cerco la cartello giusta
443300160129          exsr TarCar;
443400160129      /end-free
443500020225      *
443600020225     C                   ENDSR
443700941114     C*************************************************************
443800941121     C** CARICAMENTO CODICI E TARIFFE PARTICOLARI DI CARTELLO
443900941114     C*************************************************************
444000941110     C     CAR1P         BEGSR
444100941111     C*
444200020114     C                   DO        50            A
444300941112     C     A             OCCUR     DS1P
444400941111     C                   CLEAR                   DS1P
444500941112     C                   END
444600941111     C                   MOVEL     *BLANKS       CP
444700941110     C**
444800941110     C                   MOVEL     '1P'          COD
444900941111     C                   Z-ADD     0             K                 5 0
445000941110     C     KTAB          SETLL     TABEL                                  07
445100941110     C  N07              SETON                                          H7
445200941110     C   H7              GOTO      FINE
445300941110     C                   DO        *HIVAL        A
445400941110     C     KTAB          READE     TABEL                                  07
445500941110     C   07              GOTO      TABCP
445600941110     C     TBLKEY        IFNE      *BLANKS
445700941115     C                   MOVEL     TBLUNI        CAMPO            26
445800941115     C                   MOVE      CAMPO         WFCP              1
445900941115     C     WFCP          IFEQ      'S'
446000941110     C                   ADD       1             K
446100941110     C                   MOVEL     TBLKEY        CP(K)
446200941112     C     K             OCCUR     DS1P
446300941112     C                   MOVEL     TBLUNI        DS1P
446400941112     C**
446500941110     C                   END
446600941110     C                   END
446700941111     C                   ENDDO
446800020222     C*
446900941110     C     TABCP         ENDSR
447000941110     C****************************************************
447100941110     C** CARICAMENTO DATI VARI TASSAZIONE
447200941110     C****************************************************
447300941110     C     CARQT         BEGSR
447400941110     C                   MOVEL     'QT'          COD
447500941110     C                   MOVEL     *BLANKS       KEY
447600941110     C                   MOVEL     '1'           KEY
447700941110     C     KTABC         CHAIN     TABEL                              07
447800941113     C   07              SETON                                        H7
447900941113     C   H7              GOTO      FINE
448000941110     C  N07              MOVEL     TBLUNI        DSQT1
448100941110     C*
448200941110     C                   ENDSR
448300990917     C****************************************************
448400990922     C** CARICAMENTO MONETE DI CONTO / IMPORTI STANDARD
448500990917     C****************************************************
448600990917     C     CARDIV        BEGSR
448700990917     C*****
448800990922      *  RICERCA DELLA MONETA DI CONTO
448900990917     C                   CLEAR                   DSBS02
449000990917     C                   CLEAR                   DGED
449100990922     C*
449200990917     C                   MOVEL     'C'           T02MOD
449300990917     C                   MOVEL     KNSIF         T02SIF
449400990917     C                   MOVEL     'GED'         T02COD
449500991112     C                   MOVEL     '1'           T02KE1
449600990917      *
449700990917     C                   CALL      'TIBS02R'
449800990917     C                   PARM                    KPJBA
449900990917     C                   PARM                    DSBS02
450000990917      *
450100990917     C     T02ERR        IFEQ      *BLANKS
450200990917     C                   MOVEL     T02UNI        DGED
450300990917     C                   ENDIF
450400990922      * RICERCA DEGLI IMPORTI STANDARD NELLA MONETA DI CONTO
450500990922     C                   CLEAR                   DSBS02
450600990922     C                   CLEAR                   DGEI
450700990922     C                   MOVEL     'L'           T02TLA
450800990922     C                   MOVEL     'C'           T02MOD
450900990922     C                   MOVEL     KNSIF         T02SIF
451000990922     C                   MOVEL     'GEI'         T02COD
451100991112     C                   MOVEL     §GEDCN        T02KE1
451200990922      *
451300990922     C                   CALL      'TIBS02R'
451400990922     C                   PARM                    KPJBA
451500990922     C                   PARM                    DSBS02
451600990922      *
451700990922     C     T02ERR        IFEQ      *BLANKS
451800990922     C                   MOVEL     T02UNI        DGEI
451900990922     C                   ENDIF
452000990922      *
452100990917      *
452200990917     C                   ENDSR
452300011127     C**********************************************************
452400011127     C** RECUPERO LA DATA DEL GIORNO
452500011127     C**********************************************************
452600011127     C     RUDATE        BEGSR
452700011127      *
452800011127     C                   TIME                    W0140            14 0
452900011127     C* UDATE IN GGMMAAAA
453000011127     C                   MOVE      W0140         WDTGIO            8 0
453100011127     C*
453200011127     C                   Z-ADD     WDTGIO        G02DAT
453300011127     C                   MOVEL     *BLANK        G02ERR
453400011127     C                   CALL      'XSRDA8'
453500011127     C                   PARM                    WLBDAT
453600011127     C*
453700011127     C* UDATE IN AAAAMMGG
453800011127     C                   Z-ADD     G02INV        WDTGIO
453900011127      *
454000011127     C                   ENDSR
454100011127     C**********************************************************
454200011127     C** RECUPERO LA DATA TASSAZIONE DA USARE PER QUESTA BOLLA
454300011127     C**********************************************************
454400030131     C**!!!RECDAT        BEGSR
454500011127      *
454600030131     C**!!!              SETOFF                                       31
454700011127      * VERIFICO SE DATA VALORIZZATA E SOPRATTUTTO NUMERICA
454800020826     C**         §ASDAT    IFNE *BLANK
454900020826     C**         §ASDAT    ANDNE*ZEROS
455000011127      *
455100020826     C**                   TESTN          §ASDAT     31
455200011127      * SE 31 ACCESO VUOL DIRE CHE È NUMERICO MA VERIFICO ANCHE L'ULTIMO CARATTERE
455300020826     C**         *IN31     IFEQ *ON
455400020826     C**                   MOVE §ASDAT    W01A    1
455500020826     C**         W01A      COMP '0'                  31  31
455600020826     C**                   ENDIF
455700011127      *
455800020826     C**                   ENDIF
455900011127      * SE 31 ACCESO DATA VALIDA ALTRIMENTI PRENDO LA DATA DEL GIORNO
456000020826     C**         *IN31     IFEQ *ON
456100020826     C**                   MOVE §ASDAT    DATTAS  80
456200020826     C**                   ELSE
456300020826     C**                   Z-ADDWDTGIO    DATTAS
456400020826     C**                   ENDIF
456500020618     C*
456600011127      *
456700030203     C**!!!              ENDSR
456800990916     C****************************************************
456900990916     C** CARICAMENTO IMPORTI PASSATI DALLE DS
457000990916     C****************************************************
457100990916     C     SAVDS         BEGSR
457200990916     C*
457300990922     C     *LIKE         DEFINE    TASPOR        TASINL
457400990916     C*
457500990916     C* SALVO LE SCHIERE DELLE VARIE (SIGLE VARIE+IMPORTI)
457600990916     C                   MOVEA     SV            SVW
457700990916     C                   MOVEA     VA            VAW
457800991012     C                   CLEAR                   TASINL
457900991012     C                   CLEAR                   TASDIF
458000990916     C*
458100990916     C* RECUPERO L'IMPORTO DELL'INOLTRO SE C'E' NELLA SCHIERA DELLE VARIE
458200990916     C                   Z-ADD     1             X                 2 0
458300990923     C     §$2FIN        LOOKUP    SV(X)                                  30
458400990916     C* SE LO TROVO VALORIZZO TASINL
458500990923     C   30              Z-ADD     VA(X)         TASINL
458600991111     C  N30              Z-ADD     0             X
458700990921     C*
458800990921     C* RECUPERO L'IMPORTO DEL DIRITTO FISSO  NELLA SCHIERA DELLE VARIE
458900990921     C                   Z-ADD     1             Y                 2 0
459000990921     C     §$2VRH        LOOKUP    SV(Y)                                  30
459100990921     C* SE LO TROVO VALORIZZO TASINL
459200990923     C   30              Z-ADD     VA(Y)         TASDIF           11 3
459300990916     C*
459400990916     C                   ENDSR
459500990921     C****************************************************
459600990921     C** CONVERSIONE DELLE VARIE IN MONETA DELLA TARIFFA
459700990921     C****************************************************
459800990921     C     CNVTAS        BEGSR
459900990921     C*
460000990921     C* SE DIVISA PASSATA (DELLA BOLLA) E' DIVERSA DALLA DIVISA DELLA
460100990921     C* TARIFFA CONVERTO IN DIVISA TARIFFA
460200990921     C                   Z-ADD     1             A
460300990923    1C                   DO        20            A
460400990923    2C     VA(A)         IFNE      *ZEROS
460500990921     C                   CLEAR                   YEUR
460600011127     C                   MOVEL     DIVINP        YECDVI
460700011127     C                   MOVEL     DIVOUT        YECDVO
460800990921     C                   Z-ADD     VA(A)         YECIMI
460900991014     C   12              MOVE      '3'           YECDCO
461000991014     C  N12              MOVE      '0'           YECDCO
461100990921     C*
461200990921     C                   CALL      'YEURCO'
461300990921     C                   PARM                    YEUR
461400990921     C*
461500990923    3C     YECESI        IFEQ      ' '
461600990921     C                   Z-ADD     YECIMO        VA(A)
461700990923    3C                   END
461800990923    2C                   END
461900990923    1C                   END
462000990921     C* CONVERTO L'IMPORTO DEL PORTO
462100990921     C* SE PORTO DIVERSO DA 0
462200990923    1C     TASPOR        IFGT      0
462300990921     C                   CLEAR                   YEUR
462400011127     C                   MOVEL     DIVINP        YECDVI
462500011127     C                   MOVEL     DIVOUT        YECDVO
462600990921     C                   Z-ADD     TASPOR        YECIMI
462700991014     C   12              MOVE      '3'           YECDCO
462800991014     C  N12              MOVE      '0'           YECDCO
462900990921     C*
463000990921     C                   CALL      'YEURCO'
463100990921     C                   PARM                    YEUR
463200990921     C*
463300990923    2C     YECESI        IFEQ      ' '
463400990921     C                   Z-ADD     YECIMO        TASPOR
463500990921     C                   ELSE
463600990921     C                   MOVEL     MSG(10)       TASMSG
463700990921     C                   GOTO      FINE
463800990923    2C                   END
463900990923    1C                   ENDIF
464000990921     C* SE INOLTRO DIVERSO DA 0
464100990923    1C     TASINL        IFGT      0
464200990921     C                   CLEAR                   YEUR
464300011127     C                   MOVEL     DIVINP        YECDVI
464400011127     C                   MOVEL     DIVOUT        YECDVO
464500990921     C                   Z-ADD     TASINL        YECIMI
464600991014     C   12              MOVE      '3'           YECDCO
464700991014     C  N12              MOVE      '0'           YECDCO
464800990921     C*
464900990921     C                   CALL      'YEURCO'
465000990921     C                   PARM                    YEUR
465100990921     C*
465200990923    2C     YECESI        IFEQ      ' '
465300990921     C                   Z-ADD     YECIMO        TASINL
465400990921     C                   ELSE
465500990921     C                   MOVEL     MSG(10)       TASMSG
465600990921     C                   GOTO      FINE
465700990923    2C                   END
465800990923    1C                   ENDIF
465900990921     C* SE DIRITTO FISSO DIVERSO DA 0
466000990923    1C     TASDIF        IFGT      0
466100990921     C                   CLEAR                   YEUR
466200011127     C                   MOVEL     DIVINP        YECDVI
466300011127     C                   MOVEL     DIVOUT        YECDVO
466400990921     C                   Z-ADD     TASDIF        YECIMI
466500991014     C   12              MOVE      '3'           YECDCO
466600991014     C  N12              MOVE      '0'           YECDCO
466700990921     C*
466800990921     C                   CALL      'YEURCO'
466900990921     C                   PARM                    YEUR
467000990921     C*
467100990923    2C     YECESI        IFEQ      ' '
467200990921     C                   Z-ADD     YECIMO        TASDIF
467300990923    2C                   END
467400990923    1C                   ENDIF
467500990929     C* SE IMPORTO IMPONIBILE DIVERSO DA ZEROS
467600991014     C* LO RICALCOLO PER SICUREZZA
467700990929    1C     TASIMV        IFGT      0
467800991014     C                   CLEAR                   DSLV16
467900991014     C                   MOVEL     'T'           D17FIM
468000991014     C                   Z-ADD     TASPOR        D17POR
468100991014     C                   CALL      'FNLV16R'
468200991014     C                   PARM                    DSLV16
468300991014     C                   PARM                    DTASV
468400991014     C                   Z-ADD     O17IMV        TASIMV
468500990929    1C                   ENDIF
468600990921     C* SE QUANTITA' DA FATTURARE E' DIVERSA DA 0 E LA TARIFFA E' UNA TARIFFA
468700011127     C* A VALORE SPECIFICO DEVO CONVERTIRE IL CAMPO SE RICHIESTO
468800990921     C*
468900011127    1C     TASQFT        IFGT      0
469000011127     C     TAMFVD        ANDEQ     '4'
469100011127     C     CONQTA        ANDNE     'N'
469200011127     C                   CLEAR                   YEUR
469300011127     C                   MOVEL     DIVINP        YECDVI
469400011127     C                   MOVEL     DIVOUT        YECDVO
469500011127     C                   Z-ADD     TASQFT        YECIMI
469600011127     C   12              MOVE      '3'           YECDCO
469700011127     C  N12              MOVE      '0'           YECDCO
469800011127     C*
469900011127     C                   CALL      'YEURCO'
470000011127     C                   PARM                    YEUR
470100011127     C*
470200011127    2C     YECESI        IFEQ      ' '
470300011127     C                   Z-ADD     YECIMO        TASQFT
470400011127    2C                   END
470500011127    1C                   ENDIF
470600011127     C* SE TASIAL VALORIZZATO DEVO CONVERTIRLO SOLO SE RICHIESTO
470700011127     C*
470800011127    1C     TASIAL        IFGT      0
470900011127     C     CONIAL        ANDNE     'N'
471000011127     C                   CLEAR                   YEUR
471100011127     C                   MOVEL     DIVINP        YECDVI
471200011127     C                   MOVEL     DIVOUT        YECDVO
471300011127     C                   Z-ADD     TASIAL        YECIMI
471400011129     C   12              MOVE      '2'           YECDCO
471500011127     C  N12              MOVE      '0'           YECDCO
471600011127     C*
471700011127     C                   CALL      'YEURCO'
471800011127     C                   PARM                    YEUR
471900011127     C*
472000011127    2C     YECESI        IFEQ      ' '
472100011127     C                   Z-ADD     YECIMO        TASIAL
472200011127    2C                   END
472300011127    1C                   ENDIF
472400990921     C*
472500990921     C                   ENDSR
472600990923     C************************************************************
472700990923     C** CONVERSIONE DEL DETTAGLIO TARIFFA PARTICOLARE DI CARTELLO
472800990923     C************************************************************
472900990923     C     CONTPD        BEGSR
473000990923     C*
473100990923     C                   CLEAR                   YEUR
473200990923     C                   MOVEL     DIVCAR        YECDVI
473300990923     C                   MOVEL     §TADIV        YECDVO
473400991014     C   12              MOVE      '3'           YECDCO
473500991014     C  N12              MOVE      '0'           YECDCO
473600990923     C*
473700990923     C* CONTROLLLO IL TIPO TARIFFA SE E' VALORE OPPURE NO PER CONVERTIRE
473800990923     C* LO SCAGLIONE OPPURE NO
473900990923     C*
474000990923    1C     WTPG          IFEQ      'T'
474100990923     C     WTPG          OREQ      '4'
474200990923     C     WTPG          OREQ      'V'
474300990923     C*
474400990923     C                   Z-ADD     TPDSGL        YECIMI
474500990923     C                   CALL      'YEURCO'
474600990923     C                   PARM                    YEUR
474700990923    2C     YECESI        IFEQ      ' '
474800990923     C                   Z-ADD     YECIMO        TPDSGL
474900990923    2C                   END
475000990923     C* NON CONVERTO L'IMPORTO TARIFFA PERCHE' E' UNA PERCENTUALE
475100990923    1C                   ELSE
475200990923     C*
475300990923     C                   Z-ADD     TPDITR        YECIMI
475400990923     C                   CALL      'YEURCO'
475500990923     C                   PARM                    YEUR
475600990923    2C     YECESI        IFEQ      ' '
475700990923     C                   Z-ADD     YECIMO        TPDITR
475800990923    2C                   END
475900990923    1C                   ENDIF
476000990923     C*
476100990923     C* MINIMO
476200990923     C                   Z-ADD     TPDMIN        YECIMI
476300990923     C                   CALL      'YEURCO'
476400990923     C                   PARM                    YEUR
476500990923    1C     YECESI        IFEQ      ' '
476600990923     C                   Z-ADD     YECIMO        TPDMIN
476700990923    1C                   END
476800990923     C* MASSIMO
476900990923     C                   Z-ADD     TPDMAX        YECIMI
477000990923     C                   CALL      'YEURCO'
477100990923     C                   PARM                    YEUR
477200990923    1C     YECESI        IFEQ      ' '
477300990923     C                   Z-ADD     YECIMO        TPDMAX
477400990923    1C                   END
477500990923     C*
477600990923     C                   ENDSR
477700020826     C************************************************************
477800020826     C** CONTROLLO SE APPLICARE PESO NORMALE O VDL
477900020826     C************************************************************
478000020826     C     CTRPES        BEGSR
478100020826     C                   CLEAR                   USAPKF
478200030131     C**!!!              CLEAR                   §ASSPC
478300030131     C                   CLEAR                   TASSPC
478400030429     C                   CLEAR                   TASPKCN
478500020826     C*
478600020826     C* SE NON C'E' IL PESO VDL, PRENDO QUELLO DA FATTURARE
478700030131     C**!!!§ASPKC        IFEQ      0
478800030131     C     TASPKC        IFEQ      0
478900020826     C                   Z-ADD     TASPKF        USAPKF
479000020826     C                   GOTO      ENDPES
479100020826     C                   ELSE
479200020826     C* SE IL VDL NON + MAI DA APPLICARE --> USO IL DA FATTURARE
479300020826     C     §TATAP        IFEQ      'M'
479400020826     C                   Z-ADD     TASPKF        USAPKF
479500020826     C                   GOTO      ENDPES
479600020826     C                   ENDIF
479700020826     C                   ENDIF
479800020826     C* PESO VDL - TARA
479900150709      * verifico quale valore della tara devo recuperare
480000150709      * se data fattura <= data scadenza tara (§QTDST) calcolo la tara con la vecchia tara (§QTTCO)
480100150709      * altrimenti uso la tara attuale (§QTTCP)
480200150709     c                   if        tasdtfat <= §qtdst
480300150709     C     §QTTCO        MULT      TASNCP        WTARA
480400150709     c                   else
480500030131     C     §QTTPC        MULT      TASNCP        WTARA
480600150709     c                   endif
480700150709      *
480800030131     C     TASPKC        SUB(h)    WTARA         WNTARA
480900020826     C* SE PESO DA FATTURARE COMPRESO DA VDL E VDL-TARA
481000020826     C*  APPLICO SEMPRE IL PESO DA FATTURARE
481100020826     C*
481200020827     C     TASPKF        IFGE      WNTARA
481300030131     C**!!!TASPKF        ANDLE     §ASPKC
481400030131     C     TASPKF        ANDLE     TASPKC
481500020826     C                   Z-ADD     TASPKF        USAPKF
481600020826     C                   GOTO      ENDPES
481700020826     C                   ENDIF
481800020826     C* SE VDL-TARA > PESO FATTURARE LO APPLICO SIA PER " " CHE PER "S"
481900020827     C     WNTARA        IFGT      TASPKF
482000020827     C                   Z-ADD     WNTARA        USAPKF
482100030131     C**!!!              MOVEL     'S'           §ASSPC
482200030131     C**!!!              z-add     wntara        §ASpkc
482300030131     C                   MOVEL     'S'           TASSPC
482400030429     C                   z-add     wntara        TASpkcN
482500020826     C                   GOTO      ENDPES
482600020826     C                   ENDIF
482700020826     C* SE VDL-TARA < PESO FATTURARE LO APPLICO SOLO SE TOTALE E SE
482800020826     C* "S"
482900020827     C     WNTARA        IFLT      TASPKF
483000030131     C**!!!§ASNCP        ANDEQ     TASNCL
483100030131     C     TASNCP        ANDEQ     TASNCL
483200020826     C     §TATAP        ANDEQ     'S'
483300020827     C                   Z-ADD     WNTARA        USAPKF
483400030131     C**!!!              MOVEL     'S'           §ASSPC
483500030131     C**!!!              z-add     wntara        §ASpkc
483600030131     C                   MOVEL     'S'           TASSPC
483700030429     C                   z-add     wntara        TASpkcN
483800020826     C                   GOTO      ENDPES
483900020826     C                   ENDIF
484000020826     C* SE ANCORA A 0 (IMPOSSIBILE) USO IL PESO DA FATTURARE
484100020826     C     USAPKF        IFEQ      0
484200020826     C                   Z-ADD     TASPKF        USAPKF
484300020826     C                   ENDIF
484400020827     C**
484500020829     c     endpes        tag
484600151021      * valorizzo il flag di tipo applicazione volume
484700151021      * serve per escludere le bolle con tariffa mai applicazione VDL
484800151021      * dal calcolo del peso desunto (PRG 852)
484900151021     c                   eval      tastap = §tatap
485000151021
485100151021      * valorizzo il flag di KSC / CTR esclusione calcolo peso desunto
485200151021      * serve per escludere le bolle dei clienti che non vogliono
485300151021      * il  calcolo del peso desunto (PRG 852)
485400151021     c                   if        tamtpr = 'NO'
485500151021     c                   eval      tasepd = 'N'
485600151021     c                   else
485700151021     c                   clear                   tasepd
485800151021     c                   endif
485900151209
486000151209      * valorizzo il progressivo tariffa utilizzato in tassazione
486100151209     c                   movel     tamprg        tasprg
486200151209
486300151021
486400020829     C                   ENDSR
486500160114
486600160114      /free
486700160114       //--------------------------------------------------------------
486800160114       //?Carico la nuova cartello
486900160114       //--------------------------------------------------------------
487000160129       BEGSR TarCar;
487100160114
487200160129       //?Se ritassazione leggo il TNTAM05L con la TASDTCAR
487300160129         IF  ritass = 'S';
487400160129           setll (CARksc:CARctr:TASdtcar) TNTAM05L;
487500160129       //?Se NON è ritassazione leggo il TNTAM05L con la TASDCT
487600160129         ELSE;
487700160129           setll (CARksc:CARctr:TASdct) TNTAM05L;
487800160129         ENDIF;
487900160129       //?Se è fine file devo leggere l'ultima tariffa di KSC/CTR
488000160129         IF  not %found(TNTAM05L);
488100160129           readpe (CARksc:CARctr) TNTAM05L;
488200160129       //?Se NON è fine file leggo la tariffa successiva di KSC/CTR
488300160129         ELSE;
488400160129           reade (CARksc:CARctr) TNTAM05L;
488500160129         ENDIF;
488600160129       //?memorizzo il progressivo trovato
488700160211         CARprg = k_TAMprg;
488800160129
488900160114       ENDSR;
489000160114      /end-free
489100020412**
489200980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo del CONTRASSEGNO  1
489300980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo da ASSICURARE     2
489400980529Non trovata tariffa cliente                                                    3
489500980512Mancano importo da assicurare in bolla e valore merce in tariffa               4
489600980603Non trovato SCAGLIONE in tariffa                                               5
489700980512Manca la quantita' da fatturare                                                6
489800980603Non trovato SCAGLIONE in tariffa particolare
489900980603Non trovato CODICE TASSAZIONE in tariffa                                       8
490000980529Non trovata tariffa di CARTELLO                                                9
490100000125Tariffa cliente con decorrenza maggiore della data spedizione                 10
490200990917Divisa errata                                                                 11
490300000125Tariffa DPD ma bolla Italia                                                   12
490400000125Bolla import/export DPD  ma  tariffa NON DPD                                  13
490500011127Divisa tariffa del cliente non più utilizzabile                               14
490600020318Tariffa FEDEX ma bolla Italia                                                 15
490700020318Bolla import/export FEDEX ma  tariffa NON FEDEX                               16
490800020412Non trovato CODICE TASSAZIONE in tariffa PARTICOLARE                          17
490900030527Non presente tassazione per addebito insoluti                                 18
491000080221Tariffa valida ma imponibile minore di 0,000                                  19
