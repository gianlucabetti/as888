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
002200140227     FTIDPB01L  IF   E           K DISK
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
010600020618     D OG147         E DS
010700980504     D** DS PASSATE DAI PGM RICHIAMANTI
010800990916     D DTAS          E DS
010900050928
011000060922     D DTAS01        E DS
011100140227     D** DS DEL CAMPO TPTFLO DI TITPT
011200140227     D DTPT01        E DS
011300060922
011400130124     D DTASDTA       E DS
011500130124
011600990916     D DTASV         E DS
011700941112     D  SV                     1     20
011800941112     D                                     DIM(20)                              SIGLE VARIE
011900990916     D  VA                    21    140P 3
012000990916     D                                     DIM(20)                              IMPORTI VARIE
012100990916     D  ES                   141    160
012200011127     D*
012300941110     D TAMDS         E DS                  EXTNAME(TNTAM00F) INZ
012400130123      *
012500130123     d Status         sds
012600130123     d  SDSprm           *parms
012700130123      *
012800000000     D**  FILE TARIFFE  - MASTER
012900000000     D KPJBA         E DS
013000000125     D OG143         E DS                  INZ
013100000125     D DSQT1         E DS                  INZ
013200941107     D* DATI VARI TASSAZIONE 1
013300980429     D DS1A          E DS                  INZ
013400980429     D** TIPI INCASSO C/A
013500030205     D DSTR          E DS                  OCCURS(50) INZ
013600941107     D** TIPI TARIFFA
013700980603     D DSTB          E DS
013800980603     D** TIPI BOLLA
013900980429     D DS5E          E DS                  OCCURS(10) INZ
014000990917     D** MONETA DI CONTO/MONETA CORRENTE
014100990917     D DGED          E DS
014200990922     D** IMPORTI STANDARD NELLE MONETE DI CONTO
014300990922     D DGEI          E DS
014400040505     D** PERCENTUALE SUPPLEMENTO CARBURANTE
014500040505     D DPSC          E DS
014600080617     D** FILIALI ADDEBITO LASCIATO AVVISO
014700080617     D DLAV          E DS
014800111102     D** FILIALI ADDEBITO CENTRO STORICO
014900111102     D DZTL          E DS
015000990917     D** CALL PGM RICERCA TABELLA
015100990917     D DSBS02        E DS                  EXTNAME(TIBS02DS)
015200990917     D** CAMBIO DIVISA
015300990917     D YEUR          E DS                  EXTNAME(YEURCODS)
015400990922     D** CALL PGM CALCOLO TOTALE IMPONIBILE
015500990922     D DSLV16        E DS                  EXTNAME(FNLV16DS)
015600020221     D** CALL PGM RECUPERO TARIFFE DI CARTELLO
015700020221     D TRUL27        E DS                  EXTNAME(TRUL27DS)
015800980526     D PARAMT          DS
015900980526     D  PARCA                256    256
016000101014
016100101014     d TrulISTds     e ds
016200040505
016300040505      * -------------- variabili ------------------------------------
016400040505
016500040507     D Keytbe          S              3                                         chiave TBE
016600080617     D indx            S              3  0                                      Indice schiera
016700040507     D KX              S              3  0                                      Indice schiera
016800040507     D IMP_SCA         S             15  3                                      Impon.sup.carb.
016900040507     D PER_SCA         S              5  2                                      Perc.sup.carb.
017000060414     d applicarevers   s              1
017100060906
017200060906     d Sca_pb          s              3  0                                      scag.prez.base gasol
017300060906     d Sca_spe         s              3  0                                      scag.prez.gasol sped
017400060906     d Sca_sca         s              3  0                                      scaglioni scattati
017500080909     d Pmg_sgl         s             13  3                                      prezzo medio gasolio
017600140227     d Variafuel_min   s             11  3                                      Fuel minimo
017700040505
017800061219     d Sav_dsp         s              8  0                                      Salv. dta spedizione
017900080220
018000080220     d Wrksv           s             20                                         salvataggio sigle va
018100090922     d WvariaMag       s              1                                         salvataggio sigle va
018200090924     d WTSP            s                   like(taditr)
018300090924     d IMPON           s                   like(taditr)
018400090924     d WimpoMag        s                   like(taditr)
018500130123     d Ritass          s              1
018600160114     d MancaTariffa    s               n   inz(*off)
018700160114
018800990916     C****************************************************************
018900990916     C*  RIEPILOGO INDICATORI
019000990916     C***************************************************************
019100000125     C* 05    - DI COMODO
019200990916     C* 07    - LETTURA GENERICO
019300990916     C* 08    - OFF MANCA TARIFFA CLIENTE
019400990916     C* 09    - LETTURA TITAD
019500000103     C* 10    - LOKUP
019600990923     C* 12    - MONETA CHE ACCETTA IMPORTI CON DECIMALI
019700990916     C* 15    - GENERICO
019800990916     C* 17    - TARIFFA SCADUTA
019900990916     C* 18    - TARIFFA NON TROVATA
020000990916     C* 28    - LETTURA TARIFFE PARTICOLARI
020100990916     C* 49    - TROVATA TARIFFA DEL CAP DI ARRIVO
020200001009     C* 56    - TROVATA TARIFFA  (TARIFFA REVERSIBILE)
020300001009     C* 57    - ASSEGNATO      (NON + IN USO *!!*)
020400020225     C* 90    - MANCA TARIFFA DI CARTELLO
020500990916     C* 94    - MANCA TARIFFA
020600990916     C* 96    - SCHIERE VARIE
020700990916     C*****************************************************************
020800000000     C     *ENTRY        PLIST
020900941111     C                   PARM                    KPJBA
021000990922     C                   PARM                    DTAS
021100990922     C                   PARM                    DTASV
021200130123     C                   PARM                    DTASDTA
021300060922
021400130124      * se ricevo anche il 4° parametro vuol dire che vengo richiamato dal confronto fatturazione
021500130123      * o ritassazione
021600130124      * nel 4° parametro  DTASDTA verranno messe in caso di ritassazione 2 date.
021700130123      * la 1° per tassare ISTAT
021800130123      * la 2° per tassare il FUEL
021900160114      * c'è una terza data che è la data fattura
022000160114      * e una quarta data che è la data di riferimento per il caricamento della tariffa di cartello
022100160114      *   e cioè la data di riferimento impostata nel lancio del confronto fatturazione
022200130124     C                   IF        SDSPRM > 3
022300130123     C                   EVAL      RITASS = 'S'
022400130123     c                   else
022500130123     c                   eval      ritass = ' '
022600130123     C                   ENDIF
022700160114
022800160114      /free
022900160114       //?Se sono in ritassazione
023000160114         IF  ritass = 'S';
023100160114       //?Se la data per il caricamento della cartello è vuota imposto la TASDCT
023200160114           IF  TASdtcar = 0;
023300160114             TASdtcar = TASdct;
023400160114           ENDIF;
023500160114         ENDIF;
023600160114      /end-free
023700130123
023800060922     C                   MOVEL     TASFLO        DTAS01
023900110624      * verifico data spedizione con data attivazione varia email avviso destinatario
024000130923     c***                if        §asemd = 'S' and tasdsp < 20110901
024100130923     c***                clear                   §asemd
024200130923     c***                endif
024300110624
024400151021      * pulisco i campi della DTAS che sono di solo output
024500151021     c                   clear                   tastap
024600151021     c                   clear                   tasepd
024700151209     c                   clear                   tasprg
024800060922
024900920225     C                   Z-ADD     1             CODUT             1 0
025000000000     C*---------------------------------------------------------------*
025100020226     C*
025200020226     C** ACCESSO TITPT01L
025300941107     C     KISOT         KLIST
025400941107     C                   KFLD                    TAMKSC
025500941107     C                   KFLD                    TAMCTR
025600941107     C                   KFLD                    TAMPRG
025700990722     C                   KFLD                    FISO              2
025800020226     C     KTPTC         KLIST
025900020226     C                   KFLD                    CARKSC
026000020226     C                   KFLD                    CARCTR
026100020226     C                   KFLD                    CARPRG
026200020226     C                   KFLD                    FISO
026300930419     C*
026400020222     C** ACCESSO TFTPD02L
026500941107     C     KISOD         KLIST
026600941107     C                   KFLD                    TPTKSC
026700941107     C                   KFLD                    TPTCTR
026800941107     C                   KFLD                    TPTPRG
026900941107     C                   KFLD                    TPTFTC
027000941107     C                   KFLD                    ISOCTS
027100020412     C** ACCESSO TFTPD02L
027200020412     C     KISOD2        KLIST
027300020412     C                   KFLD                    TPTKSC
027400020412     C                   KFLD                    TPTCTR
027500020412     C                   KFLD                    TPTPRG
027600020412     C                   KFLD                    TPTFTC
027700020222     C** ACCESSO TABEL X CODICE
027800941107     C     KTAB          KLIST
027900941107     C                   KFLD                    CODUT
028000941107     C                   KFLD                    COD               2
028100020222     C** ACCESSO TABEL X CODICE + CHIAVE
028200941107     C     KTABC         KLIST
028300941107     C                   KFLD                    CODUT
028400941107     C                   KFLD                    COD
028500941107     C                   KFLD                    KEY               8
028600020222     C** ACCESSO TNTAM  TARIFFA DI CARTELLO
028700020222     C     KTAMC         KLIST
028800020226     C                   KFLD                    CARKSC
028900020226     C                   KFLD                    CARCTR
029000020226     C                   KFLD                    CARPRG
029100020222     C** ACCESSO TNTAM TARIFFE SCADUTA
029200941107     C     KTAM2         KLIST
029300941107     C                   KFLD                    KSC2
029400941111     C                   KFLD                    CTR2
029500941107     C                   KFLD                    PRG2
029600020222     C** ACCESSO TNTAM X CLIENTE
029700941107     C     KTAM          KLIST
029800941107     C                   KFLD                    TMCLI
029900941107     C                   KFLD                    TMCTR
030000020222     C** ACCESO TITAD
030100941107     C     KTAD          KLIST
030200941107     C                   KFLD                    TDCLI
030300941107     C                   KFLD                    TDPRG
030400020618     C                   KFLD                    WLNP
030500941112     C                   KFLD                    COMCTS
030600990722     C                   KFLD                    COMNAZ
030700990722     C                   KFLD                    COMCAP
030800941107     C                   KFLD                    TDCTR
030900020222     C** ACCESSO TITAD PER TARIFFA REVERSIBILE
031000941112     C     KTDREV        KLIST
031100941112     C                   KFLD                    TDCLI
031200941112     C                   KFLD                    TDPRG
031300981027     C                   KFLD                    KFIL
031400941112     C                   KFLD                    COMCTS
031500990722     C                   KFLD                    COMNAZ
031600941112     C                   KFLD                    COMCAP
031700941112     C                   KFLD                    TDCTR
031800020826     C     *LIKE         DEFINE    TASPKF        USAPKF
031900020827     C     *LIKE         DEFINE    §qttpc        WTARA
032000020827     C     *LIKE         DEFINE    TASPKF        WNTARA
032100020826     C     *LIKE         DEFINE    KSCFIL        KFIL
032200020618     C     *LIKE         DEFINE    TASLNP        SAVLNP
032300020618     C     *LIKE         DEFINE    TASLNP        WLNP
032400020618     C     *LIKE         DEFINE    TASCTS        COMCTS
032500941107     C***********     -------------       ***********
032600930419     C                   MOVEL     ' '           RT
032700930419     C*
032800941125     C* TASTLA = ' ' ELABORO E CHIUDO PGM  CON RETRN
032900941125     C* TASTLA = 'L' ELABORO E CHIUDO PGM  CON LR
033000941125     C* TASTLA = 'C' NO ELAB.  CHIUDO PGM  CON LR PER CHIUSURA FILE
033100941116     C     TASTLA        IFEQ      ' '
033200941116     C     TASTLA        OREQ      'L'
033300941107     C*
033400941116     C     TASTLA        IFEQ      ' '
033500930419     C                   MOVEL     'R'           RT                1
033600930419     C                   END
033700980505     C* C A M P I   D I   O U T P U T
033800980505     C                   CLEAR                   TASERR
033900980505     C                   CLEAR                   TASMSG
034000980505     C                   CLEAR                   TASIAL
034100980526     C                   MOVEL     KPJBU         PARAMT
034200030902      * clearizzo indicatiori x manca tariffa e inoltro
034300030902     C                   SETOFF                                       9456
034400030211
034500151209      * verifico data spedizione con data attivazione varia t Diritto di chiamata prenotazione
034600151209      * ritiro e packing list
034700151209     c                   if        Tasdsp < 20160101
034800151209     c                   clear                   TASPRT
034900151209     c                   clear                   TASSPL
035000151209     c                   Endif
035100151209
035200030211     c                   Clear                   wFiso
035300990930     C** CONTROLLO LA DIVISA CHE MI VIENE PASSATA SE DIVERSA DA BLANK VERIFICO
035400990930     C** CHE SIA CORRETTA
035500990930     C     TASDIV        IFNE      *BLANKS
035600990930     C                   CLEAR                   DSLV16
035700990930     C                   MOVEL     'D'           D17FIM
035800990930     C                   MOVE      TASDIV        D17DIV
035900991012     C****                 MOVE TASDSP    D17DSP
036000990930     C                   CALL      'FNLV16R'
036100990930     C                   PARM                    DSLV16
036200990930     C                   PARM                    DTASV
036300990930     C* CONTROLLO IL CODICE DI RITORNO
036400990930     C     O17ERR        IFNE      *BLANKS
036500990930     C                   MOVEL     'E'           TASERR
036600990930     C                   MOVEL     O17MSG        TASMSG
036700990930     C                   SETON                                        94
036800990930     C                   GOTO      FINE
036900990930     C                   ENDIF
037000990930     C                   ENDIF
037100941125     C*
037200980605     C** CARICA TABELLE SOLO LA 1° VOLTA
037300941125     C     CARTBL        IFEQ      *BLANK
037400941125     C                   MOVE      '1'           CARTBL            1
037500941125     C                   EXSR      CARTAB
037600011127      * RECUPERO UDATE SOLO LA PRIMA VOLTA
037700011127     C                   EXSR      RUDATE
037800020320     C     TASMSG        IFNE      *BLANKS
037900020320     C                   SETON                                        94
038000020320     C                   GOTO      FINE
038100020320     C                   ENDIF
038200941125     C                   END
038300011127     C* R E C U P E R O   D A T A   D I   T A S S A Z I O N E
038400030131     C**!!!              EXSR      RECDAT
038500991012     C*
038600991012     C** SALVO GLI IMPORTI CHE MI VENGONO PASSATI
038700991012     C                   EXSR      SAVDS
038800941125     C*
038900941110     C                   SETOFF                                       94
039000160114     c                   eval      MancaTariffa = *off
039100030925      *
039200030925      * se richiamato per il solo calcolo della varia D  VADO A FINE
039300030925     C                   IF        TASSVA = 'D'
039400030925     C                   GOTO      FINE
039500030925     C                   ENDIF
039600030925      *+
039700941124     C                   MOVEL     TASTBL        TIPBOL            1
039800941124     C                   MOVEL     TASKSC        KSCFIL            3 0
039900941124     C                   MOVE      TASKSC        KSCCOD            4 0
040000080617      * da giugno 2008 viene riattivato per alcune filiali l'addebito del lasciato avviso
040100080617      * se tasric diverso da blank .. quindi la spedizione ha un evento di lasciato avviso
040200080617      * verifico se la filiale del cliente è presente nella tabella LAV oppure nella tabella
040300080617      * LAV esiste la filiale 999
040400080617    1c                   If        TasRic <> ' '
040500080617     c                   clear                   indx
040600080617     c                   eval      indx = %lookup(kscfil:sfillav)
040700080617      * se indice =  a zero cerco con la filiale 999
040800080617    2c                   if        indx = *zeros
040900080617     c                   eval      indx = %lookup(999:sfillav)
041000080617    2c                   endif
041100080617      * pulisco il flag del calcolo dell'addebito del lasciato avviso perchè filiale cliente
041200080617      * non abilitato all'addebito
041300080617    2c                   If        indx = *zeros
041400080617     c                   clear                   tasric
041500080617      * altrimenti verifico la data attivazione tassazione
041600080617   x2c                   else
041700080617      * se data spedizione inferiore all'attivazione pulisco il flag
041800080618    3c                   if        tasdsp < sdtalav(indx)
041900080617     c                   clear                   tasric
042000080617    3c                   endif
042100080617    2c                   endif
042200080617    1c                   endif
042300080617     c
042400980429     C* VEDO SE TIPO INCASSO E' INTESTATO AL MITTENTE
042500980430     C                   CLEAR                   TASBM
042600061016      * se il primo carattere del campo TASTIC è uguale a blank
042700061016      * ci troviamo a tassare una spedizione in sede in quanto
042800061016      * in TNCSB (file contrassegni ) la combinazione di tpa (tipo assegno)
042900061018      * e tpi (tipo intestazione) o fus (tipo intestazione in bolla)
043000061018      * potrebbe non esistere in tabella  1A tipo incasso c/assegno
043100061016      * a questo punto prendo solo il tipo intestazione assegno che
043200061016      * è uguale a  M per mittente  e "B" o " " per Vettore
043300061016      * In Tasbm passo solo se è uguale a M
043400061016
043500061016     C                   If        %subst(tastic:1:1) = ' '
043600061016     C                   If        %subst(tastic:2:1) = 'M'
043700061016     c                   eval      TASBM = 'M'
043800061016     c                   endif
043900061016
044000061016     c                   else
044100980429     C     TASTIC        LOOKUP    TIC                                    96
044200980429     C   96              MOVEL     'M'           TASBM             1
044300061016     c                   endif
044400980605     C** TASSAZIONE
044500941107     C                   EXSR      TARIFF
044600941110     C     FINE          TAG
044700980605     C** 94 ON - MANCA TARIFFA
044800990922     C     *IN94         IFEQ      '1'
044900990929     C* E NON C'E' TOTALE IMPONIBILE  DO ERRORE
045000990929     C     TASIMV        IFEQ      *ZEROS
045100990922     C                   MOVEL     'E'           TASERR            1
045200990929     C                   ENDIF
045300160114      * se ho il flag MancaTariffa impostato, vuol dire che è un puro manca tariffa
045400160114      * msg 3 o msg 10, quindi imposto 'M' nel campo TASERR
045500160114     c                   IF        MancaTariffa
045600160114     c                   eval      TASerr = 'M'
045700160114     c                   ENDIF
045800990922     C* REIMPOSTO LA DS COME PRIMA SENZA CONVERSIONI IN MONETA
045900990922     C*    MI SEMBRA INUTILE ADESSO EVENTUALMENTE LO FACCIO DOPO
046000990922     C*
046100990922     C                   ELSE
046200990929     C* CALCOLO IL TOTALE IMPONIBILE SE E' UGUALE A ZEROS
046300990929     C*
046400990929     C     TASIMV        IFEQ      *ZEROS
046500990923     C                   CLEAR                   DSLV16
046600990923     C                   MOVEL     'T'           D17FIM
046700990923     C                   Z-ADD     TASPOR        D17POR
046800990923     C                   CALL      'FNLV16R'
046900990923     C                   PARM                    DSLV16
047000990923     C                   PARM                    DTASV
047100001205     C*
047200001205     C* SE SPEDIZIONI CON DATA MAGGIORE O UGUALE AL 01012001 CALCOLO L'ADDIZIONALE DI GESTIONE
047300001211     C* SUL TOTALE IMPONIBILE E DOPO CALCOLO SUL TOTALE IMPONIBILE + ADDIZ. GESTIONE L'ISTAT
047400001205     C*
047500001221     C     TASDSP        IFGE      20010101
047600020416     C* ESEGUO SOLO SE NON HO RICHIAMATO LA TASSAZIONE PER UNA VARIA
047700031120     C*  SOLA SE VARIA NON PRESENTE NELLA TABELLA T3A (TABELLA DELLE VARIE DELLE BOLLE CHE
047800031121     C* ACCETTANO UNA SINGOLA  VARIA ESEMPIO "O" "Y" "*".....)
047900031120     C*    TASSVA        ANDEQ     ' '
048000031120      *
048100031120     C                   IF        TASSVA <> ' '
048200031120     C                   SETOFF                                       11
048300031120     C     TASSVA        LOOKUP    T3A(1)                                 11
048400031120     C                   ENDIF
048500040113      *
048600040113      *
048700040113      * SE VARIA UGUALE A BLANK                                             OPPURE
048800040113      * VARIA PRESENTE NELLA TABELLA T3A                                    OPPURE
048900040113      * TASSAZIONE 2° BOLLA     TASSVA = 'G' E TASCVAG = 'S'                OPPURE
049000040113      * VARIA UGUALE A "Z"                                                  OPPURE
049100130124      * CALCOLO A.G.+ ISTAT                                                 OPPURE
049200130124      * CALCOLO ISTAT
049300130124      *
049400040113     C                   IF        TASSVA = ' ' OR
049500040113     C                             %FOUND       OR
049600040113     C                             (TASSVA = 'G' AND TASCVAG = 'S') OR
049700130124     C                             TASSVA = 'Z' OR
049800130124     C                             TASSVA = 'L'
049900001205     C                   EXSR      ADGIST
050000001205     C                   ENDIF
050100031120      *
050200031120     C                   ENDIF
050300001205     C*
050400990923     C                   Z-ADD     O17IMV        TASIMV
050500001205     C*
050600990929     C                   ENDIF
050700030924      *
050800030924      * DIRITTO DI FATTURAZIONE lo calcolo solo alla fine della TASSAZIONE senza aggiunte dell'addi-
050900030924      *                         zionale di gestione
051000030924      *                         se totale imponibile maggiore di zero
051100030924     C                   IF        TASIMV > 0 AND
051200030924     C                             (TASSVA = §$2VRD OR TASSVDF= §$2VRD)
051300030924      *
051400030924     C                   EXSR      DIRFAT
051500030924      *
051600030924     C                   ENDIF
051700990922     C* MUOVO LA DIVISA DELLA TARIFFA
051800020416     C*  SE NON PASSATA LA VARIA O SE PASSATA E IMPORTO >0
051900020416     C*
052000020416     C     TASSVA        IFEQ      *BLANKS
052100020416     C     O17IMV        ORGT      0
052200990929     C                   MOVE      §TADIV        TASDIV
052300020416     C                   ENDIF
052400020416     C*
052500020416     C* SE SI TRATTA DI UNA VARIA DA PASSARE ANCHE A 0 CONTROLLO
052600020416     C     TASSVA        IFNE      *BLANKS
052700020416     C     O17IMV        ANDEQ     0
052800020416     C     TASSVA        LOOKUP    SV0                                    05
052900020416     C     *IN05         IFEQ      *ON
053000020416     C                   MOVEL     TASSVA        SV(1)
053100020416     C                   ENDIF
053200020416     C                   ENDIF
053300080221
053400080221      * VERIFICO SE IMPONIBILE A ZERO SENZA SIGLE DI VARIE PERCHÈ QUESTO TIPO DI CONTROLLO
053500080221      * VIENE ESEGUITO DAI PGM CHIAMANTI
053600080221     c                   clear                   wrksv
053700080221     c                   movea     sv            wrksv
053800080221     C                   IF        TASIMV = 0 AND wrkSV = *BLANKS
053900080221     C                   MOVEL     'E'           TASERR            1
054000080221     C                   MOVEL     MSG(19)       TASMSG
054100080221     C                   SETON                                        94
054200080221     C                   GOTO      FINE
054300080221     C                   ENDIF
054400990922     C*
054500990922     C                   ENDIF
054600011127     C* CONTROLLO LA DIVISA IN BASE ALLA DATA TASSAZIONE SE DA CONVERTIRE OPPURE NO
054700011127     C* SE STO TASSANDO CON DATA > DEL 2001 UNA BOLLA CON DATA < DEL 2002 E LA DIVISA NON E'
054800011127     C* UGUALE ALLA MONETA DI CONTO AZIENDALE CONVERTO TUTTI GLI IMPORTI ANCHE LA QTA DA
054900011127     C* FATTURARE NELLA DIVISA DI CONTO
055000011127     C*
055100020826     C**         DATTAS    IFGE 20020101
055200020826     C     TASDIV        IFNE      §GEDCN
055300011127     C     TASDSP        ANDLT     20020101
055400011127     C* CONVERTO
055500011127     C                   MOVE      TASDIV        DIVINP
055600011127     C                   MOVE      §GEDCN        DIVOUT
055700011127     C                   MOVE      'S'           CONQTA
055800011127     C                   MOVE      'S'           CONIAL
055900011127     C*
056000011127     C* RECUPERO LE CARATTERISTICHE DELLA MONETA DI CONTO
056100011127     C                   SETOFF                                       12
056200011127     C                   Z-ADD     1             K
056300011127     C     §GEDCN        LOOKUP    CV(K)                                  10
056400011127     C     *IN10         IFEQ      *ON
056500011127     C     K             OCCUR     DSCV
056600011127     C     §CVFDC        IFEQ      'S'
056700011127     C* DIVISA CHE ACCETTA DECIMALI
056800011127     C                   SETON                                        12
056900011127     C                   ENDIF
057000011127     C                   ENDIF
057100011127     C*
057200011127     C                   EXSR      CNVTAS
057300011127     C* VALORIZZO  LA DIVISA
057400011127     C                   MOVEL     §GEDCN        TASDIV
057500011127     C*
057600011127     C                   ENDIF
057700990922     C*
057800941107     C                   END
057900941107     C*
058000941107     C     RT            IFEQ      'R'
058100941107     C                   RETURN
058200941107     C                   ELSE
058300020319     C* CHIUDO PGM TRUL27R
058400020319     C                   CLEAR                   TRUL27
058500020319     C                   MOVE      'C'           I27TLA
058600020319     C                   CALL      'TRUL27R'
058700020319     C                   PARM                    TRUL27
058800941107     C                   SETON                                        LR
058900941107     C                   END
059000941107     C************       -----------       *************
059100941107     C**
059200941112     C******************************************************
059300941110     C**  CALCOLA TASSAZIONE BOLLA
059400941112     C******************************************************
059500941110     C     TARIFF        BEGSR
059600980605     C                   MOVEL     'I'           WBOEST
059700000125     C                   CLEAR                   WBODPD
059800020318     C                   CLEAR                   WBOFED
059900991014     C                   SETOFF                                       12
060000020318     C**
060100020318     C** PER VERIFICARE SE SONO BOLLA DPD FEDEX O ESTERA CHIAMO TRUL27
060200020318     C**  SENZA LA DATA
060300020319     C                   CLEAR                   TRUL27
060400020318     C                   MOVEL     TASTSP        I27TSP
060500020318     C                   MOVEL     TASLNA        I27LNA
060600020318     C                   MOVEL     TASLNP        I27LNP
060700020319     C                   MOVEL     TASKSC        I27CLI
060800021122     c                   Movel     Tasctr        I27Ctb
060900020318     C                   CALL      'TRUL27R'
061000020318     C                   PARM                    TRUL27
061100020318      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
061200020318     C     O27ERR        IFEQ      *BLANKS
061300020318     C     O27FIE        IFEQ      'E'
061400020318     C                   MOVEL     'E'           WBOEST            1
061500020318     C                   ENDIF
061600020318     C*
061700020318     C     O27FIE        IFEQ      'D'
061800020318     C                   MOVEL     'S'           WBODPD            1
061900020318     C                   MOVEL     'I'           WBOEST            1
062000020318     C                   ENDIF
062100020318     C*
062200020318     C     O27FIE        IFEQ      'F'
062300021122     C     O27FIE        orEQ      'M'
062400021122     C     O27FIE        orEQ      'N'
062500020318     C                   MOVEL     'S'           WBOFED            1
062600020318     C                   MOVEL     'E'           WBOEST            1
062700020318     C                   ENDIF
062800020318     C                   ELSE
062900020319     C                   SETON                                        94
063000020319     C                   MOVEL     O27MSG        TASMSG
063100020318     C                   GOTO      FINE
063200020318     C                   ENDIF
063300020320     C**
063400030429     C* LO PASSO IN OUTPUT
063500030131     C**!!!              MOVEL     O27FIE        §ASFIE
063600030131     c                   MOVEL     O27FIE        TASFIE
063700021122      * Per tutte le tipologie di bolla FedEx imposto sempre 'F' xchè il
063800021122      * TNSF02 testa 'F' x definire che è una bolla FedEx (descrizione varie)
063900021122     c                   If        O27Fie = 'M' or O27Fie = 'N'
064000030131     c**!!!              Movel     'F'           §AsFie
064100030131     c                   Movel     'F'           TAsFie
064200021122     c                   EndIf
064300980605     C**
064400980605     C** VEDO SE BOLLA ITALIA O ESTERA (IMPORT O EXPORT)
064500020318     C**         TASLNP    LOKUPEST                      05
064600020318     C**N05      TASLNA    LOKUPEST                      05
064700020318     C**         *IN05     IFEQ *ON
064800020318     C**                   MOVEL'E'       WBOEST  1
064900020318     C**                   ENDIF
065000020318     C**         KSCFIL    LOKUPDPD                      05
065100020318     C**N05      TASLNA    LOKUPDPD                      05
065200000828     C******* TOLTO CONTROLLO CON LA LNP BASTA LNA E LINEA KSC 27/7
065300000828     C**      28/8
065400000828     C**      BISOGNA LASCIARE IL CONTROLLO ANCHE PER LNP PER I RESI
065500000828     C**      DPD CHE SONO IN ASSEGNATO SU LNA E CLIENTE ITALIA MA
065600000828     C**      LNP DPD
065700000828     C**      NON RICORDO I PROBLEMI CHE PUO' DARE USARE ANCHE LA LNP
065800000828     C**      HO DEI DUBBI SUI DROTTAMENTI 01 PER 190 E POI 190 PER 310
065900000828     C**      FORSE E' STATO SCELTO IL "MALE MINORE"
066000020318     C**N05      TASLNP    LOKUPDPD                      05
066100020318     C**         *IN05     IFEQ *ON
066200020318     C**                   MOVEL'S'       WBODPD  1
066300020318     C**                   MOVEL'I'       WBOEST  1
066400020318     C**                   ENDIF
066500001201     C*
066600001201     C* VERIFICO SE CODICE APPARTIENE ALLA SDI
066700001201     C*
066800001201     C                   CLEAR                   KSDIT
066900001201     C     KSCFIL        CHAIN     AZORG01L                           07
067000001201     C     *IN07         IFEQ      *OFF
067100001201     C                   MOVEL     ORGDIT        KSDIT             3
067200001201     C                   ENDIF
067300980605     C**
067400980605     C**
067500020222     C* PER 8888 E 9999 --> CERCO LA TARIFFA DI CARTELLO RELATIVA AL TIPO DI SPEDIZIONE
067600980605    1C     KSCCOD        IFEQ      8888
067700980527     C     KSCCOD        OREQ      9999
067800020225      * CERCO LA TARIFFA DI CARTELLO
067900020225     C                   EXSR      CERTCA
068000020319     C                   Z-ADD     CARKSC        KSC2
068100020319     C                   Z-ADD     CARCTR        CTR2
068200020319     C                   Z-ADD     CARCTR        CODCTR
068300020319     C                   Z-ADD     CARPRG        PRG2
068400020225      *
068500980605     C     KTAM2         CHAIN     TNTAM00L                           94
068600020222      *
068700980605     C** NON TROVATA (IMPOSSIBILE!!!)
068800980605    2C     *IN94         IFEQ      *ON
068900980605     C                   MOVEL     MSG(10)       TASMSG
069000980605     C                   GOTO      FINE
069100980605    2C                   ENDIF
069200980605     C* KSCCOD NON LO IMPOSTO PERCHE'PER I 9999 VOGLIO CHE RIMANGA TALE
069300980605     C**
069400980605   X1C                   ELSE
069500941112     C** ACCESSO TNTAM
069600980605     C                   Z-ADD     TASCTR        CODCTR            3 0
069700980605     C                   Z-ADD     TASKSC        CODCLI            7 0
069800980605     C**
069900941110     C                   Z-ADD     CODCLI        TMCLI             7 0
070000980515     C                   Z-ADD     CODCTR        TMCTR             3 0
070100941110     C                   EXSR      CERTAM
070200020227     C** SE 90 /94 SONO SPENTI NON HO TROVATO LA TARIFFA DI CARTELLO RELATIVA ALLA TARIFFA
070300020319     C** DEL CLIENTE   (QUESTO CASO L'HO ELIMINATO)
070400020227     C     *IN90         IFEQ      *OFF
070500020227     C     *IN94         ANDEQ     *OFF
070600020225     C*** NON SO COSA FARE ANCHE PERCHE' E' IMPOSSIBILE MA VADO A FINE LO STESSO
070700020225     C                   SETON                                        94
070800020225     C                   GOTO      FINE
070900020225     C                   ENDIF
071000980604     C** NON C'E' TARIFFA CLIENTE
071100980605    2C     *IN94         IFEQ      *ON
071200980604     C**
071300980605     C* SE NON HO CHIAMATO PER UNA VARIA  -- > ERRORE
071400020416     C* NON DO ERRORE SOLO PER LA VARIA "R"
071500020416    3C     TASSVA        IFNE      'R'
071600980604     C                   GOTO      FINE
071700980605   X3C                   ELSE
071800980604     C**
071900020416     C** SE HO CHIAMATO PER LA VARIA 'R' --> IMPOSTO LA TAR.CARTELLO
072000980605     C* IMPOSTO ANCHE KSCCOD perche' voglio che me lo tratti come
072100980605     C*  cliente generico da tariffa di cartello per l'assicurazione
072200980605     c*  Non ho problemi per i 9999 èerche' avendoli gia' impostati
072300980605     c*  la prima volta che fa  certam trova la tariffa (di cartello)
072400980605     C                   MOVE      8888          KSCCOD
072500020225     C* CERCO LA CARTELLO IN BASE ALLE CARATTERISTICHE DELLA SPEDIZIONE EW DEL CLIENTE
072600020225     C                   EXSR      CERTCA
072700020319     C                   Z-ADD     CARKSC        KSC2
072800020319     C                   Z-ADD     CARCTR        CTR2
072900020319     C                   Z-ADD     CARCTR        CODCTR
073000020319     C                   Z-ADD     CARPRG        PRG2
073100020225     C*
073200980605     C     KTAM2         CHAIN     TNTAM00L                           94
073300980605     C** NON TROVATA (IMPOSSIBILE!!!)
073400980605    4C     *IN94         IFEQ      *ON
073500980605     C                   MOVEL     MSG(10)       TASMSG
073600980605     C                   GOTO      FINE
073700980605    4C                   ENDIF
073800020225     C* MI CARICO LA DIVISA DELLA CARTELLO
073900020225     C                   MOVEL     TAMFLO        DSTA01
074000020225     C                   MOVEL     §TADIV        DIVCAR
074100020225     C*
074200980605    3C                   ENDIF
074300980605    2C                   ENDIF
074400980605    1C                   ENDIF
074500980604     C**
074600941110     C                   Z-ADD     TAMKSC        TDCLI             7 0
074700941110     C                   Z-ADD     TAMPRG        TDPRG             3 0
074800941110     C                   Z-ADD     TAMCTR        TDCTR             3 0
074900980609     C* CAMPO DI OUTPUT
075000000125     C                   MOVEL     TAMBAP        TASBAP
075100980609     C                   MOVEL     TAMFLO        DSTA01
075200000125     C*
075300000125     C* SE BOLLA DPD E TARIFFA DI CARTELLO --> ERRORE SE NON RICHIAMO
075400020319     C*  PER UNA VARIA E SE LA CARTELLO NON E' UNA CARTELLO DPD
075500000125     C     KSCCOD        IFEQ      8888
075600000125     C     KSCCOD        OREQ      9999
075700020416     C     TASSVA        IFNE      'R'
075800000125     C     WBODPD        ANDEQ     'S'
075900020225      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
076000020225     C     §TADPD        ANDNE     'S'
076100000125     C                   SETON                                        94
076200000125     C                   MOVEL     MSG(13)       TASMSG
076300000125     C                   GOTO      FINE
076400000125     C                   ENDIF
076500020416     C     TASSVA        IFNE      'R'
076600020319     C     WBOFED        ANDEQ     'S'
076700020319      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
076800020319     C     §TAFED        ANDNE     'S'
076900020319     C                   SETON                                        94
077000020319     C                   MOVEL     MSG(16)       TASMSG
077100020319     C                   GOTO      FINE
077200020319     C                   ENDIF
077300000125     C                   ENDIF
077400000125     C*
077500000125     C* SE TARIFFA O CLIENTE TARIFFA DPD E BOLLA NON LO E'--> MANCA TAR
077600000125     C     §TADPD        IFEQ      'S'
077700000125     C     WBODPD        IFEQ      ' '
077800000125     C                   SETON                                        94
077900000125     C                   MOVEL     MSG(12)       TASMSG
078000000125     C                   GOTO      FINE
078100000125     C                   ENDIF
078200000125     C                   ELSE
078300000125     C     WBODPD        IFEQ      'S'
078400000125     C                   SETON                                        94
078500000125     C                   MOVEL     MSG(13)       TASMSG
078600000125     C                   GOTO      FINE
078700000125     C                   ENDIF
078800000125     C                   ENDIF
078900020319     C* SE TARIFFA O CLIENTE TARIFFA FED E BOLLA NON LO E'--> MANCA TAR
079000020319     C     §TAFED        IFEQ      'S'
079100020319     C     WBOFED        IFEQ      ' '
079200020319     C                   SETON                                        94
079300020319     C                   MOVEL     MSG(15)       TASMSG
079400020319     C                   GOTO      FINE
079500020319     C                   ENDIF
079600020319     C                   ELSE
079700020319     C     WBOFED        IFEQ      'S'
079800020319     C                   SETON                                        94
079900020319     C                   MOVEL     MSG(16)       TASMSG
080000020319     C                   GOTO      FINE
080100020319     C                   ENDIF
080200020319     C                   ENDIF
080300000125     C**
080400990927     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
080500990927     C     §TADIV        IFEQ      *BLANK
080600990927     C                   MOVE      'ITL'         §TADIV
080700990927     C                   ENDIF
080800011127      * VERIFICO IN BASE ALLA DATA TASSAZIONE E DATA SPEDIZIONE SE LA DIVISA DELLA TARIFFA
080900011127      * E' CORRETTA
081000011127      *
081100011127      * SE STO TASSANDO con data > del 2001 un bolla con data > del 2001 E LA DIVISA NON E'
081200011127      * UGUALE ALLA MONETA DI CONTO AZIENDALE
081300020826     C***        DATTAS    IFGT 20011231
081400020826     C     §TADIV        IFNE      §GEDCN
081500011127     C     TASDSP        ANDGT     20011231
081600011127      * MANCA TARIFFA
081700011127     C                   SETON                                        94
081800011127     C                   MOVEL     MSG(14)       TASMSG
081900011127     C                   GOTO      FINE
082000011127     C                   ENDIF
082100991014     C*
082200991014     C* RECUPERO LE CARATTERISTICHE DELLA DIVISA
082300991014     C                   Z-ADD     1             K
082400991014     C     §TADIV        LOOKUP    CV(K)                                  10
082500991014     C     *IN10         IFEQ      *ON
082600991014     C     K             OCCUR     DSCV
082700991014     C     §CVFDC        IFEQ      'S'
082800991014     C* DIVISA CHE ACCETTA DECIMALI
082900991014     C                   SETON                                        12
083000991014     C                   ENDIF
083100991014     C                   ENDIF
083200990921     C** RICERCA TNTAM
083300990921     C** VERIFICO SE LA MONETA PASSATA E'= A QUELLA  DELLA TARIFFA
083400990921     C     TASDIV        IFNE      §TADIV
083500990928     C* E LA TARIFFA PASSAT NON E' UGUALE A BLANK
083600990928     C     TASDIV        ANDNE     *BLANKS
083700990921     C** CONVERTO GLI IMPORTI NELLA MONETA DELLA TARIFFA CLIENTE
083800990921     C*  ANCHE LE VARIE
083900011127     C*
084000011127     C* PASSO LE MONETE PER LA CONVERSIONE ED IN PIÙ IL FLAG SE DEVO
084100011127     C* CONVERTIRE LA QUANTITA' DA FATTURARE OPPURE NO
084200011127     C* NEL CASO IN CUI CONVERTO NELLA MONETA DELLA TARIFFA
084300011127     C* NON E' NECESSARIO CONVERTIRE LA QTA DA FATTURARE E
084400011127     C* L'IMPORTO D'ASSICURARE CALCOLATO IN TASSAZIONE
084500011127     C                   MOVE      TASDIV        DIVINP            3
084600011127     C                   MOVE      §TADIV        DIVOUT            3
084700011127     C                   MOVE      'N'           CONQTA            1
084800011127     C                   MOVE      'N'           CONIAL            1
084900990921     C                   EXSR      CNVTAS
085000990921     C*
085100990921     C                   ENDIF
085200990929     C*------------------------------------------------------
085300990929     C* SE IMPONIBILE VALORIZZATO VADO A FINE
085400990929     C     TASIMV        IFNE      *ZEROS
085500000103     C* E NON E' STATA RICHIESTA NESSUNA VARIA
085600000103     C     TASSVA        ANDEQ     ' '
085700990929     C                   GOTO      FINE
085800990929     C                   ENDIF
085900980506     C*------------------------------------------------------
086000020826     C* VERIFICO SE APPLICARE IL PESO VDL O MENO
086100020826     C                   EXSR      CTRPES
086200020826     C*------------------------------------------------------
086300980506     C** PORTO O INOLTRO PRETASSATO
086400990721     C**  OPPURE DEVO CALCOLARE UNA VARIA SPECIFICA --> NON CERCO TITAD
086500941110     C     TASPOR        IFGT      0
086600980506     C     TASINL        ORGT      0
086700980506     C     TASSVA        ORNE      ' '
086800000516     C**                   Z-ADD0         TASPVL
086900941110     C                   GOTO      TASSAT
087000941110     C                   END
087100941110     C*
087200941112     C                   MOVEL     CODCTR        UNOCTR            1 0
087300980506     C* QUANTITA' DA FATTURARE
087400950119     C     UNOCTR        IFEQ      5
087500950210     C     TASQFT        COMP      0                                      94
087600980512     C   94              MOVEL     MSG(6)        TASMSG
087700980512     C   94              GOTO      FINE
087800950119     C                   END
087900950119     C** TARIFFA A QUANTITA' - MANCA Q.TA' DA FATTURARE
088000950119     C     UNOCTR        IFEQ      4
088100950119     C     TAMFVD        IFEQ      *BLANKS
088200950119     C     TAMFVD        OREQ      '2'
088300961007     C     TAMFVD        OREQ      '4'
088400950210     C     TASQFT        COMP      0                                      94
088500980512     C   94              MOVEL     MSG(6)        TASMSG
088600950119     C   94              GOTO      FINE
088700950120     C                   GOTO      POIQTF
088800950119     C                   END
088900950119     C** TARIFFA A VALORE FLAG 2 E BLANK
089000950119     C     TAMFVD        IFEQ      '1'
089100950210     C     TASQFT        CABGT     0             POIQTF
089200950119     C                   END
089300950119     C*
089400950210     C                   Z-ADD     TASIAS        TASQFT
089500950119     C                   Z-ADD     1             K
089600950119     C     TASVAS        LOOKUP    CV(K)                                  05
089700980527     C     *IN05         IFEQ      *OFF
089800980527     C                   SETON                                        94
089900980527     C                   MOVEL     MSG(2)        TASMSG
090000980527     C                   GOTO      FINE
090100980527     C                   ENDIF
090200990917     C**
090300990917     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA
090400990917     C                   CLEAR                   YEUR
090500990917     C                   MOVEL     TASVAS        YECDVI
090600990917     C                   MOVEL     §TADIV        YECDVO
090700990917     C                   Z-ADD     TASQFT        YECIMI
090800991014     C   12              MOVE      '3'           YECDCO
090900991014     C  N12              MOVE      '0'           YECDCO
091000990917     C*
091100990917     C                   CALL      'YEURCO'
091200990917     C                   PARM                    YEUR
091300990917     C*
091400990917     C     YECESI        IFEQ      ' '
091500990917     C                   Z-ADD     YECIMO        TASQFT
091600990917     C                   END
091700990923     C****                 ENDIF
091800990917     C****       K         OCUR DSCV
091900990917     C****       TASVAS    IFNE §CVITA
092000990917     C****       TASQFT    MULT §CVCMB    TASQFT
092100990917     C****                 END
092200950119     C* CAMBIO
092300950210     C     TASQFT        IFEQ      0
092400950119     C                   EXSR      CALVAS
092500950210     C                   Z-ADD     IMPVAS        TASQFT
092600950119     C                   END
092700950210     C     TASQFT        COMP      0                                      94
092800980527     C   94              MOVEL     MSG(6)        TASMSG
092900950119     C   94              GOTO      FINE
093000950119     C** TARIFFA A VALORE FLAG 1 E 3 - NON ESISTE VALORE ASSICURATIVO
093100950119     C                   END
093200950119     C     POIQTF        TAG
093300980518     C* MI SERVE MINTAS PER IL CARICAMENTO DELLO SCAGLIONE DEL
093400980518     C*  MINIMO TASSABILE
093500980518     C                   Z-ADD     1             A
093600980518     C                   MOVEL     UNOCTR        WTPG              1
093700980518     C     WTPG          LOOKUP    CTR(A)                                 05
093800980518     C   05A             OCCUR     DSTR
093900980518     C   05              Z-ADD     §TRATA        MINTAS
094000990722     C*
094100990722     C** RECUPERO CODICE NAZIONE MITTENTE E DESTINATARIO DAL CODIEC TASSAZIONE
094200990722     C*
094300990923     C***                  Z-ADD1         A
094400990923     C***        TASCTS    LOKUPCTS,A                    05
094500990923     C***05      A         OCUR DSCT
094600990923     C***05                MOVEL§CTNAR    TASNAD  3
094700990722     C*
094800990923     C***                  Z-ADD1         A
094900990923     C***        TASMCT    LOKUPCTS,A                    05
095000990923     C***05      A         OCUR DSCT
095100990923     C***05                MOVEL§CTNAR    TASNAM  3
095200980518     C*
095300990721     C** RICERCA TITAD
095400980518     C                   EXSR      CARTAR
095500980518     C*
095600980313     C** PESO TASSABILE E QUANTITA' DA FATTURARE
095700980429     C                   MOVEL     UNOCTR        WTPG              1
095800980312     C                   CLEAR                   WRPV
095900040603     C                   CLEAR                   RAPPV
096000980312     C                   MOVEL     'TAD'         WDET              3
096100980313     C                   Z-ADD     TAMARL        WARL
096200980313     C                   Z-ADD     TAMARF        WARF
096300980313     C                   Z-ADD     TAMARO        WARO
096400980312     C                   EXSR      CALCOL
096500980313     C                   Z-ADD     ISOPES        PESTAS
096600941112     C*--------------------------------------------------------------
096700980313     C*  PESO TASSABILE PER TARIFFE A PESO
096800980313     C                   EXSR      CALPT
096900941112     C*--------------------------------------------------------------
097000941110     C     TASSAT        TAG
097100990922     C** ESEGUE TASSAZIONE BOLLA
097200941110     C                   EXSR      TASSA
097300990922     C** MANCA TARIFFA
097400941110     C   94              GOTO      FINE
097500941110     C                   ENDSR
097600941112     C******************************************************
097700941112     C** GUIDA PER TASSAZIONE SPEDIZIONE
097800941112     C******************************************************
097900941110     C     TASSA         BEGSR
098000941110     C                   SETOFF                                       94
098100090923     C                   clear                   WvariaMag
098200090924     c                   clear                   WImpoMag
098300090923
098400970522     C** FLAG PER APPLICAZIONE INOLTRO
098500001009     C*!!* PRIMA SI FACEVA COSÌ *!!*
098600001009     C*!!* ** ASSEGNATI-MITTENTE FRANCHI-DESTINATARIO
098700001009     C*!!* ** PER 99 SEMPRE DESTINATARIO
098800001009     C*!!*           TIPBOL    COMP 'A'                      57
098900001009     C*!!*   57      KSCCOD    COMP 9999                 5757
099000001009     C*!!*   57                MOVELTASFAP    FLGINO  1
099100001009     C*!!*  N57                MOVELTASFIN    FLGINO
099200001009     C*!!*
099300001009     C*!!* ORA CONTROLLO LA TARIFFA :
099400001009     C*!!* SE E' USO LA TARIFFA REVERSIBILE (56 ON)  PRENDO COME INOLTRO L'ANTEPORTO TASFAP
099500001009     C*!!* SE USO LA TARIFFA NORMALE (56 OFF) USO L'INOLTRO TASFIN
099600001009     C*!!*
099700001009     C  N56              MOVEL     TASFIN        FLGINO            1
099800001009     C   56              MOVEL     TASFAP        FLGINO
099900001009     C*!!*
100000001009     C*!!*   FINE   *!!*
100100970522     C**
100200980506     C** SE DEVO CALCOLARE UNA VARIA VADO SUBITO DA QUELLA
100300980506     C     TASSVA        IFNE      '  '
100400990916     C     *LIKE         DEFINE    TASQFT        WQFT
100500980506     C** VARIA 'G' --> CONTRASSEGNO
100600980506     C     TASSVA        IFEQ      §$2VRG
100700980506     C                   GOTO      VARIAG
100800980506     C                   ENDIF
100900980525     C** VARIA 'R' --> ASSICURAZIONE PER CONTO
101000980526     C     TASSVA        IFEQ      §$2VRR
101100980525     C                   GOTO      VARIAR
101200980525     C                   ENDIF
101300020412     C** VARIA 'O' --> RICERCA DOCUMENTI
101400020412     C     TASSVA        IFEQ      §$2VRO
101500020412     C                   GOTO      VARIAO
101600020412     C                   ENDIF
101700020516     C** VARIA 'Y' --> RITIRI ANNULLATI
101800020516     C     TASSVA        IFEQ      §$2VRY
101900020516     C                   GOTO      VARIAY
102000020516     C                   ENDIF
102100020516     C** VARIA '*' --> DIROTTAMENTI
102200020523     C     TASSVA        IFEQ      §$2AST
102300020523     C                   GOTO      VARAST
102400020516     C                   ENDIF
102500030403     C** VARIA 'W' --> RECAPITO C/ASSEGNI
102600030403     C     TASSVA        IFEQ      §$2VRW
102700030403     C                   GOTO      VARVRW
102800030403     C                   ENDIF
102900030527     C** VARIA 'M' --> ADDEBITO INSOLUTI INTERESSI DI MORA
103000030527     C     TASSVA        IFEQ      §$2VRM
103100030527     C                   GOTO      VARVRM
103200030527     C                   ENDIF
103300030626     C** VARIA 'T' --> ORM COMMISSIONATO
103400030626     C     TASSVA        IFEQ      §$2VRT
103500030626     C                   GOTO      VARVRT
103600030626     C                   ENDIF
103700040910     C** VARIA 'a' --> P.O.D IMMAGE
103800040910     C     TASSVA        IFEQ      §$2POD
103900040910     C                   GOTO      VARPOD
104000040910     C                   ENDIF
104100030604     C** VARIA 'D' --> DIRITTO DI FATTURAZIONE
104200030604     C     TASSVA        IFEQ      §$2VRD
104300030922     C                   GOTO      FINE
104400030604     C                   ENDIF
104500130124     C** VARIA 'Z' --> ADDIZIONALE DI GESTIONE + ISTAT
104600130124     C     TASSVA        IFEQ      §$2VRZ
104700130124     C                   GOTO      FINE
104800130124     C                   ENDIF
104900130124     C** VARIA 'L' --> SOLO ISTAT
105000130124     C     TASSVA        IFEQ      §$2VRV
105100040113     C                   GOTO      FINE
105200040113     C                   ENDIF
105300130124     C** VARIA 'f' --> SOLO FUEL
105400130124     C     TASSVA        IFEQ      §$2VFS
105500130124     C                   GOTO      VARVFS
105600130124     C                   ENDIF
105700980506     C                   ENDIF
105800970522     C**
105900941123     C** SE IL PORTO O L'INOLTRO SONO PRETASSATI
106000941110     C** DEBBO ESCLUDERE LA FASE DI CALCOLO TARIFFA
106100090924     C     TASPOR        CABGT     0             DOPOIN
106200090924     C     TASINL        CABGT     0             DOPOIN
106300980508     C*****
106400980508     C** CALCOLO    P O R T O     ED    I N O L T R O
106500980508     C*****
106600980506     C**
106700020218     C                   DO        200           A
106800941110     C     A             OCCUR     TARDS
106900941110     C     TSCG          IFGE      PESTAS
107000941110     C                   GOTO      ENDTAS
107100941110     C                   END
107200941110     C                   END
107300980508     C** NON TROVATO SCAGLIONE ESATTO
107400941110     C     TSCG          IFLT      PESTAS
107500941110     C                   SETON                                        94
107600980512     C                   MOVEL     MSG(5)        TASMSG
107700151209     c                   eval      %subst(tasmsg: 37)= %trim(%editc(PESTAS:'3'))
107800980512     C                   GOTO      FINE
107900941110     C                   END
108000941110     C     ENDTAS        TAG
108100980512     C** MI SALVO LA POSIZIONE DELLO SCAGLIO TROVATO CHE MI
108200980512     C*  SERVE PER IL CALCOLO TARIFFA CON GARANTITO MAX SCAGLIONE PREC
108300980512     C     *LIKE         DEFINE    A             WA
108400980512     C                   Z-ADD     A             WA
108500980508     C**
108600980508     C* CALCOLO PER QUALE VALORE MOLTIPLICARE LA TARIFFA
108700980508     C                   Z-ADD     PESTAS        WPTAS3
108800980508     C                   EXSR      CALQLI
108900980508     C**
109000980508     C* MODO APPLICAZIONE TARIFFA
109100980508     C                   MOVEL     TAMFLO        DSTA01
109200990927     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
109300990927     C     §TADIV        IFEQ      *BLANK
109400990927     C                   MOVE      'ITL'         §TADIV
109500990927     C                   ENDIF
109600980512     C**
109700980512     C* CALCOLO NORMALE ANCHE PER SCALARE AL DI SOTTO
109800980512     C*  DELL'APPLICAZIONE TARIFFA FINITA E PER MAX SCAGLIONE PREC
109900980512     C     §TAF02        IFNE      'S'
110000980512     C     §TAF02        OREQ      'S'
110100980512     C     WATA          ANDNE     ' '
110200980508     C** MOLTIPLICAZIONE TARIFFA
110300980508     C                   EXSR      MULTAR
110400980515     C* APPLICAZIONE MINIMO E MASSIMO
110500980515     C                   EXSR      MINMAX
110600980508     C                   Z-ADD     WPOR          TASPOR
110700980508     C                   Z-ADD     WINL          TASINL
110800980514     C                   ENDIF
110900980508     C**
111000980508     C** S C A L A R E
111100980512     C     §TAF02        IFEQ      'S'
111200980508     C                   EXSR      SCALAR
111300980512     C                   ENDIF
111400980508     C**
111500980508     C** M A X   S C A G L I O N E   P R E C E D E N TE
111600980512     C     §TAF02        IFEQ      'G'
111700980512     C                   EXSR      MAXSCA
111800980512     C                   ENDIF
111900990923     C**
112000990923     C** SE DIVISA DELLA TARIFFA NON ACCETTA I DECIMALI PREPARO IL
112100990923     C** DEL PORTO E DELL'INOLTRO SENZA DECIMALI
112200990923     C     *IN12         IFEQ      '0'
112300990923     C* PORTO
112400991112     C     TASPOR        ADD       0,5           CAMPOW           11 0
112500990923     C                   Z-ADD     CAMPOW        TASPOR
112600990923     C* INOLTRO
112700991112     C     TASINL        ADD       0,5           CAMPOW
112800990923     C                   Z-ADD     CAMPOW        TASINL
112900990923     C                   ENDIF
113000090924     c                   eval      WImpoMag=taspor+tasinl
113100980508     C**
113200970521     C*******************************
113300990921     C****************
113400990921     C** TIPO SERVIZIO
113500990921     C****************
113600001127     C*
113700090924     C**                 Z-ADD     1             A
113800090924     C**                 clear                   ESP
113900001127     C*
114000001127     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
114100001127     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
114200001127     C*
114300090924     C**   KSDIT         IFEQ      'SDI'
114400090924     C**   TASDSP        ANDLE     20001231
114500090924     C**   TASTSP        ANDEQ     'C'
114600090924     C**   'E'           LOOKUP    TS(A)                                  05
114700090924     C**                 ELSE
114800090924     C**   TASTSP        LOOKUP    TS(A)                                  05
114900090924     C**                 ENDIF
115000001127     C*
115100090924     C**N05              GOTO      DOPOIN
115200090924     C**   A             OCCUR     DS5E
115300090924     C**   §5EFTC        CABEQ     *BLANKS       DOPOIN
115400090922
115500980430     C** TIPO SERVIZIO SENZA TARIFFA PARTICOLARE
115600990923     C** PORTO
115700090924     C**                 Z-ADD     0             WTSP
115800090924    1C**   TASPOR        IFGT      0
115900090924     c**                 exsr      CerESPRESSO
116000090924     c**
116100060414     c* Se c'e' già la varia, non la calcolo
116200090924    2c**                 if        giaespr<>'S'
116300090924     C**                 Z-ADD     TASPOR        IMPON
116400090924     c**                 if        *in56
116500090924     c**                 eval      applicarevers='S'
116600090924     c**                 endif
116700090924     C**                 EXSR      CALTS
116800090924     c**                 clear                   applicarevers
116900090924    3C**   WTSP          IFGT      0
117000060414     C*****              ADD       WTSP          TASPOR
117100060414     c
117200090924    4c**                 if        esp<>999
117300090924     C**                 MOVEL     wVariaMag     SV(ESP)
117400090924     C**                 add       wtsp          VA(ESP)
117500090924     c**                 else
117600060414     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
117700090924     C**                 ADD       wtsp          TASPOR
117800090924    4c**                 endif
117900090924     c**
118000090924    3C**                 ENDif
118100090924    2C**                 ENDif
118200090924    1C**                 ENDif
118300990923     C** INOLTRO
118400090924     C**                 Z-ADD     0             WTSP
118500090924    1C**   TASINL        IFGT      0
118600090924     c**                 EXSR      CerESPRESSO
118700090924    2c**                 if        giaespr<>'S'
118800090924     C**                 Z-ADD     TASINL        IMPON
118900090924     c**                 if        *in56
119000090924     c**                 eval      applicarevers='S'
119100090924     c*+                 endif
119200090924     C**                 EXSR      CALTS
119300090924     c**                 clear                   applicarevers
119400090924    3C**   WTSP          IFGT      0
119500060414     C*****              ADD       WTSP          TASINL
119600090924    4c**                 if        esp<>999
119700090924     C**                 MOVEL     wVariaMag     SV(ESP)
119800090924     C**                 add       wtsp          VA(ESP)
119900090924   x4c**                 else
120000060414     C** SE NON CI SONO VARIE LIBERE AGGIUNGO ALla voce
120100090924     C**                 ADD       WTSP          TASINL
120200090924    4c**                 endif
120300090924    3C**                 END
120400090924    2C**                 END
120500090924    1C**                 END
120600090924     c
120700991012     C     DOPOIN        TAG
120800991012     C**
120900990923     C* VALORIZZO LA VARIA DELL'INOLTRO NELLA SCHIERA DELLE VARIE
121000991012     C     TASINL        IFGT      0
121100990923     C                   Z-ADD     1             X                 2 0
121200990923     C     §$2FIN        LOOKUP    SV(X)                                  30
121300990923     C* SE NON ESISTE CARICOLA SCHIERA
121400990923     C     *IN30         IFEQ      '0'
121500990923     C                   Z-ADD     1             X
121600990923     C     ' '           LOOKUP    SV(X)                                  30
121700990923     C   30              MOVEA     §$2FIN        SV(X)
121800990923     C                   ENDIF
121900990923     C* SE LO TROVO VALORIZZO TASINL
122000990923     C   30              Z-ADD     TASINL        VA(X)
122100991012     C                   END
122200990921     C****************************************
122300990921     C** ISOLA  LOCALITA' DISAGIATE C.STORICI
122400990921     C****************************************
122500970521     C** SE TROVATA TARIFFA DEL PAESE APPLICO LO STESSO L'ISOLA
122600970521     C**
122700970721    1C     FLGINO        IFEQ      'I'
122800970521     C     FLGINO        OREQ      'D'
122900110624      * sostituisco il controllo del centro storico con i nuovi flag T o Z
123000110624      * T = centro storico in città
123100110624      * Z = centro storico in provincia
123200110624     C**   FLGINO        OREQ      'S'
123300110624     C     FLGINO        OREQ      'T'
123400110624     C     FLGINO        OREQ      'Z'
123500970521     C* IMPOSTO LA TARIFFA PARTICOLARE
123600970521     C                   SELECT
123700970521     C     FLGINO        WHENEQ    'I'                                          ISOLA
123800970521     C                   MOVEL     'I'           TASTC
123900970521     C     FLGINO        WHENEQ    'D'                                          LOCALITA' DI
124000970521     C                   MOVEL     'K'           TASTC
124100110624      * sostituisco il controllo del centro storico con i nuovi flag T o Z
124200110624      * T = centro storico in città
124300110624      * Z = centro storico in provincia
124400110624     C**   FLGINO        WHENEQ    'S'                                          C.STORICO
124500110624     C     FLGINO        WHENEQ    'T'                                          C.STORICO
124600970521     C                   MOVEL     'Q'           TASTC
124700110624     C     FLGINO        WHENEQ    'Z'                                          C.STORICO
124800110624     C                   MOVEL     'Q'           TASTC
124900970521     C                   ENDSL
125000110624      ** visto che l'attivazione del calcolo del centro storico dovrebbe essere
125100110909      ** il 3/10/11  attivo il flag "Q" in tastc confrontando la dta spedizione
125200110909     c                   if        tasdsp < 20111003 and tastc = 'Q'
125300110630     c                   clear                   tastc
125400110630     c                   endif
125500110628      ** Se TASTC è valorizzato calcolo la varia. Questa specifica si può togliere
125600110628      ** quando abilitiamo il centro storico
125700110628 bis1c                   If        tastc <> ' '
125800110628     c
125900970521     C**
126000060414     c* 14/4/06-ES--> il pgm per tassare questa tar.particolare usa
126100060414     c*           sempre il cod.tassazione mittente anche se non
126200060414     c*           applica la reversibilità. Da sempre lo fa.
126300060414     c*           Abbiamo valutato che è sbagliato ma deve seguire
126400060414     c*           la regola della reversibilità. Per cui non lo devo
126500060414     c*           più fare per tipbol='A' ma per 56 ON
126600060414     C**   TIPBOL        IFEQ      'A'
126700060414     C                   IF        *in56
126800981008     C                   MOVEL     TASCTS        WCTS              2
126900941123     C                   MOVEL     TASMCT        TASCTS
127000060414     C                   ENDif
127100970521     C*
127200970521     C                   EXSR      TASPAR
127300060414     C**   TIPBOL        IFEQ      'A'
127400060414     C                   IF        *in56
127500970721     C                   MOVEL     WCTS          TASCTS
127600970721     C                   END
127700970721     C**
127800970721     C* SOLO SE L'HO CALCOLATA FACCIO LE COSE SOTTOSTANTI
127900970721    2C     WNOEST        IFEQ      'S'
128000970721     C     TASCP         ANDGT     0
128100970522     C* MEMORIZZO PORTO E POSIZIONE VARIA DOVE HO MEMORIZZATO
128200970522     C                   Z-ADD     TASCP         TASISO
128300090924     c* imponibile per maggiorazione "E" o "H"
128400090924     c                   eval      wImpoMag=wImpoMag+TASISO
128500970522     C*
128600970522     C  N96              MOVEL     'S'           WPORTO            1
128700970522     C* SALVO L'INDICE DELLA SCHIERA DELLA VARIA
128800970522     C   96              Z-ADD     A             ISO               2 0
128900970522     C   96              MOVEL     ' '           WPORTO            1
129000970521     C*
129100970521     C** SE VA IN SOSTITUZIONE CLEARO L'INOLTRO
129200970521     C** FLAG=S INOLTRO+ISOLA
129300970521     C** FLAG=N SOLO ISOLA
129400110628     c** In caso di centro storico  non considero il flag FLGISO in quanto non
129500110628     c** non deve andare in sostituzione dell'inoltro ed in tariffa non viene
129600110628     c** richiesta la valorizzazione quindi è sempre diverso da "S"
129700970721    3C     FLGISO        IFNE      'S'
129800110628     c     TASTC         ANDNE     'Q'
129900090924     c* tolgo dall'imponibile per la maggiornazione l'Inoltro
130000090924     c                   eval      wImpoMag=wImpoMag-TASINL
130100090924     c*
130200090924     C                   CLEAR                   TASINL
130300990921     C** RICHIAMO LA VECCHIA POSIZIONE DELLA VARIA E LA SOSTIUISCO CON IL
130400990921     C**  VALORE DELL'ISOLA
130500990921     C** L'INDICE "X" PUNTA ALLA VARIA DELL'INOLTRO
130600990921     C     X             IFGT      *ZERO
130700990921     C* PULISCO L'INOLTRO
130800990921     C                   CLEAR                   VA(X)
130900990921     C                   CLEAR                   SV(X)
131000990921     C* UTILIZZO LA POSIZIONE VUOTA DELLA SCHIERA PER METTERE L'IMPORTO DELLA
131100990921     C* VARIA DELL'ISOLA
131200990921     C                   MOVEL     §1PVAR        SV(X)
131300990921     C                   Z-ADD     TASCP         VA(X)
131400990921     C* PULISCO LA POSIZIONE DELLA SCHIERA PRECEDENTE
131500990921     C   96              CLEAR                   VA(A)
131600990921     C   96              CLEAR                   SV(A)
131700990921     C* SALVO L'INDICE ATTUALE DELLA SCHIERA DELLA VARIA
131800990921     C   96              Z-ADD     X             ISO
131900990922     C                   END
132000970721    3C                   END
132100990921     C***********************
132200990921     C** IS/L.D./C.S  - TIPO SERVIZIO
132300990921     C***********************
132400090924     C**                 Z-ADD     1             A                 5 0
132500001127     C*
132600001127     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
132700001127     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
132800001127     C*
132900090924     C**   KSDIT         IFEQ      'SDI'
133000090924     C**   TASDSP        ANDLE     20001231
133100090924     C**   TASTSP        ANDEQ     'C'
133200090924     C**   'E'           LOOKUP    TS(A)                                  05
133300090924     C**                 ELSE
133400090924     C**   TASTSP        LOOKUP    TS(A)                                  05
133500090924     C*+                 ENDIF
133600001127     C*
133700090924     C** 05A             OCCUR     DS5E
133800090924    3C** 05§5EFTC        IFNE      *BLANKS
133900970521     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
134000090924     c**                 EXSR      CerESPRESSO
134100090924     c**
134200090924   3ac**                 if        giaespr<>'S'
134300090924     C**                 Z-ADD     0             WTSP
134400090924     C**                 Z-ADD     TASISO        IMPON
134500060414     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
134600090924     C**                 IF        *in56
134700090924     c**                 eval      applicarevers='S'
134800090924     c**                 endif
134900090924     C**                 EXSR      CALTS
135000090924     c**                 clear                   applicarevers
135100060414     c
135200090924    4C**   WTSP          IFGT      0
135300090924    5c**                 if        esp<>999
135400090924     C**                 MOVEL     WVariaMag     SV(ESP)
135500090924     C**                 add       wtsp          VA(ESP)
135600090924   x5c**                 else
135700060414     c* Se non c'e' una varia disponibile, sommo al posto o alla
135800060414     c*  varia dell' isola
135900090924    6C**   WPORTO        IFEQ      'S'
136000090924     C**                 ADD       WTSP          TASPOR
136100090924   x6C**                 ELSE
136200090924     C**                 ADD       WTSP          TASISO
136300090924     C**                 Z-ADD     TASISO        VA(ISO)
136400090924    6C**                 END
136500090924    5C**                 END
136600090924    4C**                 END
136700060414     c
136800090924   3aC**                 ENDif
136900090924    3C**                 END
137000090924     c
137100970721    2C                   END
137200110628 bis1C                   ENDIF
137300970721    1C                   ENDIF
137400090924     c
137500090924     C***********************
137600090924     C** Maggiorazione per tipo servizio sul totale imponibile da maggiorare
137700110628     c** ovvero:  PORTO + INOLTRO + ISOLA/DISAGIATE/CENTRO STORICO
137800110628     C** Se porto o inoltro già presenti: solo su ISOLA/DISAGIATE/CENTRO STORICO
137900110628     c** Se isola/disagiate/centro storico già presente: solo su PORTO + INOLTRO
138000090924     C***********************
138100090924     C                   Z-ADD     1             A                 5 0
138200090924     C                   clear                   ESP
138300090924     c
138400090924    1c                   if        WimpoMag>0
138500090924     C*
138600090924     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
138700090924     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
138800090924     C*
138900090924    2C     KSDIT         IFEQ      'SDI'
139000090924     C     TASDSP        ANDLE     20001231
139100090924     C     TASTSP        ANDEQ     'C'
139200090924     C     'E'           LOOKUP    TS(A)                                  05
139300090924     C                   ELSE
139400090924     C     TASTSP        LOOKUP    TS(A)                                  05
139500090924    2C                   ENDIF
139600090924     C*
139700090924     C   05A             OCCUR     DS5E
139800090924    2C   05§5EFTC        IFNE      *BLANKS
139900090924     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
140000090924     c                   EXSR      CerESPRESSO
140100090924     c
140200090924    3c                   if        giaespr<>'S'
140300090924     C                   Z-ADD     0             WTSP
140400090924     c
140500090924     C                   Z-ADD     WImpoMag      IMPON
140600090924     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
140700090924     C                   IF        *in56
140800090924     c                   eval      applicarevers='S'
140900090924     c                   endif
141000090924     C                   EXSR      CALTS
141100090924     c                   clear                   applicarevers
141200090924     c
141300090924    4C     WTSP          IFGT      0
141400090924    5c                   if        esp<>999
141500090924     C                   MOVEL     WVariaMag     SV(ESP)
141600090924     C                   add       wtsp          VA(ESP)
141700090924   x5c                   else
141800090924     c* Se non c'e' una varia disponibile, sommo nel porto
141900090924     C                   ADD       WTSP          TASPOR
142000090924    5C                   END
142100090924    4C                   END
142200090924     c
142300090924    3C                   ENDif
142400090924    2C                   END
142500090924    1C                   END
142600990921     C****************
142700990921     C** DIRITTO FISSO
142800990921     C****************
142900980424     C     TASDIF        IFEQ      0
143000980424     C                   MOVEL     'H'           TASTC
143100990921     C* RICHIAMO LA ROUTINE DI CALCOLO DELLE TARIFFE PARTICOLARI
143200990921     C                   EXSR      TASPAR
143300990921     C   96              Z-ADD     TASCP         TASDIF
143400990921     C***
143500980424     C                   ENDIF
143600990921     C***********************
143700990921     C** PROVVIGIONE ASSEGNO
143800990921     C***********************
143900980506     C     VARIAG        TAG
144000980506     C     *LIKE         DEFINE    TASVA1        PROVAS
144100980506     C                   Z-ADD     0             PROVAS
144200980429     C** MANCA C/A
144300941112     C     TASCAS        CABEQ     0             POIT01
144400980429     C** PROVV. ASSEGNO GIA' PRESENTE
144500941112     C                   Z-ADD     1             A
144600941112     C     §$2VRG        LOOKUP    SV(A)                                  05
144700980429    1C  N05              DO
144800941116     C*
144900941116     C                   SETOFF                                       94
145000941116     C     *LIKE         DEFINE    TASCAS        CASLIT
145100941116     C                   Z-ADD     TASCAS        CASLIT
145200980429     C**
145300990921    2C*!*        TASCMB    IFEQ 0
145400990921     C*!*                  Z-ADD1         K
145500990921     C*!*        TASVCA    LOKUPCV,K                     05
145600990921    3C*!*        *IN05     IFEQ *OFF
145700990921     C*!*                  SETON                     94
145800990921     C*!*                  MOVELMSG,1     TASMSG
145900990921     C*!*                  GOTO FINE
146000990921    3C*!*                  ENDIF
146100941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
146200990921     C*!*        K         OCUR DSCV
146300990921    3C*!*        TASVCA    IFNE §CVITA
146400990921     C*!*        TASCAS    MULT §CVCMB    CASLIT    H
146500941116     C** CAMBIO IN BASE ALLA DIVISA STANDARD
146600990921    3C*!*                  END
146700990921   X2C*!*                  ELSE
146800990921     C*!*        TASCAS    MULT TASCMB    CASLIT    H
146900941116     C** CAMBIO IN BASE AL VALORE ESISTENTE IN BOLLA
147000990921    2C*!*                  END
147100990921     C* CALCOLO IL VALORE DEL CONTRASSEGNO  IN BASE ALLA MONETA
147200020319     C*******
147300020319     C*******
147400020319     C* SE PRESENTE IL C/A ANNULLATO VEDO IN BASE ALLA TARIFFA CLIENTE
147500020319     C*  SE FATTURARE O MENO
147600020319     C*  §TAPCA='N' NON FATTURO GLI ANNULLATI
147700030131     C**!!!§ASSTA        IFEQ      '9'
147800030131     C     TASSTA        IFEQ      '9'
147900030131     C     §TAPCA        ANDEQ     'N'
148000060922      * calcolo sempre la provvigione del C/A  annullato se si tratta di consegna anomala dirottata
148100060922     C     §ASCCA        ANDNE     '1'
148200060922      *
148300020319     C                   GOTO      POIT01
148400020319     C                   ENDIF
148500020319     C**
148600990921     C     TASVCA        IFNE      §TADIV
148700990921     C                   CLEAR                   YEUR
148800990921     C                   MOVEL     TASVCA        YECDVI
148900990921     C                   MOVEL     §TADIV        YECDVO
149000990921     C                   Z-ADD     TASCAS        YECIMI
149100991014     C   12              MOVE      '3'           YECDCO
149200991014     C  N12              MOVE      '0'           YECDCO
149300990921     C*
149400990921     C                   CALL      'YEURCO'
149500990921     C                   PARM                    YEUR
149600990921     C*
149700990921     C     YECESI        IFEQ      ' '
149800990921     C                   Z-ADD     YECIMO        CASLIT
149900990921     C                   ELSE
150000990921     C                   SETON                                        94
150100990921     C                   MOVEL     MSG(1)        TASMSG
150200990921     C                   GOTO      FINE
150300990923     C                   END
150400990921     C                   ENDIF
150500980429     C* PROVV ASSEGNO MITTENTE
150600980429    2C     TASBM         IFEQ      'M'
150700980429     C                   MOVEL     'W'           TASTC
150800980429     C                   ELSE
150900980429     C* PROVV ASSEGNO VETTORE
151000980429     C                   MOVEL     'V'           TASTC
151100980429    2C                   ENDIF
151200060712      * se spedizione EEX cerco sempre provvigione assegno mittente
151300060712     c                   If        o27fie = 'E'
151400060712     c                   movel     'W'           tastc
151500060712     c                   endif
151600980429     C**
151700980429     C                   MOVEL     TASTC         FISO
151800980506     C                   MOVEL     'S'           WCAR
151900980429     C                   EXSR      CERTPT
152000980429    2C     WEND          IFEQ      ' '
152100980429     C**
152200980429    3C     TPTTPG        IFEQ      'V'                                          VALORE
152300980429     C                   Z-ADD     CASLIT        TPDPES
152400980429     C                   ELSE                                                   ALTRO
152500980429     C                   Z-ADD     ISOPES        TPDPES
152600980429    3C                   ENDIF
152700980429     C**
152800980429     C                   EXSR      CALACD
152900941116     C*
153000980429    3C     VARIA         IFGT      0
153100941112     C                   Z-ADD     1             A
153200941112     C     ' '           LOOKUP    SV(A)                                  96
153300941112     C   96              MOVEL     §$2VRG        SV(A)
153400980429     C   96              Z-ADD     VARIA         VA(A)
153500980429     C  N96              ADD       VARIA         TASPOR
153600980429     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
153700980429    3C                   ENDIF
153800980429    2C                   ENDIF
153900980429    1C                   ENDDO
154000980506     C**
154100980506     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
154200980506     C     TASSVA        IFEQ      §$2VRG
154300980506     C                   GOTO      FINE
154400980506     C                   ENDIF
154500990921     C***********************
154600990921     C** DIRITTO ASSICURATIVO
154700990921     C***********************
154800941110     C     POIT01        TAG
154900941112     C                   Z-ADD     1             A
155000941112     C     §$2VRE        LOOKUP    SV(A)                                  05
155100941112     C  N05              DO
155200941112     C                   Z-ADD     0             DIRITT
155300941112     C     *LIKE         DEFINE    TASVA1        DIRITT
155400941110     C                   EXSR      CALVE
155500941112     C     DIRITT        IFGT      0
155600941112     C                   Z-ADD     1             A
155700941112     C     ' '           LOOKUP    SV(A)                                  96
155800941112     C   96              MOVEL     §$2VRE        SV(A)
155900941112     C   96              Z-ADD     DIRITT        VA(A)
156000941112     C  N96              ADD       DIRITT        TASPOR
156100941112     C                   END
156200941112     C                   END
156300941112     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
156400990921     C***********************
156500990921     C** ASS. X CONTO
156600990921     C***********************
156700980525     C     VARIAR        TAG
156800980525     C                   Z-ADD     0             WVARIR
156900060322     C                   Z-ADD     0             WVARIACB
157000980525     C     *LIKE         DEFINE    TASVA1        WVARIR
157100060321     C     *LIKE         DEFINE    TASVA1        WVARIACB
157200060320      * cerco la varia R sia in caso di bolla con importo d'assicurare e sia in caso di
157300060320      * ricerca di mandato
157400941112     C                   EXSR      CALVR
157500941112     C** MANCA TARIFFA:MANDATO VALORE = 0 E TASIAS=0
157600980526     C   94              GOTO      FINE
157700980526     C**
157800980525     C     WVARIR        IFGT      0
157900941112     C                   Z-ADD     1             A
158000941112     C     ' '           LOOKUP    SV(A)                                  96
158100941112     C   96              MOVEL     §$2VRR        SV(A)
158200980525     C   96              Z-ADD     WVARIR        VA(A)
158300060320      ** se non ci sono varie libere aggiungo al porto
158400980525     C  N96              ADD       WVARIR        TASPOR
158500060320
158600060320      * se varia uguale a zero e non ho trovato nessun mandato e non c'è importo d'assicurare in
158700060320      * bolla cerco la nuova AC base
158800060320
158900060320     c                   else
159000060320
159100060321     c                   If        task88 = 'X' and
159200060321      ** 88 = non si calcola
159300060322     c                             (Ksccod <> 8888 and Ksccod <> 9999) and
159400060322      ** data spedizione maggiore del 28 02 2006
159500060322     c                             tasdsp > 20060228
159600060320     c                   exsr      CALVACB
159700060320      ** carico la varia nella schiera
159800060321     c                   If        wvariacb > 0
159900060320     c                   z-add     1             a
160000060320     c     ' '           Lookup    sv(a)                                  96
160100060320     c   96              movel     §$2Acb        sv(a)
160200060321     c   96              z-add     Wvariacb      va(a)
160300060320      ** se non ci sono varie libere aggiungo al porto
160400060321     c  N96              add       Wvariacb      TASpor
160500060320     c                   endif
160600060320
160700060320     c                   endif
160800060320
160900060320     c                   END
161000980525     C**
161100980525     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
161200980525     C     TASSVA        IFEQ      §$2VRR
161300980525     C                   GOTO      FINE
161400980525     C                   ENDIF
161500990722     C***************
161600990722     C* T.C.P.
161700990722     C***************
161800990722     C*
161900990722     C                   MOVEL     §$2VRP        TASTC
162000990722     C                   EXSR      TASPAR
162100030624     C***************************************
162200030624     C* ANNULLAMENTO PORTO ASSEGNATO VARIA &
162300030624     C***************************************
162400030624     C*
162500030624     C* SE IL FLAG DI CALCOLO VARIA & È UGUALE A S
162600030624     C                   IF        TASFCAA = 'S'
162700030624     C                   MOVEL     §$2VPA        TASTC
162800030624     C                   EXSR      TASPAR
162900030624     C                   ENDIF
163000050928     C****************************
163100050928     C* LASCIATO AVVISO  VARIA 'c'
163200050928     C****************************
163300050929     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
163400050928     C                   If        tasric <> ' '
163500090305      * verifico se linea di arrivo o linea di partenza è estera non calcolo la varia "c"
163600051028     c     taslna        lookup    esttot                                 30
163700090305      * se linea di arrivo non è estero verifico la linea di partenza
163800090305     c  n30taslnp        lookup    esttot                                 30
163900090305      * se spedizione italiana calcolo
164000051028     c                   If        not *in30
164100050928     C                   Movel     §$2vla        Tastc
164200050928     C                   Exsr      TASpar
164300051005     c                   Endif
164400051005
164500050928     c                   Endif
164600110628     C****************************
164700110628     C* AVVISO AL DESTINATARIO AFFIDAMENTO SPEDIZIONE VARIA 'm'
164800110628     C****************************
164900110628     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
165000110628     C* in TASFLO §asemd
165100130923     C                   If        §asemd = 'S' or §asemd = 'X' or
165200130923     c                             §asemd = 'E'
165300110628     C                   Movel     §$2vad        Tastc
165400110628     C                   Exsr      TASpar
165500110628     c                   Endif
165600151209     C****************************
165700151209     C* PIN CODE  CONSEGNA CON VERIFICA PIN CODE VARIA "p"  (anche se non serve)
165800151209     C****************************
165900151209     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
166000151209     C* in TASFLO §aspin
166100151209     C                   If        §aspin = 'S'
166200151209     C                   Movel     §$2pin        Tastc
166300151209     C                   Exsr      TASpar
166400151209     c                   Endif
166500151209
166600151209     C*************************************************
166700151209     C* DIRITTO DI CHIAMATA PRENOTAZIONE ORM TELEFONICO
166800151209     C*************************************************
166900151209     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
167000151209     C* TASPRT
167100151209     C                   If        TASPRT = 'S'
167200151209     C                   Movel     §$2ORMT       Tastc
167300151209     C                   Exsr      TASpar
167400151209     c                   Endif
167500151209
167600151209     C****************************
167700151209     C* PACKING LIST
167800151209     C****************************
167900151209     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
168000151209     C* TASSPL
168100151209     C                   If        TASSPL = 'S'
168200151209     C                   Movel     §$2pkl        Tastc
168300151209     C                   Exsr      TASpar
168400151209     c                   Endif
168500151209
168600110628
168700020412     C***************
168800020412     C* RICERCA DOCUMENTI - SOLO SE RICHIESTA DA SOLA
168900020412     C***************
169000020412     C*
169100020412     C     VARIAO        TAG
169200020412     C     TASSVA        IFEQ      §$2VRO
169300020412     C                   MOVEL     §$2VRO        TASTC
169400020412     C                   EXSR      TASPAR
169500020412     C**
169600020412     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
169700020412     C     TASSVA        IFEQ      §$2VRO
169800020412     C                   GOTO      FINE
169900020412     C                   ENDIF
170000020412     C                   ENDIF
170100020516     C***************
170200020516     C* RITIRI ANNULLATI  - SOLO SE RICHIESTA DA SOLA
170300020516     C***************
170400020516     C*
170500020516     C     VARIAY        TAG
170600020516     C     TASSVA        IFEQ      §$2VRY
170700020516     C                   MOVEL     §$2VRY        TASTC
170800020516     C                   EXSR      TASPAR
170900020516     C**
171000020516     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
171100020516     C     TASSVA        IFEQ      §$2VRY
171200020516     C                   GOTO      FINE
171300020516     C                   ENDIF
171400020516     C                   ENDIF
171500020516     C***************
171600020516     C* DIROTTAMENTI      - SOLO SE RICHIESTA DA SOLA
171700020516     C***************
171800020516     C*
171900020523     C     VARAST        TAG
172000020523     C     TASSVA        IFEQ      §$2AST
172100020523     C                   MOVEL     §$2AST        TASTC
172200020516     C                   EXSR      TASPAR
172300020516     C**
172400020516     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
172500020523     C     TASSVA        IFEQ      §$2AST
172600020516     C                   GOTO      FINE
172700020516     C                   ENDIF
172800020516     C                   ENDIF
172900030403     C***************
173000030403     C* RECAPITO C/ASSEGNI- SOLO SE RICHIESTA DA SOLA
173100030403     C***************
173200030403     C*
173300030403     C     VARVRW        TAG
173400030403     C     TASSVA        IFEQ      §$2VRW
173500030403     C                   MOVEL     'G'           TASTC
173600030403     C                   EXSR      TASPAR
173700030403     C**
173800030403     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
173900030403     C     TASSVA        IFEQ      §$2VRW
174000030403     C                   GOTO      FINE
174100030403     C                   ENDIF
174200030403     C                   ENDIF
174300030527     C*
174400030527     C***************
174500030527     C* ADDEBITO INSOLUTI - INT.MORA  SOLO SE RICHIESTA DA SOLA
174600030527     C***************
174700030527     C     VARVRM        TAG
174800030527     C     TASSVA        IFEQ      §$2VRM
174900030527      * SE NON ESISTONO VARIE IMPOSTATE DO MANCA TARIFFA
175000030527     c                   MOVEA     SV            WRKSV            20
175100030527     C                   IF        WRKSV = *BLANKS
175200030527     C                   MOVEL     MSG(18)       TASMSG
175300030527     C                   MOVEL     'E'           TASERR            1
175400030527     C                   SETON                                        94
175500030527     C                   ENDIF
175600030527     C                   GOTO      FINE
175700030527     C                   ENDIF
175800030626     C***************
175900030626     C* ORM COMMISSIONATO  - SOLO SE RICHIESTA DA SOLA
176000030626     C***************
176100030626     C*
176200030626     C     VARVRT        TAG
176300030626     C     TASSVA        IFEQ      §$2VRT
176400030626     C                   MOVEL     §$2VRT        TASTC
176500030626     C                   EXSR      TASPAR
176600030626     C**
176700030626     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
176800030626     C     TASSVA        IFEQ      §$2VRT
176900030626     C                   GOTO      FINE
177000030626     C                   ENDIF
177100030626     C                   ENDIF
177200040910     C***************
177300040910     C* P.O.D IMMAGE      - SOLO SE RICHIESTA DA SOLA
177400040910     C***************
177500040910     C*
177600040910     C     VARPOD        TAG
177700040910     C     TASSVA        IFEQ      §$2POD
177800040910     C                   MOVEL     §$2POD        TASTC
177900040910     C                   EXSR      TASPAR
178000040910     C**
178100040910     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
178200040910     C     TASSVA        IFEQ      §$2POD
178300040910     C                   GOTO      FINE
178400040910     C                   ENDIF
178500040910     C                   ENDIF
178600990922     C************************
178700990922     C* CONSEGNE PARTICOLARI
178800990922     C************************
178900990722     C*
179000990922     C** CONSEGNE PARTICOLARI 1
179100941110     C     TASTC1        IFNE      *BLANKS
179200970521     C                   MOVEL     TASTC1        TASTC
179300970521     C                   EXSR      TASPAR
179400941112     C                   END
179500990922     C** CONSEGNE PARTICOLARI 2
179600941110     C     TASTC2        IFNE      *BLANKS
179700970521     C                   MOVEL     TASTC2        TASTC
179800970521     C                   EXSR      TASPAR
179900941110     C                   END
180000020826     C************************
180100020826     C* DITITTO DI PESATURA
180200020826     C************************
180300020826     C*
180400020826     C     §TATAP        IFEQ      'S'
180500020826     C                   MOVEL     '5'           TASTC
180600020826     C                   EXSR      TASPAR
180700020826     C                   END
180800990922     C************************
180900990922     C* RITIRO
181000990922     C************************
181100050504     C* LA VARIA "U" DI RITIRO  E'
181200050504     C** DA CALCOLARE: SUI PORTI ASSEGNATI
181300050504     C**               SUI PORTI FRANCHI NON CODIFICATI
181400050504     C**                   CON FLAG CONSEGNA A MAGAZZINO = A BLANK
181500050504     C**               SULLE BOLLE EXPORT ASSEGNATO NON DPD
181600050504     C**               SULLE BOLLE IMPORT ASSEGNATO TUTTE (ANCHE DPD)
181700990915     C* FRANCO
181800000919     C*!!*       TIPBOL    IFNE 'A'
181900000919     C*!!*       TASFDN    ANDEQ'S'
182000000919     C*!!*                 GOTO ENDRIT
182100000919     C*!!*                 ENDIF
182200990915     C*
182300990915     C* FRANCO CON CODICE CLIENTE UGUALE A 8888 OPPURE UGUALE ALLA TARIFFA DI
182400990915     C* CARTELLO
182500000919     C     TIPBOL        IFNE      'A'
182600000919     C     KSCCOD        ANDNE     8888
182700020417     C****       TASKSC    ANDNECARKSC
182800020417     C* SE NON SI TRATTA NEMMENO DI UNA TARIFFA DI CARTELLO NON TASSO
182900020417     C     TASKSC        LOOKUP    TAC                                    15
183000020417     C  N15              GOTO      ENDRIT
183100000919     C                   ENDIF
183200990930     C** DA CALCOLARE SOLO PER PORTI ASSEGNATI RITIRATI
183300000919     C*!!*       TIPBOL    CABNE'A'       ENDRIT
183400990930     C*
183500970213     C*
183600990914     C* SE BOLLA ASSEGNATA EXPORT --> NON CONSIDERO IL FLAG CONSEGNA A MAGAZZINO
183700990914     C*                               PERCHE' DEVE SEMPRE CALCOLARE LA VARIA "U"
183800970213     C     TASFDN        IFEQ      'S'
183900020319     C* PER ASSEGNATO EXPORT --> SEMPRE  (ANCHE DPD CHE FINO A OGGI
184000050504     C*                          ESCLUDEVA 4/4/02--> RIESCLUDO DPD)
184100050504     C* PER ASSEGNATO IMPORT --> SEMPRE  (ANCHE DPD 04/05/05)
184200050504     C*
184300001204     C* PER ALTRI ASSEGNATI  --> SOLO SE NON E' PORTATA A MAGAZZINO
184400001204     C     TIPBOL        IFEQ      'A'
184500001204     C*
184600980605     C     TASLNA        LOOKUP    EST                                    05
184700050504     c
184800050504     C  n05TASLNP        LOOKUP    ESTtot                                 05
184900050504     c
185000980605     C  N05              GOTO      ENDRIT
185100001204     C                   ELSE
185200001204     C* PER 8888 --> NON APPLICARE SE PORTATA A MAGAZZINO SIA PER EXP
185300001204     C*              CHE PER LE ALTRE SPEDIZIONI
185400001204     C                   GOTO      ENDRIT
185500970213     C                   ENDIF
185600001204     C                   ENDIF
185700060720      *----------------------------------------------------------------------------*
185800060720      * In caso di bolla di reso non si può NON CALCOLARE la varia U in quanto     *
185900060720      * questa serve per far pagare la tratta int.le della spedizione.             *
186000060720      * Infatti quando in Aprile 2006 è stata tolta la signora Villa ha reclamato. *
186100060720      * asterisco le seguenti specifiche                                           *
186200060720     c*** Per bolla import, non applico varia "U" se bolla di reso                 *
186300060720     C***     TASLNP        LOOKUP    ESTtot                                 05    *
186400060720     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
186500060720     C***                   GOTO      ENDRIT                                       *
186600060720     c***                   endif                                                  *
186700060720     c***                                                                          *
186800060720     c*** Per bolla EXport, non applico varia "U" se bolla di reso                 *
186900060720     C***     TASLNA        LOOKUP    EST                                    05    *
187000060720     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
187100060720     C***                   GOTO      ENDRIT                                       *
187200060720     c***                   endif                                                  *
187300060720      *----------------------------------------------------------------------------*
187400060424     c
187500970213     C**
187600970521     C* TARIFFA PARTICOLARE --> U
187700941121     C                   MOVEL     'U'           TASTC
187800970521     C**
187900110617     C** 17/06/2011 LB DN visto che sono state cambiate le tariffe della cartello
188000110617     c** per la tariffa particolare di ritiro per l'Estero e non essendo + uguale
188100110617     c** a quelle per l'italia in caso di prepagati (quindi non codificati) per la GB
188200110617     c** la tariffa di ritiro diventa di 50 euro circa rispetto i 5 euro circa di prima ?
188300110617     c** quindi risulta necessario correggere un errore che ci portiamo dietro da anni
188400110617     c** che tassiamo controllando il codice tassazione mittente solo per gli assegnati
188500110617     c** invece si deve fare anche per i poveri franchi (prepagati) per i quali viene
188600110617     c** calcolato il ritiro. Quindi sia per i franchi che assegnati il codice tassazione
188700110617     c** per la tariffa particolare di ritiro diventa il codice tassazione mittente
188800110617     c** come è giusto che venga tassata da dove viene ritirata e non dove viene
188900110617     c** consegnata la merce. A.G.
189000110617     C**   TIPBOL        IFEQ      'A'
189100970521     C                   MOVEL     TASCTS        WCTS
189200970521     C                   MOVEL     TASMCT        TASCTS
189300110617     C**                 END
189400970521     C*
189500970521     C                   EXSR      TASPAR
189600970521     C*
189700110617     C**   TIPBOL        IFEQ      'A'
189800970521     C                   MOVEL     WCTS          TASCTS
189900110617     C**                 END
190000941121     C     ENDRIT        TAG
190100990922     C************************
190200990922     C* SI DDT
190300031104      * Tutti i pgm che richiamano il TNSF20R x tassare le bolle arrivi
190400031104      * testano il flag DDT e passano 'S' nel caso di DDT SI, in questi pgm ho aggiunto anche i
190500031104      * nuovi flag 'P' e 'Q'
190600031104      * l'unico che non lo fa è FNLG20R così ho aggiunto anche qua il controllo con i 2 nuovi flag
190700031104      * 'P' è uguale a 'S' e viene impostato in arrivo quando si stampa il ddt si
190800031104      * 'Q' è uguale a 'C' e viene impostato in arrivo quando si stampa il ddt si
190900990922     C************************
191000970409     C     TASDDT        IFEQ      'S'
191100031104     C     TASDDT        orEQ      'P'
191200031104     C     TASDDT        orEQ      'Q'
191300970521     C* TARIFFA PARTICOLARE --> B
191400970521     C                   MOVEL     'B'           TASTC
191500970521     C                   EXSR      TASPAR
191600970409     C                   END
191700030203      ***************
191800030203      * BANCALI
191900030203      ***************
192000030203     C*
192100030203      * Non si tassano i bancali per le bolle in porto assegnato
192200030203     c                   If        TasBan > *Zeros and TipBol <> 'A'
192300030203     c                   Movel     §$2Vba        TASTC
192400030203     c                   ExSr      TasPar
192500030203     c                   EndIf
192600001205     C*
192700040506      ************************************************
192800040506      * SUPPLEMENTO CARBURANTE SPEDIZIONI FEDEX
192900040506      ************************************************
193000130124     C                   IF        WBOFED = 'S'
193100060905     C                   EXSR      SR_SUPCARB
193200130124     C                   ENDIF
193300060905      ************************************************
193400060905      * FUEL SURCHARGE  SPEDIZIONI NO FEDEX
193500060905      ************************************************
193600130124     C     VARVFS        TAG
193700130124     C                   IF        WBOFED = ' '
193800060905     C                   EXSR      SR_FUEL
193900130124     C**
194000130124     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
194100130124     C     TASSVA        IFEQ      §$2VFS
194200130124     C                   GOTO      FINE
194300130124     C                   ENDIF
194400130124     C                   ENDIF
194500040506      *
194600941110     C                   SETOFF                                       49
194700941110     C                   ENDSR
194800060414     C*****************************************************************
194900090922     C** Cerco la posizione per la varia della maggiorazione
195000060414     C*****************************************************************
195100060414     c     CerESPRESSO   BEGSR
195200060414     c                   if        esp=0
195300060414     c                   clear                   giaespr           1
195400090923     c                   clear                   wVariaMag         1
195500060414     C                   Z-ADD     1             A
195600090922
195700090922     c* Maggiorazione per tipo servizio E-priority
195800090922     c                   select
195900090922     c                   when      tastsp='E'
196000090922     c                   eval      wvariaMag=§$2tsp
196100090922
196200090922     c* Maggiorazione per tipo servizio H-H1030
196300090922     c                   when      tastsp='H'
196400090922     c                   eval      wvariaMag=§$2tsh
196500090922     c                   endsl
196600090922
196700090922     C     wvariaMag     LOOKUP    SV(a)                                  96
196800090922     c* gia esiste la varia della maggioraz --> non la calcolo
196900060414     c                   if        *in96
197000060414     c                   z-add     a             ESP               3 0
197100060414     c                   eval      giaespr='S'
197200060414     c                   else
197300060414     C                   Z-ADD     1             A
197400060414     C     ' '           LOOKUP    SV(A)                                  96
197500060414     c   96              z-add     a             ESP               3 0
197600060414     C  n96              ADD       999           ESP
197700060414     c                   endif
197800060414     c                   endif
197900060414     c                   ENDSR
198000980508     C*****************************************************************
198100980508     C** VALORE COL QUALE  MOLTIPLICARE LA TARIFFA
198200980508     C*****************************************************************
198300980508     C     CALQLI        BEGSR
198400991013     C                   Z-ADD     0             PESQLI           14 6
198500980508     C** SE LO SCAGLIONE SCELTO PER TASSARE E' INFERIORE AL MINIMO
198600980508     C** TASSABILE DA TARIFFA FORZO PESO TASSABILE = MINTAS
198700980512     C**
198800980512     C* WATA MI IDENTIFICA SE L'APPLICAZIONE TARIFFA FINITA E' DOVUTA
198900980512     C*  ALLO SCAGLIONE CONFRONTATO CON MINTAR (R)
199000980512     C*  O SE E' DOVUTA DAL PESO TASSABILE CONFRONTATO COL MINIMO
199100980515     C*  TASSABILE DI QUELLA TARIFFA (S) - differenza che ora non serve
199200980515     C*  + ma dal momento che c'e' la lascio
199300980508     C                   CLEAR                   WATA
199400980508    1C                   SELECT
199500980508     C     TSCG          WHENLE    MINTAR
199600980508     C                   Z-ADD     1             PESQLI
199700980514     C                   MOVEL     'R'           WATA              1
199800980508     C**
199900980515     C** SE PESO TASSABILE E' ANCORA INFERIORE
200000980514     C*  AL MINIMO TASSABILE DA TARIFFA IMPOSTO PESO TASS = 1
200100980515     C*  NON LO CONTROLLO PER LA TARIFFA A SCALARE PERCHE' ABBIAMO
200200980515     C*  SEMPRE INSERITO UNO SCAGLIONE=A MINIMO TASSABILE E
200300980515     C*  QUINDI E' GIA' CONTROLLATO NEL WHLE PRECEDENTE
200400980515     C     WPTAS3        WHENLE    MINTAS
200500980515     C     §TAF02        ANDNE     'S'
200600980508     C                   Z-ADD     1             PESQLI
200700980508     C                   MOVEL     'S'           WATA
200800980508   X1C                   OTHER
200900980508     C** CALCOLO : PESO IN Q.LI PER TARIFFA A PESO
201000980508     C** CALCOLO   VALORE/100   PER TARIFFA A VALORE (ESSENDO UNA %)
201100980508     C** PER LE ALTRE TARIFFE PESO TASSABILE INVARIATO
201200980508    2C     UNOCTR        IFEQ      0
201300980508     C     UNOCTR        OREQ      4
201400980508     C     WPTAS3        DIV       100           PESQLI
201500980508   X2C                   ELSE
201600980508     C                   Z-ADD     WPTAS3        PESQLI
201700980508    2C                   END
201800980508    1C                   ENDSL
201900980508     C                   ENDSR
202000980508     C*****************************************************************
202100980508     C* MOLTIPLICATO TARIFFA PER VALORE TROVATO (PESO TASSABILE)
202200980508     C*****************************************************************
202300980508     C     MULTAR        BEGSR
202400980508     C     *LIKE         DEFINE    TASPOR        WPOR
202500980508     C     *LIKE         DEFINE    TASINL        WINL
202600980518     C                   CLEAR                   WPOR
202700980518     C                   CLEAR                   WINL
202800980508     C**
202900980508     C     TITR          MULT(H)   PESQLI        WPOR                           ** PORTO
203000980508     C**
203100980508     C** SE TROVATA TARIFFA DEL PAESE NON SI APPLICA MAI INOLTRO
203200110624     C*****  N49FLGINO        IFNE      'C'
203300110624     C** Sostituisco il controllo del flag inoltro diverso da C=città ma
203400110624     c** anche da "T" Città centro storico
203500110624     C                   IF        NOT *IN49  AND
203600110624     C                             FLGINO <> 'C' AND FLGINO <> 'T'
203700980508     C     TINO          MULT(H)   PESQLI        WINL                           ** INOLTRO
203800980508     C                   END
203900980508     C                   ENDSR
204000980508     C*****************************************************************
204100980508     C* CALCOLO TARIFFA A SCALARE
204200980508     C*****************************************************************
204300980508     C     SCALAR        BEGSR
204400980508     C**
204500980514     C     *LIKE         DEFINE    PESTAS        WPTAS1
204600980508     C     *LIKE         DEFINE    PESTAS        WDIFSC
204700980508     C     *LIKE         DEFINE    PESTAS        WPTAS3
204800980511     C     *LIKE         DEFINE    PESQLI        WPESQ
204900980511     C     *LIKE         DEFINE    PESQLI        WPESSV
205000980511     C     *LIKE         DEFINE    TITR          WTITR
205100980511     C     *LIKE         DEFINE    TITR          WSTITR
205200980511     C     *LIKE         DEFINE    TINO          WTINO
205300980511     C     *LIKE         DEFINE    TINO          WSTINO
205400980508     C     *LIKE         DEFINE    TSCG          WPRESC
205500980508     C**
205600980508     C                   CLEAR                   TASPOR
205700980508     C                   CLEAR                   TASINL
205800980508     C                   CLEAR                   WTROVA
205900980508     C                   CLEAR                   WPTAS3
206000980508     C                   CLEAR                   WDIFSC
206100980508     C                   CLEAR                   WPRESC
206200980511     C                   CLEAR                   WPESSV
206300980511     C                   CLEAR                   WPESQ
206400980511     C                   CLEAR                   WTITR
206500980511     C                   CLEAR                   WSTITR
206600980511     C                   CLEAR                   WTINO
206700980511     C                   CLEAR                   WSTINO
206800980508     C                   Z-ADD     PESTAS        WPTAS1
206900980508     C                   Z-ADD     1             A
207000980508     C**
207100020218    1C     A             DOWLE     200
207200980508     C     WTROVA        ANDEQ     ' '
207300980508     C     TSCG          ANDGT     0
207400980508     C     A             OCCUR     TARDS
207500980512     C**
207600980508     C* FACCIO LA DIFFERENZA TRA LO SCAGLIONE LETTO ED IL PRECEDENTE
207700980508     C     TSCG          SUB       WPRESC        WDIFSC
207800980508     C**
207900980508     C* SOTRAGGO DAL PESO TASSABILE RIMASTO LA PARTE DA TASSARE ORA
208000980508     C* IN WPTAS1 HO SEMPRE LA PARTE DI PESO TASSABILE ANCORA DA TASSAR
208100980508     C                   SUB       WDIFSC        WPTAS1
208200980508     C**
208300980508     C** SE IL VALORE E' DIVENTATO < DI 0 SIGNIFICA CHE HO TERMINATO LA
208400980508     C**  TASSAZIONE PER CUI USO SOLO LA PARTE RIMASTA DI WPTAS1
208500980508    2C     WPTAS1        IFLE      0
208600980511     C     WDIFSC        ADD       WPTAS1        WPTAS3
208700980514     C* PESO TASSABILE GLOBALE DELLA SPEDIZIONE
208800980508     C                   MOVEL     'S'           WTROVA            1
208900980508     C                   ELSE
209000980508     C**
209100980508     C* PARTE DI PESO DA TASSARE CON QUESTO SCAGLIONE
209200980508     C                   Z-ADD     WDIFSC        WPTAS3
209300980508     C* SALVO LO SCAGLIONE PRECEDENTE
209400980508     C                   Z-ADD     TSCG          WPRESC
209500980508    2C                   ENDIF
209600980508     C**
209700980508     C* IMPOSTO IL VALORE DA MOLTIPLICARE PER LA TARIFFA
209800980508     C                   EXSR      CALQLI
209900980512     C**
210000980511     C* SE LO SCAGLIONE E' APPLICAZIONE TARIFFA FINITA DEVO CONSIDERARE
210100980512     C*  SOLO L'ULTIMO SCAGLIONE COSI' E NON I PRECEDENTI
210200980514    2C     WATA          IFNE      ' '
210300980511     C     WTROVA        ANDEQ     ' '
210400980511     C* MI SALVO IL VALORE MA PER ORA NON LO MOLTIPLICO
210500980511     C                   Z-ADD     PESQLI        WPESQ
210600980511     C                   Z-ADD     TITR          WTITR
210700980511     C                   Z-ADD     TINO          WTINO
210800980511   X2C                   ELSE
210900980511     C**
211000980511     C* SE LO SCAGLIONE TROVATO E' L'ULTIMO MA COMUNQUE TARIFFA FINITA
211100980511     C* DEVO MOLTIPLICARE SOLO QUESTO --> AZZERO UN EVENTUALE PRECEDENT
211200980511     C*  CHE AVEVA LA TASSAZIONE TARIFFA FINITA
211300980511    3C     WTROVA        IFEQ      'S'
211400980514     C     WATA          ANDNE     ' '
211500980511     C                   CLEAR                   WPESQ
211600980511     C                   CLEAR                   WTITR
211700980511     C                   CLEAR                   WTINO
211800980511    3C                   ENDIF
211900980511     C*
212000980511     C* PRIMA DI MOLTIPLICARE LA TARIFFA TROVATA VEDO SE CE NE E'
212100980511     C*  UNA PRECEDENTE TARIFFA FINITA DA CONSIDERARE
212200980511    3C     WPESQ         IFGT      0
212300980511     C* SALVO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
212400980511     C*  APPENA LETTO
212500980511     C                   Z-ADD     PESQLI        WPESSV
212600980511     C                   Z-ADD     TITR          WSTITR
212700980511     C                   Z-ADD     TINO          WSTINO
212800980511     C* IMPOSTO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
212900980511     C*  PRECEDENTE PER IL CALCOLO
213000980511     C                   Z-ADD     WPESQ         PESQLI
213100980511     C                   Z-ADD     WTITR         TITR
213200980511     C                   Z-ADD     WTINO         TINO
213300980511     C                   EXSR      MULTAR
213400980511     C                   ADD       WPOR          TASPOR
213500980511     C                   ADD       WINL          TASINL
213600980511     C                   CLEAR                   WPESQ
213700980511     C                   CLEAR                   WTITR
213800980511     C                   CLEAR                   WTINO
213900980511     C* REIMPOSTO I DATI DELLO SCAGLIONE APPENA LETTO
214000980511     C                   Z-ADD     WPESSV        PESQLI
214100980511     C                   Z-ADD     WSTITR        TITR
214200980511     C                   Z-ADD     WSTINO        TINO
214300980511    3C                   ENDIF
214400980511     C**
214500980508     C* MOLTIPLICAZIONE TARIFFA
214600980508     C                   EXSR      MULTAR
214700980508     C                   ADD       WPOR          TASPOR
214800980508     C                   ADD       WINL          TASINL
214900980511    2C                   ENDIF
215000980515     C                   ADD       1             A
215100980508     C**
215200980508    1C                   ENDDO
215300980515     C**
215400980515     C* APPLICO MINIMO E MASSIMO
215500980515     C                   Z-ADD     TASPOR        WPOR
215600980515     C                   Z-ADD     TASINL        WINL
215700980515     C                   EXSR      MINMAX
215800980515     C                   Z-ADD     WPOR          TASPOR
215900980515     C                   Z-ADD     WINL          TASINL
216000980508     C                   ENDSR
216100980512     C*****************************************************************
216200980512     C* CALCOLO MAX SCAGLIONE PRECEDENTE
216300980512     C*****************************************************************
216400980512     C     MAXSCA        BEGSR
216500980513     C* SOLO SE LO SCAGLIONE APPLICATO NON ERA IL PRIMO
216600980513     C     WA            IFGT      1
216700980512     C* PER CONFRONTARLO CON IL VALORE NORMALE CALCOLATO
216800980512     C* IL CONFRONTO E' DATO DALLA SOMMA TRA PORT E INOLTRO
216900980512     C                   SUB       1             WA
217000980512     C     WA            OCCUR     TARDS
217100980512     C* CALCOLO PESO DA MOLTIPLICARE
217200980512     C                   Z-ADD     TSCG          WPTAS3
217300980512     C                   EXSR      CALQLI
217400980515     C** MOLTIPLICO LA TARIFFA
217500980512     C                   EXSR      MULTAR
217600980515     C** MINIMO E MASSIMO
217700980515     C                   EXSR      MINMAX
217800980515     C**
217900990923     C     TASPOR        ADD       TASINL        T0100            15 3
218000990923     C     WPOR          ADD       WINL          W0100            15 3
218100980515     C** PRENDO DELLE 2 LA TARIFFA PIU' ALTA
218200980512     C     W0100         IFGT      T0100
218300980512     C                   Z-ADD     WPOR          TASPOR
218400980512     C                   Z-ADD     WINL          TASINL
218500980512     C                   ENDIF
218600980513     C                   ENDIF
218700980512     C**
218800980512     C                   ENDSR
218900980515     C*****************************************************************
219000980515     C** MINIMO E MASSIMO DA APPLICARE -SE APPLICATO TUTTO NEL PORTO
219100980515     C*****************************************************************
219200980515     C     MINMAX        BEGSR
219300980515     C** NON ESISTE MINIMO E MASSIMO
219400980515     C     TMIN          IFEQ      0
219500980515     C     TMAX          ANDEQ     0
219600980515     C                   GOTO      DOPOMM
219700980515     C                   END
219800980515     C** MINIMO
219900990923     C     WPOR          ADD       WINL          WMAX             15 3
220000980515     C     TMIN          IFGT      WMAX
220100980515     C                   Z-ADD     TMIN          WPOR
220200980515     C                   Z-ADD     0             WINL
220300980515     C                   Z-ADD     TMIN          WMAX
220400980515     C                   END
220500980515     C** MASSIMO
220600980515     C     TMAX          IFGT      0
220700980515     C     WMAX          IFGT      TMAX
220800980515     C                   Z-ADD     TMAX          WPOR
220900980515     C                   Z-ADD     0             WINL
221000980515     C                   END
221100980515     C                   END
221200980515     C     DOPOMM        ENDSR
221300980429     C*****************************************************************
221400980429     C* CERCO VARIA ISOLA LOCALITA' DISAGIATA CENTRI STORICI
221500090922     c* cerco anche la maggiorazione
221600980429     C*****************************************************************
221700980429     C     CERISO        BEGSR
221800980429     C                   CLEAR                   VAISO
221900060414     C                   CLEAR                   VATSP
222000140204      * vaaggfuel varie che vengono sommate solo all'imponibile fuel e supplemento carburante
222100140204     C                   CLEAR                   VAAGGFUEL
222200980429     C     *LIKE         DEFINE    TASVA1        VAISO
222300060414     C     *LIKE         DEFINE    TASVA1        VATSP
222400140204     C     *LIKE         DEFINE    TASVA1        VAAGGFUEL
222500981020     C                   Z-ADD     1             CC                3 0
222600980429     C     §$2VRJ        LOOKUP    SV(CC)                                 96     ISOLA
222700980429     C   96              ADD       VA(CC)        VAISO
222800981008     C                   Z-ADD     1             CC
222900980429     C     §$2VRK        LOOKUP    SV(CC)                                 96     DISAGIATE
223000980429     C   96              ADD       VA(CC)        VAISO
223100981008     C                   Z-ADD     1             CC
223200980429     C     §$2VRQ        LOOKUP    SV(CC)                                 96     C.STORICI
223300980429     C   96              ADD       VA(CC)        VAISO
223400060414     C                   Z-ADD     1             CC                3 0
223500090923     c* "E" o "H"
223600090923     c                   if        WvariaMag<>' '
223700140204     C     WVariaMag     LOOKUP    SV(CC)                                 96     maggiorazione e o h
223800060414     C   96              z-add     VA(CC)        VAtsp
223900090923     c                   endif
224000140204      * CALCOLO LE VARIE  AGGIUNTIVE AL SOLO IMPONIBILE FUEL
224100140204     C                   Z-ADD     1             CC                3 0
224200140204     C     '='           LOOKUP    SV(CC)                                 96     RESO BANCALI
224300140204     C   96              ADD       VA(CC)        VAAGGFUEL
224400140204     C                   Z-ADD     1             CC                3 0
224500140204     C     'c'           LOOKUP    SV(CC)                                 96     LASCIATO AVVISO
224600140204     C   96              ADD       VA(CC)        VAAGGFUEL
224700140204     C                   Z-ADD     1             CC                3 0
224800140204     C     'A'           LOOKUP    SV(CC)                                 96     APPUNTAMENTO
224900140204     C   96              ADD       VA(CC)        VAAGGFUEL
225000140204     C                   Z-ADD     1             CC                3 0
225100140204     C     'B'           LOOKUP    SV(CC)                                 96     CONSEGNA DDT
225200140204     C   96              ADD       VA(CC)        VAAGGFUEL
225300140204     C                   Z-ADD     1             CC                3 0
225400140204     C     'C'           LOOKUP    SV(CC)                                 96     FACCH. ARR.
225500140204     C   96              ADD       VA(CC)        VAAGGFUEL
225600140204     C                   Z-ADD     1             CC                3 0
225700140204     C     'F'           LOOKUP    SV(CC)                                 96     FUORI MISURA
225800140204     C   96              ADD       VA(CC)        VAAGGFUEL
225900140204     C                   Z-ADD     1             CC                3 0
226000140204     C     'H'           LOOKUP    SV(CC)                                 96     DIRITTO FISSO
226100140204     C   96              ADD       VA(CC)        VAAGGFUEL
226200140204     C                   Z-ADD     1             CC                3 0
226300140204     C     'I'           LOOKUP    SV(CC)                                 96     SPESE DI GIACENZA
226400140204     C   96              ADD       VA(CC)        VAAGGFUEL
226500140204     C                   Z-ADD     1             CC                3 0
226600140204     C     'N'           LOOKUP    SV(CC)                                 96     ANTEPORTO
226700140204     C   96              ADD       VA(CC)        VAAGGFUEL
226800140204     C                   Z-ADD     1             CC                3 0
226900140204     C     'P'           LOOKUP    SV(CC)                                 96     AI PIANI
227000140204     C   96              ADD       VA(CC)        VAAGGFUEL
227100140204     C                   Z-ADD     1             CC                3 0
227200140204     C     'S'           LOOKUP    SV(CC)                                 96     SUPERMERCATO
227300140204     C   96              ADD       VA(CC)        VAAGGFUEL
227400140204     C                   Z-ADD     1             CC                3 0
227500140204     C     'U'           LOOKUP    SV(CC)                                 96     RITIRO
227600140204     C   96              ADD       VA(CC)        VAAGGFUEL
227700140204     C                   Z-ADD     1             CC                3 0
227800140204     C     'X'           LOOKUP    SV(CC)                                 96     CONS.DISAG.
227900140204     C   96              ADD       VA(CC)        VAAGGFUEL
228000151209     C                   Z-ADD     1             CC                3 0
228100151209     C     'p'           LOOKUP    SV(CC)                                 96     PIN CODE
228200151209     C   96              ADD       VA(CC)        VAAGGFUEL
228300151209     c
228400151209     C     'z'           LOOKUP    SV(CC)                                 96     EXPO
228500151209     C   96              ADD       VA(CC)        VAAGGFUEL
228600151209     c
228700151209     C     't'           LOOKUP    SV(CC)                                 96     Diritto Pren.ORM
228800151209     C   96              ADD       VA(CC)        VAAGGFUEL
228900151209     c
229000151209     C     'k'           LOOKUP    SV(CC)                                 96     Packing List
229100151209     C   96              ADD       VA(CC)        VAAGGFUEL
229200151209     c
229300140204     c
229400980429     C                   ENDSR
229500130124     C*****************************************************************
229600130124     C* CERCO VARIA ISOLA LOCALITA' DISAGIATA CENTRI STORICI INOLTRO
229700130124     c* MAGGIORAZIONI PER CALCOLO DIRETTO DEL SOLO FUEL
229800130124     C*****************************************************************
229900130124     C     CER_IMPFUEL   BEGSR
230000130124     C                   CLEAR                   IMP_SCA
230100130124     C                   Z-ADD     1             CC                3 0
230200130124     C     §$2VRJ        LOOKUP    SV(CC)                                 96     ISOLA
230300130124     C   96              ADD       VA(CC)        IMP_SCA
230400130124     C                   Z-ADD     1             CC
230500130124     C     §$2VRK        LOOKUP    SV(CC)                                 96     DISAGIATE
230600130124     C   96              ADD       VA(CC)        IMP_SCA
230700130124     C                   Z-ADD     1             CC
230800130124     C     §$2VRQ        LOOKUP    SV(CC)                                 96     C.STORICI
230900130124     C   96              ADD       VA(CC)        IMP_SCA
231000130124     C                   Z-ADD     1             CC
231100130124     C     §$2FIN        LOOKUP    SV(CC)                                 96     INOLTRO
231200130124     C   96              ADD       VA(CC)        IMP_SCA
231300130124     C                   Z-ADD     1             CC                3 0
231400130124     C     §$2TSP        LOOKUP    SV(CC)                                 96     ESPRESSO
231500130124     C   96              ADD       VA(CC)        IMP_SCA
231600130124     C                   Z-ADD     1             CC                3 0
231700130124     C     §$2TSH        LOOKUP    SV(CC)                                 96     H 10,30
231800130124     C   96              ADD       VA(CC)        IMP_SCA
231900130125      *
232000130125     c                   add       taspor        imp_sca
232100140204      * se data fattura > 20140130 aggiungo nuove varia all'imponibile fuel
232200140204     c                   If        tasdtfat > 20140130
232300140204     C                   Z-ADD     1             CC                3 0
232400140204     C     '='           LOOKUP    SV(CC)                                 96     RESO BANCALI
232500140204     C   96              ADD       VA(CC)        imp_sca
232600140204     C                   Z-ADD     1             CC                3 0
232700140204     C     'c'           LOOKUP    SV(CC)                                 96     LASCIATO AVVISO
232800140204     C   96              ADD       VA(CC)        imp_sca
232900140204     C                   Z-ADD     1             CC                3 0
233000140204     C     'A'           LOOKUP    SV(CC)                                 96     APPUNTAMENTO
233100140204     C   96              ADD       VA(CC)        imp_sca
233200140204     C                   Z-ADD     1             CC                3 0
233300140204     C     'B'           LOOKUP    SV(CC)                                 96     CONSEGNA DDT
233400140204     C   96              ADD       VA(CC)        imp_sca
233500140204     C                   Z-ADD     1             CC                3 0
233600140204     C     'C'           LOOKUP    SV(CC)                                 96     FACCH. ARR.
233700140204     C   96              ADD       VA(CC)        imp_sca
233800140204     C                   Z-ADD     1             CC                3 0
233900140204     C     'F'           LOOKUP    SV(CC)                                 96     FUORI MISURA
234000140204     C   96              ADD       VA(CC)        imp_sca
234100140204     C                   Z-ADD     1             CC                3 0
234200140204     C     'H'           LOOKUP    SV(CC)                                 96     DIRITTO FISSO
234300140204     C   96              ADD       VA(CC)        imp_sca
234400140204     C                   Z-ADD     1             CC                3 0
234500140204     C     'I'           LOOKUP    SV(CC)                                 96     SPESE DI GIACENZA
234600140204     C   96              ADD       VA(CC)        imp_sca
234700140204     C                   Z-ADD     1             CC                3 0
234800140204     C     'N'           LOOKUP    SV(CC)                                 96     ANTEPORTO
234900140204     C   96              ADD       VA(CC)        imp_sca
235000140204     C                   Z-ADD     1             CC                3 0
235100140204     C     'P'           LOOKUP    SV(CC)                                 96     AI PIANI
235200140204     C   96              ADD       VA(CC)        imp_sca
235300140204     C                   Z-ADD     1             CC                3 0
235400140204     C     'S'           LOOKUP    SV(CC)                                 96     SUPERMERCATO
235500140204     C   96              ADD       VA(CC)        imp_sca
235600140204     C                   Z-ADD     1             CC                3 0
235700140204     C     'U'           LOOKUP    SV(CC)                                 96     RITIRO
235800140204     C   96              ADD       VA(CC)        imp_sca
235900140204     C                   Z-ADD     1             CC                3 0
236000140204     C     'X'           LOOKUP    SV(CC)                                 96     CONS.DISAG.
236100140204     C   96              ADD       VA(CC)        imp_sca
236200140204     c
236300140204     c                   endif
236400130124     c
236500130124     C                   ENDSR
236600060905     C*****************************************************************
236700060905     C* Fuel Surcharge
236800060905     C*****************************************************************
236900060905     c     SR_fuel       BEGSR
237000060906     C                   CLEAR                   SCA_PB
237100060906     C                   CLEAR                   SCA_SPE
237200060906     C                   CLEAR                   SCA_SCA
237300130124      * Se al programma vengono passate le date aggiuntive per la ritassazione
237400130124      * imposto come data di spedizione quella passata se maggiore di zero
237500130124     c                   clear                   Sav_dsp
237600130124     c                   If        Ritass = 'S' and tasdtfue > 0
237700130124     c                   eval      Sav_dsp = Tasdsp
237800130124     c                   movel     tasdtfue      tasdsp
237900130124     c                   endif
238000060905      *
238100060907      * verifico se esiste già la varia non la calcolo
238200060905      *
238300060905     c     §$2VFS        lookup    sv                                     96
238400060907     c                   IF        %found
238500060907     c                   goto      Endfuel
238600060907     c                   endif
238700060905
238800060905      * verifico se cliente ha in tariffa il calcolo del FUEL Surcharge
238900060907      * altrimenti non vado a cercare nella cartello e vado a fine
239000060906     C                   MOVEL     'f'           FISO
239100060906     C                   MOVEL     'f'           WFISO
239200060905
239300060906     c                   z-add     1             a
239400060906     c     Wfiso         Lookup    Cp(a)                                  05
239500060906     C  N05              GOTO      ENDFUEL
239600060906      * aggancio la ds della tabella 1P
239700060906     c     A             Occur     DS1P
239800060906
239900060905     C     KISOT         CHAIN     TITPT01L
240000060907      * cliente non ha la tariffa  non faccio calcolo
240100060907     C                   IF        not %FOUND(TITPT01L)
240200060907     c                   goto      Endfuel
240300060907     c                   endif
240400140227      * ds x recupero % minima applicabile
240500140227     c                   eval      DTPT01 = TPTflo
240600140227      *
240700140228      * data prezzo base maggiore della data spedizone non faccio calcolo
240800060907     C                   IF        TPTDPB > TASDSP
240900060907     c                   goto      Endfuel
241000060907     c                   endif
241100140227      * Ricerco il valore minimo applicabile e lo scaglione corrispondente in vigore
241200140227      * alla data spedizione
241300150217      * solo se in tariffa non è stato specificato che non si deve applicare il VMA
241400150217
241500150217     c                   If        §TPTvma = 'N'
241600150217     c                   clear                   dpbvma
241700150217     c                   clear                   dpbsvm
241800150217     c                   else
241900140227
242000150217      * si applicazione VMA
242100140227     C     TASDSP        SETLL     TIDPB01L
242200140227     C                   READ      TIDPB01L
242300140227     c                   if        not %found(tidpb01l)
242400140227     c                   clear                   dpbvma
242500140227     c                   clear                   dpbsvm
242600140227     c                   endif
242700150217     c                   endif
242800060906
242900060905      * Ricerco scaglione di appartenenza del prezzo base
243000060905
243100060911     C     TPTDPB        SETLL     TIPMG01L
243200060906     C                   READ      TIPMG01L
243300060906     C                   IF        NOT %EOF(TIPMG01L)
243400060906      *
243500060906     C                   Z-ADD     PMGSCA        SCA_PB
243600060906     C                   ENDIF
243700060906
243800060906      * Ricerco scaglione di appartenenza del prezzo del gasolio al momento della spedizione
243900060906
244000060911     C     TASDSP        SETLL     TIPMG01L
244100060906     C                   READ      TIPMG01L
244200060906     C                   IF        NOT %EOF(TIPMG01L)
244300060906      *
244400080909      * recupero il prezzo per ricercare il F.D. in tariffa
244500080909      *
244600080909     c                   z-add     pmgpmg        PMG_SGL
244700080909
244800060906     C                   Z-ADD     PMGSCA        SCA_SPE
244900060906     C                   ENDIF
245000140227      * verifico se il prezzo del gasolio al momento della spedizione è minore del valore minimo
245100140227      * applicabile sostituisco il prezzo del gasolio e il suo scaglione con il valore minimo
245200140227      * applicabile e lo scaglione corrispondente
245300140227
245400140227     c                   if        pmgpmg < dpbvma
245500140227     c                   z-add     dpbvma        PMG_SGL
245600140227     c                   z-add     dpbsvm        SCA_SPE
245700140227     c                   Endif
245800140227
245900060906
246000060906      * calcolo gli scaglioni di aumento
246100060906
246200060906     C                   EVAL      SCA_SCA = SCA_SPE - SCA_PB
246300060906     C                   IF        SCA_SCA < 0
246400060906     C                   CLEAR                   SCA_SCA
246500060906     C                   ENDIF
246600140228      * calcolo dell'importo da addebitare al cliente
246700140228
246800140228     C                   CLEAR                   VARIA
246900140228     C                   CLEAR                   IMP_SCA
247000140228     C                   CLEAR                   VARIAfuel_min
247100140228      * calcolo imponibile FUEL
247200140228
247300140228      * se sono diretta per il solo calcolo del Fuel(principalmente da ritassazione)
247400140228      * mi calcolo imponibile in maniera diversa dalla normale tassazione
247500140228     C                   If        tassva = §$2VFS
247600140228     c                   exsr      Cer_impfuel
247700140228     c                   else
247800140228     C                   EXSR      CERISO
247900140228     c                   If        tasdtfat <= 20140130
248000140228     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP
248100140228     c                   else
248200140228      * recupero importi di isola e località disagiate ed espresso + nuove varie introdotte
248300140228      * dal 3/2/2014 dv 2473
248400140228     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
248500140228     c                                       VAAGGFUEL
248600140228     c                   endif
248700140228     c                   endif
248800140228      *
248900060906
249000060906      * se gli scaglioni scattati sono maggiore di zero cerco l'incidenza percentuale di
249100060906      * aumento nel dettaglio tariffario
249200060906     C                   IF        SCA_SCA > 0
249300080909      * cambia con il progetto 666 aumento supplemento carburante in tariffa
249400080909      * per cercare il Fattore demoltiplicatore non usiamo più il numero di scaglioni
249500080909      * di aumento ma il prezzo medio del gasolio della settimana della data di spedizione
249600080909     C****               Z-ADD     SCA_SCA       TPDPES
249700080909     C                   Z-ADD     PMG_SGL       TPDPES
249800060906     C                   EXSR      CALACD
249900060907      * Se la percentuale di incidenza è maggiore di zero
250000060907    1C                   IF        AITR > 0
250100060905      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
250200060905      *                     per abbandonare il concetto di monete diverse dall'EURO)
250300060907     C                   EVAL(H)   VARIA = (IMP_SCA * aitr * sca_sca) / 100
250400140227
250500140227     C                   ENDIF
250600140228     C                   ENDIF
250700140227      * anche se il cliente ha negato la tariffa verifico
250800140227      * se nella tariffa particolare del cliente è stato espressa la % minima di apllicazione
250900140227      * calcolo l'importo del fuel utilizzando questa percentuale
251000140227
251100140227     c                   if        §tptpma > 0
251200140227     c                   eval(H)   variafuel_min =  (IMP_SCA * §TPTpma)/ 100
251300140227     c                   Endif
251400140227
251500140227      * prendo il valore della varia maggiore tra quella calcolata con la % minima applicabile
251600140227      * e quella calcolata con il FD della tariffa
251700140227
251800140227     c                   if        variafuel_min > Varia
251900140227     c                   eval      Varia =  variafuel_min
252000140227     c                   endif
252100140227
252200060905      **
252300140227    1c                   If        Varia > 0
252400060905     C                   Z-ADD     1             A
252500060905     C     ' '           LOOKUP    SV(A)                                  96
252600060907     C   96              MOVEL     §$2VFS        SV(A)
252700060905     C   96              Z-ADD     VARIA         VA(A)
252800060905     C  N96              ADD       VARIA         TASPOR
252900060905    1C                   ENDIF
253000060905      *
253100140228
253200130124     c     endfuel       TAG
253300060905      *
253400130124      * se impostato il campo del salvataggio della data spedizione
253500130124     c                   if        Sav_dsp <> *zeros
253600130124     c                   eval      tasdsp = Sav_dsp
253700130124     c                   clear                   Sav_dsp
253800130124     c                   endif
253900130124
254000130124     C                   ENDSR
254100040506     C*****************************************************************
254200060905     C* SUPPLEMENTO CARBURANTE FEDEX
254300040506     C*****************************************************************
254400060905     C     SR_SUPCARB    BEGSR
254500040507      *
254600040507      * verifico se esiste già la varia
254700040507      *
254800040507     c     §$2VSC        lookup    sv                                     96
254900040507     c                   IF        not %found
255000040506
255100040506      * RICERCO IN BASE ALLA DATA SPEDIZIONE LA GIUSTA PERCENTUALE DI
255200040506      * SUPPLEMNENTO
255300130124      * Se al programma vengono passate le date aggiuntive per la ritassazione
255400130124      * imposto come data di spedizione quella passata se maggiore di zero
255500130124     c                   clear                   Sav_dsp
255600130124     c                   If        Ritass = 'S' and tasdtfue > 0
255700130124     c                   eval      Sav_dsp = Tasdsp
255800130124     c                   movel     tasdtfue      tasdsp
255900130124     c                   endif
256000130124      *
256100040506
256200040507     C                   CLEAR                   VARIA
256300040507     C                   CLEAR                   IMP_SCA
256400040507     C                   CLEAR                   PER_SCA
256500040507     C                   Z-ADD     1             KX
256600040507    1c                   DO        *HIVAL
256700040507      * SCHIERA IN ORDINE DISCENDENTE
256800040507     C                   MOVE      DSCA(KX)      WDSPSC
256900040507      * SE DATA UGUALE A ZERO VADO A FINE
257000040507    2C                   IF        DS_DSC = *ZEROS
257100040507     C                   LEAVE
257200040507    2C                   ENDIF
257300040507      * SE DATA SPEDIZIONE MAGGIORE DI QUELLA IN SCHIERA RECUPERO LA PERCENTUALE DI CALCOLO
257400040510    2C                   IF        TASDSP >=  DS_DSC
257500040507     c                   z-add     DS_psc        PER_SCA
257600040507     C                   LEAVE
257700040507     C                   ELSE
257800040507      * SE DATA MINORE LEGGO LA DATA SUCCESSIVA
257900040507    3C                   IF        KX < 999
258000040507     C                   ADD       1             KX
258100040507     C                   ELSE
258200040507     C                   LEAVE
258300040507    3C                   ENDIF
258400040507    2C                   ENDIF
258500040507
258600040507    1C                   ENDDO
258700040507      * SE PERCENTUALE MAGGIORE DI ZERO ESEGUO IL CALCOLO
258800040507    1C                   IF        PER_SCA > 0
258900040507      * CALCOLO L'IMPONIBILE
259000060414      * recupero importi di isola e località disagiate ed espresso
259100040507     C                   EXSR      CERISO
259200040507      *
259300060414     c* Di fatto qui l'espresso non ci sarà mai!!! bolla estera
259400140204      *
259500140204     c                   If        tasdtfat <= 20140130
259600060414     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP
259700140204     c                   else
259800140204     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
259900140204     c                             vaaggfuel
260000140204     c                   Endif
260100040507      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
260200040507      *                     per abbandonare il concetto di monete diverse dall'EURO)
260300040507     C                   EVAL(H)   VARIA = (IMP_SCA * PER_SCA) / 100
260400040507      **
260500040507     C                   Z-ADD     1             A
260600040507     C     ' '           LOOKUP    SV(A)                                  96
260700040507     C   96              MOVEL     §$2VSC        SV(A)
260800040507     C   96              Z-ADD     VARIA         VA(A)
260900040507     C  N96              ADD       VARIA         TASPOR
261000040507    1C                   ENDIF
261100130124      *
261200130124      * se impostato il campo del salvataggio della data spedizione
261300130124     c                   if        Sav_dsp <> *zeros
261400130124     c                   eval      tasdsp = Sav_dsp
261500130124     c                   clear                   Sav_dsp
261600130124     c                   endif
261700130124      *
261800130124     C                   ENDIF
261900040507      *
262000040507     C                   ENDSR
262100970521     C*****************************************************************
262200970521     C* TASSAZIONE TARIFFE PARTICOLARI
262300970521     C*****************************************************************
262400970521     C     TASPAR        BEGSR
262500970521     C     *LIKE         DEFINE    TASVA1        TASCP
262600970522     C     *LIKE         DEFINE    TASVA1        TASISO
262700970521     C     *LIKE         DEFINE    TASTC1        TASTC
262800970721     C                   CLEAR                   TASCP
262900970721     C                   CLEAR                   WNOEST
263000970721     C* SE NON ESISTE LA SIGLA DELLA VARIA O E' GIA' IN TASSAZIONE
263100970721     C*  NON RITASSO NE APPLICO L'ESPRESSO
263200970521     C**
263300970521     C                   Z-ADD     1             A
263400970521     C     TASTC         LOOKUP    CP(A)                                  05
263500970521     C  N05              GOTO      ENDPAR
263600970521     C**
263700970521     C     A             OCCUR     DS1P
263800970521     C                   Z-ADD     1             A
263900970521     C     §1PVAR        LOOKUP    SV(A)                                  05
264000970521     C* SIGLA VARIA NON PRESENTE --> CALCOLO
264100970521     C     *IN05         IFEQ      *OFF
264200970721     C                   MOVEL     'S'           WNOEST            1
264300970521     C                   EXSR      CALCP
264400970521     C**
264500970521     C     TASCP         IFGT      0
264600130927      * se sto tassando la tariffa particolare m = avviso affidamento spedizione
264700130927      * ed il flag EMD è uguale a "E" (Sia sms che mail) raddoppio la varia
264800130927     c                   if        §1pvar = §$2VAD and
264900130927     c                             §asemd = 'E'
265000130927     c                   eval      tascp = tascp * 2
265100130927     c                   endif
265200130927      *
265300970521     C                   Z-ADD     1             A
265400970521     C     ' '           LOOKUP    SV(A)                                  96
265500970521     C   96              MOVEL     §1PVAR        SV(A)
265600970521     C   96              Z-ADD     TASCP         VA(A)
265700970521     C  N96              ADD       TASCP         TASPOR
265800970521     C                   END
265900970521     C                   END
266000970521     C     ENDPAR        ENDSR
266100941112     C******************************************************
266200941107     C** CALCOLO PESO TASSABILE PER TARIFFA A PESO
266300941112     C******************************************************
266400941107     C     CALPT         BEGSR
266500980313     C** MI MEMORIZZO SU BLTAS IL PESO TASSABILE SE
266600980313     C** IL PESO E' SUPERIORE AL MINIMO TASSABILE
266700980313     C** SE ESISTE IL RAPPORTO PESO VOLUME IN TARIFFA  E VOL IN BOLLA
266800980313     C** IL PESO TASSABILE SU BLTAS E' SEMPRE ARROTONDATO AL KG SUPERIO
266900980311     C                   CLEAR                   TASPVL
267000050912     C***  UNOCTR        IFEQ      0
267100050912     C**** UNOCTR        OREQ      3
267200050912      * modifico --- il peso tassabile si stampa e si calcola nei seguenti casi :
267300050912      * Tariffa a quintale "0" il peso da fatturare maggiore del  minimo tassabile
267400050912      *                        (definito nella tabella TR = 100) oppure volume da fatturare
267500050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
267600050912      *                        tariffa maggiore di zero
267700050912      * Tariffa a kg       "3" non controllo il peso da fatturare maggiore del minimo tassabile
267800050912      *                        (definito nella tabella TR = 0,1) ma solo volume da fatturare
267900050912      *                        maggiore di zero e rapporto peso volume nello scaglione della
268000050912      *                        tariffa maggiore di zero
268100050912     C***  TARIFFA A QUINTALE
268200050912
268300050912     C     UNOCTR        IFEQ      0
268400050912
268500020826     C     USAPKF        IFGT      MINTAS
268600950901     C     PESTAS        ADD       0,9           TASPVL
268700941107     C                   ELSE
268800941107     C     TASVLF        IFNE      0
268900980311     C     RAPPV         ANDNE     0
269000950901     C     PESTAS        ADD       0,9           TASPVL
269100980313     C                   ENDIF
269200980313     C                   ENDIF
269300980313     C                   ENDIF
269400050912
269500050912     C***  TARIFFA A KG
269600050912
269700050912     C     UNOCTR        IFEQ      3
269800050912
269900050912     C     TASVLF        IFNE      0
270000050912     C     RAPPV         ANDNE     0
270100050912     C     PESTAS        ADD       0,9           TASPVL
270200050912     C                   ENDIF
270300050912     C                   ENDIF
270400050912
270500980313     C                   ENDSR
270600950119     C******************************************************
270700950119     C** CALCOLA VALORE DA ASSICURARE PER TARIFFA A VALORE
270800950119     C******************************************************
270900950119     C     CALVAS        BEGSR
271000950119     C     *LIKE         DEFINE    TASIAS        IMPVAS
271100950119     C                   Z-ADD     0             IMPVAS
271200950119     C                   MOVEL     'C'           FISO
271300990722     C                   MOVEL     'C'           WFISO             1
271400950119     C**FLAG ASSICURAZIONE X CONTO = C
271500950119     C                   Z-ADD     1             A
271600990722     C     WFISO         LOOKUP    CP(A)                                  05
271700950119     C  N05              GOTO      DOPVA1
271800950119     C**----------------------------------------
271900990721     C     KISOT         CHAIN     TITPT01L                           28
272000980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
272100980528     C     *IN28         IFEQ      *ON
272200980528     C     TPTATB        ORNE      ' '
272300980528     C                   GOTO      DOPVA1
272400980528     C                   ENDIF
272500980528     C**
272600950119     C** ESCLUDO GLI ANNULLATI
272700950119     C     TPTFVM        IFEQ      '3'
272800020826     C     TPTVLM        MULT      USAPKF        IMPVAS
272900950119     C                   END
273000950119     C*PESO
273100950119     C     TPTFVM        IFEQ      '1'
273200950119     C     TPTVLM        MULT      TASNCL        IMPVAS
273300950119     C                   END
273400950119     C* COLLI
273500950119     C     TPTFVM        IFEQ      '2'
273600950119     C                   Z-ADD     TPTVLM        IMPVAS
273700950119     C                   END
273800950119     C*SPEDIZIONE
273900950119     C     DOPVA1        ENDSR
274000941112     C******************************************************
274100941107     C** CALCOLA VARIA R  --- ASSICURAZIONE PER CONTO
274200941112     C******************************************************
274300941107     C     CALVR         BEGSR
274400050525     c* Il falg task88 cambia significato:indica se bolla con mandato o no
274500050525     c*
274600050525     c* con mandato significa: x mandato a val variabile--> tutte le bolle
274700050525     c*                        x mandato a val.indicato --> solo bolle con
274800050525     c*                        tasias=0 o con tasias <=al valore del mandat
274900050525     c*
275000050525     c* Imposto task88=N se mandato
275100050525     c* Imposto task88=S se no mandato ma tasias>0
275200050525     c* Imposto task88=X per bolle senza mandato e senza importo
275300050525     c
275400050525     C                   MOVEL     'X'           TASK88
275500980603     C** 88 = SI CALCOLA SOLO SE ESISTE IL VALORE MERCE IN BOLLA
275600941112     C     KSCCOD        IFEQ      8888
275700980527     C     KSCCOD        OREQ      9999
275800980709     C     TASIAS        IFEQ      0
275900050525     C**                 MOVEL     'S'           TASK88
276000980709     C                   GOTO      ENDVR
276100941112     C                   END
276200980709     C                   END
276300980603     C**
276400980603     C** SE TIPO BOLLA CHE NON PREVEDE L'ASSICURAZIONE, ESCO SENZA
276500980603     C**  DARE ERRORE
276600050525     C**   TASIAS        IFEQ      0
276700050525     C**   TASTBL        LOOKUP    TBN                                    05
276800050525     C** 05              MOVEL     'S'           TASK88
276900050525     C** 05              GOTO      ENDVR
277000050525     C**                 ENDIF
277100991012     C**
277200991012     C** SE IMPORTO D'ASSICURARE MAGGIORE DI ZEROS MUOVO 'S' TASK88
277300991012     C** PER STAMPA BOLLE CON ASSICURAZIONE PER TABULATONE DELLA CONSULDANNI
277400050525     C**   TASIAS        IFGT      0
277500050525     C**                 MOVEL     'S'           TASK88
277600050525     C**                 ENDIF
277700980603     C**
277800941116     C                   SETOFF                                       94
277900941116     C     *LIKE         DEFINE    TASIAS        IASLIT
278000941116     C                   Z-ADD     TASIAS        IASLIT
278100941116     C     IASLIT        IFNE      0
278200941116     C                   Z-ADD     1             K
278300941116     C     TASVAS        LOOKUP    CV(K)                                  05
278400980504     C     *IN05         IFEQ      *OFF
278500980504     C                   SETON                                        94
278600980504     C                   MOVEL     MSG(2)        TASMSG
278700980504     C                   GOTO      FINE
278800980504     C                   ENDIF
278900941116     C** NON ESISTE CAMBIO - MANCA TARIFFA
279000990922     C**
279100990922     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA SE E' DIVERSA
279200990922     C* DALLA VALUTA DELLA TARIFFA
279300990922     C     §TADIV        IFNE      TASVAS
279400990922     C                   CLEAR                   YEUR
279500990922     C                   MOVEL     TASVAS        YECDVI
279600990922     C                   MOVEL     §TADIV        YECDVO
279700990922     C                   Z-ADD     IASLIT        YECIMI
279800011129     C   12              MOVE      '2'           YECDCO
279900991014     C  N12              MOVE      '0'           YECDCO
280000990922     C*
280100990922     C                   CALL      'YEURCO'
280200990922     C                   PARM                    YEUR
280300990922     C*
280400990922     C     YECESI        IFEQ      ' '
280500990922     C                   Z-ADD     YECIMO        IASLIT
280600990922     C                   END
280700990922     C                   ENDIF
280800990922     C                   ENDIF
280900941123     C**----------------------------------------
281000941107     C                   MOVEL     'C'           FISO
281100990722     C                   MOVEL     'C'           WFISO
281200941107     C**FLAG ASSICURAZIONE X CONTO = C
281300941123     C                   Z-ADD     1             A
281400990722     C     WFISO         LOOKUP    CP(A)                                  05
281500941123     C  N05              GOTO      ENDVR
281600020226      * AGGANCIO LA DS DELA TABELLA 1P
281700020226     C     A             OCCUR     DS1P
281800941123     C**----------------------------------------
281900990721     C     KISOT         CHAIN     TITPT01L                           28
282000980528     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
282100980528     C     *IN28         IFEQ      *ON
282200980528     C     TPTATB        ORNE      *BLANKS
282300980528     C                   GOTO      DOPAC1
282400980528     C                   ENDIF
282500980505     C**
282600980505     C** CONTROLLO IL TIPO APPLICAZIONE DELLA TARIFFA ASSIC X CONTO
282700980505     C**  SE INCONGRUENTE CON TIPO BOLLA PRENDO LA CARTELLO
282800980505     C     TPTAPL        IFEQ      'A'
282900980505     C     TIPBOL        ANDEQ     'F'
283000980505     C     TPTAPL        OREQ      'P'
283100980505     C     TIPBOL        ANDEQ     'A'
283200980505     C                   GOTO      DOPAC1
283300980505     C                   ENDIF
283400980505     C**
283500980504     C* VALORE MERCE = 0 --> ESCLUDO SE NON E' UN VERO MANDATO
283600980505     C                   SETOFF                                       94
283700980504    1C     TPTVLM        IFEQ      0
283800980709     C* CHE SIA UN MANDATO FITTIZIO O UN MANDATO A VALORE VARIABILE
283900980709     C*  DEVO SEMPRE SCRIVERE LA BOLLA COME 8888 PERCHE L'ASSICURAZIONE
284000980709     C*  VUOLE VEDERE BOLLA PER BOLLA QUANTO SONO ASSICURATE
284100050525     C****               MOVEL     'S'           TASK88
284200980709     C**
284300980504    2C     IASLIT        IFEQ      0
284400980504     c* MANDATO FITTIZIO
284500990922    3C     TPTTMA        IFEQ      'F'
284600980504     C                   GOTO      ENDVR
284700980504    3C                   ENDIF
284800050525     c**
284900050526     C** ES - non serve: già fatto    sopra!! Ripetizione inutile
285000050526    3C**   TPTAPL        IFEQ      'P'
285100050526     C**   TIPBOL        COMP      'F'                                    94
285200050526    3C**                 END
285300050526    3C**   TPTAPL        IFEQ      'A'
285400050526     C**   TIPBOL        COMP      'A'                                    94
285500050526    3C**                 END
285600050526    3C**   TPTAPL        IFEQ      ' '
285700050526     C**                 SETON                                            94
285800050526    3C**                 END
285900050525     c
286000050525     c
286100050525     c* anche se manca tariffa è comunque con mandato
286200050525     c                   eval      task88='N'
286300050525     c*
286400050525     c* non devo dare manca tariffa se tipo bolla che non prevede
286500050525     c* Assicurazione
286600050525     C     TASTBL        LOOKUP    TBN                                    05
286700050525     c                   if        not *in05
286800050526     c                   eval      *in94='1'
286900050525     C                   MOVEL     MSG(4)        TASMSG
287000050525     C                   GOTO      FINE
287100050525     c                   endif
287200050525     c
287300050525   x2c                   else
287400050525     c* Mandato a valore variabile con importo: se fittizio 8888 altriment
287500050525     c* con mandato
287600050525    3C     TPTTMA        IFEQ      'F'
287700050525     c                   eval      task88='S'
287800050525     c                   else
287900050525     c                   eval      task88='N'
288000050525    3C                   END
288100050525    2C                   END
288200050525     c
288300941107     C** MANCA TARIFFA: MANDATO CON VALORE = 0
288400941107     C**                E NON ESISTE VALORE IN BOLLA (SOLO COD.)
288500980504   X1C                   ELSE
288600980504    2C     IASLIT        IFGT      0
288700941112     C                   Z-ADD     0             VALSPE           15 3
288800980504    3C     TPTFVM        IFEQ      '3'
288900020826     C     TPTVLM        MULT      USAPKF        VALSPE
289000980504    3C                   END
289100941107     C*PESO
289200980504    3C     TPTFVM        IFEQ      '1'
289300941107     C     TPTVLM        MULT      TASNCL        VALSPE
289400980504    3C                   END
289500941107     C* COLLI
289600980504    3C     TPTFVM        IFEQ      '2'
289700941107     C                   Z-ADD     TPTVLM        VALSPE
289800980504    3C                   END
289900030908      *
290000030908      * ARROTONDO AL DECIMALE DELLA TARIFFA
290100030908     C                   CLEAR                   YEUR
290200030908     C                   MOVEL     §TADIV        YECDVI
290300030908     C                   MOVEL     §TADIV        YECDVO
290400030908     C                   Z-ADD     VALSPE        YECIMI
290500030908     C                   CALL      'YEURCO'
290600030908     C                   PARM                    YEUR
290700030908     C*
290800030908     C     YECESI        IFEQ      ' '
290900030908     C                   Z-ADD     YECIMO        VALSPE
291000030908     C                   END
291100941107     C*SPEDIZIONE
291200980504    3C     IASLIT        IFGT      VALSPE
291300941107     C                   GOTO      DOPAC1
291400050526     c                   else
291500050526     c
291600050526     c* Se valore rientra nel mandato, imposto che si tratta di mandato
291700050526     C                   MOVEL     'N'           TASK88
291800980504    3C                   END
291900050526     c
292000050526   x2c                   else
292100050526     c* se senza valore in bolla ma madanto con valore -->ok da mandato
292200050526     c                   eval      task88='N'
292300980504    2C                   END
292400980504    1C                   END
292500050526     c
292600980525     C** SE IL CLIENTE E' 8888 HO IMPOSTATO PRIMA LA TARIFFA DI CARTELL
292700980525     C     KSCCOD        IFEQ      8888
292800050525     C                   MOVEL     'S'           TASK88
292900980525     C                   ENDIF
293000050525     c
293100050525     c** Anche se lo considero con mandato, se non c'e' importo e tipo
293200050525     c**  bolla senza assicurazione, non calcolo varia R
293300050525     C     TASIAS        IFEQ      0
293400050525     C     TASTBL        LOOKUP    TBN                                    05
293500050525     C   05              GOTO      ENDVR
293600050525     C                   ENDIF
293700980505     C**
293800941107     C                   GOTO      DOPAC2
293900941107     C     DOPAC1        TAG
294000941107     C** NON ESISTE TARIFFA AXC DEL CLIENTE
294100941121     C*-----------------------------------------------------------------
294200980505     C* SE USATA SI TRATTA DI TARIFFA DI CARTELLO
294300050525     C***                MOVEL     'S'           TASK88
294400941112     C     IASLIT        CABEQ     0             ENDVR
294500050525     c                   eval      task88='S'
294600020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
294700020226     C     KTPTC         CHAIN     TITPT01L                           28
294800020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
294900020226     C     *IN28         IFEQ      *ON
295000020226     C     TPTATB        OREQ      'A'
295100020226     C                   GOTO      ENDVR
295200020226     C                   ENDIF
295300941107     C*-----------------
295400941107     C     DOPAC2        TAG
295500980504     C** SE NON SERVE --> ESCO DAL CALCOLO
295600980504     C** TIPO APPLICAZIONE
295700980504     C** P - SOLO PER FRANCHI
295800980504     C** A - SOLO PER ASSEGNATI
295900980504    1C     TPTAPL        IFNE      *BLANKS
296000980504     C     IASLIT        ANDEQ     0
296100980504    2C     TIPBOL        IFEQ      'F'
296200980504    3C     TPTAPL        IFEQ      'A'
296300980504     C                   GOTO      ENDVR
296400980504    3C                   ENDIF
296500980504   X2C                   ELSE
296600980504    3C     TPTAPL        IFEQ      'P'
296700980504     C                   GOTO      ENDVR
296800980504    3C                   ENDIF
296900980504    2C                   END
297000980504    1C                   END
297100980504     C**
297200941112     C     *LIKE         DEFINE    TASIAS        VALVR
297300941112     C     *LIKE         DEFINE    TASIAS        ASCPES
297400941112     C     *LIKE         DEFINE    TASIAS        ASCPE2
297500941112     C                   Z-ADD     0             VALVR
297600980526    1C     IASLIT        IFEQ      0
297700980526    2C     TPTFVM        IFEQ      '3'
297800020826     C     USAPKF        MULT      TPTVLM        VALVR
297900980526    2C                   END
298000980526    2C     TPTFVM        IFEQ      '1'
298100941107     C     TASNCL        MULT      TPTVLM        VALVR
298200980526    2C                   END
298300980526    2C     TPTFVM        IFEQ      '2'
298400941107     C                   Z-ADD     TPTVLM        VALVR
298500980526    2C                   END
298600980526   X1C                   ELSE
298700941112     C                   Z-ADD     IASLIT        VALVR
298800980526    1C                   END
298900011119     C***** OBSOLETO NON RESTITUISCO + IL VALORE IN LIRE MA NELLA DIVISA DELLA TARIFFA
299000011129     C***** PERO' ARROTONDO AL DECIMALE DELLA TARIFFA
299100011129     C                   CLEAR                   YEUR
299200011129     C                   MOVEL     §TADIV        YECDVI
299300011129     C                   MOVEL     §TADIV        YECDVO
299400011129     C                   Z-ADD     VALVR         YECIMI
299500011129     C                   CALL      'YEURCO'
299600011129     C                   PARM                    YEUR
299700011129     C*
299800011129     C     YECESI        IFEQ      ' '
299900011129     C                   Z-ADD     YECIMO        VALVR
300000011129     C                   END
300100011129     C* RESTITUISCO IN OUTPUT L'IMPORTO DA ASSICURARE
300200011129     C                   Z-ADD     VALVR         TASIAL
300300941107     C** VALORE SPEDIZIONE PER CALCOLO ASSICURAZIONE
300400980312     C**
300500980526     C** CALCOLO SOLO SE NON HO IMPOSTATO IL FLAG
300600980526    1C     PARCA         IFEQ      ' '
300700980505     C**
300800980505     C** SE C'E' GIA' LA VARIA R NON LA RICALCOLO
300900980505     C                   Z-ADD     1             A
301000980505     C     §$2VRR        LOOKUP    SV(A)                                  05
301100980526    2C     *IN05         IFEQ      *OFF
301200980312     C                   MOVEL     TPTTPG        WTPG
301300980312     C                   MOVEL     TPTRPV        WRPV
301400980313     C                   CLEAR                   WDET
301500980313     C                   Z-ADD     TPTARL        WARL
301600980313     C                   Z-ADD     TPTARF        WARF
301700980313     C                   Z-ADD     TPTARO        WARO
301800980312     C                   EXSR      CALCOL
301900980312     C**
302000980312     C** A VALORE
302100980526    3C     TPTTPG        IFEQ      'V'
302200980312     C                   Z-ADD     VALVR         ISOPES
302300980526    3C                   END
302400980312     C     *LIKE         DEFINE    TASIAS        TPDPES
302500980504     C**
302600980504     C**CALCOLO IMPORTO ASSICURAZIONE PER CONT
302700980504     C                   Z-ADD     ISOPES        TPDPES
302800941107     C                   EXSR      CALACD
302900980525     C                   Z-ADD     VARIA         WVARIR
303000980526    2C                   ENDIF
303100980526    1C                   ENDIF
303200980504     C     ENDVR         ENDSR
303300060320     C******************************************************
303400060320     C** CALCOLA VARIA d  --- A C BASE
303500060320     C******************************************************
303600060321     c     CALVACB       BEGSR
303700060320     c* Il falg task88 cambia significato:indica se bolla con mandato o no
303800060320     c* Imposto task88=N se mandato
303900060321     c* Lascio  task88=X per bolle senza mandato e senza importo
304000060321
304100060321      **----------------------------------------
304200060321     c                   movel     'd'           Fiso
304300060321     c                   movel     'd'           WFiso
304400060321      ** Flag A C BASE  = d
304500060321     c                   z-add     1             a
304600060321     c     Wfiso         Lookup    Cp(a)                                  05
304700060321     c  N05              Goto      EndVacb
304800060321      * aggancio la ds della tabella 1P
304900060321     c     A             Occur     DS1P
305000060321      **----------------------------------------
305100060321     c     Kisot         Chain     TITPT01L
305200060321      ** esiste Tariffa A C base
305300060321    1c                   If        %found(titpt01l)
305400060321      **
305500060321      ** Controllo il tipo applicazione della tariffa A C Base
305600060321      **
305700060321    2c                   If        (tptapl = 'A' and tipbol = 'A') or
305800060321     c                             (tptapl = 'P' and tipbol = 'F') or
305900060321     c                              tptapl = ' '
306000060321      *
306100060321     c                   eval      task88='N'
306200060321      ** Anche se lo considero con mandato,  tipo
306300060321      **  bolla senza assicurazione, non calcolo varia d
306400060321     c     TAStbl        Lookup    Tbn                                    05
306500060321     c   05              Goto      ENDVacb
306600060321
306700060321      * calcolo valore assicurato
306800060321
306900060321     c     *Like         Define    TASias        VALVacb
307000060321
307100060321     c                   clear                   VALVacb
307200060321
307300060321      * Peso
307400060321    3c                   If        TPTfvm = '3'
307500060321     c     TPTvlm        Mult      USAPkf        Valvacb
307600060321    3c                   Endif
307700060321      * Colli
307800060321    3c                   If        TPTfvm = '1'
307900060321     c     TPTvlm        Mult      TASncl        Valvacb
308000060321    3c                   Endif
308100060321      * Spedizione
308200060321    3c                   If        TPTfvm = '2'
308300060321     c                   Z-add     TPTvlm        Valvacb
308400060321    3c                   Endif
308500060321      *
308600060321      * Arrotondo al decimale della tariffa
308700060321     c                   Clear                   YEUR
308800060321     c                   Movel     §TAdiv        YECdvi
308900060321     c                   Movel     §TAdiv        YECdvo
309000060321     c                   Z-add     VALvacb       YECimi
309100060321     c                   call      'YEURCO'
309200060321     c                   Parm                    YEUR
309300060321      *
309400060321     c                   If        YECesi = ' '
309500060321     c                   Z-add     YECimo        VALvacb
309600060321     c                   Endif
309700060321
309800060321      ** Calcolo solo se non ho impostato il flag
309900060321    3c                   IF        Parca = ' '
310000060321
310100060321      ** Se c'è' già la varia non la ricalcolo
310200060321     c                   Z-add     1             a
310300060321     c     §$2acb        Lookup    sv(a)                                  05
310400060321    4c     *IN05         Ifeq      *off
310500060321     c                   movel     TPTtpg        Wtpg
310600060321     c                   Movel     TPTrpv        Wrpv
310700060321     c                   clear                   Wdet
310800060321     c                   Z-add     TPTarl        Warl
310900060321     c                   Z-add     TPTarf        Warf
311000060321     c                   Z-add     TPTaro        Waro
311100060321     c                   Exsr      Calcol
311200060321
311300060321      ** A Valore
311400060321    5c                   IF        tpttpg =  'V'
311500060321     c                   z-add     valvacb       ISOpes
311600060321    5c                   Endif
311700060321
311800060321     c                   Z-add     ISOpes        TPDpes
311900060321     c                   Exsr      Calacd
312000060321     c                   Z-add     varia         Wvariacb
312100060321    4c                   endif
312200060321
312300060321    3c                   endif
312400060321
312500060321    2c                   endif
312600060321    1c                   Endif
312700060321
312800060321     c     EndVacb       ENDSR
312900941112     C******************************************************
313000980312     C** CALCOLA TARIFFA PARTICOLARE
313100941112     C******************************************************
313200941107     C     CALACD        BEGSR
313300990923     C* CAMPO VARIA CON  DECIMALI
313400990923     C                   Z-ADD     0             VARIA            15 3
313500990923     C* CAMPO VARIA SENZA DECIMALI ALTRIMENTI NON FUNZIONA L'ARROTONDAMENTO
313600990923     C                   Z-ADD     0             VARIAD           15 0
313700990923     C*
313800941107     C                   DO        100           A
313900941122     C     A             OCCUR     ACDS
314000941107     C                   CLEAR                   ACDS
314100941107     C                   END
314200941112     C** PULIZIA DS TARIFFA
314300020412     C*
314400020412     C* SE ESISTE ALMENO UNA RIGA DI DETTAGLIO E NON RIESCO AD APPLICAR
314500020412     C*  QUESTA TAR.PARTICOLARE --> MANCA TARIFFA
314600980605     C* BOLLA IMPORT / ESXPORT
314700980605     C     WBOEST        IFEQ      'E'
314800020226     C     §1PCTE        IFNE      *BLANKS
314900020226     C                   MOVEL     §1PCTE        ISOCTS
315000941112     C                   GOTO      POIAC
315100941112     C                   END
315200941112     C                   ELSE
315300020226     C     §1PCTS        IFNE      *BLANKS
315400020226     C                   MOVEL     §1PCTS        ISOCTS
315500941112     C                   GOTO      POIAC
315600941112     C                   END
315700941112     C                   END
315800941112     C** CODICI TASSAZIONE FISSI DA TABELLA TARIFFE PARTICOLARI
315900941110     C                   MOVEL     TASCTS        ISOCTS            2
316000060414     c
316100060414      * Se assegnato e tariffa FEDEX metto in ISOCTS  quello mittente
316200050124     c                   If        TIPBOL = 'A' and WBOFED = 'S'
316300050124     c                   movel     tasmct        isocts
316400050124     c                   endif
316500060414     c* Se devo utilizzare quello del mittente per via della
316600060414     c*  reversibilità --> lo uso
316700060414     c                   if        applicarevers='S'
316800060414     c                   movel     tasmct        isocts
316900060414     c                   endif
317000050124      *
317100990721     C     KISOD         SETLL     TITPD02L                               28
317200941107     C   28              GOTO      DOPAC3
317300941107     C                   Z-ADD     1             A
317400941107     C     ISOCTS        LOOKUP    CTS(A)                                 05
317500941107     C   05A             OCCUR     DSCT
317600941107     C   05              MOVEL     §CTRAP        ISOCTS
317700941107     C  N05              MOVEL     *BLANKS       ISOCTS
317800941107     C** REGIONE
317900990721     C     KISOD         SETLL     TITPD02L                               28
318000941107     C   28              GOTO      DOPAC3
318100980605     C     WBOEST        IFEQ      'E'
318200941112     C                   MOVEL     'EE'          ISOCTS
318300941112     C                   ELSE
318400941112     C                   MOVEL     'IT'          ISOCTS
318500941112     C                   END
318600941112     C     POIAC         TAG
318700990721     C     KISOD         SETLL     TITPD02L                               28
318800020412     C**N28                GOTO ENDACD
318900020412     C** N 28 --> MANCA TARIFFA
319000020412     C     *IN28         IFEQ      *OFF
319100020412     C* SE NON È PRESENTE NESSUNA RIGA DI DETTAGLIO, NON DO
319200020412     C*  MANCA TARIFFA: SIGNIFICA CHE È INSERITA UNA TASSAZIONE =0
319300020412     C     KISOD2        SETLL     TITPD02L                               28
319400020412     C  N28              GOTO      ENDACD
319500020412     C*
319600020412     C                   SETON                                        94
319700020412     C                   MOVEL     MSG(17)       TASMSG
319800020412     C                   GOTO      FINE
319900020412     C                   ENDIF
320000020412     C**
320100020412     C** NON TROVATA TARIFFA
320200941107     C     DOPAC3        TAG
320300941107     C                   DO        *HIVAL        A
320400990721     C     KISOD         READE     TITPD02L                               28
320500941107     C   28              GOTO      DOPAC4
320600941107     C     A             OCCUR     ACDS
320700990923     C* VERIFICO SE STO CARICANDO LA TARIFFA DI CARTELLO
320800990923     C     TPTKSC        IFEQ      CARKSC
320900990923     C     TPTCTR        ANDEQ     CARCTR
321000990923     C     TPTPRG        ANDEQ     CARPRG
321100990923     C* E LA DIVISA DELLA TARIFFA DI CARTELLO E' DIVERSA DALLA DIVISA DELLA
321200990923     C* TARIFFA DEL CLIENTE  CONVERTO I VALORI O SCAGLIONI NELLA DIVISA
321300990923     C* DELLA TARIFFA DEL CLIENTE
321400990923     C     §TADIV        ANDNE     DIVCAR
321500990923     C*
321600990923     C                   EXSR      CONTPD
321700990923     C*
321800990923     C                   ENDIF
321900990923     C*
322000941112     C                   Z-ADD     TPDSGL        ASGL
322100941112     C                   Z-ADD     TPDITR        AITR
322200941112     C                   Z-ADD     TPDMIN        AMIN
322300941112     C                   Z-ADD     TPDMAX        AMAX
322400941112     C                   MOVEL     TPDAIN        AAIN
322500941107     C                   END
322600941107     C     DOPAC4        TAG
322700941107     C                   DO        100           A
322800941107     C     A             OCCUR     ACDS
322900941112     C     ASGL          IFGE      TPDPES
323000941107     C                   GOTO      DOPAC5
323100941107     C                   END
323200941107     C                   END
323300941123     C     ASGL          IFLT      TPDPES
323400941122     C                   SETON                                        94
323500980527     C                   MOVEL     MSG(7)        TASMSG
323600980527     C                   GOTO      FINE
323700980527     C                   ENDIF
323800941122     C** NON TROVATO SCAGLIONE ESATTO
323900941107     C     DOPAC5        TAG
324000941112     C*-----------
324100941122     C     ASGL          IFLE      MINTAS
324200941122     C                   Z-ADD     MINTAS        TPDPES
324300941112     C                   END
324400941122     C     TPDPES        IFLE      MINTAS
324500941122     C                   Z-ADD     MINTAS        TPDPES
324600941122     C                   END
324700991013     C                   Z-ADD     0             TPDQLI           18 6
324800980429     C* DIVIDO PER 100 TARIFFA 0 PER AVERE QUINTALI
324900980429     C*                TARIFFA 4 PERCHE' E' TARIFFA %
325000980429     C     WTPG          IFEQ      '0'
325100980429     C     WTPG          OREQ      '4'
325200941112     C     TPDPES        DIV       100           TPDQLI
325300941112     C                   ELSE
325400941112     C                   Z-ADD     TPDPES        TPDQLI
325500941112     C                   END
325600980429     C** PESO TASSABILE
325700060906      * se trovato dettaglio tariffario della tariffa "f" vado a fine routine in quanto il calcolo
325800060906      * viene eseguito nella routine FUEL
325900060906     c                   If        wfiso = 'f'
326000060906     c                   goto      endacd
326100060906     c                   endif
326200060906
326300990923     C   12AITR          MULT(H)   TPDQLI        VARIA
326400990923     C  N12AITR          MULT(H)   TPDQLI        VARIAD
326500941112     C                   MOVEL     AAIN          FLGISO
326600980429     C*
326700980430     C     VARIA         IFGT      0
326800991012     C     *IN12         ANDEQ     *ON
326900991012     C     VARIAD        ORGT      0
327000991012     C     *IN12         ANDEQ     *OFF
327100991012     C*
327200980430     C     WTPG          IFEQ      'V'
327300980430     C     WTPG          OREQ      'T'
327400990923     C   12VARIA         DIV(H)    100           VARIA
327500990923     C  N12VARIAD        DIV(H)    100           VARIAD
327600980430     C                   ENDIF
327700980430     C                   ENDIF
327800980429     C**
327900991012     C* SPOSTO L'IMPORTO DEL CAMPO VARIAD NEL CAMPO VARIA SOLO SE E' STATO USATO
328000991012     C  N12              Z-ADD     VARIAD        VARIA
328100991012     C*
328200991012     C** MINIMO
328300941112     C     AMIN          IFGT      VARIA
328400991012     C                   Z-ADD     AMIN          VARIA
328500941107     C                   END
328600991012     C** MASSIMO
328700941115     C     AMAX          IFGT      0
328800941112     C     AMAX          IFLT      VARIA
328900991012     C                   Z-ADD     AMAX          VARIA
329000941112     C                   END
329100941115     C                   END
329200000103     C** SE VARIA CON ESENZIONE IVA DEVE AVERE SOLO 2 DCEIMALI
329300000103     C* CERCO LA SIGLA VARIA CORRISPONDENTE
329400000103     C                   MOVEL     TPTFTC        WFTC              1
329500000103     C                   Z-ADD     1             A
329600000103     C     WFTC          LOOKUP    CP(A)                                  10
329700000103     C     *IN10         IFEQ      *ON
329800000103     C     A             OCCUR     DS1P
329900000103     C     §1PVAR        LOOKUP    VES                                    10
330000000103     C     *IN10         IFEQ      *ON
330100000103     C                   MOVE      VARIA         W001A             1
330200000103     C     W001A         IFNE      '0'
330300000103     C     VARIA         ADD       0,001         COMOD2           15 2
330400000103     C                   Z-ADD     COMOD2        VARIA
330500000103     C                   ENDIF
330600000103     C                   ENDIF
330700000103     C                   ENDIF
330800000103     C**
330900941107     C     ENDACD        TAG
331000990923     C*
331100941107     C                   ENDSR
331200941107     C/SPACE
331300941112     C******************************************************
331400980424     C** CALCOLO TARIFFE  PARTICOLARI
331500970521     C******************************************************
331600980428     C     CALCP         BEGSR
331700990721     C** CERO LA TARIFFA PARTICOLARE IN TITPT O CARTELLO
331800980430     C                   MOVEL     TASTC         FISO
331900980506     C                   MOVEL     'S'           WCAR
332000110908      ** se sto calcolando la varia "Q" = Centro storico o ZTL
332100110908      ** non devo cercare la tariffa nella Cartello in mancanza
332200110908      ** di quella del cliente
332300111102      ** E' stata aggiuna una tabella che pilota il recupero della
332400111102      ** tariffa di cartello se la filiale del cliente di tassazione
332500111102      ** è presente nella tabella ztl e la data spedizione è maggiore
332600111102      ** uguale alla data attivazione della tassazione
332700111102     c                   clear                   indztl            3 0
332800110908     c                   If        tastc = 'Q'
332900111102     c                   movel     'N'           WCAR
333000111102      * cerco la filiale del cliente nella tabella ZTL
333100111102     c                   eval      indztl = %lookup(kscfil:sfilztl)
333200111102      * se indice =  a zero cerco con la filiale 999
333300111102    2c                   if        indztl = *zeros
333400111102     c                   eval      indztl = %lookup(999:sfilztl)
333500111102    2c                   endif
333600111102      * imposto il flag della ricerca tariffa di cartello = 'S'  se filiale cliente
333700111102      * abilitata e la data spedizione > = data attivazione tassazione
333800111102    2c                   If        indztl > *zeros and
333900111102    3c                             tasdsp >= sdtaztl(indztl)
334000111102     c                   movel     'S'           WCAR
334100111102    3c                   endif
334200110908     c                   endif
334300110908      *
334400980428     C                   EXSR      CERTPT
334500980428     C** SOLO SE L'HO TROVATA
334600980428    1C     WEND          IFEQ      ' '
334700970521     C                   Z-ADD     ISOPES        TPDPES
334800970521     C                   EXSR      CALACD
334900970521     C                   Z-ADD     VARIA         TASCP
335000980428    1C                   ENDIF
335100970521     C*
335200970521     C                   ENDSR
335300001205     C/SPACE
335400001205     C******************************************************
335500001205     C** CALCOLO ADDIZIONALE DI GESTIONE + ISTAT
335600001205     C******************************************************
335700001205     C     ADGIST        BEGSR
335800130124
335900130124      * verifico se devo calcolare solo l'ISTAT
336000130124     c                   If        tassva = §$2VRV
336100130124     c                   goto      Tag_istat
336200130124     c                   endif
336300001205     C**
336400001205     C*
336500001205     C****************
336600041129     C** ADDIZIONALE DI GESTIONE  anni precedenti       varia "Z"
336700001205     C****************
336800001205     C*
336900031121     C     *LIKE         DEFINE    TASVA1        TASADG
337000001205     C     TAMPAG        IFNE      0
337100001205     C                   Z-ADD     1             A
337200001205     C     §$2VRZ        LOOKUP    SV(A)                                  05
337300001205     C  N05              DO
337400001205     C                   Z-ADD     0             TASADG
337500001205     C* TOTALE IMPONIBILE
337600001205     C                   Z-ADD     O17IMV        IMPADG           15 3
337700001205     C**
337800001205     C                   Z-ADD     0             COMADG           15 3
337900001205     C     IMPADG        MULT      TAMPAG        COMADG
338000001205     C     COMADG        IFGT      0
338100001205     C     COMADG        DIV(H)    100           TASADG
338200001205     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
338300001205     C     *IN12         IFEQ      '0'
338400001205     C     TASADG        ADD       0,5           CAMPOW           11 0
338500001205     C                   Z-ADD     CAMPOW        TASADG
338600001205     C                   END
338700001205     C                   END
338800001205     C*
338900040128     c     tasadg        ifgt      0
339000040128      *
339100001205     C                   Z-ADD     1             A
339200001205     C     ' '           LOOKUP    SV(A)                                  96
339300001205     C   96              MOVEL     §$2VRZ        SV(A)
339400001205     C   96              Z-ADD     TASADG        VA(A)
339500001205     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
339600001205     C  N96              ADD       TASADG        TASPOR
339700001205     C*
339800001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
339900001205     C*
340000001205     C                   CLEAR                   DSLV16
340100001205     C                   MOVEL     'T'           D17FIM
340200001205     C                   Z-ADD     TASPOR        D17POR
340300001205     C                   CALL      'FNLV16R'
340400001205     C                   PARM                    DSLV16
340500001205     C                   PARM                    DTASV
340600040128      *
340700040128     c                   endif
340800001205     C*
340900001205     C                   END
341000001205     C                   END
341100041129     C*
341200041129     C****************
341300041129     C** ADDIZIONALE DI GESTIONE    anno corrente       varia "b"
341400041129     C****************
341500041129     C*
341600041129     c                   clear                   tasadg
341700041129     c                   clear                   impadg
341800041129     c                   clear                   comadg
341900041129
342000041129     C     TAMPPA        IFNE      0
342100041129     C                   Z-ADD     1             A
342200041129     C     §$2VAG        LOOKUP    SV(A)                                  05
342300041129     C  N05              DO
342400041129     C                   Z-ADD     0             TASADG
342500041129     C* TOTALE IMPONIBILE
342600041129     C                   Z-ADD     O17IMV        IMPADG
342700041129     C**
342800041129     C                   Z-ADD     0             COMADG
342900041129     C     IMPADG        MULT      TAMPPA        COMADG
343000041129     C     COMADG        IFGT      0
343100041129     C     COMADG        DIV(H)    100           TASADG
343200041129     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
343300041129     C     *IN12         IFEQ      '0'
343400041129     C     TASADG        ADD       0,5           CAMPOW           11 0
343500041129     C                   Z-ADD     CAMPOW        TASADG
343600041129     C                   END
343700041129     C                   END
343800041129     C*
343900041129     c     tasadg        ifgt      0
344000041129      *
344100041129     C                   Z-ADD     1             A
344200041129     C     ' '           LOOKUP    SV(A)                                  96
344300041129     C   96              MOVEL     §$2VAG        SV(A)
344400041129     C   96              Z-ADD     TASADG        VA(A)
344500041129     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
344600041129     C  N96              ADD       TASADG        TASPOR
344700041129     C*
344800041129     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
344900041129     C*
345000041129     C                   CLEAR                   DSLV16
345100041129     C                   MOVEL     'T'           D17FIM
345200041129     C                   Z-ADD     TASPOR        D17POR
345300041129     C                   CALL      'FNLV16R'
345400041129     C                   PARM                    DSLV16
345500041129     C                   PARM                    DTASV
345600041129      *
345700041129     c                   endif
345800041129     C*
345900041129     C                   END
346000041129     C                   END
346100001205     C**
346200001205     C***************
346300001205     C** ISTAT
346400001205     C***************
346500001205     C*
346600130124     c     Tag_istat     tag
346700130124
346800130124      * Se al programma vengono passate le date aggiuntive per la ritassazione
346900130124      * imposto come data di spedizione quella passata se maggiore di zero
347000130124     c                   clear                   Sav_dsp
347100130124     c                   If        Ritass = 'S' and tasdtist > 0
347200130124     c                   eval      Sav_dsp = Tasdsp
347300130124     c                   movel     tasdtist      tasdsp
347400130124     c                   endif
347500130124      *
347600031121     C     *LIKE         DEFINE    TASVA1        TASIAC
347700001205     C                   Z-ADD     1             A
347800001205     C     §$2VRV        LOOKUP    SV(A)                                  05
347900101014    1C  N05TAMrct        IFGT      0
348000001205     C                   Z-ADD     0             TASIAC
348100001205     C*
348200001205     C* CALCOLO ISTAT SU TOTALE IMPONIBILE
348300001205     C*
348400001205     C                   Z-ADD     O17IMV        IMPIAC           15 3
348500101014     C                   Z-ADD     0             PUNIAC            7 4
348600101014
348700101014      * richiamo nuovo pgm per calcolo % istat da aggiungere all'imponibile
348800101014     c
348900101014     c                   clear                   trulistds
349000101014     c                   eval      IISTsca = tamrct
349100101014     c                   eval      IISTdsp = tasdsp
349200101014     c                   call      'TRULISTR'
349300101014     c                   parm                    trulistds
349400101014     c                   IF        OISTerr = *blanks
349500101014     c                   eval      puniac = OISTiac
349600101014     c                   ENDIF
349700101014     c
349800050629     C                   Z-ADD     0             PERIAC           15 6
349900050629     C                   Z-ADD     0             PERIAX           15 4
350000001205     C     PUNIAC        MULT      TAMPRI        PERIAX
350100001205    3C     PERIAX        IFNE      0
350200031121     C     PERIAX        DIV       100           PERIAC
350300001205    3C                   END
350400001205     C** PERCENTUALE DI ISTAT DA APPLICARE
350500001205     C** APPLICO I PUNTI X TRIMESTRE SOLO SE
350600001205     C** LA DATA DI SPEDIZIONE E'
350700001205     C** MAGGIORE O UGUALE ALLA DATA DI SCATTO
350800001205     C** DEI PUNTI NEL TRIMESTRE
350900001205    3C     PERIAC        IFNE      0
351000031121     C                   DIV       100           PERIAC
351100001205     C     IMPIAC        MULT(H)   PERIAC        TASIAC
351200001205    3C                   END
351300001205     C*
351400001205    3C     TASIAC        IFGT      0
351500001205     C*  SENZA DECIMALI
351600001205    4C     *IN12         IFEQ      *OFF
351700001205     C     TASIAC        ADD       0,5           CAMPOW           11 0
351800001205     C                   Z-ADD     CAMPOW        TASIAC
351900001205    4C                   END
352000001205     C*
352100001205     C                   Z-ADD     1             A
352200001205     C     ' '           LOOKUP    SV(A)                                  96
352300001205     C   96              MOVEL     §$2VRV        SV(A)
352400001205     C   96              Z-ADD     TASIAC        VA(A)
352500001205     C  N96              ADD       TASIAC        TASPOR
352600001205     C*
352700001205     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
352800001205     C*
352900001205     C                   CLEAR                   DSLV16
353000001205     C                   MOVEL     'T'           D17FIM
353100001205     C                   Z-ADD     TASPOR        D17POR
353200001205     C                   CALL      'FNLV16R'
353300001205     C                   PARM                    DSLV16
353400001205     C                   PARM                    DTASV
353500001205     C*
353600001205    3C                   END
353700001205    1C                   ENDIF
353800130124      * se impostato il campo del salvataggio della data spedizione
353900130124     c                   if        Sav_dsp <> *zeros
354000130124     c                   eval      tasdsp = Sav_dsp
354100130124     c                   clear                   Sav_dsp
354200130124     c                   endif
354300001205     C*
354400001205     C                   ENDSR
354500030922     C*******************************************************
354600030922     C** CALCOLO DIRITTO DI FATTURAZIONE  + TOTALE IMPONIBILE
354700030922     C******************************************************
354800030922     C     DIRFAT        BEGSR
354900030922     C**
355000030922      * prima verifico se esiste già
355100030922     C                   Z-ADD     1             A
355200030922     C     §$2VRD        LOOKUP    SV(A)                                  05
355300030922     C* SIGLA VARIA NON PRESENTE --> CALCOLO
355400030922     C     *IN05         IFEQ      *OFF
355500030922     C                   Z-ADD     1             A
355600030922     C     ' '           LOOKUP    SV(A)                                  96
355700030922     C   96              MOVEL     §$2VRD        SV(A)
355800030922     C   96              Z-ADD     TASIVDF       VA(A)
355900030922     C  N96              ADD       TASIVDF       TASPOR
356000030922     C*
356100030922     C* RICALCOLO L'IMPONIBILE SOMMANDO IL DIRITTO DI FATTURAZIONE
356200030922     C*
356300030922     C                   CLEAR                   DSLV16
356400030922     C                   MOVEL     'T'           D17FIM
356500030922     C                   Z-ADD     TASPOR        D17POR
356600030922     C                   CALL      'FNLV16R'
356700030922     C                   PARM                    DSLV16
356800030922     C                   PARM                    DTASV
356900030924     C*
357000030924     C                   Z-ADD     O17IMV        TASIMV
357100030924     C*
357200030922     C*
357300030922     C                   ENDIF
357400030922     C*
357500030922     C                   ENDSR
357600980428     C******************************************************
357700980428     C* CERCO TARIFFA PARTICOLARE
357800980428     C******************************************************
357900980428     C     CERTPT        BEGSR
358000980428     C                   CLEAR                   TASCP
358100980428     C                   CLEAR                   WEND
358200980428     C                   MOVEL     *BLANKS       FLGISO            1            FLAG.APPLICAZI.
358300990722     C                   MOVEL     FISO          WFISO
358400980428     C                   Z-ADD     1             A
358500990722     C     WFISO         LOOKUP    CP(A)                                  05
358600980428     C**
358700980428    1C     *IN05         IFEQ      *ON
358800020226     C     A             OCCUR     DS1P
358900980428     C*-----------------------------------------
359000990721     C     KISOT         CHAIN     TITPT01L                           28
359100980528     C** ESISTE TARIFFA DEL CLIENTE
359200980528     C     *IN28         IFEQ      *OFF
359300980528     C     TPTATB        ANDEQ     *BLANKS
359400980428     C                   GOTO      DOPIS2
359500980528     C                   ENDIF
359600980428     C*-----------------------------------------
359700980428     C** MEMORIZZO DATI DA TARIFFA DI CARTELLO
359800980506     C**  SOLO SE HO DETTO CHE LA POSSO USARE E SE C'E'
359900980428     C*-----------------------------------------
360000020226      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
360100020226     C     WCAR          IFEQ      'S'
360200020226     C     KTPTC         CHAIN     TITPT01L                           28
360300020226      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
360400020226     C     *IN28         IFEQ      *ON
360500020226     C     TPTATB        OREQ      'A'
360600020226     C                   MOVEL     'S'           WEND
360700020226     C                   GOTO      DOPIS3
360800020226     C                   ENDIF
360900020226      * E SE NON DEVO CARICARE LA CARTELLO ERRORE NON TROVATA TARIFFA PARTICOLARE
361000020226     C                   ELSE
361100020226     C                   MOVEL     'S'           WEND
361200020226     C                   GOTO      DOPIS3
361300020226     C                   ENDIF
361400980430     C     DOPIS2        TAG
361500980430     C* ARROTONDAMENTI E RPV
361600980430     C                   MOVEL     TPTTPG        WTPG
361700090310      * in caso di pod-immage varia "a" imposto come tipo pagamento a colli
361800090310
361900090310     c                   if        fiso = §$2POD
362000090310     C                   MOVEL     '1'           WTPG
362100090310     c                   endif
362200090310
362300980430     C                   MOVEL     TPTRPV        WRPV
362400980430     C                   CLEAR                   WDET
362500980430     C                   Z-ADD     TPTARL        WARL
362600980430     C                   Z-ADD     TPTARF        WARF
362700980430     C                   Z-ADD     TPTARO        WARO
362800980430     C                   EXSR      CALCOL
362900980428     C** NON TROVATA TARIFFA PARTICOLARE
363000980428   X1C                   ELSE
363100980428     C                   MOVEL     'S'           WEND              1
363200980428    1C                   ENDIF
363300020226     C     DOPIS3        ENDSR
363400980312     C******************************************************
363500980312     C** CALCOLO IL VALORE DA MOLTIPLICARE/RPV/ARROTONDAMENTI
363600980312     C******************************************************
363700980312     C     CALCOL        BEGSR
363800980313     C     *LIKE         DEFINE    TAMARL        ARR
363900980313     C     *LIKE         DEFINE    TASIAS        PESTAS
364000980313     C     *LIKE         DEFINE    TASIAS        ISOPES
364100980312     C     *LIKE         DEFINE    TASIAS        ISOPE2
364200980312     C     *LIKE         DEFINE    TPTRPV        WRPV
364300980313     C     *LIKE         DEFINE    TPTARL        WARL
364400980313     C     *LIKE         DEFINE    TPTARF        WARF
364500980313     C     *LIKE         DEFINE    TPTARO        WARO
364600980312     C                   Z-ADD     0             ISOPES
364700980316     C                   SELECT
364800030203
364900030204     c                   When      wFiso = '='
365000030203     c                   Z-Add     TasBan        Isopes
365100030203
365200980429     C     WTPG          WHENEQ    '0'
365300980429     C     WTPG          OREQ      '3'                                          KG
365400020826     C                   Z-ADD     USAPKF        ISOPES
365500980316     C*
365600980429     C     WTPG          WHENEQ    '1'                                          COLLI
365700980312     C                   Z-ADD     TASNCL        ISOPES
365800980316     C*
365900980429     C     WTPG          WHENEQ    '2'                                          SPEDIZ
366000980312     C                   Z-ADD     1             ISOPES
366100980316     C*
366200980429     C     WTPG          WHENEQ    '4'                                          VALORE
366300980429     C     WTPG          OREQ      '5'                                          Q.TA'
366400980312     C                   Z-ADD     TASQFT        ISOPES
366500980429     C*
366600980429     C     WTPG          WHENEQ    'T'                                          TRASPORTO
366700980429     C                   Z-ADD     TASPOR        ISOPES
366800980429     C                   ADD       TASINL        ISOPES
366900980429     C                   EXSR      CERISO
367000980429     C                   ADD       VAISO         ISOPES
367100060414     C                   ADD       VAtsp         ISOPES
367200980316     C                   ENDSL
367300980312     C** MINIMO TASSABILE STD
367400980312     C                   Z-ADD     0             MINTAS
367500980312     C**
367600980312     C                   Z-ADD     1             A
367700980312     C     WTPG          LOOKUP    CTR(A)                                 05
367800980312     C   05A             OCCUR     DSTR
367900980312     C   05              Z-ADD     §TRATA        MINTAS
368000980312     C**
368100980312     C** SOLO PER TAD (PER TPD NON MI SERVE)
368200980428    1C     WDET          IFEQ      'TAD'
368300980312     C                   CLEAR                   MINTAR
368400980312     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
368500980312     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
368600980312     C   05              Z-ADD     §TRATA        MINTAR
368700980428    2C     TAMATA        IFGT      0
368800980312     C                   Z-ADD     TAMATA        MINTAR
368900980428    2C                   ENDIF
369000980428    1C                   ENDIF
369100980312     C**
369200980428     C** RPV SOLO PER TARIFFA 0 E 3
369300980428    1C     §TRRPV        IFEQ      'S'
369400980428     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
369500980428    2C     WDET          IFEQ      'TAD'
369600980428     C     TASVLF        ANDGT     0
369700020218    3C                   DO        200           A
369800980428     C     A             OCCUR     TARDS
369900980428    4C     TSCG          IFGE      ISOPES
370000980428     C                   GOTO      POITAS
370100980428    4C                   END
370200980428    3C                   END
370300980428     C** RICERCO RPV CON PESO REALE
370400980428    3C     TSCG          IFLT      ISOPES
370500980428     C                   SETON                                        94
370600980527     C                   MOVEL     MSG(5)        TASMSG
370700151209     c                   eval      %subst(tasmsg: 37)= %trim(%editc(ISOPES:'3'))
370800980527     C                   GOTO      FINE
370900980428    3C                   END
371000980428     C     POITAS        TAG
371100980428     C                   Z-ADD     TRPV          WRPV
371200040603     C                   Z-ADD     TRPV          rappv
371300980428    2C                   ENDIF
371400980428     C**
371500980428    2C     TASVLF        IFNE      0
371600980428     C     WRPV          ANDNE     0
371700980428     C                   Z-ADD     0             COMPRV            9 3
371800980428     C     TASVLF        MULT      WRPV          COMPRV
371900980428     C     COMPRV        MULT      100           ISOPE2
372000980428    3C     ISOPE2        IFGT      ISOPES
372100980428     C                   Z-ADD     ISOPE2        ISOPES
372200980428    3C                   ENDIF
372300980428    2C                   ENDIF
372400980428    1C                   ENDIF
372500990923     C**
372600990923     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
372700990923    0C     §TRARR        IFEQ      'S'
372800980313     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
372900980312     C     ISOPES        IFGE      MINTAS
373000980312     C                   Z-ADD     0             ARR
373100980313     C     ISOPES        IFGT      WARL
373200980313     C                   Z-ADD     WARO          ARR
373300980312     C                   ELSE
373400980313     C                   Z-ADD     WARF          ARR
373500980312     C                   END
373600980313     C**
373700980313     C                   Z-ADD     0             RESTO            15 5
373800980312     C                   Z-ADD     0             ISOPEX           13 0
373900980312     C     ARR           IFNE      0
374000980312     C     ISOPES        ANDNE     0
374100980312     C     ISOPES        DIV       ARR           ISOPEX
374200980312     C                   MVR                     RESTO
374300980312     C     RESTO         IFGT      0
374400980312     C     ISOPEX        MULT      ARR           ISOPES
374500980312     C                   ADD       ARR           ISOPES
374600980312     C                   ENDIF
374700980312     C                   ENDIF
374800980312     C                   ENDIF
374900980312     C**
375000980312     C***        GIUARR    TAG
375100980312    0C                   ENDIF
375200980312     C                   ENDSR
375300970521     C******************************************************
375400941107     C** CALCOLA DIRITTO ASSICURATIVO - VARIA E
375500941112     C******************************************************
375600941107     C     CALVE         BEGSR
375700941107     C                   MOVEL     'R'           FISO
375800980506     C                   MOVEL     'N'           WCAR
375900980430     C                   EXSR      CERTPT
376000980430     C     WEND          IFEQ      ' '
376100990922     C** TARIFFA A VALORE TOLGO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DI CONTO
376200990922     C*  E CALCOLO VALORE AL KG (FISSO) PER LA PARTE RESTANTE
376300980428    1C     TPTTPG        IFEQ      'V'
376400980312     C                   CLEAR                   ISOPES
376500990922     C* CONVERTO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DELLA TARIFFA
376600990922     C*
376700990922     C     *LIKE         DEFINE    §GELRP        WLRP
376800990922     C*
376900990922     C     §TADIV        IFNE      §GEDCN
377000990922     C                   CLEAR                   YEUR
377100990922     C                   MOVEL     §GEDCN        YECDVI
377200990922     C                   MOVEL     §TADIV        YECDVO
377300990922     C                   Z-ADD     §GELRP        YECIMI
377400991014     C   12              MOVE      '3'           YECDCO
377500991014     C  N12              MOVE      '0'           YECDCO
377600990922     C*
377700990922     C                   CALL      'YEURCO'
377800990922     C                   PARM                    YEUR
377900990922     C*
378000990922     C     YECESI        IFEQ      ' '
378100990922     C                   Z-ADD     YECIMO        WLRP
378200990922     C                   END
378300991112     C* SE TARIFFE UGUALI VALORIZZO WLRP
378400991112     C                   ELSE
378500991118     C                   Z-ADD     §GELRP        WLRP
378600990922     C                   ENDIF
378700990922     C*
378800990922    2C     TPTVLM        IFGT      WLRP
378900990922     C     TPTVLM        SUB       WLRP          ISOPES
379000020826     C     ISOPES        MULT      USAPKF        ISOPES
379100980312    2C                   ENDIF
379200980312    1C                   ENDIF
379300980430     C**
379400980312     C                   Z-ADD     ISOPES        TPDPES
379500941112     C                   EXSR      CALACD
379600941112     C                   Z-ADD     VARIA         DIRITT
379700941112     C**
379800941112     C     TPTAPL        IFNE      *BLANKS
379900941112     C     TIPBOL        IFEQ      'F'
380000941112     C     TPTAPL        IFEQ      'A'
380100941112     C                   Z-ADD     0             DIRITT
380200941112     C                   END
380300941112     C                   ELSE
380400941112     C     TPTAPL        IFEQ      'P'
380500941112     C                   Z-ADD     0             DIRITT
380600941112     C                   END
380700941112     C                   END
380800941112     C                   END
380900941112     C** TIPO APPLICAZIONE
381000941112     C** P - SOLO PER FRANCHI
381100941112     C** A - SOLO PER ASSEGNATI
381200980430     C                   ENDIF
381300941107     C                   ENDSR
381400941107     C/SPACE
381500941112     C******************************************************
381600941112     C** CALCOLA MAGGIORAZIONE TIPO SERVIZIO
381700941112     C******************************************************
381800941112     C     CALTS         BEGSR
381900980506     C                   MOVEL     'S'           WCAR              1
382000090924     c
382100090924     C* 24/09/09 --> Ora applichiamo sempre la maggiorazine espresso
382200090924     c
382300980506     C* SE TIPO SERVIZIO DELLA TARIFFA = A TIPO SERVIZIO BOLLA
382400980506     C*  NON PRENDO LA TARIFFA DI CARTELLO
382500090922     C***  TAMTSP        IFEQ      TASTSP
382600090922     C***                MOVEL     'N'           WCAR              1
382700090922     C***                ENDIF
382800980506     C* SIGLA VARIA CORRISPONDENTE
382900980506     C                   MOVEL     §5EFTC        FISO
383000980506     C                   EXSR      CERTPT
383100980506     C     WEND          IFEQ      ' '
383200980506     C**
383300980506     C** A TRASPORTO
383400980430     C     TPTTPG        IFEQ      'T'
383500980312     C                   Z-ADD     IMPON         ISOPES
383600941112     C                   END
383700980312     C**
383800980312     C                   Z-ADD     ISOPES        TPDPES
383900941112     C                   EXSR      CALACD
384000941112     C                   Z-ADD     VARIA         WTSP
384100980506     C                   ENDIF
384200941112     C**
384300941112     C                   ENDSR
384400941112     C******************************************************
384500020225     C** CERCA TARIFFA TNTAM PIU' RELATIVA CARTELLO
384600941112     C******************************************************
384700941107     C     CERTAM        BEGSR
384800020225      *
384900941111     C     *LIKE         DEFINE    TAMPRG        PRG2
385000941111     C     *LIKE         DEFINE    TAMKSC        KSC2
385100941111     C     *LIKE         DEFINE    TAMCTR        CTR2
385200061219      * se in tasflo c'è una data riferimento per il recupero della tariffa la imposto
385300061219      * solo temporaneamente al posto della data spedizione
385400061219     c                   clear                   Sav_dsp
385500070108     c                   if        §asdrt  > *zeros
385600061219     c                   eval      Sav_dsp = Tasdsp
385700070108     c***                eval      tasdsp = §asdrt
385800070108     c                   movel     §asdrt        tasdsp
385900061219     c                   endif
386000020225      *
386100941112     C                   SETOFF                                       180717
386200020227     C                   SETOFF                                       0890
386300941111     C                   Z-ADD     0             PRG2
386400941111     C                   Z-ADD     0             KSC2
386500941111     C                   Z-ADD     0             CTR2
386600941107     C                   Z-ADD     0             TAMKSC
386700941107     C                   Z-ADD     0             TAMPRG
386800941107     C                   Z-ADD     0             TAMCTR
386900980605     C     KTAM          SETLL     TNTAM000                               08
387000980605     C* MANCA TARIFFA CLIENTE
387100980605    1C     *IN08         IFEQ      *OFF
387200980605     C                   MOVEL     MSG(3)        TASMSG
387300980527     C                   SETON                                        94
387400160114     c                   eval      MancaTariffa = *on
387500980527     C                   GOTO      ENDTAM
387600980605    1C                   ENDIF
387700980605     C**
387800941107     C     SUTAM         TAG
387900980605     C**
388000941107     C     KTAM          READE     TNTAM000                               07
388100980605     c**
388200941107     C** SE HO LETTO UNA TARIFFA SCADUTA (17) E IL
388300980605     C** RECORD SUCCESSIVO MI ACCENDE FINE FILE
388400980605     C** chaino vecchio progressivo per tassare con tariffa scaduTa
388500980605    1C     *IN07         IFEQ      *ON
388600980605     C   17KTAM2         CHAIN     TNTAM000                           18
388700980605     c**
388800980605     C     *IN17         IFEQ      *OFF
388900980605    2C     *IN18         OREQ      *ON
389000980605     C                   MOVEL     MSG(3)        TASMSG
389100160114     c                   eval      MancaTariffa = *on
389200980605     C                   SETON                                        94
389300980605    2C                   ENDIF
389400980605     C**
389500980605     C                   GOTO      ENDTAM
389600980605     C**
389700980605     C                   ELSE
389800980605     C**
389900980605     C** SE NON E' TARIFFA ADATTA PER LA BOLLA (ITALIA INVECE CHE ESTER
390000980605     C**  O VICEVERSA) --> RILEGGO COME SE NON AVESSI LETTO
390100980605     C     TAMFIE        IFNE      WBOEST
390200980605     C     TAMFIE        ANDNE     ' '
390300980605     C                   GOTO      SUTAM
390400980605     C                   ENDIF
390500980605    1C                   ENDIF
390600980605     C**
390700980605     C** TARIFFA DA DECORRERE : SEGNALO SE NON C'E' UNA TARIFFA SCADUTA
390800980605     C**    (di fatto non e' possibile perche' se c'e' una da decorrere
390900980605     C**    e ce n'e' una prima e' la tariffa valida dal momento che
391000980605     C**    non e' possibile che esista una buco di decorrenze scadenze
391100980605     C**    tra i vari progressivi)
391200980605    1C     TASDSP        IFLT      TAMDDT
391300980605     C   17KTAM2         CHAIN     TNTAM000                           18
391400980605     C* NON C'E' TARIFFA SCADUTA O NON TROVATA DI NUOVO
391500980605    2C     *IN17         IFEQ      *OFF
391600980605     C     *IN18         OREQ      *ON
391700980605     C                   MOVEL     MSG(10)       TASMSG
391800980605     C                   SETON                                        94
391900160114     c                   eval      MancaTariffa = *on
392000980605    2C                   ENDIF
392100980605     C**
392200941107     C                   GOTO      ENDTAM
392300980605    1C                   END
392400980605     C**
392500980605     C** TARIFFA SCADUTA: DATA SPEDIZIONE MAGGIORE DELLA DATA  SCADENZA
392600980605    1C     TASDSP        IFGT      TAMDST
392700941107     C                   SETON                                        17
392800980605     C                   Z-ADD     TAMKSC        KSC2
392900980605     C                   Z-ADD     TAMCTR        CTR2
393000980605     C                   Z-ADD     TAMPRG        PRG2
393100941107     C                   GOTO      SUTAM
393200980605    1C                   ENDIF
393300020225     C*
393400020225     C** CERCO LA CARTELLO RELATIVA ALLA TARIFFA DEL CLIENTE
393500020227     C*  SE NON HO GIA' UNA TARIFFA DI CARTELLO E SE 94 SPENTO
393600020227     C     ENDTAM        TAG
393700020227     C*
393800020225     C*
393900020227    0C     *IN94         IFEQ      *OFF
394000020227      *
394100020227     C                   SETOFF                                       90
394200020227     C                   Z-ADD     1             T
394300020227     C     TAMKSC        LOOKUP    TAC(T)                                 90
394400020227      *
394500020227      * PER 90 SPENTO VUOL DIRE CHE NON HO TROVATO NESSUNA CARTELLO
394600020227    1C     *IN90         IFEQ      *OFF
394700020319     C***                  CLEARWNET
394800020225     C                   MOVEL     TAMFLO        DSTA01
394900020225     C*
395000020319     C**                   SELEC
395100020225     C* VERIFICO IL TIPO SERVIZIO
395200020319     C**         TAMTSP    WHEQ 'P'
395300020319     C**                   MOVEL'PPT'     WNET    3
395400020225     C* VERIFICO SE TARIFFA ITALIA VERIFICO SE NETWORK DPD
395500020319     C**         TAMFIE    WHEQ 'I'
395600020319     C**         §TADPD    ANDEQ'S'
395700020319     C**                   MOVEL'DPD'     WNET
395800020225     C* VERIFICO SE TARIFFA ESTERA VERIFICO SE NETWORK FEDEX
395900020319     C**         TAMFIE    WHEQ 'E'
396000020319     C**         §TAFED    ANDEQ'S'
396100020319     C**                   MOVEL'FED'     WNET
396200020225     C* VERIFICO SE TARIFFA EUROEXPRESS
396300020319     C**         TAMFIE    WHEQ 'E'
396400020319     C**                   MOVEL'EEX'     WNET
396500020225     C* VERIFICO SE TARIFFA ITALIA
396600020319     C**         TAMFIE    WHEQ 'I'
396700020319     C**                   MOVEL'COR'     WNET
396800020225     C*
396900020319     C**                   ENDSL
397000020225     C* SE NETWORK UGUALE A BLANK CERCO LA TARIFFA DI CARTELLO CORRIERE
397100020319     C**         WNET      IFEQ *BLANK
397200020319     C**                   MOVEL'COR'     WNET
397300020319     C**                   END
397400020225     C* AGGANCIO LA TABELLA DELLE TARIFFE DI CARTELLO
397500020319     C                   EXSR      CERTCA
397600020319     C**                   SETOF                     90
397700020319     C**         WNET      LOKUPNTW,T                    90
397800020225     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
397900020319     C**         *IN90     IFEQ *OFF
398000020319     C**                   MOVELMSG,9     TASMSG
398100020319     C**                   GOTO ENDTA2
398200020319     C**                   ENDIF
398300020319    1C                   ENDIF
398400020227     C*
398500020319     C**                   Z-ADDTAC,T     CARKSC
398600020319     C**                   Z-ADDCTC,T     CARCTR
398700020319     C**                   Z-ADDPRC,T     CARPRG
398800020319     C**                   MOVE DIC,T     DIVCAR
398900020227     C*
399000020227    0C                   ENDIF
399100061220      * se impostato il campo del salvataggio della data spedizione
399200061220     c                   if        Sav_dsp <> *zeros
399300061220     c                   eval      tasdsp = Sav_dsp
399400061220     c                   clear                   Sav_dsp
399500061220     c                   endif
399600020225     C*
399700941107     C*
399800020227     C     ENDTA2        ENDSR
399900020618     C******************************************************
400000020618     C* RICERCA DETTAGLIO SU TUTAD
400100020618     C******************************************************
400200020618     C     RICTAD        BEGSR
400300020618     C     KTAD          SETLL     TITAD000                               09
400400020618     C** 49  TROVATA TARIFFA DEL CAP   DI ARRIVO
400500020618     C   09              SETON                                        49
400600020618     C**
400700020618    1C     *IN09         IFEQ      *OFF
400800020618     C                   MOVEL     *BLANKS       COMNAZ
400900020618     C                   MOVEL     *BLANKS       COMCAP
401000020618     C     KTAD          SETLL     TITAD000                               09
401100020618    2C     *IN09         IFEQ      *OFF
401200020618     C*
401300020618     C                   Z-ADD     1             A
401400020618     C     COMCTS        LOOKUP    CTS(A)                                 05
401500020618     C**
401600020618     C* SE TROVATO COD.TASSAZIONE
401700020618    3C     *IN05         IFEQ      *ON
401800020618     C     A             OCCUR     DSCT
401900020618     C                   MOVEL     §CTRAP        COMCTS
402000020618     C** CARICO REGIONE AL POSTO DEL CODICE DI TASSAZIONE
402100020618     C     KTAD          SETLL     TITAD000                               09
402200020618    3C                   ENDIF
402300020618     C**
402400020618    3C     *IN09         IFEQ      *OFF
402500020618     C*
402600020618     C     WBOEST        IFEQ      'E'
402700020618     C                   MOVEL     'EE'          COMCTS
402800020618     C                   ELSE
402900020618     C                   MOVEL     'IT'          COMCTS
403000020618     C                   END
403100020618     C     KTAD          SETLL     TITAD000                               09
403200020618    3C                   ENDIF
403300020618    2C                   ENDIF
403400020618    1C                   ENDIF
403500020618     C                   ENDSR
403600941107     C/SPACE
403700941112     C******************************************************
403800990721     C** CERCA TARIFFA TITAD DETTAGLIO E CARICA DS
403900941112     C******************************************************
404000941107     C     CARTAR        BEGSR
404100941112C    C                   SETOFF                                       495694
404200941112     C                   MOVEL     TASCAD        COMCAP            9
404300990923     C                   MOVEL     TASNZD        COMNAZ            3
404400941112     C                   MOVEL     TASCTS        COMCTS
404500020618     C                   MOVEL     TASLNP        WLNP
404600020618     C                   EXSR      RICTAD
404700020618     C   09              GOTO      LATAR
404800020618     C**
404900020618     C** SE NON TROVATO TITAD CON LA LINEA DI PARTENZA, CERCO CON LA
405000020618     C**  REGIONE TROVATA DA ORGANIGRANNA DI TASLNP
405100020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
405200020618     C     TASLNP        IFNE      SAVLNP
405300020618     C     TASLNP        CHAIN     AZORG01L                           31
405400020618     C   31              CLEAR                   ORGDE7
405500020618     C                   MOVEL     ORGDE7        OG147
405600020618     C                   MOVEL     TASLNP        SAVLNP
405700020618     C                   ENDIF
405800020618     C**
405900020618     C                   MOVEL     TASCAD        COMCAP            9
406000020618     C                   MOVEL     TASNZD        COMNAZ            3
406100020618     C                   MOVEL     TASCTS        COMCTS
406200020618     C                   MOVEL     §OGRET        WLNP
406300020618     C                   EXSR      RICTAD
406400020618     C   09              GOTO      LATAR
406500020618     C**
406600020618     C** SE NON TROVATO TITAD CON LA REGIONE DI TASLNP, CERCO CON LA
406700020618     C**  LINEA ITALIA 950
406800020618     C                   MOVEL     TASCAD        COMCAP            9
406900020618     C                   MOVEL     TASNZD        COMNAZ            3
407000020618     C                   MOVEL     TASCTS        COMCTS
407100020618     C                   MOVEL     950           WLNP
407200020618     C                   EXSR      RICTAD
407300020618     C   09              GOTO      LATAR
407400941112     C*
407500941112     C*--------------------TARIFFA REVERSIBILE----------------------------------
407600941107     C     TIPBOL        CABNE     'A'           MANTAR
407700941107     C** TARIFFA REVERSIBILE SOLO PER RITORNO(TBL=A)
407800941107     C     TASLNP        CABEQ     68            MANTAR
407900941107     C** NO TARIFFA REVERSIBILE PER 68
408000981027     C     KSCFIL        IFEQ      888
408100981027     C                   Z-ADD     TASLNA        KFIL
408200981027     C                   ELSE
408300981027     C                   MOVEL     KSCFIL        KFIL
408400981027     C                   ENDIF
408500981008     C**
408600941107     C                   SETON                                        56
408700941112     C                   MOVEL     TASCAM        COMCAP
408800990923     C                   MOVEL     TASNZM        COMNAZ
408900941123     C                   MOVEL     TASMCT        COMCTS
409000020618     C                   MOVEL     KFIL          WLNP
409100020618     C                   EXSR      RICTAD
409200020618     C   09              GOTO      LATAR
409300020618     C*
409400020618     C** SE NON TROVATO TITAD CON LA LINEA CLIENTE, CERCO CON LA
409500020618     C**  REGIONE TROVATA DA ORGANIGRANNA DELLA LIENA CLIENTE
409600020618     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
409700020618     C     KFIL          IFNE      SAVLNP
409800020618     C     KFIL          CHAIN     AZORG01L                           31
409900020618     C   31              CLEAR                   ORGDE7
410000020618     C                   MOVEL     ORGDE7        OG147
410100020618     C                   MOVEL     KFIL          SAVLNP
410200020618     C                   ENDIF
410300020618     C*
410400020618     C                   MOVEL     TASCAM        COMCAP
410500020618     C                   MOVEL     TASNZM        COMNAZ
410600020618     C                   MOVEL     TASMCT        COMCTS
410700020618     C                   MOVEL     §OGRET        WLNP
410800020618     C                   EXSR      RICTAD
410900020618     C   09              GOTO      LATAR
411000020618     C**
411100020618     C** SE NON TROVATO TITAD CON LA REGIONE DI KFIL, CERCO CON LA
411200020618     C**  LINEA ITALIA 950
411300020618     C                   MOVEL     TASCAM        COMCAP
411400020618     C                   MOVEL     TASNZM        COMNAZ
411500020618     C                   MOVEL     TASMCT        COMCTS
411600020618     C                   MOVEL     950           WLNP
411700020618     C                   EXSR      RICTAD
411800020618     C   09              GOTO      LATAR
411900020618     C**
412000020618     C**         KTDREV    SETLLTITAD000                 09
412100020618     C** 09                SETON                     49
412200020618     C** 09                GOTO LATAR
412300020618     C**
412400020618     C**                   MOVEL*BLANKS   COMNAZ
412500020618     C**                   MOVEL*BLANKS   COMCAP
412600020618     C**         KTDREV    SETLLTITAD000                 09
412700020618     C** 09                GOTO LATAR
412800020618     C**
412900020618     C**                   Z-ADD1         A
413000020618     C**         COMCTS    LOKUPCTS,A                    05
413100020618     C**N05                GOTO POIT2
413200020618     C**         A         OCUR DSCT
413300020618     C**                   MOVEL§CTRAP    COMCTS
413400020618     C**         KTDREV    SETLLTITAD000                 09
413500020618     C** 09                GOTO LATAR
413600020618     C**         POIT2     TAG
413700020618     C**         WBOEST    IFEQ 'E'
413800020618     C**                   MOVEL'EE'      COMCTS
413900020618     C**                   ELSE
414000020618     C**                   MOVEL'IT'      COMCTS
414100020618     C**                   END
414200020618     C**         KTDREV    SETLLTITAD000                 09
414300020618     C** 09                GOTO LATAR
414400941107     C** NON TROVATO NEMMENO LA TARIFFA REVERSIBILE
414500941107     C     MANTAR        TAG
414600980527     C     *IN09         IFEQ      *OFF
414700980527     C                   SETON                                        94
414800980527     C                   MOVEL     MSG(8)        TASMSG
414900980527     C                   GOTO      FINE
415000980527     C                   ENDIF
415100941107     C** MANCA TARIFFA
415200941107     C     LATAR         TAG
415300030902     C                   EXSR      CARDST
415400941112     C** CARICA DS DELLA TARIFFA
415500941107     C                   ENDSR
415600941107     C/SPACE
415700941112     C******************************************************
415800941107     C** CARICA DS TARIFFA
415900941112     C******************************************************
416000941107     C     CARDST        BEGSR
416100980514     C     *LIKE         DEFINE    TADRPV        RAPPV
416200980514     C**
416300020218     C                   DO        200           A
416400941112     C     A             OCCUR     TARDS
416500941112     C                   CLEAR                   TARDS
416600941112     C                   END
416700941112     C                   Z-ADD     0             RAPPV
416800980514     C                   CLEAR                   WELAMT
416900941112     C**
417000980514    1C                   DO        *HIVAL        A
417100980514     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
417200980514    2C     WELAMT        IFNE      'S'
417300020618     C** 56      KTDREV    READETITAD000                 15
417400020618     C**N56      KTAD      READETITAD000                 15
417500020618     C     KTAD          READE     TITAD000                               15
417600941107     C   15              GOTO      ENDDST
417700980514    2C                   ENDIF
417800980514     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
417900980514    2C     WELAMT        IFEQ      'S'
418000980515     C                   MOVEL     'E'           WELAMT            1
418100980514    2C                   ENDIF
418200980514     C**
418300941107     C     A             OCCUR     TARDS
418400980514     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
418500980514     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
418600980514     C**
418700941120     C                   Z-ADD     TADLNP        TLNP
418800941107     C                   MOVEL     TADCTS        TCTS
418900990721     C                   MOVEL     TADNAZ        TNAZ
419000941110     C                   MOVEL     TADCAP        TCAP
419100941107     C                   Z-ADD     TADCTR        TCTR
419200941107     C                   Z-ADD     TADRPV        TRPV
419300941122     C                   Z-ADD     TADMIN        TMIN
419400941122     C                   Z-ADD     TADMAX        TMAX
419500040603     C***!!!****         Z-ADD     TADRPV        RAPPV
419600980514     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
419700980514    2C     WELAMT        IFNE      'E'
419800980514     C**
419900980514    3C     TADSGL        IFEQ      MINTAS
420000980514     C                   MOVEL     'E'           WELAMT
420100980514   X3C                   ELSE
420200980514     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
420300980514     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
420400980514     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
420500980514     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
420600980515    4C     TADSGL        IFGT      MINTAS
420700980514     C                   MOVEL     'S'           WELAMT
420800980514     C                   Z-ADD     MINTAS        TSCG
420900980514     C**
421000980522     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
421100980522    5C     TAMATA        IFEQ      0
421200980522     C     TAMATA        ORLE      MINTAS
421300980522     C     TADSGL        ORGT      TAMATA
421400980522     C**
421500980522    6C     UNOCTR        IFEQ      4
421600980514     C     UNOCTR        OREQ      0
421700980514     C     MINTAS        DIV(H)    100           WMTAS            15 5
421800980514     C                   ELSE
421900980514     C                   Z-ADD     MINTAS        WMTAS
422000980522    6C                   ENDIF
422100980514     C* TARIFFA
422200980514     C     WMTAS         MULT(H)   TADITR        TITR
422300980514     C     WMTAS         MULT(H)   TADINO        TINO
422400980522     C                   ELSE
422500980522     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
422600980522     C                   Z-ADD     TADITR        TITR
422700980522     C                   Z-ADD     TADINO        TINO
422800980522    5C                   ENDIF
422900980522    4C                   ENDIF
423000980514    3C                   ENDIF
423100980514    2C                   ENDIF
423200980514     C*
423300980514     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
423400980514    2C     WELAMT        IFNE      'S'
423500980514     C                   Z-ADD     TADSGL        TSCG
423600980514     C                   Z-ADD     TADITR        TITR
423700980514     C                   Z-ADD     TADINO        TINO
423800980514    2C                   ENDIF
423900980514    1C                   ENDDO
424000941107     C     ENDDST        ENDSR
424100941107     C/SPACE
424200941112     C******************************************************
424300941110     C** CARICAMENTO TABELLE SOLO LA 1 VOLTA
424400941112     C******************************************************
424500941110     C     CARTAB        BEGSR
424600980605     C     *LIKE         DEFINE    TAMATA        MINTAS
424700980605     C     *LIKE         DEFINE    TAMATA        MINTAR
424800990923     C     *LIKE         DEFINE    §TADIV        DIVCAR
424900020618     C                   CLEAR                   SAVLNP
425000980605     C**
425100980605     C                   OPEN      TABEL00F
425200980605     C**
425300941110     C                   EXSR      CARQT
425400941110     C** CARICAMENTO IAC
425500941110     C                   EXSR      CARTR
425600941110     C** CARICAMENTO TIPI TARIFFA
425700941110     C                   EXSR      CAR$2
425800941110     C** CARICAMENTO SIGLE VARIE
425900000103     C                   EXSR      CARCC
426000000103     C** CARICAMENTO SIGLE VARIE ESENTI
426100941112     C                   EXSR      CARCV
426200941112     C** CARICAMENTO CAMBI
426300941110     C                   EXSR      CARCT
426400941110     C** CARICAMENTO CODIFICI TASSAZIONE
426500941112     C                   EXSR      CAR5E
426600941112     C** CARICA TIPI SERVIZIO
426700980429     C                   EXSR      CAR1A
426800980429     C** CARICA TIPI INCASSO C/A MITTENTI
426900980603     C                   EXSR      CARTB
427000980603     C** CARICA DS TIPI BOLLA CHE NON PREVEDONO ASSICURAZIONE
427100020319     C                   EXSR      CAREST
427200980605     C** CARICA P.O. ESTERI
427300990917     C                   EXSR      CARDIV
427400990917     C** CARICAMENTO MONETA DI CONTO
427500020320     C                   EXSR      CARPAR
427600020320     C** CARICA TARIFFE DI CARTELLO
427700031120     C                   EXSR      CAR3A
427800031120     C** CARICO VARIE DELLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
427900980605     C                   CLOSE     TABEL00F
428000040505     C**
428100040505     C                   OPEN      TNTBE01L
428200040505     C** caricamento percentuale carburante
428300040505     C                   EXSR      CARPSC
428400080617     C** caricamento filiali addebito lasciato avviso
428500080617     C                   EXSR      CARLAV
428600111102     C** caricamento filiali addebito centro storico da cartello
428700111102     C                   EXSR      CARZTL
428800040505     C**
428900040505     C                   CLOSE     TNTBE01L
429000040505     C**
429100941110     C                   ENDSR
429200941110     C**********************************************************
429300941110     C** CARICAMENTO DS MULTIPLA  CODICI TASSAZIONE
429400941110     C**********************************************************
429500941110     C     CARCT         BEGSR
429600041220     C                   DO        600           A
429700941110     C     A             OCCUR     DSCT
429800941110     C                   CLEAR                   DSCT
429900941110     C                   END
430000941110     C** PULIZIA DS
430100941110     C                   MOVEL     'CT'          COD
430200941110     C     KTAB          SETLL     TABEL                                  07
430300941110     C  N07              SETON                                          H7
430400941110     C   H7              GOTO      FINE
430500941110     C                   Z-ADD     0             K
430600941110     C                   DO        *HIVAL        A
430700941110     C     KTAB          READE     TABEL                                  07
430800941110     C   07              GOTO      ENDCTS
430900941110     C     TBLKEY        IFNE      *BLANKS
431000941110     C                   ADD       1             K
431100941110     C                   MOVEL     TBLKEY        CTS(K)
431200941110     C     K             OCCUR     DSCT
431300941110     C                   MOVEL     TBLUNI        DSCT
431400941110     C                   END
431500941110     C                   END
431600941110     C     ENDCTS        TAG
431700941110     C                   ENDSR
431800941110     C/SPACE
431900941112     C**********************************************************
432000941112     C** CARICAMENTO TIPI SERVIZIO
432100941112     C**********************************************************
432200941112     C     CAR5E         BEGSR
432300941112     C                   DO        10            A
432400941112     C     A             OCCUR     DS5E
432500941112     C                   CLEAR                   DS5E
432600941112     C                   END
432700941112     C** PULIZIA DS
432800941112     C                   MOVEL     '5E'          COD
432900941112     C     KTAB          SETLL     TABEL                                  07
433000941112     C  N07              SETON                                          H7
433100941112     C   H7              GOTO      FINE
433200941112     C                   Z-ADD     0             K
433300941112     C                   DO        *HIVAL        A
433400941112     C     KTAB          READE     TABEL                                  07
433500941112     C   07              GOTO      END5E
433600941112     C     TBLKEY        IFNE      *BLANKS
433700941112     C                   ADD       1             K
433800941112     C                   MOVEL     TBLKEY        TS(K)
433900941112     C     K             OCCUR     DS5E
434000941112     C                   MOVEL     TBLUNI        DS5E
434100941112     C                   END
434200941112     C                   END
434300941112     C     END5E         TAG
434400941112     C                   ENDSR
434500980429     C**********************************************************
434600980429     C** CARICAMENTO TIPI INCASSO C/A
434700980429     C**********************************************************
434800980429     C     CAR1A         BEGSR
434900980430     C                   MOVEA     *ALL'9'       TIC
435000980429     C                   MOVEL     '1A'          COD
435100980429     C                   Z-ADD     0             K
435200980429     C     KTAB          SETLL     TABEL                                  07
435300980429     C     KTAB          READE     TABEL                                  07
435400980430    1C     *IN07         DOWEQ     *OFF
435500980430    2C     TBLKEY        IFNE      *BLANKS
435600980430     C     TBLFLG        ANDEQ     ' '
435700980429     C                   MOVEL     TBLUNI        DS1A
435800980430    3C     §1AFMB        IFEQ      'M'
435900980429     C                   ADD       1             K
436000980429     C                   MOVEL     TBLKEY        TIC(K)
436100980430    3C                   ENDIF
436200980430    2C                   ENDIF
436300980429     C     KTAB          READE     TABEL                                  07
436400980430    1C                   ENDDO
436500980429     C                   ENDSR
436600980603     C**********************************************************
436700980603     C** CARICAMENTO TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
436800980603     C**********************************************************
436900980603     C     CARTB         BEGSR
437000980603     C                   MOVEL     'TB'          COD
437100980603     C                   Z-ADD     0             K
437200980603     C     KTAB          SETLL     TABEL                                  07
437300980603     C     KTAB          READE     TABEL                                  07
437400980603    1C     *IN07         DOWEQ     *OFF
437500980603    2C     TBLKEY        IFNE      *BLANKS
437600980603     C     TBLFLG        ANDEQ     ' '
437700980603     C                   MOVEL     TBLUNI        DSTB
437800980603    3C     §TBFAS        IFEQ      '0'
437900980603     C                   ADD       1             K
438000980603     C                   MOVEL     TBLKEY        TBN(K)
438100980603    3C                   ENDIF
438200980603    2C                   ENDIF
438300980603     C     KTAB          READE     TABEL                                  07
438400980603    1C                   ENDDO
438500980603     C                   ENDSR
438600031120     C**********************************************************
438700031120     C** CARICAMENTO VARIE PER TIPI BOLLE CHE ACCETTANO UNA SINGOLA VARIA
438800031120     C**********************************************************
438900031120     C     CAR3A         BEGSR
439000031120     C                   MOVEL     '3A'          COD
439100031120     C                   Z-ADD     0             K
439200031120     C     KTAB          SETLL     TABEL                                  07
439300031120     C     KTAB          READE     TABEL                                  07
439400031120    1C     *IN07         DOWEQ     *OFF
439500031120    2C     TBLKEY        IFNE      *BLANKS
439600031120     C     TBLFLG        ANDEQ     ' '
439700031120     C                   MOVEL     TBLUNI        DS3A
439800031120    3C     §3ASVA        IFNE      ' '
439900031120     C                   ADD       1             K
440000031120     C                   MOVEL     §3ASVA        T3A(K)
440100031120    3C                   ENDIF
440200031120    2C                   ENDIF
440300031120     C     KTAB          READE     TABEL                                  07
440400031120    1C                   ENDDO
440500031120     C                   ENDSR
440600020319     C**********************************************************
440700020319     C** CARICAMENTO PO ESTERI
440800020319     C**********************************************************
440900020319     C     CAREST        BEGSR
441000020319     C*
441100020319     C                   CLEAR                   K
441200050504     C                   CLEAR                   KK                4 0
441300020319     C**
441400020319     C                   CLEAR                   CC
441500020319     C     *LOVAL        SETLL     AZORG01L
441600020319     C                   READ      AZORG01L                               07
441700020319    1C     *IN07         DOWEQ     *OFF
441800020319     C** FIL O AGENZIA
441900020319    2C     ORGFAG        IFEQ      'F'
442000020319     C     ORGFAG        OREQ      'A'
442100020319     C* NON ANNULLATO
442200020319    3C     ORGFVA        IFEQ      ' '
442300020319    4C                   MOVEL     ORGDE3        OG143
442400050504     c* p.o. esteri senza dpd
442500020319     C     §OGNTW        IFEQ      'EEX'
442600020319     C     §OGNTW        OREQ      'EUP'
442700020404     C*******    §OGNTW    OREQ 'DPD'
442800020319     C     §OGNTW        OREQ      'FED'
442900020319     C                   ADD       1             K
443000020319     C                   MOVEL     ORGFIL        EST(K)
443100020319    4C                   ENDIF
443200050504     c* p.o. estero totale, anche con DPD
443300050504    4C     §OGNTW        IFEQ      'EEX'
443400050504     C     §OGNTW        OREQ      'EUP'
443500050504     C     §OGNTW        OREQ      'DPD'
443600050504     C     §OGNTW        OREQ      'FED'
443700050504     C                   ADD       1             KK
443800050504     C                   MOVEL     ORGFIL        ESTtot(KK)
443900050504    4C                   ENDIF
444000020319    3C                   ENDIF
444100020319    2C                   ENDIF
444200020319     C**
444300020319     C                   READ      AZORG01L                               07
444400020319    1C                   ENDDO
444500020319     C                   ENDSR
444600941110     C********************************************************
444700941110     C** CARICAMENTO SIGLE VARIE
444800941110     C********************************************************
444900941110     C     CAR$2         BEGSR
445000941110     C                   CLEAR                   DS$2
445100941110     C                   MOVEL     '$2'          COD
445200941110     C                   MOVEL     *BLANKS       KEY
445300941110     C                   MOVEL     '1'           KEY
445400941110     C     KTABC         CHAIN     TABEL                              07
445500941110     C   07              SETON                                          H7
445600941110     C   H7              GOTO      FINE
445700941110     C                   MOVEL     TBLUNI        DS$2
445800941110     C                   ENDSR
445900000103     C********************************************************
446000000103     C** CARICAMENTO SIGLE VARIE  ESENTI IVA
446100000103     C********************************************************
446200000103     C     CARCC         BEGSR
446300000103     C                   CLEAR                   DSCC
446400020416     C                   CLEAR                   KK
446500000103     C                   MOVEL     'CC'          COD
446600000103     C                   MOVEL     *BLANKS       KEY
446700000103     C                   MOVEL     'VARIE'       KEY
446800000103     C     KTABC         SETLL     TABEL                                  07
446900020416     C**N07                GOTO ENDCC
447000000103     C                   DO        *HIVAL
447100000103     C     KTAB          READE     TABEL                                  07
447200000103     C   07              LEAVE
447300000103     C                   MOVEL     TBLUNI        DSCC
447400000103     C* CONTROLLO SE VARIA ESENTE CARICO LA SIGLA IN TABELLA
447500000103     C     §CCJEI        IFNE      *BLANKS
447600000103     C                   ADD       1             K
447700000103     C                   MOVE      TBLKEY        VES(K)
447800000103     C                   ENDIF
447900020416     C* CONTROLLO SE DA RESTITUIRE CON SIGAL VARIA E IMPORTO =0
448000020416     C     §CCSV0        IFEQ      'S'
448100050504     C                   ADD       1             KK                4 0
448200020416     C                   MOVE      TBLKEY        SV0(KK)
448300020416     C                   ENDIF
448400000103     C                   ENDDO
448500000103     C                   Z-ADD     0             K
448600020416     C                   Z-ADD     0             KK
448700000103     C     ENDCC         ENDSR
448800941112     C********************************************************
448900941112     C** CARICAMENTO CAMBI
449000941112     C********************************************************
449100941112     C     CARCV         BEGSR
449200941116     C                   Z-ADD     0             K
449300941116     C                   DO        50            A
449400941116     C     A             OCCUR     DSCV
449500941112     C                   CLEAR                   DSCV
449600941116     C                   END
449700941116     C                   MOVEL     *BLANKS       CV
449800941116     C*
449900941112     C                   MOVEL     'CV'          COD
450000941116     C     KTAB          SETLL     TABEL                                  07
450100941116     C  N07              GOTO      ENDCV
450200941116     C                   DO        *HIVAL        A
450300941116     C     KTAB          READE     TABEL                                  07
450400941116     C   07              GOTO      ENDCV
450500941116     C     TBLKEY        IFNE      *BLANKS
450600941116     C                   ADD       1             K
450700941116     C                   MOVEL     TBLKEY        CV(K)
450800941116     C     K             OCCUR     DSCV
450900941116     C                   MOVEL     TBLUNI        DSCV
451000941116     C                   END
451100941116     C                   END
451200941112     C     ENDCV         ENDSR
451300941110     C******************************************************
451400941110     C** CARICAMENTO TIPI TARIFFA
451500941110     C******************************************************
451600941110     C     CARTR         BEGSR
451700030205     C                   DO        50            A
451800941110     C     A             OCCUR     DSTR
451900941110     C                   CLEAR                   DSTR
452000941110     C                   END
452100941110     C**
452200941110     C                   MOVEL     'TR'          COD
452300941112     C                   Z-ADD     0             K
452400941110     C     KTAB          SETLL     TABEL                                  07
452500941110     C  N07              SETON                                          H7
452600941110     C   H7              GOTO      FINE
452700941110     C                   DO        *HIVAL        A
452800941110     C     KTAB          READE     TABEL                                  07
452900941110     C   07              GOTO      TAB1
453000941110     C     TBLKEY        IFNE      *BLANKS
453100941110     C                   ADD       1             K
453200941110     C                   MOVEL     TBLKEY        CTR(K)
453300941110     C     K             OCCUR     DSTR
453400941110     C                   MOVEL     TBLUNI        DSTR
453500941110     C                   END
453600941110     C                   END
453700941110     C     TAB1          ENDSR
453800941110     C**********************************************************
453900020221     C** MEMORIZZA
454000941110     C** TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
454100941110     C**********************************************************
454200941110     C     CARPAR        BEGSR
454300020225      *
454400020225     C     *LIKE         DEFINE    TAMKSC        CARKSC
454500020225     C     *LIKE         DEFINE    TAMCTR        CARCTR
454600020225     C     *LIKE         DEFINE    TAMPRG        CARPRG
454700020221      *
454800020222      * RICHIAMO IL TRUL27R PER OGNI NETWORK PER CARICARMI LE TARIFFE DI CARTELLO IN SCHIERA
454900020221      *
455000020221     C* CORRIERE BARTOLINI
455100020221     C                   CLEAR                   TRUL27
455200020221     C                   MOVEL     'COR'         I27NTW
455300020222     C                   EXSR      SRTRUL
455400020221      * POSTE
455500020222     C                   CLEAR                   TRUL27
455600020222     C                   MOVEL     'PPT'         I27NTW
455700020222     C                   EXSR      SRTRUL
455800020222      * DPD
455900020222     C                   CLEAR                   TRUL27
456000020222     C                   MOVEL     'DPD'         I27NTW
456100020222     C                   EXSR      SRTRUL
456200020222      * FEDEX
456300020222     C                   CLEAR                   TRUL27
456400020222     C                   MOVEL     'FED'         I27NTW
456500020222     C                   EXSR      SRTRUL
456600020222      * EUROEXPRESS
456700020222     C                   CLEAR                   TRUL27
456800020226     C                   MOVE      'L'           I27TLA
456900020222     C                   MOVEL     'EEX'         I27NTW
457000020222     C                   EXSR      SRTRUL
457100020226      *
457200020226     C                   CLEAR                   CARKSC
457300020226     C                   CLEAR                   CARCTR
457400020226     C                   CLEAR                   CARPRG
457500020222      *
457600020222      ** CARICO LE TARIFFE PARTICOLARI
457700941111     C                   EXSR      CAR1P
457800020222      *
457900941110     C                   ENDSR
458000040505     C**********************************************************
458100040505     C** CARICAMENTO SCHIERA SUPPLEMEMNTO CARBURANTE
458200040505     C**********************************************************
458300040505     C     CARPSC        BEGSR
458400040505
458500040507     C                   CLEAR                   KX
458600040505
458700040505     C                   MOVEL     'PSC'         KEYTBE
458800040505     C     KEYTBE        SETLL     TNTBE01L
458900040505     C                   DO        *HIVAL
459000040505     C     KEYTBE        READE     TNTBE01L
459100040505
459200040505     C                   IF        %EOF(TNTBE01L)
459300040505     C                   LEAVE
459400040505     C                   ENDIF
459500040505
459600040505     C                   IF        TBEATB = ' '
459700040507     C                   ADD       1             KX
459800040507     C                   IF        KX <= 999
459900040505     C                   MOVEL     TBEUNI        DPSC
460000040507     C                   Z-ADD     §PSCDAD       DS_DSC
460100040507     C                   Z-ADD     §PSCPER       DS_PSC
460200040507     C                   MOVE      WDSPSC        DSCA(KX)
460300040507     C                   ENDIF
460400040505     C                   ENDIF
460500040505
460600040505     C                   ENDDO
460700040507
460800040507     C                   SORTA     DSCA
460900040505
461000040505     C                   ENDSR
461100080617      *****************************************************************
461200080617      ** CARICAMENTO SCHIERA FILIALI ABILITATE ADDEBITO LASCIATO AVVISO
461300080617      *****************************************************************
461400080617     C     CARLAV        BEGSR
461500080617
461600080617     C                   CLEAR                   KX
461700080617
461800080617     C                   MOVEL     'LAV'         KEYTBE
461900080617     C     KEYTBE        SETLL     TNTBE01L
462000080617     C                   DO        *HIVAL
462100080617     C     KEYTBE        READE     TNTBE01L
462200080617
462300080617     C                   IF        %EOF(TNTBE01L)
462400080617     C                   LEAVE
462500080617     C                   ENDIF
462600080617
462700080617     C                   IF        TBEATB = ' '
462800080617     C                   ADD       1             KX
462900080617     C                   IF        KX <= 999
463000080617     C                   MOVEL     TBEUNI        DLAV
463100080617     C                   EVAL      SFILLAV(KX) = %dec(tbeke1:3:0)
463200080618     C                   EVAL      SDTALAV(KX) = D§LAVTAS
463300080617     C                   ENDIF
463400080617     C                   ENDIF
463500080617
463600080617     C                   ENDDO
463700080617
463800080617
463900080617     C                   ENDSR
464000111102      ******************************************************************
464100111102      ** CARICAMENTO SCHIERA FILIALI ADDEBITO DA CARTELLO CENTRO STORICO
464200111102      ******************************************************************
464300111102     C     CARZTL        BEGSR
464400111102
464500111102     C                   CLEAR                   KX
464600111102
464700111102     C                   MOVEL     'ZTL'         KEYTBE
464800111102     C     KEYTBE        SETLL     TNTBE01L
464900111102     C                   DO        *HIVAL
465000111102     C     KEYTBE        READE     TNTBE01L
465100111102
465200111102     C                   IF        %EOF(TNTBE01L)
465300111102     C                   LEAVE
465400111102     C                   ENDIF
465500111102
465600111102     C                   IF        TBEATB = ' '
465700111102     C                   ADD       1             KX
465800111102     C                   IF        KX <= 999
465900111102     C                   MOVEL     TBEUNI        DZTL
466000111102     C                   EVAL      SFILZTL(KX) = %dec(tbeke1:3:0)
466100111102     C                   EVAL      SDTAZTL(KX) = D§ZTLTAS
466200111102     C                   ENDIF
466300111102     C                   ENDIF
466400111102
466500111102     C                   ENDDO
466600111102
466700111102
466800111102     C                   ENDSR
466900020222     C*************************************************************
467000020222     C** RICHIAMO DEL TRUL27R
467100020222     C*************************************************************
467200020222     C     SRTRUL        BEGSR
467300020222      *
467400160114     C***                Z-ADD     TASDCT        I27DTA
467500160114      /free
467600160114       //?Passo la data giusta al TRUL27R
467700160114        IF  Ritass = 'S';
467800160114           I27dta = TASdtcar;
467900160114         ELSE;
468000160114           I27dta = TASdct;
468100160114         ENDIF;
468200160114      /end-free
468300021122     c                   Movel     TasCtr        I27Ctb
468400020222     C                   CALL      'TRUL27R'
468500020222     C                   PARM                    TRUL27
468600020222      *
468700020222     C     O27ERR        IFEQ      *BLANKS
468800020319     C     O27PRG        ANDNE     *BLANKS
468900020222     C                   ADD       1             T                 2 0
469000020222     C                   MOVE      I27NTW        NTW(T)
469100020222     C                   Z-ADD     O27KSC        TAC(T)
469200020222     C                   Z-ADD     O27CTR        CTC(T)
469300020226     C                   MOVE      O27PRG        PRC(T)
469400020222      *
469500020222      * AGGANCIO LA TARIFFA PER RECUPERARE LA DIVISA
469600020222      *
469700020226     C                   Z-ADD     O27KSC        CARKSC
469800020226     C                   Z-ADD     O27CTR        CARCTR
469900020226     C                   MOVE      O27PRG        CARPRG
470000020222     C     KTAMC         CHAIN     TNTAM00L                           94
470100020222      *
470200020222     C     *IN94         IFEQ      *OFF
470300020222     C                   MOVEL     TAMFLO        DSTA01
470400020222     C                   MOVE      §TADIV        DIC(T)
470500020226      * SE C'E ERRORE VADO A FINE PROGRAMMA
470600020226     C                   ELSE
470700020226     C                   MOVEL     MSG(9)        TASMSG
470800020226     C                   ENDIF
470900020226      *
471000020222      * SE C'E ERRORE VADO A FINE PROGRAMMA
471100020222     C                   ELSE
471200020222     C                   SETON                                        94
471300020222     C                   MOVEL     MSG(9)        TASMSG
471400020222     C                   ENDIF
471500020222      *
471600020222     C                   ENDSR
471700020225     C*************************************************************
471800020225     C** CERCO LA TARIFFA DI CARTELLO
471900020225     C*************************************************************
472000020225     C     CERTCA        BEGSR
472100020225      *
472200020225      * RICHIAMO IL TRUL27R PASANDOGLI LE CARATTERISTICHE DELLA SPEDIZIONE
472300020225      *
472400020318     C**                   CLEARTRUL27
472500020318     C**                   MOVE 'L'       I27TLA
472600020318     C**                   MOVE TASTSP    I27TSP
472700020318     C**                   MOVE TASLNA    I27LNA
472800020318     C**                   MOVE TASLNP    I27LNP
472900020318     C**                   MOVE TASKSC    I27CLI
473000020318     C**                   MOVE TASDCT    I27DTA
473100020318     C**                   CALL 'TRUL27R'
473200020318     C**                   PARM           TRUL27
473300020225      *
473400020318     C**         O27ERR    IFEQ ' '
473500020318     C**                   Z-ADDO27KSC    KSC2
473600020318     C**                   Z-ADDO27CTR    CTR2
473700020318     C**                   Z-ADDO27CTR    CODCTR
473800020318     C**                   MOVE O27PRG    PRG2
473900020225      * CARICO ANCHE I CAMPI  RELATIVI ALLA TARIFFA DI CARTELLO
474000020318     C**                   Z-ADDO27KSC    CARKSC
474100020318     C**                   Z-ADDO27CTR    CARCTR
474200020318     C**                   MOVE O27PRG    CARPRG
474300020225      *
474400020318     C**                   ENDIF
474500020318     C** DALLE SCHIERE CARICO LA TARIFFA DI CARTELLO
474600020318     C                   SETOFF                                       90
474700020319     C                   Z-ADD     1             T
474800020318     C     O27NTW        LOOKUP    NTW(T)                                 90
474900020318     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
475000020318     C     *IN90         IFEQ      *OFF
475100020318     C                   MOVEL     MSG(9)        TASMSG
475200020319     C                   SETON                                        94
475300020318     C                   GOTO      FINE
475400020318     C                   ENDIF
475500020318     C*
475600020318     C                   Z-ADD     TAC(T)        CARKSC
475700020318     C                   Z-ADD     CTC(T)        CARCTR
475800020318     C                   Z-ADD     PRC(T)        CARPRG
475900020318     C                   MOVE      DIC(T)        DIVCAR
476000160114
476100160114      /free
476200160114       //?Se sono in ritassazione
476300160114         IF  Ritass = 'S';
476400160114       //?Se la data TASDCT è diversa dalla data di caricemento della cartello
476500160114           IF  TASdct <> TASdtcar;
476600160114         //?Cerco la cartello giusta
476700160114             exsr NewTarCar;
476800160114           ENDIF;
476900160114         ENDIF;
477000160114      /end-free
477100160114
477200020225      *
477300020225     C                   ENDSR
477400941114     C*************************************************************
477500941121     C** CARICAMENTO CODICI E TARIFFE PARTICOLARI DI CARTELLO
477600941114     C*************************************************************
477700941110     C     CAR1P         BEGSR
477800941111     C*
477900020114     C                   DO        50            A
478000941112     C     A             OCCUR     DS1P
478100941111     C                   CLEAR                   DS1P
478200941112     C                   END
478300941111     C                   MOVEL     *BLANKS       CP
478400941110     C**
478500941110     C                   MOVEL     '1P'          COD
478600941111     C                   Z-ADD     0             K                 5 0
478700941110     C     KTAB          SETLL     TABEL                                  07
478800941110     C  N07              SETON                                          H7
478900941110     C   H7              GOTO      FINE
479000941110     C                   DO        *HIVAL        A
479100941110     C     KTAB          READE     TABEL                                  07
479200941110     C   07              GOTO      TABCP
479300941110     C     TBLKEY        IFNE      *BLANKS
479400941115     C                   MOVEL     TBLUNI        CAMPO            26
479500941115     C                   MOVE      CAMPO         WFCP              1
479600941115     C     WFCP          IFEQ      'S'
479700941110     C                   ADD       1             K
479800941110     C                   MOVEL     TBLKEY        CP(K)
479900941112     C     K             OCCUR     DS1P
480000941112     C                   MOVEL     TBLUNI        DS1P
480100941112     C**
480200941110     C                   END
480300941110     C                   END
480400941111     C                   ENDDO
480500020222     C*
480600941110     C     TABCP         ENDSR
480700941110     C****************************************************
480800941110     C** CARICAMENTO DATI VARI TASSAZIONE
480900941110     C****************************************************
481000941110     C     CARQT         BEGSR
481100941110     C                   MOVEL     'QT'          COD
481200941110     C                   MOVEL     *BLANKS       KEY
481300941110     C                   MOVEL     '1'           KEY
481400941110     C     KTABC         CHAIN     TABEL                              07
481500941113     C   07              SETON                                        H7
481600941113     C   H7              GOTO      FINE
481700941110     C  N07              MOVEL     TBLUNI        DSQT1
481800941110     C*
481900941110     C                   ENDSR
482000990917     C****************************************************
482100990922     C** CARICAMENTO MONETE DI CONTO / IMPORTI STANDARD
482200990917     C****************************************************
482300990917     C     CARDIV        BEGSR
482400990917     C*****
482500990922      *  RICERCA DELLA MONETA DI CONTO
482600990917     C                   CLEAR                   DSBS02
482700990917     C                   CLEAR                   DGED
482800990922     C*
482900990917     C                   MOVEL     'C'           T02MOD
483000990917     C                   MOVEL     KNSIF         T02SIF
483100990917     C                   MOVEL     'GED'         T02COD
483200991112     C                   MOVEL     '1'           T02KE1
483300990917      *
483400990917     C                   CALL      'TIBS02R'
483500990917     C                   PARM                    KPJBA
483600990917     C                   PARM                    DSBS02
483700990917      *
483800990917     C     T02ERR        IFEQ      *BLANKS
483900990917     C                   MOVEL     T02UNI        DGED
484000990917     C                   ENDIF
484100990922      * RICERCA DEGLI IMPORTI STANDARD NELLA MONETA DI CONTO
484200990922     C                   CLEAR                   DSBS02
484300990922     C                   CLEAR                   DGEI
484400990922     C                   MOVEL     'L'           T02TLA
484500990922     C                   MOVEL     'C'           T02MOD
484600990922     C                   MOVEL     KNSIF         T02SIF
484700990922     C                   MOVEL     'GEI'         T02COD
484800991112     C                   MOVEL     §GEDCN        T02KE1
484900990922      *
485000990922     C                   CALL      'TIBS02R'
485100990922     C                   PARM                    KPJBA
485200990922     C                   PARM                    DSBS02
485300990922      *
485400990922     C     T02ERR        IFEQ      *BLANKS
485500990922     C                   MOVEL     T02UNI        DGEI
485600990922     C                   ENDIF
485700990922      *
485800990917      *
485900990917     C                   ENDSR
486000011127     C**********************************************************
486100011127     C** RECUPERO LA DATA DEL GIORNO
486200011127     C**********************************************************
486300011127     C     RUDATE        BEGSR
486400011127      *
486500011127     C                   TIME                    W0140            14 0
486600011127     C* UDATE IN GGMMAAAA
486700011127     C                   MOVE      W0140         WDTGIO            8 0
486800011127     C*
486900011127     C                   Z-ADD     WDTGIO        G02DAT
487000011127     C                   MOVEL     *BLANK        G02ERR
487100011127     C                   CALL      'XSRDA8'
487200011127     C                   PARM                    WLBDAT
487300011127     C*
487400011127     C* UDATE IN AAAAMMGG
487500011127     C                   Z-ADD     G02INV        WDTGIO
487600011127      *
487700011127     C                   ENDSR
487800011127     C**********************************************************
487900011127     C** RECUPERO LA DATA TASSAZIONE DA USARE PER QUESTA BOLLA
488000011127     C**********************************************************
488100030131     C**!!!RECDAT        BEGSR
488200011127      *
488300030131     C**!!!              SETOFF                                       31
488400011127      * VERIFICO SE DATA VALORIZZATA E SOPRATTUTTO NUMERICA
488500020826     C**         §ASDAT    IFNE *BLANK
488600020826     C**         §ASDAT    ANDNE*ZEROS
488700011127      *
488800020826     C**                   TESTN          §ASDAT     31
488900011127      * SE 31 ACCESO VUOL DIRE CHE È NUMERICO MA VERIFICO ANCHE L'ULTIMO CARATTERE
489000020826     C**         *IN31     IFEQ *ON
489100020826     C**                   MOVE §ASDAT    W01A    1
489200020826     C**         W01A      COMP '0'                  31  31
489300020826     C**                   ENDIF
489400011127      *
489500020826     C**                   ENDIF
489600011127      * SE 31 ACCESO DATA VALIDA ALTRIMENTI PRENDO LA DATA DEL GIORNO
489700020826     C**         *IN31     IFEQ *ON
489800020826     C**                   MOVE §ASDAT    DATTAS  80
489900020826     C**                   ELSE
490000020826     C**                   Z-ADDWDTGIO    DATTAS
490100020826     C**                   ENDIF
490200020618     C*
490300011127      *
490400030203     C**!!!              ENDSR
490500990916     C****************************************************
490600990916     C** CARICAMENTO IMPORTI PASSATI DALLE DS
490700990916     C****************************************************
490800990916     C     SAVDS         BEGSR
490900990916     C*
491000990922     C     *LIKE         DEFINE    TASPOR        TASINL
491100990916     C*
491200990916     C* SALVO LE SCHIERE DELLE VARIE (SIGLE VARIE+IMPORTI)
491300990916     C                   MOVEA     SV            SVW
491400990916     C                   MOVEA     VA            VAW
491500991012     C                   CLEAR                   TASINL
491600991012     C                   CLEAR                   TASDIF
491700990916     C*
491800990916     C* RECUPERO L'IMPORTO DELL'INOLTRO SE C'E' NELLA SCHIERA DELLE VARIE
491900990916     C                   Z-ADD     1             X                 2 0
492000990923     C     §$2FIN        LOOKUP    SV(X)                                  30
492100990916     C* SE LO TROVO VALORIZZO TASINL
492200990923     C   30              Z-ADD     VA(X)         TASINL
492300991111     C  N30              Z-ADD     0             X
492400990921     C*
492500990921     C* RECUPERO L'IMPORTO DEL DIRITTO FISSO  NELLA SCHIERA DELLE VARIE
492600990921     C                   Z-ADD     1             Y                 2 0
492700990921     C     §$2VRH        LOOKUP    SV(Y)                                  30
492800990921     C* SE LO TROVO VALORIZZO TASINL
492900990923     C   30              Z-ADD     VA(Y)         TASDIF           11 3
493000990916     C*
493100990916     C                   ENDSR
493200990921     C****************************************************
493300990921     C** CONVERSIONE DELLE VARIE IN MONETA DELLA TARIFFA
493400990921     C****************************************************
493500990921     C     CNVTAS        BEGSR
493600990921     C*
493700990921     C* SE DIVISA PASSATA (DELLA BOLLA) E' DIVERSA DALLA DIVISA DELLA
493800990921     C* TARIFFA CONVERTO IN DIVISA TARIFFA
493900990921     C                   Z-ADD     1             A
494000990923    1C                   DO        20            A
494100990923    2C     VA(A)         IFNE      *ZEROS
494200990921     C                   CLEAR                   YEUR
494300011127     C                   MOVEL     DIVINP        YECDVI
494400011127     C                   MOVEL     DIVOUT        YECDVO
494500990921     C                   Z-ADD     VA(A)         YECIMI
494600991014     C   12              MOVE      '3'           YECDCO
494700991014     C  N12              MOVE      '0'           YECDCO
494800990921     C*
494900990921     C                   CALL      'YEURCO'
495000990921     C                   PARM                    YEUR
495100990921     C*
495200990923    3C     YECESI        IFEQ      ' '
495300990921     C                   Z-ADD     YECIMO        VA(A)
495400990923    3C                   END
495500990923    2C                   END
495600990923    1C                   END
495700990921     C* CONVERTO L'IMPORTO DEL PORTO
495800990921     C* SE PORTO DIVERSO DA 0
495900990923    1C     TASPOR        IFGT      0
496000990921     C                   CLEAR                   YEUR
496100011127     C                   MOVEL     DIVINP        YECDVI
496200011127     C                   MOVEL     DIVOUT        YECDVO
496300990921     C                   Z-ADD     TASPOR        YECIMI
496400991014     C   12              MOVE      '3'           YECDCO
496500991014     C  N12              MOVE      '0'           YECDCO
496600990921     C*
496700990921     C                   CALL      'YEURCO'
496800990921     C                   PARM                    YEUR
496900990921     C*
497000990923    2C     YECESI        IFEQ      ' '
497100990921     C                   Z-ADD     YECIMO        TASPOR
497200990921     C                   ELSE
497300990921     C                   MOVEL     MSG(10)       TASMSG
497400990921     C                   GOTO      FINE
497500990923    2C                   END
497600990923    1C                   ENDIF
497700990921     C* SE INOLTRO DIVERSO DA 0
497800990923    1C     TASINL        IFGT      0
497900990921     C                   CLEAR                   YEUR
498000011127     C                   MOVEL     DIVINP        YECDVI
498100011127     C                   MOVEL     DIVOUT        YECDVO
498200990921     C                   Z-ADD     TASINL        YECIMI
498300991014     C   12              MOVE      '3'           YECDCO
498400991014     C  N12              MOVE      '0'           YECDCO
498500990921     C*
498600990921     C                   CALL      'YEURCO'
498700990921     C                   PARM                    YEUR
498800990921     C*
498900990923    2C     YECESI        IFEQ      ' '
499000990921     C                   Z-ADD     YECIMO        TASINL
499100990921     C                   ELSE
499200990921     C                   MOVEL     MSG(10)       TASMSG
499300990921     C                   GOTO      FINE
499400990923    2C                   END
499500990923    1C                   ENDIF
499600990921     C* SE DIRITTO FISSO DIVERSO DA 0
499700990923    1C     TASDIF        IFGT      0
499800990921     C                   CLEAR                   YEUR
499900011127     C                   MOVEL     DIVINP        YECDVI
500000011127     C                   MOVEL     DIVOUT        YECDVO
500100990921     C                   Z-ADD     TASDIF        YECIMI
500200991014     C   12              MOVE      '3'           YECDCO
500300991014     C  N12              MOVE      '0'           YECDCO
500400990921     C*
500500990921     C                   CALL      'YEURCO'
500600990921     C                   PARM                    YEUR
500700990921     C*
500800990923    2C     YECESI        IFEQ      ' '
500900990921     C                   Z-ADD     YECIMO        TASDIF
501000990923    2C                   END
501100990923    1C                   ENDIF
501200990929     C* SE IMPORTO IMPONIBILE DIVERSO DA ZEROS
501300991014     C* LO RICALCOLO PER SICUREZZA
501400990929    1C     TASIMV        IFGT      0
501500991014     C                   CLEAR                   DSLV16
501600991014     C                   MOVEL     'T'           D17FIM
501700991014     C                   Z-ADD     TASPOR        D17POR
501800991014     C                   CALL      'FNLV16R'
501900991014     C                   PARM                    DSLV16
502000991014     C                   PARM                    DTASV
502100991014     C                   Z-ADD     O17IMV        TASIMV
502200990929    1C                   ENDIF
502300990921     C* SE QUANTITA' DA FATTURARE E' DIVERSA DA 0 E LA TARIFFA E' UNA TARIFFA
502400011127     C* A VALORE SPECIFICO DEVO CONVERTIRE IL CAMPO SE RICHIESTO
502500990921     C*
502600011127    1C     TASQFT        IFGT      0
502700011127     C     TAMFVD        ANDEQ     '4'
502800011127     C     CONQTA        ANDNE     'N'
502900011127     C                   CLEAR                   YEUR
503000011127     C                   MOVEL     DIVINP        YECDVI
503100011127     C                   MOVEL     DIVOUT        YECDVO
503200011127     C                   Z-ADD     TASQFT        YECIMI
503300011127     C   12              MOVE      '3'           YECDCO
503400011127     C  N12              MOVE      '0'           YECDCO
503500011127     C*
503600011127     C                   CALL      'YEURCO'
503700011127     C                   PARM                    YEUR
503800011127     C*
503900011127    2C     YECESI        IFEQ      ' '
504000011127     C                   Z-ADD     YECIMO        TASQFT
504100011127    2C                   END
504200011127    1C                   ENDIF
504300011127     C* SE TASIAL VALORIZZATO DEVO CONVERTIRLO SOLO SE RICHIESTO
504400011127     C*
504500011127    1C     TASIAL        IFGT      0
504600011127     C     CONIAL        ANDNE     'N'
504700011127     C                   CLEAR                   YEUR
504800011127     C                   MOVEL     DIVINP        YECDVI
504900011127     C                   MOVEL     DIVOUT        YECDVO
505000011127     C                   Z-ADD     TASIAL        YECIMI
505100011129     C   12              MOVE      '2'           YECDCO
505200011127     C  N12              MOVE      '0'           YECDCO
505300011127     C*
505400011127     C                   CALL      'YEURCO'
505500011127     C                   PARM                    YEUR
505600011127     C*
505700011127    2C     YECESI        IFEQ      ' '
505800011127     C                   Z-ADD     YECIMO        TASIAL
505900011127    2C                   END
506000011127    1C                   ENDIF
506100990921     C*
506200990921     C                   ENDSR
506300990923     C************************************************************
506400990923     C** CONVERSIONE DEL DETTAGLIO TARIFFA PARTICOLARE DI CARTELLO
506500990923     C************************************************************
506600990923     C     CONTPD        BEGSR
506700990923     C*
506800990923     C                   CLEAR                   YEUR
506900990923     C                   MOVEL     DIVCAR        YECDVI
507000990923     C                   MOVEL     §TADIV        YECDVO
507100991014     C   12              MOVE      '3'           YECDCO
507200991014     C  N12              MOVE      '0'           YECDCO
507300990923     C*
507400990923     C* CONTROLLLO IL TIPO TARIFFA SE E' VALORE OPPURE NO PER CONVERTIRE
507500990923     C* LO SCAGLIONE OPPURE NO
507600990923     C*
507700990923    1C     WTPG          IFEQ      'T'
507800990923     C     WTPG          OREQ      '4'
507900990923     C     WTPG          OREQ      'V'
508000990923     C*
508100990923     C                   Z-ADD     TPDSGL        YECIMI
508200990923     C                   CALL      'YEURCO'
508300990923     C                   PARM                    YEUR
508400990923    2C     YECESI        IFEQ      ' '
508500990923     C                   Z-ADD     YECIMO        TPDSGL
508600990923    2C                   END
508700990923     C* NON CONVERTO L'IMPORTO TARIFFA PERCHE' E' UNA PERCENTUALE
508800990923    1C                   ELSE
508900990923     C*
509000990923     C                   Z-ADD     TPDITR        YECIMI
509100990923     C                   CALL      'YEURCO'
509200990923     C                   PARM                    YEUR
509300990923    2C     YECESI        IFEQ      ' '
509400990923     C                   Z-ADD     YECIMO        TPDITR
509500990923    2C                   END
509600990923    1C                   ENDIF
509700990923     C*
509800990923     C* MINIMO
509900990923     C                   Z-ADD     TPDMIN        YECIMI
510000990923     C                   CALL      'YEURCO'
510100990923     C                   PARM                    YEUR
510200990923    1C     YECESI        IFEQ      ' '
510300990923     C                   Z-ADD     YECIMO        TPDMIN
510400990923    1C                   END
510500990923     C* MASSIMO
510600990923     C                   Z-ADD     TPDMAX        YECIMI
510700990923     C                   CALL      'YEURCO'
510800990923     C                   PARM                    YEUR
510900990923    1C     YECESI        IFEQ      ' '
511000990923     C                   Z-ADD     YECIMO        TPDMAX
511100990923    1C                   END
511200990923     C*
511300990923     C                   ENDSR
511400020826     C************************************************************
511500020826     C** CONTROLLO SE APPLICARE PESO NORMALE O VDL
511600020826     C************************************************************
511700020826     C     CTRPES        BEGSR
511800020826     C                   CLEAR                   USAPKF
511900030131     C**!!!              CLEAR                   §ASSPC
512000030131     C                   CLEAR                   TASSPC
512100030429     C                   CLEAR                   TASPKCN
512200020826     C*
512300020826     C* SE NON C'E' IL PESO VDL, PRENDO QUELLO DA FATTURARE
512400030131     C**!!!§ASPKC        IFEQ      0
512500030131     C     TASPKC        IFEQ      0
512600020826     C                   Z-ADD     TASPKF        USAPKF
512700020826     C                   GOTO      ENDPES
512800020826     C                   ELSE
512900020826     C* SE IL VDL NON + MAI DA APPLICARE --> USO IL DA FATTURARE
513000020826     C     §TATAP        IFEQ      'M'
513100020826     C                   Z-ADD     TASPKF        USAPKF
513200020826     C                   GOTO      ENDPES
513300020826     C                   ENDIF
513400020826     C                   ENDIF
513500020826     C* PESO VDL - TARA
513600150709      * verifico quale valore della tara devo recuperare
513700150709      * se data fattura <= data scadenza tara (§QTDST) calcolo la tara con la vecchia tara (§QTTCO)
513800150709      * altrimenti uso la tara attuale (§QTTCP)
513900150709     c                   if        tasdtfat <= §qtdst
514000150709     C     §QTTCO        MULT      TASNCP        WTARA
514100150709     c                   else
514200030131     C     §QTTPC        MULT      TASNCP        WTARA
514300150709     c                   endif
514400150709      *
514500030131     C     TASPKC        SUB(h)    WTARA         WNTARA
514600020826     C* SE PESO DA FATTURARE COMPRESO DA VDL E VDL-TARA
514700020826     C*  APPLICO SEMPRE IL PESO DA FATTURARE
514800020826     C*
514900020827     C     TASPKF        IFGE      WNTARA
515000030131     C**!!!TASPKF        ANDLE     §ASPKC
515100030131     C     TASPKF        ANDLE     TASPKC
515200020826     C                   Z-ADD     TASPKF        USAPKF
515300020826     C                   GOTO      ENDPES
515400020826     C                   ENDIF
515500020826     C* SE VDL-TARA > PESO FATTURARE LO APPLICO SIA PER " " CHE PER "S"
515600020827     C     WNTARA        IFGT      TASPKF
515700020827     C                   Z-ADD     WNTARA        USAPKF
515800030131     C**!!!              MOVEL     'S'           §ASSPC
515900030131     C**!!!              z-add     wntara        §ASpkc
516000030131     C                   MOVEL     'S'           TASSPC
516100030429     C                   z-add     wntara        TASpkcN
516200020826     C                   GOTO      ENDPES
516300020826     C                   ENDIF
516400020826     C* SE VDL-TARA < PESO FATTURARE LO APPLICO SOLO SE TOTALE E SE
516500020826     C* "S"
516600020827     C     WNTARA        IFLT      TASPKF
516700030131     C**!!!§ASNCP        ANDEQ     TASNCL
516800030131     C     TASNCP        ANDEQ     TASNCL
516900020826     C     §TATAP        ANDEQ     'S'
517000020827     C                   Z-ADD     WNTARA        USAPKF
517100030131     C**!!!              MOVEL     'S'           §ASSPC
517200030131     C**!!!              z-add     wntara        §ASpkc
517300030131     C                   MOVEL     'S'           TASSPC
517400030429     C                   z-add     wntara        TASpkcN
517500020826     C                   GOTO      ENDPES
517600020826     C                   ENDIF
517700020826     C* SE ANCORA A 0 (IMPOSSIBILE) USO IL PESO DA FATTURARE
517800020826     C     USAPKF        IFEQ      0
517900020826     C                   Z-ADD     TASPKF        USAPKF
518000020826     C                   ENDIF
518100020827     C**
518200020829     c     endpes        tag
518300151021      * valorizzo il flag di tipo applicazione volume
518400151021      * serve per escludere le bolle con tariffa mai applicazione VDL
518500151021      * dal calcolo del peso desunto (PRG 852)
518600151021     c                   eval      tastap = §tatap
518700151021
518800151021      * valorizzo il flag di KSC / CTR esclusione calcolo peso desunto
518900151021      * serve per escludere le bolle dei clienti che non vogliono
519000151021      * il  calcolo del peso desunto (PRG 852)
519100151021     c                   if        tamtpr = 'NO'
519200151021     c                   eval      tasepd = 'N'
519300151021     c                   else
519400151021     c                   clear                   tasepd
519500151021     c                   endif
519600151209
519700151209      * valorizzo il progressivo tariffa utilizzato in tassazione
519800151209     c                   movel     tamprg        tasprg
519900151209
520000151021
520100020829     C                   ENDSR
520200160114
520300160114      /free
520400160114       //--------------------------------------------------------------
520500160114       //?Carico la nuova cartello
520600160114       //--------------------------------------------------------------
520700160114       BEGSR NewTarCar;
520800160114
520900160114       //?Leggo TNTAM00L
521000160114         setgt (CARksc:CARctr:CARprg) TNTAM00L;
521100160114         reade (CARksc:CARctr) TNTAM00L;
521200160114         DOW not %eof(TNTAM00L);
521300160114           IF  TASdct >= TAMddt and TASdct <= TAMdst;
521400160114             CARprg = TAMprg;
521500160114             leave;
521600160114           ENDIF;
521700160114           reade (CARksc:CARctr) TNTAM00L;
521800160114         ENDDO;
521900160114
522000160114       ENDSR;
522100160114      /end-free
522200020412**
522300980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo del CONTRASSEGNO  1
522400980512DIVISA ERRATA per ricerca cambio per il calcolo dell'importo da ASSICURARE     2
522500980529Non trovata tariffa cliente                                                    3
522600980512Mancano importo da assicurare in bolla e valore merce in tariffa               4
522700980603Non trovato SCAGLIONE in tariffa                                               5
522800980512Manca la quantita' da fatturare                                                6
522900980603Non trovato SCAGLIONE in tariffa particolare
523000980603Non trovato CODICE TASSAZIONE in tariffa                                       8
523100980529Non trovata tariffa di CARTELLO                                                9
523200000125Tariffa cliente con decorrenza maggiore della data spedizione                 10
523300990917Divisa errata                                                                 11
523400000125Tariffa DPD ma bolla Italia                                                   12
523500000125Bolla import/export DPD  ma  tariffa NON DPD                                  13
523600011127Divisa tariffa del cliente non più utilizzabile                               14
523700020318Tariffa FEDEX ma bolla Italia                                                 15
523800020318Bolla import/export FEDEX ma  tariffa NON FEDEX                               16
523900020412Non trovato CODICE TASSAZIONE in tariffa PARTICOLARE                          17
524000030527Non presente tassazione per addebito insoluti                                 18
524100080221Tariffa valida ma imponibile minore di 0,000                                  19
