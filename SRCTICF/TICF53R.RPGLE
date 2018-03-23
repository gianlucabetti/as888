000100160509     H DECEDIT('0,') DATEDIT(*DMY.) Option(*nodebugio)
000200160509     H* TNSF20R  *---------------------------------------------------*
000300160509     H*             - TASSAZIONE SINGOLA SPEDIZIONE
000400160509     H*--------------------------------------------------------------*
000500160509     H* PGM STORICIZZATO NELLA SRCOLD2016 IN DATA 14/03/16           *
000600160509     H* PGM STORICIZZATO NELLA SRCOLD2003 IN DATA 21/11/03           *
000700160509     H*--------------------------------------------------------------*
000800160509     H* IN TASFLO DELLA DS DTAS E' STATA AGGIUNTA UNA DATA PER LA    *
000900160509     H* RICERCA DEL PROGRESSIVO DELLA TARIFFA. QUESTA DATA VIENE     *
001000160509     H* PASSATA DAL PROGRAMMA DI RITASSAZIONE E SOLO DA LUI          *
001100160509     H*                                                              *
001200160509     H* ATTENZIONE QUANDO VERRANNO MODIFICATE LE LUNGHEZZE DEL CODICE*
001300160509     H* TARIFFE PARTICOLARI MODIFICARE LE LUNGHEZZE DELLE SCHIERE    *
001400160509     H* E DEI CAMPI CHE LI RICEVONO                                  *
001500160509     H*--------------------------------------------------------------*
001600160509     FTABEL00F  IF   E           K DISK    USROPN
001700160509     FTNTBE01L  IF   E           K DISK    USROPN
001800160509     FTNTAM00L  IF   E           K DISK
001900160509     FTITAD00L  IF   E           K DISK
002000160509     FTITPT01L  IF   E           K DISK
002100160509     FTITPD02L  IF   E           K DISK
002200160509     FTIPMG01L  IF   E           K DISK
002300160509     FTIDPB01L  IF   E           K DISK
002400160509     FAZORG01L  IF   E           K DISK
002500160509     D CTR             S              1    DIM(50)                              CODICI TARIFFA
002600160509     D CP              S              1    DIM(50)                              TARIFFE PARTICOLARI
002700160509     D TS              S              1    DIM(10)                              TIPI SERVIZIO
002800160509     D CTS             S              2    DIM(600)                             CODICI TASSAZIONE
002900160509     D SVW             S              1    DIM(20)                              SALVO SIGLE VARIE
003000160509     D VAW             S             11  3 DIM(20)                              SALVO IMPORTI VARIE
003100160509     D CV              S              3    DIM(50)                              CAMBI
003200160509     D TIC             S              2    DIM(20)                              TIPI INCASSO
003300160509     D* VARIE LEGATE ALLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
003400160509     D T3A             S              1    DIM(20)                              VARIE X BOLLE SING.V
003500160509     D* TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
003600160509     D TBN             S              2    DIM(20)                              TIPI BOLLA NO ASSIC
003700160509     D* P.O. ESTERI senza DPD
003800160509     D EST             S              3  0 DIM(300)                             P.O. ESTERI
003900160509     D* P.O. ESTERI tutti con DPD
004000160509     D ESTtot          S              3  0 DIM(300)                             P.O. ESTERI
004100160509     D* SIGLE VARIE ESENTI IVA
004200160509     D VES             S              1    DIM(20)                              SIGLE VARIE ESENTI D
004300160509     D* SIGLE VARIE DA RESTITUIRE CON IMPORTO =0
004400160509     D SV0             S              1    DIM(20)                              SIGLE VARIE IMP0 I D
004500160509     D* TARIFFE DI CARTELLO PER OGNI NETWORK
004600160509     D NTW             S              3    DIM(10)                              NETWORK
004700160509     D TAC             S              7  0 DIM(10)                              TARIFFA CARTELLO
004800160509     D CTC             S              3  0 DIM(10)                              CODICE TARIFFA CARTE
004900160509     D PRC             S              3  0 DIM(10)                              PROGRESSIVO TARIFFA
005000160509     D DIC             S              3    DIM(10)                              DIVISA      TARIFFA
005100160509     D TIP             S              2    DIM(10)                              Tipologia FEDEX
005200160509      * date decorrenza + supplemento carburante
005300160509     D DSCA            S             13  0 DIM(999) DESCEND                     DTA DEC. + % carbur
005400160509      * filiali abilitate addebito lasciato avviso  + date inizio tassazione
005500160509     D SFilLav         S              3  0 DIM(999)                             Filiale
005600160509     D SDtaLav         S              8  0 DIM(999)                             Data tassazione
005700160509      * filiali abilitate addebito centro storico   + date inizio tassazione
005800160509     D SFilZtl         S              3  0 DIM(999)                             Filiale
005900160509     D SDtaZtl         S              8  0 DIM(999)                             Data tassazione
006000160509
006100160509
006200160509     D* MESSAGGI DI ERRORE DEI MANCA TARIFFA
006300160509     D MSG             S             78    DIM(19) CTDATA PERRCD(1)             MSG DI ERRORE
006400160509
006500160509
006600160509     D WLBDAT          DS                  INZ
006700160509     D  G02DAT                 1      8  0
006800160509     D  G02INV                 9     16  0
006900160509     D  G02ERR                17     17
007000160509     D  G02TGI                18     22  0
007100160509     D ACDS            DS                  OCCURS(100)
007200160509     D  ASGL                   1     13  3
007300160509     D  AITR                  14     24  3
007400160509     D  AMIN                  25     35  3
007500160509     D  AMAX                  36     46  3
007600160509     D  AAIN                  47     47
007700160509     D** VALORI TITPT DELLE TARIFFE PARTICOLARI
007800160509     D TARDS           DS                  OCCURS(200)
007900160509     D  TLNP                   1      3  0
008000160509     D  TCTS                   4      5
008100160509     D  TNAZ                   6      8
008200160509     D  TCAP                   9     17
008300160509     D  TCTR                  18     20  0
008400160509     D  TSCG                  21     33  3
008500160509     D  TITR                  34     44  3
008600160509     D  TINO                  45     55  3
008700160509     D  TRPV                  56     58  1
008800160509     D  TMIN                  59     69  3
008900160509     D  TMAX                  70     80  3
009000160509      * DS SUPPLEMMENTO CARBURANTE
009100160509     D WDSPSC          DS
009200160509     D  DS_DSC                 1      8  0
009300160509     D  DS_PSC                 9     13  2
009400160509     D** CODICI DI TASSAZIONE
009500160509     D DSCT          E DS                  OCCURS(600) INZ
009600160509     D*  TARIFFE PARTICOLARI
009700160509     D DS1P          E DS                  OCCURS(50) INZ
009800160509     D** SIGLE VARIE
009900160509     D DS$2          E DS
010000160509     D** VARIA ESENTE
010100160509     D DSCC          E DS
010200160509     D** 3A TIPIO BOLLE
010300160509     D DS3A          E DS
010400160509     D** CAMBI
010500160509     D DSCV          E DS                  OCCURS(50)
010600160509     D** DS DEL CAMPO TAMFLO DI TNTAM
010700160509     D DSTA01        E DS
010800160509     D** DS DEL CAMPO TPTFLO DI TITPT
010900160509     D DTPT01        E DS
011000160509      *
011100160509     D OG147         E DS
011200160509     D** DS PASSATE DAI PGM RICHIAMANTI
011300160509     D DTAS          E DS
011400160509
011500160509     D DTAS01        E DS
011600160509
011700160509     D DTASV         E DS
011800160509     D  SV                     1     20
011900160509     D                                     DIM(20)                              SIGLE VARIE
012000160509     D  VA                    21    140P 3
012100160509     D                                     DIM(20)                              IMPORTI VARIE
012200160509     D  ES                   141    160
012300160509     D*
012400160509     D TAMDS         E DS                  EXTNAME(TNTAM00F) INZ
012500160509     D**  FILE TARIFFE  - MASTER
012600160509     D KPJBA         E DS
012700160509     D OG143         E DS                  INZ
012800160509     D DSQT1         E DS                  INZ
012900160509     D* DATI VARI TASSAZIONE 1
013000160509     D DS1A          E DS                  INZ
013100160509     D** TIPI INCASSO C/A
013200160509     D DSTR          E DS                  OCCURS(50) INZ
013300160509     D** TIPI TARIFFA
013400160509     D DSTB          E DS
013500160509     D** TIPI BOLLA
013600160509     D DS5E          E DS                  OCCURS(10) INZ
013700160509     D** MONETA DI CONTO/MONETA CORRENTE
013800160509     D DGED          E DS
013900160509     D** IMPORTI STANDARD NELLE MONETE DI CONTO
014000160509     D DGEI          E DS
014100160509     D** PERCENTUALE SUPPLEMENTO CARBURANTE
014200160509     D DPSC          E DS
014300160509     D** FILIALI ADDEBITO LASCIATO AVVISO
014400160509     D DLAV          E DS
014500160509     D** FILIALI ADDEBITO CENTRO STORICO
014600160509     D DZTL          E DS
014700160509     D** CALL PGM RICERCA TABELLA
014800160509     D DSBS02        E DS                  EXTNAME(TIBS02DS)
014900160509     D** CAMBIO DIVISA
015000160509     D YEUR          E DS                  EXTNAME(YEURCODS)
015100160509     D** CALL PGM CALCOLO TOTALE IMPONIBILE
015200160509     D DSLV16        E DS                  EXTNAME(FNLV16DS)
015300160509     D** CALL PGM PER IDENTIFICARE NETWORK DELLA SPEDIZIONE
015400160509     D TRUL27        E DS                  EXTNAME(TRUL27DS1)
015500160509     D** CALL PGM PER RECUPERO TARIFFA DI CARTELLO
015600160509     D TRULC7        E DS                  EXTNAME(TRULC7DS)
015700160509     D PARAMT          DS
015800160509     D  PARCA                256    256
015900160509
016000160509     d TrulISTds     e ds
016100160509
016200160509      * -------------- variabili ------------------------------------
016300160509
016400160509     D Keytbe          S              3                                         chiave TBE
016500160509     D indx            S              3  0                                      Indice schiera
016600160509     D KX              S              3  0                                      Indice schiera
016700160509     D IMP_SCA         S             15  3                                      Impon.sup.carb.
016800160509     D PER_SCA         S              5  2                                      Perc.sup.carb.
016900160509     d applicarevers   s              1
017000160509     D NtwTam          S              3                                         Network di TNTAM
017100160509     D TipTam          S              2                                         tipo tariffa Fedex
017101170906     D UsaVlf          S                   like(tasvlf)                         Volume tassazione
017200160509
017300160509     d Sca_pb          s              3  0                                      scag.prez.base gasol
017400160509     d Sca_spe         s              3  0                                      scag.prez.gasol sped
017500160509     d Sca_sca         s              3  0                                      scaglioni scattati
017600160509     d Pmg_sgl         s             13  3                                      prezzo medio gasolio
017700160509     d Variafuel_min   s             11  3                                      Fuel minimo
017800160509
017900160509     d Sav_dsp         s              8  0                                      Salv. dta spedizione
018000160509
018100160509     d Wrksv           s             20                                         salvataggio sigle va
018200160509     d WvariaMag       s              1                                         salvataggio sigle va
018300160509     d WTSP            s                   like(taditr)
018400160509     d IMPON           s                   like(taditr)
018500160509     d WimpoMag        s                   like(taditr)
018600160509     C****************************************************************
018700160509     C*  RIEPILOGO INDICATORI
018800160509     C***************************************************************
018900160509     C* 05    - DI COMODO
019000160509     C* 07    - LETTURA GENERICO
019100160509     C* 08    - OFF MANCA TARIFFA CLIENTE
019200160509     C* 09    - LETTURA TITAD
019300160509     C* 10    - LOKUP
019400160509     C* 12    - MONETA CHE ACCETTA IMPORTI CON DECIMALI
019500160509     C* 15    - GENERICO
019600160509     C* 17    - TARIFFA SCADUTA
019700160509     C* 18    - TARIFFA NON TROVATA
019800160509     C* 28    - LETTURA TARIFFE PARTICOLARI
019900160509     C* 49    - TROVATA TARIFFA DEL CAP DI ARRIVO
020000160509     C* 56    - TROVATA TARIFFA  (TARIFFA REVERSIBILE)
020100160509     C* 57    - ASSEGNATO      (NON + IN USO *!!*)
020200160509     C* 90    - MANCA TARIFFA DI CARTELLO
020300160509     C* 94    - MANCA TARIFFA
020400160509     C* 96    - SCHIERE VARIE
020500160509     C*****************************************************************
020600160509     C     *ENTRY        PLIST
020700160509     C                   PARM                    KPJBA
020800160509     C                   PARM                    DTAS
020900160509     C                   PARM                    DTASV
021000160509
021100160509     C                   MOVEL     TASFLO        DTAS01
021200160509      * verifico data spedizione con data attivazione varia email avviso destinatario
021300160509     c***                if        §asemd = 'S' and tasdsp < 20110901
021400160509     c***                clear                   §asemd
021500160509     c***                endif
021600160509
021700160509      * pulisco i campi della DTAS che sono di solo output
021800160509     c                   clear                   tastap
021900160509     c                   clear                   tasepd
022000160509     c                   clear                   tasprg
022100160509
022200160509     C                   Z-ADD     1             CODUT             1 0
022300160509     C*---------------------------------------------------------------*
022400160509     C*
022500160509     C** ACCESSO TITPT01L
022600160509     C     KISOT         KLIST
022700160509     C                   KFLD                    TAMKSC
022800160509     C                   KFLD                    TAMCTR
022900160509     C                   KFLD                    TAMPRG
023000160509     C                   KFLD                    FISO              2
023100160509     C     KTPTC         KLIST
023200160509     C                   KFLD                    CARKSC
023300160509     C                   KFLD                    CARCTR
023400160509     C                   KFLD                    CARPRG
023500160509     C                   KFLD                    FISO
023600160509     C*
023700160509     C** ACCESSO TFTPD02L
023800160509     C     KISOD         KLIST
023900160509     C                   KFLD                    TPTKSC
024000160509     C                   KFLD                    TPTCTR
024100160509     C                   KFLD                    TPTPRG
024200160509     C                   KFLD                    TPTFTC
024300160509     C                   KFLD                    ISOCTS
024400160509     C** ACCESSO TFTPD02L
024500160509     C     KISOD2        KLIST
024600160509     C                   KFLD                    TPTKSC
024700160509     C                   KFLD                    TPTCTR
024800160509     C                   KFLD                    TPTPRG
024900160509     C                   KFLD                    TPTFTC
025000160509     C** ACCESSO TABEL X CODICE
025100160509     C     KTAB          KLIST
025200160509     C                   KFLD                    CODUT
025300160509     C                   KFLD                    COD               2
025400160509     C** ACCESSO TABEL X CODICE + CHIAVE
025500160509     C     KTABC         KLIST
025600160509     C                   KFLD                    CODUT
025700160509     C                   KFLD                    COD
025800160509     C                   KFLD                    KEY               8
025900160509     C** ACCESSO TNTAM  TARIFFA DI CARTELLO
026000160509     C     KTAMC         KLIST
026100160509     C                   KFLD                    CARKSC
026200160509     C                   KFLD                    CARCTR
026300160509     C                   KFLD                    CARPRG
026400160509     C** ACCESSO TNTAM TARIFFE SCADUTA
026500160509     C     KTAM2         KLIST
026600160509     C                   KFLD                    KSC2
026700160509     C                   KFLD                    CTR2
026800160509     C                   KFLD                    PRG2
026900160509     C** ACCESSO TNTAM X CLIENTE
027000160509     C     KTAM          KLIST
027100160509     C                   KFLD                    TMCLI
027200160509     C                   KFLD                    TMCTR
027300160509     C** ACCESO TITAD
027400160509     C     KTAD          KLIST
027500160509     C                   KFLD                    TDCLI
027600160509     C                   KFLD                    TDPRG
027700160509     C                   KFLD                    WLNP
027800160509     C                   KFLD                    COMCTS
027900160509     C                   KFLD                    COMNAZ
028000160509     C                   KFLD                    COMCAP
028100160509     C                   KFLD                    TDCTR
028200160509     C** ACCESSO TITAD PER TARIFFA REVERSIBILE
028300160509     C     KTDREV        KLIST
028400160509     C                   KFLD                    TDCLI
028500160509     C                   KFLD                    TDPRG
028600160509     C                   KFLD                    KFIL
028700160509     C                   KFLD                    COMCTS
028800160509     C                   KFLD                    COMNAZ
028900160509     C                   KFLD                    COMCAP
029000160509     C                   KFLD                    TDCTR
029100160509     C     *LIKE         DEFINE    TASPKF        USAPKF
029200160509     C     *LIKE         DEFINE    §qttpc        WTARA
029300160509     C     *LIKE         DEFINE    TASPKF        WNTARA
029400160509     C     *LIKE         DEFINE    KSCFIL        KFIL
029500160509     C     *LIKE         DEFINE    TASLNP        SAVLNP
029600160509     C     *LIKE         DEFINE    TASLNP        WLNP
029700160509     C     *LIKE         DEFINE    TASCTS        COMCTS
029800160509     C***********     -------------       ***********
029900160509     C                   MOVEL     ' '           RT
030000160509     C*
030100160509     C* TASTLA = ' ' ELABORO E CHIUDO PGM  CON RETRN
030200160509     C* TASTLA = 'L' ELABORO E CHIUDO PGM  CON LR
030300160509     C* TASTLA = 'C' NO ELAB.  CHIUDO PGM  CON LR PER CHIUSURA FILE
030400160509     C     TASTLA        IFEQ      ' '
030500160509     C     TASTLA        OREQ      'L'
030600160509     C*
030700160509     C     TASTLA        IFEQ      ' '
030800160509     C                   MOVEL     'R'           RT                1
030900160509     C                   END
031000160509     C* C A M P I   D I   O U T P U T
031100160509     C                   CLEAR                   TASERR
031200160509     C                   CLEAR                   TASMSG
031300160509     C                   CLEAR                   TASIAL
031400160509     C                   MOVEL     KPJBU         PARAMT
031500160509      * clearizzo indicatiori x manca tariffa e inoltro
031600160509     C                   SETOFF                                       9456
031700160509      * verifico data spedizione con data attivazione varia t Diritto di chiamata prenotazione
031800160509      * ritiro e packing list
031900160509     c                   if        Tasdsp < 20160101
032000160509     c                   clear                   TASPRT
032100160509     c                   clear                   TASSPL
032200160509     c                   Endif
032300160509
032400160509
032500160509     c                   Clear                   wFiso
032600160509     C** CONTROLLO LA DIVISA CHE MI VIENE PASSATA SE DIVERSA DA BLANK VERIFICO
032700160509     C** CHE SIA CORRETTA
032800160509     C     TASDIV        IFNE      *BLANKS
032900160509     C                   CLEAR                   DSLV16
033000160509     C                   MOVEL     'D'           D17FIM
033100160509     C                   MOVE      TASDIV        D17DIV
033200160509     C****                 MOVE TASDSP    D17DSP
033300160509     C                   CALL      'FNLV16R'
033400160509     C                   PARM                    DSLV16
033500160509     C                   PARM                    DTASV
033600160509     C* CONTROLLO IL CODICE DI RITORNO
033700160509     C     O17ERR        IFNE      *BLANKS
033800160509     C                   MOVEL     'E'           TASERR
033900160509     C                   MOVEL     O17MSG        TASMSG
034000160509     C                   SETON                                        94
034100160509     C                   GOTO      FINE
034200160509     C                   ENDIF
034300160509     C                   ENDIF
034400160509     C*
034500160509     C** CARICA TABELLE SOLO LA 1° VOLTA
034600160509     C     CARTBL        IFEQ      *BLANK
034700160509     C                   MOVE      '1'           CARTBL            1
034800160509     C                   EXSR      CARTAB
034900160509      * RECUPERO UDATE SOLO LA PRIMA VOLTA
035000160509     C                   EXSR      RUDATE
035100160509     C     TASMSG        IFNE      *BLANKS
035200160509     C                   SETON                                        94
035300160509     C                   GOTO      FINE
035400160509     C                   ENDIF
035500160509     C                   END
035600160509     C* R E C U P E R O   D A T A   D I   T A S S A Z I O N E
035700160509     C**!!!              EXSR      RECDAT
035800160509     C*
035900160509     C** SALVO GLI IMPORTI CHE MI VENGONO PASSATI
036000160509     C                   EXSR      SAVDS
036100160509     C*
036200160509     C                   SETOFF                                       94
036300160509      *
036400160509      * se richiamato per il solo calcolo della varia D  VADO A FINE
036500160509     C                   IF        TASSVA = 'D'
036600160509     C                   GOTO      FINE
036700160509     C                   ENDIF
036800160509      *+
036900160509     C                   MOVEL     TASTBL        TIPBOL            1
037000160509     C                   MOVEL     TASKSC        KSCFIL            3 0
037100160509     C                   MOVE      TASKSC        KSCCOD            4 0
037200160509      * da giugno 2008 viene riattivato per alcune filiali l'addebito del lasciato avviso
037300160509      * se tasric diverso da blank .. quindi la spedizione ha un evento di lasciato avviso
037400160509      * verifico se la filiale del cliente è presente nella tabella LAV oppure nella tabella
037500160509      * LAV esiste la filiale 999
037600160509    1c                   If        TasRic <> ' '
037700160509     c                   clear                   indx
037800160509     c                   eval      indx = %lookup(kscfil:sfillav)
037900160509      * se indice =  a zero cerco con la filiale 999
038000160509    2c                   if        indx = *zeros
038100160509     c                   eval      indx = %lookup(999:sfillav)
038200160509    2c                   endif
038300160509      * pulisco il flag del calcolo dell'addebito del lasciato avviso perchè filiale cliente
038400160509      * non abilitato all'addebito
038500160509    2c                   If        indx = *zeros
038600160509     c                   clear                   tasric
038700160509      * altrimenti verifico la data attivazione tassazione
038800160509   x2c                   else
038900160509      * se data spedizione inferiore all'attivazione pulisco il flag
039000160509    3c                   if        tasdsp < sdtalav(indx)
039100160509     c                   clear                   tasric
039200160509    3c                   endif
039300160509    2c                   endif
039400160509    1c                   endif
039500160509     c
039600160509     C* VEDO SE TIPO INCASSO E' INTESTATO AL MITTENTE
039700160509     C                   CLEAR                   TASBM
039800160509      * se il primo carattere del campo TASTIC è uguale a blank
039900160509      * ci troviamo a tassare una spedizione in sede in quanto
040000160509      * in TNCSB (file contrassegni ) la combinazione di tpa (tipo assegno)
040100160509      * e tpi (tipo intestazione) o fus (tipo intestazione in bolla)
040200160509      * potrebbe non esistere in tabella  1A tipo incasso c/assegno
040300160509      * a questo punto prendo solo il tipo intestazione assegno che
040400160509      * è uguale a  M per mittente  e "B" o " " per Vettore
040500160509      * In Tasbm passo solo se è uguale a M
040600160509
040700160509     C                   If        %subst(tastic:1:1) = ' '
040800160509     C                   If        %subst(tastic:2:1) = 'M'
040900160509     c                   eval      TASBM = 'M'
041000160509     c                   endif
041100160509
041200160509     c                   else
041300160509     C     TASTIC        LOOKUP    TIC                                    96
041400160509     C   96              MOVEL     'M'           TASBM             1
041500160509     c                   endif
041600160509     C** TASSAZIONE
041700160509     C                   EXSR      TARIFF
041800160509     C     FINE          TAG
041900160509     C** 94 ON - MANCA TARIFFA
042000160509     C     *IN94         IFEQ      '1'
042100160509     C* E NON C'E' TOTALE IMPONIBILE  DO ERRORE
042200160509     C     TASIMV        IFEQ      *ZEROS
042300160509     C                   MOVEL     'E'           TASERR            1
042400160509     C                   ENDIF
042500160509     C* REIMPOSTO LA DS COME PRIMA SENZA CONVERSIONI IN MONETA
042600160509     C*    MI SEMBRA INUTILE ADESSO EVENTUALMENTE LO FACCIO DOPO
042700160509     C*
042800160509     C                   ELSE
042900160509     C* CALCOLO IL TOTALE IMPONIBILE SE E' UGUALE A ZEROS
043000160509     C*
043100160509     C     TASIMV        IFEQ      *ZEROS
043200160509     C                   CLEAR                   DSLV16
043300160509     C                   MOVEL     'T'           D17FIM
043400160509     C                   Z-ADD     TASPOR        D17POR
043500160509     C                   CALL      'FNLV16R'
043600160509     C                   PARM                    DSLV16
043700160509     C                   PARM                    DTASV
043800160509     C*
043900160509     C* SE SPEDIZIONI CON DATA MAGGIORE O UGUALE AL 01012001 CALCOLO L'ADDIZIONALE DI GESTIONE
044000160509     C* SUL TOTALE IMPONIBILE E DOPO CALCOLO SUL TOTALE IMPONIBILE + ADDIZ. GESTIONE L'ISTAT
044100160509     C*
044200160509     C     TASDSP        IFGE      20010101
044300160509     C* ESEGUO SOLO SE NON HO RICHIAMATO LA TASSAZIONE PER UNA VARIA
044400160509     C*  SOLA SE VARIA NON PRESENTE NELLA TABELLA T3A (TABELLA DELLE VARIE DELLE BOLLE CHE
044500160509     C* ACCETTANO UNA SINGOLA  VARIA ESEMPIO "O" "Y" "*".....)
044600160509     C*    TASSVA        ANDEQ     ' '
044700160509      *
044800160509     C                   IF        TASSVA <> ' '
044900160509     C                   SETOFF                                       11
045000160509     C     TASSVA        LOOKUP    T3A(1)                                 11
045100160509     C                   ENDIF
045200160509      *
045300160509      *
045400160509      * SE VARIA UGUALE A BLANK                                             OPPURE
045500160509      * VARIA PRESENTE NELLA TABELLA T3A                                    OPPURE
045600160509      * TASSAZIONE 2° BOLLA     TASSVA = 'G' E TASCVAG = 'S'                OPPURE
045700160509      * VARIA UGUALE A "Z"                                                  OPPURE
045800160509      * CALCOLO A.G.+ ISTAT
045900160509      *
046000160509     C                   IF        TASSVA = ' ' OR
046100160509     C                             %FOUND       OR
046200160509     C                             (TASSVA = 'G' AND TASCVAG = 'S') OR
046300160509     C                             TASSVA = 'Z'
046400160509     C                   EXSR      ADGIST
046500160509     C                   ENDIF
046600160509      *
046700160509     C                   ENDIF
046800160509     C*
046900160509     C                   Z-ADD     O17IMV        TASIMV
047000160509     C*
047100160509     C                   ENDIF
047200160509      *
047300160509      * DIRITTO DI FATTURAZIONE lo calcolo solo alla fine della TASSAZIONE senza aggiunte dell'addi-
047400160509      *                         zionale di gestione
047500160509      *                         se totale imponibile maggiore di zero
047600160509     C                   IF        TASIMV > 0 AND
047700160509     C                             (TASSVA = §$2VRD OR TASSVDF= §$2VRD)
047800160509      *
047900160509     C                   EXSR      DIRFAT
048000160509      *
048100160509     C                   ENDIF
048200160509     C* MUOVO LA DIVISA DELLA TARIFFA
048300160509     C*  SE NON PASSATA LA VARIA O SE PASSATA E IMPORTO >0
048400160509     C*
048500160509     C     TASSVA        IFEQ      *BLANKS
048600160509     C     O17IMV        ORGT      0
048700160509     C                   MOVE      §TADIV        TASDIV
048800160509     C                   ENDIF
048900160509     C*
049000160509     C* SE SI TRATTA DI UNA VARIA DA PASSARE ANCHE A 0 CONTROLLO
049100160509     C     TASSVA        IFNE      *BLANKS
049200160509     C     O17IMV        ANDEQ     0
049300160509     C     TASSVA        LOOKUP    SV0                                    05
049400160509     C     *IN05         IFEQ      *ON
049500160509     C                   MOVEL     TASSVA        SV(1)
049600160509     C                   ENDIF
049700160509     C                   ENDIF
049800160509
049900160509      * VERIFICO SE IMPONIBILE A ZERO SENZA SIGLE DI VARIE PERCHÈ QUESTO TIPO DI CONTROLLO
050000160509      * VIENE ESEGUITO DAI PGM CHIAMANTI
050100160509     c                   clear                   wrksv
050200160509     c                   movea     sv            wrksv
050300160509     C                   IF        TASIMV = 0 AND wrkSV = *BLANKS
050400160509     C                   MOVEL     'E'           TASERR            1
050500160509     C                   MOVEL     MSG(19)       TASMSG
050600160509     C                   SETON                                        94
050700160509     C                   GOTO      FINE
050800160509     C                   ENDIF
050900160509     C*
051000160509     C                   ENDIF
051100160509     C* CONTROLLO LA DIVISA IN BASE ALLA DATA TASSAZIONE SE DA CONVERTIRE OPPURE NO
051200160509     C* SE STO TASSANDO CON DATA > DEL 2001 UNA BOLLA CON DATA < DEL 2002 E LA DIVISA NON E'
051300160509     C* UGUALE ALLA MONETA DI CONTO AZIENDALE CONVERTO TUTTI GLI IMPORTI ANCHE LA QTA DA
051400160509     C* FATTURARE NELLA DIVISA DI CONTO
051500160509     C*
051600160509     C**         DATTAS    IFGE 20020101
051700160509     C     TASDIV        IFNE      §GEDCN
051800160509     C     TASDSP        ANDLT     20020101
051900160509     C* CONVERTO
052000160509     C                   MOVE      TASDIV        DIVINP
052100160509     C                   MOVE      §GEDCN        DIVOUT
052200160509     C                   MOVE      'S'           CONQTA
052300160509     C                   MOVE      'S'           CONIAL
052400160509     C*
052500160509     C* RECUPERO LE CARATTERISTICHE DELLA MONETA DI CONTO
052600160509     C                   SETOFF                                       12
052700160509     C                   Z-ADD     1             K
052800160509     C     §GEDCN        LOOKUP    CV(K)                                  10
052900160509     C     *IN10         IFEQ      *ON
053000160509     C     K             OCCUR     DSCV
053100160509     C     §CVFDC        IFEQ      'S'
053200160509     C* DIVISA CHE ACCETTA DECIMALI
053300160509     C                   SETON                                        12
053400160509     C                   ENDIF
053500160509     C                   ENDIF
053600160509     C*
053700160509     C                   EXSR      CNVTAS
053800160509     C* VALORIZZO  LA DIVISA
053900160509     C                   MOVEL     §GEDCN        TASDIV
054000160509     C*
054100160509     C                   ENDIF
054200160509     C*
054300160509     C                   END
054400160509     C*
054500160509     C     RT            IFEQ      'R'
054600160509     C                   RETURN
054700160509     C                   ELSE
054800160509     C* CHIUDO PGM TRUL27R1
054900160509     C                   CLEAR                   TRUL27
055000160509     C                   MOVE      'C'           I27TLA
055100160509     C                   CALL      'TRUL27R1'
055200160509     C                   PARM                    TRUL27
055300160509     C* CHIUDO PGM TRULC7R
055400160509     C                   CLEAR                   TRULC7
055500160509     C                   MOVE      'C'           IC7TLA
055600160509     C                   CALL      'TRULC7R'
055700160509     C                   PARM                    TRULC7
055800160509     C                   SETON                                        LR
055900160509     C                   END
056000160509     C************       -----------       *************
056100160509     C**
056200160509     C******************************************************
056300160509     C**  CALCOLA TASSAZIONE BOLLA
056400160509     C******************************************************
056500160509     C     TARIFF        BEGSR
056600160509     C                   MOVEL     'I'           WBOEST
056700160509     C                   CLEAR                   WBODPD
056800160509     C                   CLEAR                   WBOFED
056900160509     C                   CLEAR                   tasfie
057000160509     C                   CLEAR                   TRUL27
057100160509     C                   CLEAR                   TRULC7
057200160509     C                   SETOFF                                       12
057300160509     C*
057400160509     C* VERIFICO SE CODICE APPARTIENE ALLA SDI
057500160509     C*
057600160509     C                   CLEAR                   KSDIT
057700160509     C     KSCFIL        CHAIN     AZORG01L                           07
057800160509     C     *IN07         IFEQ      *OFF
057900160509     C                   MOVEL     ORGDIT        KSDIT             3
058000160509     C                   ENDIF
058100160509     C**
058200160509     C**
058300160509     C* PER 8888 E 9999 --> CERCO LA TARIFFA DI CARTELLO RELATIVA AL TIPO DI SPEDIZIONE
058400160509    1C     KSCCOD        IFEQ      8888
058500160509     C     KSCCOD        OREQ      9999
058600160509      * CERCO LA TARIFFA DI CARTELLO in base al network della spedizione
058700160509     C** PER VERIFICARE SE SONO BOLLA DPD FEDEX O ESTERA CHIAMO TRUL27
058800160509     C**  SENZA LA DATA
058900160509     C                   CLEAR                   TRUL27
059000160509     C                   MOVEL     TASTSP        I27TSP
059100160509     C                   MOVEL     TASLNA        I27LNA
059200160509     C                   MOVEL     TASLNP        I27LNP
059300160509     C                   MOVEL     TASKSC        I27CLI
059400160509     c                   Movel     Tasctr        I27Ctb
059500160509     C                   CALL      'TRUL27R1'
059600160509     C                   PARM                    TRUL27
059700160509      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
059800160509     C     O27ERR        IFEQ      *BLANKS
059900160509     C     O27FIE        IFEQ      'E'
060000160509     C                   MOVEL     'E'           WBOEST            1
060100160509     C                   ENDIF
060200160509     C*
060300160509     C     O27FIE        IFEQ      'D'
060400160509     C                   MOVEL     'S'           WBODPD            1
060500160509     C                   MOVEL     'I'           WBOEST            1
060600160509     C                   ENDIF
060700160509     C*
060800160509     C     O27FIE        IFEQ      'X'
060900160509     C     O27NTW        ANDEQ     'DPD'
061000160509     C                   MOVEL     'S'           WBODPD
061100160509     C                   MOVEL     'I'           WBOEST
061200160509     C                   ENDIF
061300160509     C*
061400160509     C     O27FIE        IFEQ      'X'
061500160509     C     O27NTW        ANDEQ     'EEX'
061600160509     C                   MOVEL     'E'           WBOEST
061700160509     C                   ENDIF
061800160509     C*
061900160509     C     O27FIE        IFEQ      'F'
062000160509     C     O27FIE        orEQ      'M'
062100160509     C     O27FIE        orEQ      'N'
062200160509     C                   MOVEL     'S'           WBOFED            1
062300160509     C                   MOVEL     'E'           WBOEST            1
062400160509     C                   ENDIF
062500160509     C                   ELSE
062600160509     C                   SETON                                        94
062700160509     C                   MOVEL     O27MSG        TASMSG
062800160509     C                   GOTO      FINE
062900160509     C                   ENDIF
063000160509     C**
063100160509     C* LO PASSO IN OUTPUT (ha senso solo per fedex)
063200160509     c                   MOVEL     O27FIE        TASFIE
063300160509      * Per tutte le tipologie di bolla FedEx imposto sempre 'F' xchè il
063400160509      * TNSF07 testa 'F' x definire che è una bolla FedEx (descrizione varie)
063500160509     c                   If        O27Fie = 'M' or O27Fie = 'N'
063600160509     c                   Movel     'F'           TAsFie
063700160509     c                   EndIf
063800160509     c
063900160509     c                   eval      NtwTam = O27ntw
064000160509     c                   clear                   tiptam
064100160509      * imposto il tipo tariffa fedex
064200160509     c                   if        NtwTam = 'FED' and
064300160509     c                             o27fie = 'N'
064400160509     c                   eval      TIPTam = 'FD'
064500160509     c                   endif
064600160509
064700160509     c                   if        NtwTam = 'FED' and
064800160509     c                             (o27fie = 'M' or o27fie = 'F')
064900160509     c                   eval      TIPTam = 'FM'
065000160509     c                   endif
065100160509
065200160509
065300160509     C                   EXSR      CERTCA
065400160509     C                   Z-ADD     CARKSC        KSC2
065500160509     C                   Z-ADD     CARCTR        CTR2
065600160509     C                   Z-ADD     CARCTR        CODCTR
065700160509     C                   Z-ADD     CARPRG        PRG2
065800160509      *
065900160509     C     KTAM2         CHAIN     TNTAM00L                           94
066000160509      *
066100160509     C** NON TROVATA (IMPOSSIBILE!!!)
066200160509    2C     *IN94         IFEQ      *ON
066300160509     C                   MOVEL     MSG(10)       TASMSG
066400160509     C                   GOTO      FINE
066500160509    2C                   ENDIF
066600160509     C* KSCCOD NON LO IMPOSTO PERCHE'PER I 9999 VOGLIO CHE RIMANGA TALE
066700160509     C**
066800160509   X1C                   ELSE
066900160509     C** ACCESSO TNTAM
067000160509     C                   Z-ADD     TASCTR        CODCTR            3 0
067100160509     C                   Z-ADD     TASKSC        CODCLI            7 0
067200160509     C**
067300160509     C                   Z-ADD     CODCLI        TMCLI             7 0
067400160509     C                   Z-ADD     CODCTR        TMCTR             3 0
067500160509     C                   EXSR      CERTAM
067600160509     C** SE 90 /94 SONO SPENTI NON HO TROVATO LA TARIFFA DI CARTELLO RELATIVA ALLA TARIFFA
067700160509     C** DEL CLIENTE   (QUESTO CASO L'HO ELIMINATO)
067800160509     C     *IN90         IFEQ      *OFF
067900160509     C     *IN94         ANDEQ     *OFF
068000160509     C*** NON SO COSA FARE ANCHE PERCHE' E' IMPOSSIBILE MA VADO A FINE LO STESSO
068100160509     C                   SETON                                        94
068200160509     C                   GOTO      FINE
068300160509     C                   ENDIF
068400160509     C** NON C'E' TARIFFA CLIENTE
068500160509    2C     *IN94         IFEQ      *ON
068600160509     C**
068700160509     C* SE NON HO CHIAMATO PER UNA VARIA  -- > ERRORE
068800160509     C* NON DO ERRORE SOLO PER LA VARIA "R"
068900160509    3C     TASSVA        IFNE      'R'
069000160509     C                   GOTO      FINE
069100160509   X3C                   ELSE
069200160509     C**
069300160509     C** SE HO CHIAMATO PER LA VARIA 'R' --> IMPOSTO LA TAR.CARTELLO
069400160509     C* IMPOSTO ANCHE KSCCOD perche' voglio che me lo tratti come
069500160509     C*  cliente generico da tariffa di cartello per l'assicurazione
069600160509     c*  Non ho problemi per i 9999 èerche' avendoli gia' impostati
069700160509     c*  la prima volta che fa  certam trova la tariffa (di cartello)
069800160509     C                   MOVE      8888          KSCCOD
069900160509     C                   CLEAR                   TRUL27
070000160509     C                   MOVEL     TASTSP        I27TSP
070100160509     C                   MOVEL     TASLNA        I27LNA
070200160509     C                   MOVEL     TASLNP        I27LNP
070300160509     C                   MOVEL     TASKSC        I27CLI
070400160509     c                   Movel     Tasctr        I27Ctb
070500160509     C                   CALL      'TRUL27R1'
070600160509     C                   PARM                    TRUL27
070700160509      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
070800160509     C     O27ERR        IFEQ      *BLANKS
070900160509     C     O27FIE        IFEQ      'E'
071000160509     C                   MOVEL     'E'           WBOEST            1
071100160509     C                   ENDIF
071200160509     C*
071300160509     C     O27FIE        IFEQ      'D'
071400160509     C                   MOVEL     'S'           WBODPD            1
071500160509     C                   MOVEL     'I'           WBOEST            1
071600160509     C                   ENDIF
071700160509     C*
071800160509     C     O27FIE        IFEQ      'X'
071900160509     C     O27NTW        ANDEQ     'DPD'
072000160509     C                   MOVEL     'S'           WBODPD
072100160509     C                   MOVEL     'I'           WBOEST
072200160509     C                   ENDIF
072300160509     C*
072400160509     C     O27FIE        IFEQ      'X'
072500160509     C     O27NTW        ANDEQ     'EEX'
072600160509     C                   MOVEL     'E'           WBOEST
072700160509     C                   ENDIF
072800160509     C*
072900160509     C     O27FIE        IFEQ      'F'
073000160509     C     O27FIE        orEQ      'M'
073100160509     C     O27FIE        orEQ      'N'
073200160509     C                   MOVEL     'S'           WBOFED            1
073300160509     C                   MOVEL     'E'           WBOEST            1
073400160509     C                   ENDIF
073500160509     C                   ELSE
073600160509     C                   SETON                                        94
073700160509     C                   MOVEL     O27MSG        TASMSG
073800160509     C                   GOTO      FINE
073900160509     C                   ENDIF
074000160509     C**
074100160509     C* LO PASSO IN OUTPUT
074200160509     c                   MOVEL     O27FIE        TASFIE
074300160509      * Per tutte le tipologie di bolla FedEx imposto sempre 'F' xchè il
074400160509      * TNSF02 testa 'F' x definire che è una bolla FedEx (descrizione varie)
074500160509     c                   If        O27Fie = 'M' or O27Fie = 'N'
074600160509     c                   Movel     'F'           TAsFie
074700160509     c                   EndIf
074800160509     c
074900160509     c                   eval      NtwTam = O27ntw
075000160509     c                   clear                   tiptam
075100160509      * imposto il tipo tariffa fedex
075200160509     c                   if        NtwTam = 'FED' and
075300160509     c                             o27fie = 'N'
075400160509     c                   eval      TIPTam = 'FD'
075500160509     c                   endif
075600160509
075700160509     c                   if        NtwTam = 'FED' and
075800160509     c                             (o27fie = 'M' or o27fie = 'F')
075900160509     c                   eval      TIPTam = 'FM'
076000160509     c                   endif
076100160509
076200160509
076300160509     C* CERCO LA CARTELLO IN BASE ALLE CARATTERISTICHE DELLA SPEDIZIONE EW DEL CLIENTE
076400160509     C                   EXSR      CERTCA
076500160509     C                   Z-ADD     CARKSC        KSC2
076600160509     C                   Z-ADD     CARCTR        CTR2
076700160509     C                   Z-ADD     CARCTR        CODCTR
076800160509     C                   Z-ADD     CARPRG        PRG2
076900160509     C*
077000160509     C     KTAM2         CHAIN     TNTAM00L                           94
077100160509     C** NON TROVATA (IMPOSSIBILE!!!)
077200160509    4C     *IN94         IFEQ      *ON
077300160509     C                   MOVEL     MSG(10)       TASMSG
077400160509     C                   GOTO      FINE
077500160509    4C                   ENDIF
077600160509     C* MI CARICO LA DIVISA DELLA CARTELLO
077700160509     C                   MOVEL     TAMFLO        DSTA01
077800160509     C                   MOVEL     §TADIV        DIVCAR
077900160509     C*
078000160509    3C                   ENDIF
078100160509    2C                   ENDIF
078200160509    1C                   ENDIF
078300160509     C**
078400160509     C                   Z-ADD     TAMKSC        TDCLI             7 0
078500160509     C                   Z-ADD     TAMPRG        TDPRG             3 0
078600160509     C                   Z-ADD     TAMCTR        TDCTR             3 0
078700160509     C* CAMPO DI OUTPUT
078800160509     C                   MOVEL     TAMBAP        TASBAP
078900160509     C                   MOVEL     TAMFLO        DSTA01
079000160509     C*
079100160509     C* SE BOLLA DPD E TARIFFA DI CARTELLO --> ERRORE SE NON RICHIAMO
079200160509     C*  PER UNA VARIA E SE LA CARTELLO NON E' UNA CARTELLO DPD
079300160509     C     KSCCOD        IFEQ      8888
079400160509     C     KSCCOD        OREQ      9999
079500160509     C     TASSVA        IFNE      'R'
079600160509     C     WBODPD        ANDEQ     'S'
079700160509      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
079800160509     C     §TADPD        ANDNE     'S'
079900160509     C                   SETON                                        94
080000160509     C                   MOVEL     MSG(13)       TASMSG
080100160509     C                   GOTO      FINE
080200160509     C                   ENDIF
080300160509     C     TASSVA        IFNE      'R'
080400160509     C     WBOFED        ANDEQ     'S'
080500160509      * ADESSO CHE CI DOVREBBE ESSERE UNA CARTELLO PER OGNUNA VERIFICO SE CATELLO DPD
080600160509     C     §TAFED        ANDNE     'S'
080700160509     C                   SETON                                        94
080800160509     C                   MOVEL     MSG(16)       TASMSG
080900160509     C                   GOTO      FINE
081000160509     C                   ENDIF
081100160509     C                   ENDIF
081200160509     C*
081300160509     C* SE TARIFFA O CLIENTE TARIFFA DPD E BOLLA NON LO E'--> MANCA TAR
081400160509     C     §TADPD        IFEQ      'S'
081500160509     C     WBODPD        IFEQ      ' '
081600160509     C                   SETON                                        94
081700160509     C                   MOVEL     MSG(12)       TASMSG
081800160509     C                   GOTO      FINE
081900160509     C                   ENDIF
082000160509     C                   ELSE
082100160509     C     WBODPD        IFEQ      'S'
082200160509     C                   SETON                                        94
082300160509     C                   MOVEL     MSG(13)       TASMSG
082400160509     C                   GOTO      FINE
082500160509     C                   ENDIF
082600160509     C                   ENDIF
082700160509     C* SE TARIFFA O CLIENTE TARIFFA FED E BOLLA NON LO E'--> MANCA TAR
082800160509     C     §TAFED        IFEQ      'S'
082900160509     C     WBOFED        IFEQ      ' '
083000160509     C                   SETON                                        94
083100160509     C                   MOVEL     MSG(15)       TASMSG
083200160509     C                   GOTO      FINE
083300160509     C                   ENDIF
083400160509     C                   ELSE
083500160509     C     WBOFED        IFEQ      'S'
083600160509     C                   SETON                                        94
083700160509     C                   MOVEL     MSG(16)       TASMSG
083800160509     C                   GOTO      FINE
083900160509     C                   ENDIF
084000160509     C                   ENDIF
084100160509     C**
084200160509     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
084300160509     C     §TADIV        IFEQ      *BLANK
084400160509     C                   MOVE      'ITL'         §TADIV
084500160509     C                   ENDIF
084600160509      * VERIFICO IN BASE ALLA DATA TASSAZIONE E DATA SPEDIZIONE SE LA DIVISA DELLA TARIFFA
084700160509      * E' CORRETTA
084800160509      *
084900160509      * SE STO TASSANDO con data > del 2001 un bolla con data > del 2001 E LA DIVISA NON E'
085000160509      * UGUALE ALLA MONETA DI CONTO AZIENDALE
085100160509     C***        DATTAS    IFGT 20011231
085200160509     C     §TADIV        IFNE      §GEDCN
085300160509     C     TASDSP        ANDGT     20011231
085400160509      * MANCA TARIFFA
085500160509     C                   SETON                                        94
085600160509     C                   MOVEL     MSG(14)       TASMSG
085700160509     C                   GOTO      FINE
085800160509     C                   ENDIF
085900160509     C*
086000160509     C* RECUPERO LE CARATTERISTICHE DELLA DIVISA
086100160509     C                   Z-ADD     1             K
086200160509     C     §TADIV        LOOKUP    CV(K)                                  10
086300160509     C     *IN10         IFEQ      *ON
086400160509     C     K             OCCUR     DSCV
086500160509     C     §CVFDC        IFEQ      'S'
086600160509     C* DIVISA CHE ACCETTA DECIMALI
086700160509     C                   SETON                                        12
086800160509     C                   ENDIF
086900160509     C                   ENDIF
087000160509     C** RICERCA TNTAM
087100160509     C** VERIFICO SE LA MONETA PASSATA E'= A QUELLA  DELLA TARIFFA
087200160509     C     TASDIV        IFNE      §TADIV
087300160509     C* E LA TARIFFA PASSAT NON E' UGUALE A BLANK
087400160509     C     TASDIV        ANDNE     *BLANKS
087500160509     C** CONVERTO GLI IMPORTI NELLA MONETA DELLA TARIFFA CLIENTE
087600160509     C*  ANCHE LE VARIE
087700160509     C*
087800160509     C* PASSO LE MONETE PER LA CONVERSIONE ED IN PIÙ IL FLAG SE DEVO
087900160509     C* CONVERTIRE LA QUANTITA' DA FATTURARE OPPURE NO
088000160509     C* NEL CASO IN CUI CONVERTO NELLA MONETA DELLA TARIFFA
088100160509     C* NON E' NECESSARIO CONVERTIRE LA QTA DA FATTURARE E
088200160509     C* L'IMPORTO D'ASSICURARE CALCOLATO IN TASSAZIONE
088300160509     C                   MOVE      TASDIV        DIVINP            3
088400160509     C                   MOVE      §TADIV        DIVOUT            3
088500160509     C                   MOVE      'N'           CONQTA            1
088600160509     C                   MOVE      'N'           CONIAL            1
088700160509     C                   EXSR      CNVTAS
088800160509     C*
088900160509     C                   ENDIF
089000160509     C*------------------------------------------------------
089100160509     C* SE IMPONIBILE VALORIZZATO VADO A FINE
089200160509     C     TASIMV        IFNE      *ZEROS
089300160509     C* E NON E' STATA RICHIESTA NESSUNA VARIA
089400160509     C     TASSVA        ANDEQ     ' '
089500160509     C                   GOTO      FINE
089600160509     C                   ENDIF
089700160509     C*------------------------------------------------------
089800160509     C* VERIFICO SE APPLICARE IL PESO VDL O MENO
089900160509     C                   EXSR      CTRPES
089901170906     C* VERIFICO SE APPLICARE IL VOLUME VDL O MENO
089902170906     C                   EXSR      CTRVOL
090000160509     C*------------------------------------------------------
090100160509     C** PORTO O INOLTRO PRETASSATO
090200160509     C**  OPPURE DEVO CALCOLARE UNA VARIA SPECIFICA --> NON CERCO TITAD
090300160509     C     TASPOR        IFGT      0
090400160509     C     TASINL        ORGT      0
090500160509     C     TASSVA        ORNE      ' '
090600160509     C**                   Z-ADD0         TASPVL
090700160509     C                   GOTO      TASSAT
090800160509     C                   END
090900160509     C*
091000160509     C                   MOVEL     CODCTR        UNOCTR            1 0
091100160509     C* QUANTITA' DA FATTURARE
091200160509     C     UNOCTR        IFEQ      5
091300160509     C     TASQFT        COMP      0                                      94
091400160509     C   94              MOVEL     MSG(6)        TASMSG
091500160509     C   94              GOTO      FINE
091600160509     C                   END
091700160509     C** TARIFFA A QUANTITA' - MANCA Q.TA' DA FATTURARE
091800160509     C     UNOCTR        IFEQ      4
091900160509     C     TAMFVD        IFEQ      *BLANKS
092000160509     C     TAMFVD        OREQ      '2'
092100160509     C     TAMFVD        OREQ      '4'
092200160509     C     TASQFT        COMP      0                                      94
092300160509     C   94              MOVEL     MSG(6)        TASMSG
092400160509     C   94              GOTO      FINE
092500160509     C                   GOTO      POIQTF
092600160509     C                   END
092700160509     C** TARIFFA A VALORE FLAG 2 E BLANK
092800160509     C     TAMFVD        IFEQ      '1'
092900160509     C     TASQFT        CABGT     0             POIQTF
093000160509     C                   END
093100160509     C*
093200160509     C                   Z-ADD     TASIAS        TASQFT
093300160509     C                   Z-ADD     1             K
093400160509     C     TASVAS        LOOKUP    CV(K)                                  05
093500160509     C     *IN05         IFEQ      *OFF
093600160509     C                   SETON                                        94
093700160509     C                   MOVEL     MSG(2)        TASMSG
093800160509     C                   GOTO      FINE
093900160509     C                   ENDIF
094000160509     C**
094100160509     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA
094200160509     C                   CLEAR                   YEUR
094300160509     C                   MOVEL     TASVAS        YECDVI
094400160509     C                   MOVEL     §TADIV        YECDVO
094500160509     C                   Z-ADD     TASQFT        YECIMI
094600160509     C   12              MOVE      '3'           YECDCO
094700160509     C  N12              MOVE      '0'           YECDCO
094800160509     C*
094900160509     C                   CALL      'YEURCO'
095000160509     C                   PARM                    YEUR
095100160509     C*
095200160509     C     YECESI        IFEQ      ' '
095300160509     C                   Z-ADD     YECIMO        TASQFT
095400160509     C                   END
095500160509     C****                 ENDIF
095600160509     C****       K         OCUR DSCV
095700160509     C****       TASVAS    IFNE §CVITA
095800160509     C****       TASQFT    MULT §CVCMB    TASQFT
095900160509     C****                 END
096000160509     C* CAMBIO
096100160509     C     TASQFT        IFEQ      0
096200160509     C                   EXSR      CALVAS
096300160509     C                   Z-ADD     IMPVAS        TASQFT
096400160509     C                   END
096500160509     C     TASQFT        COMP      0                                      94
096600160509     C   94              MOVEL     MSG(6)        TASMSG
096700160509     C   94              GOTO      FINE
096800160509     C** TARIFFA A VALORE FLAG 1 E 3 - NON ESISTE VALORE ASSICURATIVO
096900160509     C                   END
097000160509     C     POIQTF        TAG
097100160509     C* MI SERVE MINTAS PER IL CARICAMENTO DELLO SCAGLIONE DEL
097200160509     C*  MINIMO TASSABILE
097300160509     C                   Z-ADD     1             A
097400160509     C                   MOVEL     UNOCTR        WTPG              1
097500160509     C     WTPG          LOOKUP    CTR(A)                                 05
097600160509     C   05A             OCCUR     DSTR
097700160509     C   05              Z-ADD     §TRATA        MINTAS
097800160509     C*
097900160509     C** RECUPERO CODICE NAZIONE MITTENTE E DESTINATARIO DAL CODIEC TASSAZIONE
098000160509     C*
098100160509     C***                  Z-ADD1         A
098200160509     C***        TASCTS    LOKUPCTS,A                    05
098300160509     C***05      A         OCUR DSCT
098400160509     C***05                MOVEL§CTNAR    TASNAD  3
098500160509     C*
098600160509     C***                  Z-ADD1         A
098700160509     C***        TASMCT    LOKUPCTS,A                    05
098800160509     C***05      A         OCUR DSCT
098900160509     C***05                MOVEL§CTNAR    TASNAM  3
099000160509     C*
099100160509     C** RICERCA TITAD
099200160509     C                   EXSR      CARTAR
099300160509     C*
099400160509     C** PESO TASSABILE E QUANTITA' DA FATTURARE
099500160509     C                   MOVEL     UNOCTR        WTPG              1
099600160509     C                   CLEAR                   WRPV
099700160509     C                   CLEAR                   RAPPV
099800160509     C                   MOVEL     'TAD'         WDET              3
099900160509     C                   Z-ADD     TAMARL        WARL
100000160509     C                   Z-ADD     TAMARF        WARF
100100160509     C                   Z-ADD     TAMARO        WARO
100200160509     C                   EXSR      CALCOL
100300160509     C                   Z-ADD     ISOPES        PESTAS
100400160509     C*--------------------------------------------------------------
100500160509     C*  PESO TASSABILE PER TARIFFE A PESO
100600160509     C                   EXSR      CALPT
100700160509     C*--------------------------------------------------------------
100800160509     C     TASSAT        TAG
100900160509     C** ESEGUE TASSAZIONE BOLLA
101000160509     C                   EXSR      TASSA
101100160509     C** MANCA TARIFFA
101200160509     C   94              GOTO      FINE
101300160509     C                   ENDSR
101400160509     C******************************************************
101500160509     C** GUIDA PER TASSAZIONE SPEDIZIONE
101600160509     C******************************************************
101700160509     C     TASSA         BEGSR
101800160509     C                   SETOFF                                       94
101900160509     C                   clear                   WvariaMag
102000160509     c                   clear                   WImpoMag
102100160509
102200160509     C** FLAG PER APPLICAZIONE INOLTRO
102300160509     C*!!* PRIMA SI FACEVA COSÌ *!!*
102400160509     C*!!* ** ASSEGNATI-MITTENTE FRANCHI-DESTINATARIO
102500160509     C*!!* ** PER 99 SEMPRE DESTINATARIO
102600160509     C*!!*           TIPBOL    COMP 'A'                      57
102700160509     C*!!*   57      KSCCOD    COMP 9999                 5757
102800160509     C*!!*   57                MOVELTASFAP    FLGINO  1
102900160509     C*!!*  N57                MOVELTASFIN    FLGINO
103000160509     C*!!*
103100160509     C*!!* ORA CONTROLLO LA TARIFFA :
103200160509     C*!!* SE E' USO LA TARIFFA REVERSIBILE (56 ON)  PRENDO COME INOLTRO L'ANTEPORTO TASFAP
103300160509     C*!!* SE USO LA TARIFFA NORMALE (56 OFF) USO L'INOLTRO TASFIN
103400160509     C*!!*
103500160509     C  N56              MOVEL     TASFIN        FLGINO            1
103600160509     C   56              MOVEL     TASFAP        FLGINO
103700160509     C*!!*
103800160509     C*!!*   FINE   *!!*
103900160509     C**
104000160509     C** SE DEVO CALCOLARE UNA VARIA VADO SUBITO DA QUELLA
104100160509     C     TASSVA        IFNE      '  '
104200160509     C     *LIKE         DEFINE    TASQFT        WQFT
104300160509     C** VARIA 'G' --> CONTRASSEGNO
104400160509     C     TASSVA        IFEQ      §$2VRG
104500160509     C                   GOTO      VARIAG
104600160509     C                   ENDIF
104700160509     C** VARIA 'R' --> ASSICURAZIONE PER CONTO
104800160509     C     TASSVA        IFEQ      §$2VRR
104900160509     C                   GOTO      VARIAR
105000160509     C                   ENDIF
105100160509     C** VARIA 'O' --> RICERCA DOCUMENTI
105200160509     C     TASSVA        IFEQ      §$2VRO
105300160509     C                   GOTO      VARIAO
105400160509     C                   ENDIF
105500160509     C** VARIA 'Y' --> RITIRI ANNULLATI
105600160509     C     TASSVA        IFEQ      §$2VRY
105700160509     C                   GOTO      VARIAY
105800160509     C                   ENDIF
105900160509     C** VARIA '*' --> DIROTTAMENTI
106000160509     C     TASSVA        IFEQ      §$2AST
106100160509     C                   GOTO      VARAST
106200160509     C                   ENDIF
106300160509     C** VARIA 'W' --> RECAPITO C/ASSEGNI
106400160509     C     TASSVA        IFEQ      §$2VRW
106500160509     C                   GOTO      VARVRW
106600160509     C                   ENDIF
106700160509     C** VARIA 'M' --> ADDEBITO INSOLUTI INTERESSI DI MORA
106800160509     C     TASSVA        IFEQ      §$2VRM
106900160509     C                   GOTO      VARVRM
107000160509     C                   ENDIF
107100160509     C** VARIA 'T' --> ORM COMMISSIONATO
107200160509     C     TASSVA        IFEQ      §$2VRT
107300160509     C                   GOTO      VARVRT
107400160509     C                   ENDIF
107500160509     C** VARIA 'a' --> P.O.D IMMAGE
107600160509     C     TASSVA        IFEQ      §$2POD
107700160509     C                   GOTO      VARPOD
107800160509     C                   ENDIF
107900160509     C** VARIA 'D' --> DIRITTO DI FATTURAZIONE
108000160509     C     TASSVA        IFEQ      §$2VRD
108100160509     C                   GOTO      FINE
108200160509     C                   ENDIF
108300160509     C** VARIA 'Z' --> ADDIZIONALE DI GESTIONE + ISTAT
108400160509     C     TASSVA        IFEQ      §$2VRZ
108500160509     C                   GOTO      FINE
108600160509     C                   ENDIF
108700160509     C                   ENDIF
108800160509     C**
108900160509     C** SE IL PORTO O L'INOLTRO SONO PRETASSATI
109000160509     C** DEBBO ESCLUDERE LA FASE DI CALCOLO TARIFFA
109100160509     C     TASPOR        CABGT     0             DOPOIN
109200160509     C     TASINL        CABGT     0             DOPOIN
109300160509     C*****
109400160509     C** CALCOLO    P O R T O     ED    I N O L T R O
109500160509     C*****
109600160509     C**
109700160509     C                   DO        200           A
109800160509     C     A             OCCUR     TARDS
109900160509     C     TSCG          IFGE      PESTAS
110000160509     C                   GOTO      ENDTAS
110100160509     C                   END
110200160509     C                   END
110300160509     C** NON TROVATO SCAGLIONE ESATTO
110400160509     C     TSCG          IFLT      PESTAS
110500160509     C                   SETON                                        94
110600160509     C                   MOVEL     MSG(5)        TASMSG
110700160509     c                   eval      %subst(tasmsg: 37)= %trim(%editc(PESTAS:'3'))
110800160509     C                   GOTO      FINE
110900160509     C                   END
111000160509     C     ENDTAS        TAG
111100160509     C** MI SALVO LA POSIZIONE DELLO SCAGLIO TROVATO CHE MI
111200160509     C*  SERVE PER IL CALCOLO TARIFFA CON GARANTITO MAX SCAGLIONE PREC
111300160509     C     *LIKE         DEFINE    A             WA
111400160509     C                   Z-ADD     A             WA
111500160509     C**
111600160509     C* CALCOLO PER QUALE VALORE MOLTIPLICARE LA TARIFFA
111700160509     C                   Z-ADD     PESTAS        WPTAS3
111800160509     C                   EXSR      CALQLI
111900160509     C**
112000160509     C* MODO APPLICAZIONE TARIFFA
112100160509     C                   MOVEL     TAMFLO        DSTA01
112200160509     C* SE DIVISA DELLA TARIFFA E' UGUALE A BLANK METTO ITL
112300160509     C     §TADIV        IFEQ      *BLANK
112400160509     C                   MOVE      'ITL'         §TADIV
112500160509     C                   ENDIF
112600160509     C**
112700160509     C* CALCOLO NORMALE ANCHE PER SCALARE AL DI SOTTO
112800160509     C*  DELL'APPLICAZIONE TARIFFA FINITA E PER MAX SCAGLIONE PREC
112900160509     C     §TAF02        IFNE      'S'
113000160509     C     §TAF02        OREQ      'S'
113100160509     C     WATA          ANDNE     ' '
113200160509     C** MOLTIPLICAZIONE TARIFFA
113300160509     C                   EXSR      MULTAR
113400160509     C* APPLICAZIONE MINIMO E MASSIMO
113500160509     C                   EXSR      MINMAX
113600160509     C                   Z-ADD     WPOR          TASPOR
113700160509     C                   Z-ADD     WINL          TASINL
113800160509     C                   ENDIF
113900160509     C**
114000160509     C** S C A L A R E
114100160509     C     §TAF02        IFEQ      'S'
114200160509     C                   EXSR      SCALAR
114300160509     C                   ENDIF
114400160509     C**
114500160509     C** M A X   S C A G L I O N E   P R E C E D E N TE
114600160509     C     §TAF02        IFEQ      'G'
114700160509     C                   EXSR      MAXSCA
114800160509     C                   ENDIF
114900160509     C**
115000160509     C** SE DIVISA DELLA TARIFFA NON ACCETTA I DECIMALI PREPARO IL
115100160509     C** DEL PORTO E DELL'INOLTRO SENZA DECIMALI
115200160509     C     *IN12         IFEQ      '0'
115300160509     C* PORTO
115400160509     C     TASPOR        ADD       0,5           CAMPOW           11 0
115500160509     C                   Z-ADD     CAMPOW        TASPOR
115600160509     C* INOLTRO
115700160509     C     TASINL        ADD       0,5           CAMPOW
115800160509     C                   Z-ADD     CAMPOW        TASINL
115900160509     C                   ENDIF
116000160509     c                   eval      WImpoMag=taspor+tasinl
116100160509     C**
116200160509     C*******************************
116300160509     C****************
116400160509     C** TIPO SERVIZIO
116500160509     C****************
116600160509     C*
116700160509     C**                 Z-ADD     1             A
116800160509     C**                 clear                   ESP
116900160509     C*
117000160509     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
117100160509     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
117200160509     C*
117300160509     C**   KSDIT         IFEQ      'SDI'
117400160509     C**   TASDSP        ANDLE     20001231
117500160509     C**   TASTSP        ANDEQ     'C'
117600160509     C**   'E'           LOOKUP    TS(A)                                  05
117700160509     C**                 ELSE
117800160509     C**   TASTSP        LOOKUP    TS(A)                                  05
117900160509     C**                 ENDIF
118000160509     C*
118100160509     C**N05              GOTO      DOPOIN
118200160509     C**   A             OCCUR     DS5E
118300160509     C**   §5EFTC        CABEQ     *BLANKS       DOPOIN
118400160509
118500160509     C** TIPO SERVIZIO SENZA TARIFFA PARTICOLARE
118600160509     C** PORTO
118700160509     C**                 Z-ADD     0             WTSP
118800160509    1C**   TASPOR        IFGT      0
118900160509     c**                 exsr      CerESPRESSO
119000160509     c**
119100160509     c* Se c'e' già la varia, non la calcolo
119200160509    2c**                 if        giaespr<>'S'
119300160509     C**                 Z-ADD     TASPOR        IMPON
119400160509     c**                 if        *in56
119500160509     c**                 eval      applicarevers='S'
119600160509     c**                 endif
119700160509     C**                 EXSR      CALTS
119800160509     c**                 clear                   applicarevers
119900160509    3C**   WTSP          IFGT      0
120000160509     C*****              ADD       WTSP          TASPOR
120100160509     c
120200160509    4c**                 if        esp<>999
120300160509     C**                 MOVEL     wVariaMag     SV(ESP)
120400160509     C**                 add       wtsp          VA(ESP)
120500160509     c**                 else
120600160509     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
120700160509     C**                 ADD       wtsp          TASPOR
120800160509    4c**                 endif
120900160509     c**
121000160509    3C**                 ENDif
121100160509    2C**                 ENDif
121200160509    1C**                 ENDif
121300160509     C** INOLTRO
121400160509     C**                 Z-ADD     0             WTSP
121500160509    1C**   TASINL        IFGT      0
121600160509     c**                 EXSR      CerESPRESSO
121700160509    2c**                 if        giaespr<>'S'
121800160509     C**                 Z-ADD     TASINL        IMPON
121900160509     c**                 if        *in56
122000160509     c**                 eval      applicarevers='S'
122100160509     c*+                 endif
122200160509     C**                 EXSR      CALTS
122300160509     c**                 clear                   applicarevers
122400160509    3C**   WTSP          IFGT      0
122500160509     C*****              ADD       WTSP          TASINL
122600160509    4c**                 if        esp<>999
122700160509     C**                 MOVEL     wVariaMag     SV(ESP)
122800160509     C**                 add       wtsp          VA(ESP)
122900160509   x4c**                 else
123000160509     C** SE NON CI SONO VARIE LIBERE AGGIUNGO ALla voce
123100160509     C**                 ADD       WTSP          TASINL
123200160509    4c**                 endif
123300160509    3C**                 END
123400160509    2C**                 END
123500160509    1C**                 END
123600160509     c
123700160509     C     DOPOIN        TAG
123800160509     C**
123900160509     C* VALORIZZO LA VARIA DELL'INOLTRO NELLA SCHIERA DELLE VARIE
124000160509     C     TASINL        IFGT      0
124100160509     C                   Z-ADD     1             X                 2 0
124200160509     C     §$2FIN        LOOKUP    SV(X)                                  30
124300160509     C* SE NON ESISTE CARICOLA SCHIERA
124400160509     C     *IN30         IFEQ      '0'
124500160509     C                   Z-ADD     1             X
124600160509     C     ' '           LOOKUP    SV(X)                                  30
124700160509     C   30              MOVEA     §$2FIN        SV(X)
124800160509     C                   ENDIF
124900160509     C* SE LO TROVO VALORIZZO TASINL
125000160509     C   30              Z-ADD     TASINL        VA(X)
125100160509     C                   END
125200160509     C****************************************
125300160509     C** ISOLA  LOCALITA' DISAGIATE C.STORICI
125400160509     C****************************************
125500160509     C** SE TROVATA TARIFFA DEL PAESE APPLICO LO STESSO L'ISOLA
125600160509     C**
125700160509    1C     FLGINO        IFEQ      'I'
125800160509     C     FLGINO        OREQ      'D'
125900160509      * sostituisco il controllo del centro storico con i nuovi flag T o Z
126000160509      * T = centro storico in città
126100160509      * Z = centro storico in provincia
126200160509     C**   FLGINO        OREQ      'S'
126300160509     C     FLGINO        OREQ      'T'
126400160509     C     FLGINO        OREQ      'Z'
126500160509     C* IMPOSTO LA TARIFFA PARTICOLARE
126600160509     C                   SELECT
126700160509     C     FLGINO        WHENEQ    'I'                                          ISOLA
126800160509     C                   MOVEL     'I'           TASTC
126900160509     C     FLGINO        WHENEQ    'D'                                          LOCALITA' DI
127000160509     C                   MOVEL     'K'           TASTC
127100160509      * sostituisco il controllo del centro storico con i nuovi flag T o Z
127200160509      * T = centro storico in città
127300160509      * Z = centro storico in provincia
127400160509     C**   FLGINO        WHENEQ    'S'                                          C.STORICO
127500160509     C     FLGINO        WHENEQ    'T'                                          C.STORICO
127600160509     C                   MOVEL     'Q'           TASTC
127700160509     C     FLGINO        WHENEQ    'Z'                                          C.STORICO
127800160509     C                   MOVEL     'Q'           TASTC
127900160509     C                   ENDSL
128000160509      ** visto che l'attivazione del calcolo del centro storico dovrebbe essere
128100160509      ** il 3/10/11  attivo il flag "Q" in tastc confrontando la dta spedizione
128200160509     c                   if        tasdsp < 20111003 and tastc = 'Q'
128300160509     c                   clear                   tastc
128400160509     c                   endif
128500160509      ** Se TASTC è valorizzato calcolo la varia. Questa specifica si può togliere
128600160509      ** quando abilitiamo il centro storico
128700160509 bis1c                   If        tastc <> ' '
128800160509     c
128900160509     C**
129000160509     c* 14/4/06-ES--> il pgm per tassare questa tar.particolare usa
129100160509     c*           sempre il cod.tassazione mittente anche se non
129200160509     c*           applica la reversibilità. Da sempre lo fa.
129300160509     c*           Abbiamo valutato che è sbagliato ma deve seguire
129400160509     c*           la regola della reversibilità. Per cui non lo devo
129500160509     c*           più fare per tipbol='A' ma per 56 ON
129600160509     C**   TIPBOL        IFEQ      'A'
129700160509     C                   IF        *in56
129800160509     C                   MOVEL     TASCTS        WCTS              2
129900160509     C                   MOVEL     TASMCT        TASCTS
130000160509     C                   ENDif
130100160509     C*
130200160509     C                   EXSR      TASPAR
130300160509     C**   TIPBOL        IFEQ      'A'
130400160509     C                   IF        *in56
130500160509     C                   MOVEL     WCTS          TASCTS
130600160509     C                   END
130700160509     C**
130800160509     C* SOLO SE L'HO CALCOLATA FACCIO LE COSE SOTTOSTANTI
130900160509    2C     WNOEST        IFEQ      'S'
131000160509     C     TASCP         ANDGT     0
131100160509     C* MEMORIZZO PORTO E POSIZIONE VARIA DOVE HO MEMORIZZATO
131200160509     C                   Z-ADD     TASCP         TASISO
131300160509     c* imponibile per maggiorazione "E" o "H"
131400160509     c                   eval      wImpoMag=wImpoMag+TASISO
131500160509     C*
131600160509     C  N96              MOVEL     'S'           WPORTO            1
131700160509     C* SALVO L'INDICE DELLA SCHIERA DELLA VARIA
131800160509     C   96              Z-ADD     A             ISO               2 0
131900160509     C   96              MOVEL     ' '           WPORTO            1
132000160509     C*
132100160509     C** SE VA IN SOSTITUZIONE CLEARO L'INOLTRO
132200160509     C** FLAG=S INOLTRO+ISOLA
132300160509     C** FLAG=N SOLO ISOLA
132400160509     c** In caso di centro storico  non considero il flag FLGISO in quanto non
132500160509     c** non deve andare in sostituzione dell'inoltro ed in tariffa non viene
132600160509     c** richiesta la valorizzazione quindi è sempre diverso da "S"
132700160509    3C     FLGISO        IFNE      'S'
132800160509     c     TASTC         ANDNE     'Q'
132900160509     c* tolgo dall'imponibile per la maggiornazione l'Inoltro
133000160509     c                   eval      wImpoMag=wImpoMag-TASINL
133100160509     c*
133200160509     C                   CLEAR                   TASINL
133300160509     C** RICHIAMO LA VECCHIA POSIZIONE DELLA VARIA E LA SOSTIUISCO CON IL
133400160509     C**  VALORE DELL'ISOLA
133500160509     C** L'INDICE "X" PUNTA ALLA VARIA DELL'INOLTRO
133600160509     C     X             IFGT      *ZERO
133700160509     C* PULISCO L'INOLTRO
133800160509     C                   CLEAR                   VA(X)
133900160509     C                   CLEAR                   SV(X)
134000160509     C* UTILIZZO LA POSIZIONE VUOTA DELLA SCHIERA PER METTERE L'IMPORTO DELLA
134100160509     C* VARIA DELL'ISOLA
134200160509     C                   MOVEL     §1PVAR        SV(X)
134300160509     C                   Z-ADD     TASCP         VA(X)
134400160509     C* PULISCO LA POSIZIONE DELLA SCHIERA PRECEDENTE
134500160509     C   96              CLEAR                   VA(A)
134600160509     C   96              CLEAR                   SV(A)
134700160509     C* SALVO L'INDICE ATTUALE DELLA SCHIERA DELLA VARIA
134800160509     C   96              Z-ADD     X             ISO
134900160509     C                   END
135000160509    3C                   END
135100160509     C***********************
135200160509     C** IS/L.D./C.S  - TIPO SERVIZIO
135300160509     C***********************
135400160509     C**                 Z-ADD     1             A                 5 0
135500160509     C*
135600160509     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
135700160509     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
135800160509     C*
135900160509     C**   KSDIT         IFEQ      'SDI'
136000160509     C**   TASDSP        ANDLE     20001231
136100160509     C**   TASTSP        ANDEQ     'C'
136200160509     C**   'E'           LOOKUP    TS(A)                                  05
136300160509     C**                 ELSE
136400160509     C**   TASTSP        LOOKUP    TS(A)                                  05
136500160509     C*+                 ENDIF
136600160509     C*
136700160509     C** 05A             OCCUR     DS5E
136800160509    3C** 05§5EFTC        IFNE      *BLANKS
136900160509     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
137000160509     c**                 EXSR      CerESPRESSO
137100160509     c**
137200160509   3ac**                 if        giaespr<>'S'
137300160509     C**                 Z-ADD     0             WTSP
137400160509     C**                 Z-ADD     TASISO        IMPON
137500160509     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
137600160509     C**                 IF        *in56
137700160509     c**                 eval      applicarevers='S'
137800160509     c**                 endif
137900160509     C**                 EXSR      CALTS
138000160509     c**                 clear                   applicarevers
138100160509     c
138200160509    4C**   WTSP          IFGT      0
138300160509    5c**                 if        esp<>999
138400160509     C**                 MOVEL     WVariaMag     SV(ESP)
138500160509     C**                 add       wtsp          VA(ESP)
138600160509   x5c**                 else
138700160509     c* Se non c'e' una varia disponibile, sommo al posto o alla
138800160509     c*  varia dell' isola
138900160509    6C**   WPORTO        IFEQ      'S'
139000160509     C**                 ADD       WTSP          TASPOR
139100160509   x6C**                 ELSE
139200160509     C**                 ADD       WTSP          TASISO
139300160509     C**                 Z-ADD     TASISO        VA(ISO)
139400160509    6C**                 END
139500160509    5C**                 END
139600160509    4C**                 END
139700160509     c
139800160509   3aC**                 ENDif
139900160509    3C**                 END
140000160509     c
140100160509    2C                   END
140200160509 bis1C                   ENDIF
140300160509    1C                   ENDIF
140400160509     c
140500160509     C***********************
140600160509     C** Maggiorazione per tipo servizio sul totale imponibile da maggiorare
140700160509     c** ovvero:  PORTO + INOLTRO + ISOLA/DISAGIATE/CENTRO STORICO
140800160509     C** Se porto o inoltro già presenti: solo su ISOLA/DISAGIATE/CENTRO STORICO
140900160509     c** Se isola/disagiate/centro storico già presente: solo su PORTO + INOLTRO
141000160509     C***********************
141100160509     C                   Z-ADD     1             A                 5 0
141200160509     C                   clear                   ESP
141300160509     c
141400160509    1c                   if        WimpoMag>0
141500160509     C*
141600160509     C* NEL CASO DI BOLLE SDI CON DATA INFERIORE O UGUALE AL 31/12/2000
141700160509     C* IL TIPO SERVIZIO "C" SI DEVE COMPORTARE COME LA "E" BARTOLINI
141800160509     C*
141900160509    2C     KSDIT         IFEQ      'SDI'
142000160509     C     TASDSP        ANDLE     20001231
142100160509     C     TASTSP        ANDEQ     'C'
142200160509     C     'E'           LOOKUP    TS(A)                                  05
142300160509     C                   ELSE
142400160509     C     TASTSP        LOOKUP    TS(A)                                  05
142500160509    2C                   ENDIF
142600160509     C*
142700160509     C   05A             OCCUR     DS5E
142800160509    2C   05§5EFTC        IFNE      *BLANKS
142900160509     C** TIPO SERVIZIO SENZA MAGGIORAZIONE
143000160509     c                   EXSR      CerESPRESSO
143100160509     c
143200160509    3c                   if        giaespr<>'S'
143300160509     C                   Z-ADD     0             WTSP
143400160509     c
143500160509     C                   Z-ADD     WImpoMag      IMPON
143600160509     c* Se reversibile, anche per l'espresso uso cod tassaz. mittente
143700160509     C                   IF        *in56
143800160509     c                   eval      applicarevers='S'
143900160509     c                   endif
144000160509     C                   EXSR      CALTS
144100160509     c                   clear                   applicarevers
144200160509     c
144300160509    4C     WTSP          IFGT      0
144400160509    5c                   if        esp<>999
144500160509     C                   MOVEL     WVariaMag     SV(ESP)
144600160509     C                   add       wtsp          VA(ESP)
144700160509   x5c                   else
144800160509     c* Se non c'e' una varia disponibile, sommo nel porto
144900160509     C                   ADD       WTSP          TASPOR
145000160509    5C                   END
145100160509    4C                   END
145200160509     c
145300160509    3C                   ENDif
145400160509    2C                   END
145500160509    1C                   END
145600160509     C****************
145700160509     C** DIRITTO FISSO
145800160509     C****************
145900160509     C     TASDIF        IFEQ      0
146000160509     C                   MOVEL     'H'           TASTC
146100160509     C* RICHIAMO LA ROUTINE DI CALCOLO DELLE TARIFFE PARTICOLARI
146200160509     C                   EXSR      TASPAR
146300160509     C   96              Z-ADD     TASCP         TASDIF
146400160509     C***
146500160509     C                   ENDIF
146600160509     C***********************
146700160509     C** PROVVIGIONE ASSEGNO
146800160509     C***********************
146900160509     C     VARIAG        TAG
147000160509     C     *LIKE         DEFINE    TASVA1        PROVAS
147100160509     C                   Z-ADD     0             PROVAS
147200160509     C** MANCA C/A
147300160509     C     TASCAS        CABEQ     0             POIT01
147400160509     C** PROVV. ASSEGNO GIA' PRESENTE
147500160509     C                   Z-ADD     1             A
147600160509     C     §$2VRG        LOOKUP    SV(A)                                  05
147700160509    1C  N05              DO
147800160509     C*
147900160509     C                   SETOFF                                       94
148000160509     C     *LIKE         DEFINE    TASCAS        CASLIT
148100160509     C                   Z-ADD     TASCAS        CASLIT
148200160509     C**
148300160509    2C*!*        TASCMB    IFEQ 0
148400160509     C*!*                  Z-ADD1         K
148500160509     C*!*        TASVCA    LOKUPCV,K                     05
148600160509    3C*!*        *IN05     IFEQ *OFF
148700160509     C*!*                  SETON                     94
148800160509     C*!*                  MOVELMSG,1     TASMSG
148900160509     C*!*                  GOTO FINE
149000160509    3C*!*                  ENDIF
149100160509     C** NON ESISTE CAMBIO - MANCA TARIFFA
149200160509     C*!*        K         OCUR DSCV
149300160509    3C*!*        TASVCA    IFNE §CVITA
149400160509     C*!*        TASCAS    MULT §CVCMB    CASLIT    H
149500160509     C** CAMBIO IN BASE ALLA DIVISA STANDARD
149600160509    3C*!*                  END
149700160509   X2C*!*                  ELSE
149800160509     C*!*        TASCAS    MULT TASCMB    CASLIT    H
149900160509     C** CAMBIO IN BASE AL VALORE ESISTENTE IN BOLLA
150000160509    2C*!*                  END
150100160509     C* CALCOLO IL VALORE DEL CONTRASSEGNO  IN BASE ALLA MONETA
150200160509     C*******
150300160509     C*******
150400160509     C* SE PRESENTE IL C/A ANNULLATO VEDO IN BASE ALLA TARIFFA CLIENTE
150500160509     C*  SE FATTURARE O MENO
150600160509     C*  §TAPCA='N' NON FATTURO GLI ANNULLATI
150700160509     C**!!!§ASSTA        IFEQ      '9'
150800160509     C     TASSTA        IFEQ      '9'
150900160509     C     §TAPCA        ANDEQ     'N'
151000160509      * calcolo sempre la provvigione del C/A  annullato se si tratta di consegna anomala dirottata
151100160509     C     §ASCCA        ANDNE     '1'
151200160509      *
151300160509     C                   GOTO      POIT01
151400160509     C                   ENDIF
151500160509     C**
151600160509     C     TASVCA        IFNE      §TADIV
151700160509     C                   CLEAR                   YEUR
151800160509     C                   MOVEL     TASVCA        YECDVI
151900160509     C                   MOVEL     §TADIV        YECDVO
152000160509     C                   Z-ADD     TASCAS        YECIMI
152100160509     C   12              MOVE      '3'           YECDCO
152200160509     C  N12              MOVE      '0'           YECDCO
152300160509     C*
152400160509     C                   CALL      'YEURCO'
152500160509     C                   PARM                    YEUR
152600160509     C*
152700160509     C     YECESI        IFEQ      ' '
152800160509     C                   Z-ADD     YECIMO        CASLIT
152900160509     C                   ELSE
153000160509     C                   SETON                                        94
153100160509     C                   MOVEL     MSG(1)        TASMSG
153200160509     C                   GOTO      FINE
153300160509     C                   END
153400160509     C                   ENDIF
153500160509     C* PROVV ASSEGNO MITTENTE
153600160509    2C     TASBM         IFEQ      'M'
153700160509     C                   MOVEL     'W'           TASTC
153800160509     C                   ELSE
153900160509     C* PROVV ASSEGNO VETTORE
154000160509     C                   MOVEL     'V'           TASTC
154100160509    2C                   ENDIF
154200160509      * se spedizione EEX cerco sempre provvigione assegno mittente
154300160509     c***                If        o27fie = 'E'
154400160509     c                   if        WBOEST='E'and WBOFED=' '
154500160509     c                   movel     'W'           tastc
154600160509     c                   endif
154700160509     C**
154800160509     C                   MOVEL     TASTC         FISO
154900160509     C                   MOVEL     'S'           WCAR
155000160509     C                   EXSR      CERTPT
155100160509    2C     WEND          IFEQ      ' '
155200160509     C**
155300160509    3C     TPTTPG        IFEQ      'V'                                          VALORE
155400160509     C                   Z-ADD     CASLIT        TPDPES
155500160509     C                   ELSE                                                   ALTRO
155600160509     C                   Z-ADD     ISOPES        TPDPES
155700160509    3C                   ENDIF
155800160509     C**
155900160509     C                   EXSR      CALACD
156000160509     C*
156100160509    3C     VARIA         IFGT      0
156200160509     C                   Z-ADD     1             A
156300160509     C     ' '           LOOKUP    SV(A)                                  96
156400160509     C   96              MOVEL     §$2VRG        SV(A)
156500160509     C   96              Z-ADD     VARIA         VA(A)
156600160509     C  N96              ADD       VARIA         TASPOR
156700160509     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
156800160509    3C                   ENDIF
156900160509    2C                   ENDIF
157000160509    1C                   ENDDO
157100160509     C**
157200160509     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
157300160509     C     TASSVA        IFEQ      §$2VRG
157400160509     C                   GOTO      FINE
157500160509     C                   ENDIF
157600160509     C***********************
157700160509     C** DIRITTO ASSICURATIVO
157800160509     C***********************
157900160509     C     POIT01        TAG
158000160509     C                   Z-ADD     1             A
158100160509     C     §$2VRE        LOOKUP    SV(A)                                  05
158200160509     C  N05              DO
158300160509     C                   Z-ADD     0             DIRITT
158400160509     C     *LIKE         DEFINE    TASVA1        DIRITT
158500160509     C                   EXSR      CALVE
158600160509     C     DIRITT        IFGT      0
158700160509     C                   Z-ADD     1             A
158800160509     C     ' '           LOOKUP    SV(A)                                  96
158900160509     C   96              MOVEL     §$2VRE        SV(A)
159000160509     C   96              Z-ADD     DIRITT        VA(A)
159100160509     C  N96              ADD       DIRITT        TASPOR
159200160509     C                   END
159300160509     C                   END
159400160509     C** SE NON CI SONO VARIE LIBERE AGGIUNGO AL PORTO
159500160509     C***********************
159600160509     C** ASS. X CONTO
159700160509     C***********************
159800160509     C     VARIAR        TAG
159900160509     C                   Z-ADD     0             WVARIR
160000160509     C                   Z-ADD     0             WVARIACB
160100160509     C     *LIKE         DEFINE    TASVA1        WVARIR
160200160509     C     *LIKE         DEFINE    TASVA1        WVARIACB
160300160509      * cerco la varia R sia in caso di bolla con importo d'assicurare e sia in caso di
160400160509      * ricerca di mandato
160500160509     C                   EXSR      CALVR
160600160509     C** MANCA TARIFFA:MANDATO VALORE = 0 E TASIAS=0
160700160509     C   94              GOTO      FINE
160800160509     C**
160900160509     C     WVARIR        IFGT      0
161000160509     C                   Z-ADD     1             A
161100160509     C     ' '           LOOKUP    SV(A)                                  96
161200160509     C   96              MOVEL     §$2VRR        SV(A)
161300160509     C   96              Z-ADD     WVARIR        VA(A)
161400160509      ** se non ci sono varie libere aggiungo al porto
161500160509     C  N96              ADD       WVARIR        TASPOR
161600160509
161700160509      * se varia uguale a zero e non ho trovato nessun mandato e non c'è importo d'assicurare in
161800160509      * bolla cerco la nuova AC base
161900160509
162000160509     c                   else
162100160509
162200160509     c                   If        task88 = 'X' and
162300160509      ** 88 = non si calcola
162400160509     c                             (Ksccod <> 8888 and Ksccod <> 9999) and
162500160509      ** data spedizione maggiore del 28 02 2006
162600160509     c                             tasdsp > 20060228
162700160509     c                   exsr      CALVACB
162800160509      ** carico la varia nella schiera
162900160509     c                   If        wvariacb > 0
163000160509     c                   z-add     1             a
163100160509     c     ' '           Lookup    sv(a)                                  96
163200160509     c   96              movel     §$2Acb        sv(a)
163300160509     c   96              z-add     Wvariacb      va(a)
163400160509      ** se non ci sono varie libere aggiungo al porto
163500160509     c  N96              add       Wvariacb      TASpor
163600160509     c                   endif
163700160509
163800160509     c                   endif
163900160509
164000160509     c                   END
164100160509     C**
164200160509     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
164300160509     C     TASSVA        IFEQ      §$2VRR
164400160509     C                   GOTO      FINE
164500160509     C                   ENDIF
164600160509     C***************
164700160509     C* T.C.P.
164800160509     C***************
164900160509     C*
165000160509     C                   MOVEL     §$2VRP        TASTC
165100160509     C                   EXSR      TASPAR
165200160509     C***************************************
165300160509     C* ANNULLAMENTO PORTO ASSEGNATO VARIA &
165400160509     C***************************************
165500160509     C*
165600160509     C* SE IL FLAG DI CALCOLO VARIA & È UGUALE A S
165700160509     C                   IF        TASFCAA = 'S'
165800160509     C                   MOVEL     §$2VPA        TASTC
165900160509     C                   EXSR      TASPAR
166000160509     C                   ENDIF
166100160509     C****************************
166200160509     C* LASCIATO AVVISO  VARIA 'c'
166300160509     C****************************
166400160509     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
166500160509     C                   If        tasric <> ' '
166600160509      * verifico se linea di arrivo o linea di partenza è estera non calcolo la varia "c"
166700160509     c     taslna        lookup    esttot                                 30
166800160509      * se linea di arrivo non è estero verifico la linea di partenza
166900160509     c  n30taslnp        lookup    esttot                                 30
167000160509      * se spedizione italiana calcolo
167100160509     c                   If        not *in30
167200160509     C                   Movel     §$2vla        Tastc
167300160509     C                   Exsr      TASpar
167400160509     c                   Endif
167500160509
167600160509     c                   Endif
167700160509     C****************************
167800160509     C* AVVISO AL DESTINATARIO AFFIDAMENTO SPEDIZIONE VARIA 'm'
167900160509     C****************************
168000160509     C* viene calcolato solo se il programma chiamante me passa il flag valorizzato
168100160509     C* in TASFLO §asemd
168200160509     C                   If        §asemd = 'S' or §asemd = 'X' or
168300160509     c                             §asemd = 'E'
168400160509     C                   Movel     §$2vad        Tastc
168500160509     C                   Exsr      TASpar
168600160509     c                   Endif
168700160509
168800160509     C****************************
168900160509     C* PIN CODE  CONSEGNA CON VERIFICA PIN CODE VARIA "p"
169000160509     C****************************
169100160509     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
169200160509     C* in TASFLO §aspin
169300160509     C                   If        §aspin = 'S'
169400160509     C                   Movel     §$2pin        Tastc
169500160509     C                   Exsr      TASpar
169600160509     c                   Endif
169700160509
169800160509     C*************************************************
169900160509     C* DIRITTO DI CHIAMATA PRENOTAZIONE ORM TELEFONICO
170000160509     C*************************************************
170100160509     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
170200160509     C* TASPRT
170300160509     C                   If        TASPRT = 'S'
170400160509     C                   Movel     §$2ORMT       Tastc
170500160509     C                   Exsr      TASpar
170600160509     c                   Endif
170700160509
170800160509     C****************************
170900160509     C* PACKING LIST
171000160509     C****************************
171100160509     C* viene calcolato solo se il programma chiamante mi passa il flag valorizzato
171200160509     C* TASSPL
171300160509     C                   If        TASSPL = 'S'
171400160509     C                   Movel     §$2pkl        Tastc
171500160509     C                   Exsr      TASpar
171600160509     c                   Endif
171700160509
171800160509     C***************
171900160509     C* RICERCA DOCUMENTI - SOLO SE RICHIESTA DA SOLA
172000160509     C***************
172100160509     C*
172200160509     C     VARIAO        TAG
172300160509     C     TASSVA        IFEQ      §$2VRO
172400160509     C                   MOVEL     §$2VRO        TASTC
172500160509     C                   EXSR      TASPAR
172600160509     C**
172700160509     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
172800160509     C     TASSVA        IFEQ      §$2VRO
172900160509     C                   GOTO      FINE
173000160509     C                   ENDIF
173100160509     C                   ENDIF
173200160509     C***************
173300160509     C* RITIRI ANNULLATI  - SOLO SE RICHIESTA DA SOLA
173400160509     C***************
173500160509     C*
173600160509     C     VARIAY        TAG
173700160509     C     TASSVA        IFEQ      §$2VRY
173800160509     C                   MOVEL     §$2VRY        TASTC
173900160509     C                   EXSR      TASPAR
174000160509     C**
174100160509     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
174200160509     C     TASSVA        IFEQ      §$2VRY
174300160509     C                   GOTO      FINE
174400160509     C                   ENDIF
174500160509     C                   ENDIF
174600160509     C***************
174700160509     C* DIROTTAMENTI      - SOLO SE RICHIESTA DA SOLA
174800160509     C***************
174900160509     C*
175000160509     C     VARAST        TAG
175100160509     C     TASSVA        IFEQ      §$2AST
175200160509     C                   MOVEL     §$2AST        TASTC
175300160509     C                   EXSR      TASPAR
175400160509     C**
175500160509     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
175600160509     C     TASSVA        IFEQ      §$2AST
175700160509     C                   GOTO      FINE
175800160509     C                   ENDIF
175900160509     C                   ENDIF
176000160509     C***************
176100160509     C* RECAPITO C/ASSEGNI- SOLO SE RICHIESTA DA SOLA
176200160509     C***************
176300160509     C*
176400160509     C     VARVRW        TAG
176500160509     C     TASSVA        IFEQ      §$2VRW
176600160509     C                   MOVEL     'G'           TASTC
176700160509     C                   EXSR      TASPAR
176800160509     C**
176900160509     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
177000160509     C     TASSVA        IFEQ      §$2VRW
177100160509     C                   GOTO      FINE
177200160509     C                   ENDIF
177300160509     C                   ENDIF
177400160509     C*
177500160509     C***************
177600160509     C* ADDEBITO INSOLUTI - INT.MORA  SOLO SE RICHIESTA DA SOLA
177700160509     C***************
177800160509     C     VARVRM        TAG
177900160509     C     TASSVA        IFEQ      §$2VRM
178000160509      * SE NON ESISTONO VARIE IMPOSTATE DO MANCA TARIFFA
178100160509     c                   MOVEA     SV            WRKSV            20
178200160509     C                   IF        WRKSV = *BLANKS
178300160509     C                   MOVEL     MSG(18)       TASMSG
178400160509     C                   MOVEL     'E'           TASERR            1
178500160509     C                   SETON                                        94
178600160509     C                   ENDIF
178700160509     C                   GOTO      FINE
178800160509     C                   ENDIF
178900160509     C***************
179000160509     C* ORM COMMISSIONATO  - SOLO SE RICHIESTA DA SOLA
179100160509     C***************
179200160509     C*
179300160509     C     VARVRT        TAG
179400160509     C     TASSVA        IFEQ      §$2VRT
179500160509     C                   MOVEL     §$2VRT        TASTC
179600160509     C                   EXSR      TASPAR
179700160509     C**
179800160509     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
179900160509     C     TASSVA        IFEQ      §$2VRT
180000160509     C                   GOTO      FINE
180100160509     C                   ENDIF
180200160509     C                   ENDIF
180300160509     C***************
180400160509     C* P.O.D IMMAGE      - SOLO SE RICHIESTA DA SOLA
180500160509     C***************
180600160509     C*
180700160509     C     VARPOD        TAG
180800160509     C     TASSVA        IFEQ      §$2POD
180900160509     C                   MOVEL     §$2POD        TASTC
181000160509     C                   EXSR      TASPAR
181100160509     C**
181200160509     C* SE IMPOSTATA VARIA --> ESCO DAL PGM
181300160509     C     TASSVA        IFEQ      §$2POD
181400160509     C                   GOTO      FINE
181500160509     C                   ENDIF
181600160509     C                   ENDIF
181700160509     C************************
181800160509     C* CONSEGNE PARTICOLARI
181900160509     C************************
182000160509     C*
182100160509     C** CONSEGNE PARTICOLARI 1
182200160509     C     TASTC1        IFNE      *BLANKS
182300160509     C                   MOVEL     TASTC1        TASTC
182400160509     C                   EXSR      TASPAR
182500160509     C                   END
182600160509     C** CONSEGNE PARTICOLARI 2
182700160509     C     TASTC2        IFNE      *BLANKS
182800160509     C                   MOVEL     TASTC2        TASTC
182900160509     C                   EXSR      TASPAR
183000160509     C                   END
183100160509     C************************
183200160509     C* DITITTO DI PESATURA
183300160509     C************************
183400160509     C*
183500160509     C     §TATAP        IFEQ      'S'
183600160509     C                   MOVEL     '5'           TASTC
183700160509     C                   EXSR      TASPAR
183800160509     C                   END
183900160509     C************************
184000160509     C* RITIRO
184100160509     C************************
184200160509     C* LA VARIA "U" DI RITIRO  E'
184300160509     C** DA CALCOLARE: SUI PORTI ASSEGNATI
184400160509     C**               SUI PORTI FRANCHI NON CODIFICATI
184500160509     C**                   CON FLAG CONSEGNA A MAGAZZINO = A BLANK
184600160509     C**               SULLE BOLLE EXPORT ASSEGNATO NON DPD
184700160509     C**               SULLE BOLLE IMPORT ASSEGNATO TUTTE (ANCHE DPD)
184800160509     C* FRANCO
184900160509     C*!!*       TIPBOL    IFNE 'A'
185000160509     C*!!*       TASFDN    ANDEQ'S'
185100160509     C*!!*                 GOTO ENDRIT
185200160509     C*!!*                 ENDIF
185300160509     C*
185400160509     C* FRANCO CON CODICE CLIENTE UGUALE A 8888 OPPURE UGUALE ALLA TARIFFA DI
185500160509     C* CARTELLO
185600160509     C     TIPBOL        IFNE      'A'
185700160509     C     KSCCOD        ANDNE     8888
185800160509     C****       TASKSC    ANDNECARKSC
185900160509     C* SE NON SI TRATTA NEMMENO DI UNA TARIFFA DI CARTELLO NON TASSO
186000160509     C     TASKSC        LOOKUP    TAC                                    15
186100160509     C  N15              GOTO      ENDRIT
186200160509     C                   ENDIF
186300160509     C** DA CALCOLARE SOLO PER PORTI ASSEGNATI RITIRATI
186400160509     C*!!*       TIPBOL    CABNE'A'       ENDRIT
186500160509     C*
186600160509     C*
186700160509     C* SE BOLLA ASSEGNATA EXPORT --> NON CONSIDERO IL FLAG CONSEGNA A MAGAZZINO
186800160509     C*                               PERCHE' DEVE SEMPRE CALCOLARE LA VARIA "U"
186900160509     C     TASFDN        IFEQ      'S'
187000160509     C* PER ASSEGNATO EXPORT --> SEMPRE  (ANCHE DPD CHE FINO A OGGI
187100160509     C*                          ESCLUDEVA 4/4/02--> RIESCLUDO DPD)
187200160509     C* PER ASSEGNATO IMPORT --> SEMPRE  (ANCHE DPD 04/05/05)
187300160509     C*
187400160509     C* PER ALTRI ASSEGNATI  --> SOLO SE NON E' PORTATA A MAGAZZINO
187500160509     C     TIPBOL        IFEQ      'A'
187600160509     C*
187700160509     C     TASLNA        LOOKUP    EST                                    05
187800160509     c
187900160509     C  n05TASLNP        LOOKUP    ESTtot                                 05
188000160509     c
188100160509     C  N05              GOTO      ENDRIT
188200160509     C                   ELSE
188300160509     C* PER 8888 --> NON APPLICARE SE PORTATA A MAGAZZINO SIA PER EXP
188400160509     C*              CHE PER LE ALTRE SPEDIZIONI
188500160509     C                   GOTO      ENDRIT
188600160509     C                   ENDIF
188700160509     C                   ENDIF
188800160509      *----------------------------------------------------------------------------*
188900160509      * In caso di bolla di reso non si può NON CALCOLARE la varia U in quanto     *
189000160509      * questa serve per far pagare la tratta int.le della spedizione.             *
189100160509      * Infatti quando in Aprile 2006 è stata tolta la signora Villa ha reclamato. *
189200160509      * asterisco le seguenti specifiche                                           *
189300160509     c*** Per bolla import, non applico varia "U" se bolla di reso                 *
189400160509     C***     TASLNP        LOOKUP    ESTtot                                 05    *
189500160509     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
189600160509     C***                   GOTO      ENDRIT                                       *
189700160509     c***                   endif                                                  *
189800160509     c***                                                                          *
189900160509     c*** Per bolla EXport, non applico varia "U" se bolla di reso                 *
190000160509     C***     TASLNA        LOOKUP    EST                                    05    *
190100160509     c***                   if        *in05 and %subst(TASFLO:1:1)='S'             *
190200160509     C***                   GOTO      ENDRIT                                       *
190300160509     c***                   endif                                                  *
190400160509      *----------------------------------------------------------------------------*
190500160509     c
190600160509     C**
190700160509     C* TARIFFA PARTICOLARE --> U
190800160509     C                   MOVEL     'U'           TASTC
190900160509     C**
191000160509     C** 17/06/2011 LB DN visto che sono state cambiate le tariffe della cartello
191100160509     c** per la tariffa particolare di ritiro per l'Estero e non essendo + uguale
191200160509     c** a quelle per l'italia in caso di prepagati (quindi non codificati) per la GB
191300160509     c** la tariffa di ritiro diventa di 50 euro circa rispetto i 5 euro circa di prima ?
191400160509     c** quindi risulta necessario correggere un errore che ci portiamo dietro da anni
191500160509     c** che tassiamo controllando il codice tassazione mittente solo per gli assegnati
191600160509     c** invece si deve fare anche per i poveri franchi (prepagati) per i quali viene
191700160509     c** calcolato il ritiro. Quindi sia per i franchi che assegnati il codice tassazione
191800160509     c** per la tariffa particolare di ritiro diventa il codice tassazione mittente
191900160509     c** come è giusto che venga tassata da dove viene ritirata e non dove viene
192000160509     c** consegnata la merce. A.G.
192100160509     C**   TIPBOL        IFEQ      'A'
192200160509     C                   MOVEL     TASCTS        WCTS
192300160509     C                   MOVEL     TASMCT        TASCTS
192400160509     C**                 END
192500160509     C*
192600160509     C                   EXSR      TASPAR
192700160509     C*
192800160509     C**   TIPBOL        IFEQ      'A'
192900160509     C                   MOVEL     WCTS          TASCTS
193000160509     C**                 END
193100160509     C     ENDRIT        TAG
193200160509     C************************
193300160509     C* SI DDT
193400160509      * Tutti i pgm che richiamano il TNSF20R x tassare le bolle arrivi
193500160509      * testano il flag DDT e passano 'S' nel caso di DDT SI, in questi pgm ho aggiunto anche i
193600160509      * nuovi flag 'P' e 'Q'
193700160509      * l'unico che non lo fa è FNLG20R così ho aggiunto anche qua il controllo con i 2 nuovi flag
193800160509      * 'P' è uguale a 'S' e viene impostato in arrivo quando si stampa il ddt si
193900160509      * 'Q' è uguale a 'C' e viene impostato in arrivo quando si stampa il ddt si
194000160509     C************************
194100160509     C     TASDDT        IFEQ      'S'
194200160509     C     TASDDT        orEQ      'P'
194300160509     C     TASDDT        orEQ      'Q'
194400160509     C* TARIFFA PARTICOLARE --> B
194500160509     C                   MOVEL     'B'           TASTC
194600160509     C                   EXSR      TASPAR
194700160509     C                   END
194800160509      ***************
194900160509      * BANCALI
195000160509      ***************
195100160509     C*
195200160509      * Non si tassano i bancali per le bolle in porto assegnato
195300160509     c                   If        TasBan > *Zeros and TipBol <> 'A'
195400160509     c                   Movel     §$2Vba        TASTC
195500160509     c                   ExSr      TasPar
195600160509     c                   EndIf
195700160509     C*
195800160509      ************************************************
195900160509      * SUPPLEMENTO CARBURANTE SPEDIZIONI FEDEX
196000160509      ************************************************
196100160509     C                   IF        WBOFED = 'S'
196200160509     C                   EXSR      SR_SUPCARB
196300160509     C                   ELSE
196400160509      ************************************************
196500160509      * FUEL SURCHARGE  SPEDIZIONI NO FEDEX
196600160509      ************************************************
196700160509     C                   EXSR      SR_FUEL
196800160509     C                   ENDIF
196900160509      *
197000160509     C                   SETOFF                                       49
197100160509     C                   ENDSR
197200160509     C*****************************************************************
197300160509     C** Cerco la posizione per la varia della maggiorazione
197400160509     C*****************************************************************
197500160509     c     CerESPRESSO   BEGSR
197600160509     c                   if        esp=0
197700160509     c                   clear                   giaespr           1
197800160509     c                   clear                   wVariaMag         1
197900160509     C                   Z-ADD     1             A
198000160509
198100160509     c* Maggiorazione per tipo servizio E-priority
198200160509     c                   select
198300160509     c                   when      tastsp='E'
198400160509     c                   eval      wvariaMag=§$2tsp
198500160509
198600160509     c* Maggiorazione per tipo servizio H-H1030
198700160509     c                   when      tastsp='H'
198800160509     c                   eval      wvariaMag=§$2tsh
198900160509     c                   endsl
199000160509
199100160509     C     wvariaMag     LOOKUP    SV(a)                                  96
199200160509     c* gia esiste la varia della maggioraz --> non la calcolo
199300160509     c                   if        *in96
199400160509     c                   z-add     a             ESP               3 0
199500160509     c                   eval      giaespr='S'
199600160509     c                   else
199700160509     C                   Z-ADD     1             A
199800160509     C     ' '           LOOKUP    SV(A)                                  96
199900160509     c   96              z-add     a             ESP               3 0
200000160509     C  n96              ADD       999           ESP
200100160509     c                   endif
200200160509     c                   endif
200300160509     c                   ENDSR
200400160509     C*****************************************************************
200500160509     C** VALORE COL QUALE  MOLTIPLICARE LA TARIFFA
200600160509     C*****************************************************************
200700160509     C     CALQLI        BEGSR
200800160509     C                   Z-ADD     0             PESQLI           14 6
200900160509     C** SE LO SCAGLIONE SCELTO PER TASSARE E' INFERIORE AL MINIMO
201000160509     C** TASSABILE DA TARIFFA FORZO PESO TASSABILE = MINTAS
201100160509     C**
201200160509     C* WATA MI IDENTIFICA SE L'APPLICAZIONE TARIFFA FINITA E' DOVUTA
201300160509     C*  ALLO SCAGLIONE CONFRONTATO CON MINTAR (R)
201400160509     C*  O SE E' DOVUTA DAL PESO TASSABILE CONFRONTATO COL MINIMO
201500160509     C*  TASSABILE DI QUELLA TARIFFA (S) - differenza che ora non serve
201600160509     C*  + ma dal momento che c'e' la lascio
201700160509     C                   CLEAR                   WATA
201800160509    1C                   SELECT
201900160509     C     TSCG          WHENLE    MINTAR
202000160509     C                   Z-ADD     1             PESQLI
202100160509     C                   MOVEL     'R'           WATA              1
202200160509     C**
202300160509     C** SE PESO TASSABILE E' ANCORA INFERIORE
202400160509     C*  AL MINIMO TASSABILE DA TARIFFA IMPOSTO PESO TASS = 1
202500160509     C*  NON LO CONTROLLO PER LA TARIFFA A SCALARE PERCHE' ABBIAMO
202600160509     C*  SEMPRE INSERITO UNO SCAGLIONE=A MINIMO TASSABILE E
202700160509     C*  QUINDI E' GIA' CONTROLLATO NEL WHLE PRECEDENTE
202800160509     C     WPTAS3        WHENLE    MINTAS
202900160509     C     §TAF02        ANDNE     'S'
203000160509     C                   Z-ADD     1             PESQLI
203100160509     C                   MOVEL     'S'           WATA
203200160509   X1C                   OTHER
203300160509     C** CALCOLO : PESO IN Q.LI PER TARIFFA A PESO
203400160509     C** CALCOLO   VALORE/100   PER TARIFFA A VALORE (ESSENDO UNA %)
203500160509     C** PER LE ALTRE TARIFFE PESO TASSABILE INVARIATO
203600160509    2C     UNOCTR        IFEQ      0
203700160509     C     UNOCTR        OREQ      4
203800160509     C     WPTAS3        DIV       100           PESQLI
203900160509   X2C                   ELSE
204000160509     C                   Z-ADD     WPTAS3        PESQLI
204100160509    2C                   END
204200160509    1C                   ENDSL
204300160509     C                   ENDSR
204400160509     C*****************************************************************
204500160509     C* MOLTIPLICATO TARIFFA PER VALORE TROVATO (PESO TASSABILE)
204600160509     C*****************************************************************
204700160509     C     MULTAR        BEGSR
204800160509     C     *LIKE         DEFINE    TASPOR        WPOR
204900160509     C     *LIKE         DEFINE    TASINL        WINL
205000160509     C                   CLEAR                   WPOR
205100160509     C                   CLEAR                   WINL
205200160509     C**
205300160509     C     TITR          MULT(H)   PESQLI        WPOR                           ** PORTO
205400160509     C**
205500160509     C** SE TROVATA TARIFFA DEL PAESE NON SI APPLICA MAI INOLTRO
205600160509     C*****  N49FLGINO        IFNE      'C'
205700160509     C** Sostituisco il controllo del flag inoltro diverso da C=città ma
205800160509     c** anche da "T" Città centro storico
205900160509     C                   IF        NOT *IN49  AND
206000160509     C                             FLGINO <> 'C' AND FLGINO <> 'T'
206100160509     C     TINO          MULT(H)   PESQLI        WINL                           ** INOLTRO
206200160509     C                   END
206300160509     C                   ENDSR
206400160509     C*****************************************************************
206500160509     C* CALCOLO TARIFFA A SCALARE
206600160509     C*****************************************************************
206700160509     C     SCALAR        BEGSR
206800160509     C**
206900160509     C     *LIKE         DEFINE    PESTAS        WPTAS1
207000160509     C     *LIKE         DEFINE    PESTAS        WDIFSC
207100160509     C     *LIKE         DEFINE    PESTAS        WPTAS3
207200160509     C     *LIKE         DEFINE    PESQLI        WPESQ
207300160509     C     *LIKE         DEFINE    PESQLI        WPESSV
207400160509     C     *LIKE         DEFINE    TITR          WTITR
207500160509     C     *LIKE         DEFINE    TITR          WSTITR
207600160509     C     *LIKE         DEFINE    TINO          WTINO
207700160509     C     *LIKE         DEFINE    TINO          WSTINO
207800160509     C     *LIKE         DEFINE    TSCG          WPRESC
207900160509     C**
208000160509     C                   CLEAR                   TASPOR
208100160509     C                   CLEAR                   TASINL
208200160509     C                   CLEAR                   WTROVA
208300160509     C                   CLEAR                   WPTAS3
208400160509     C                   CLEAR                   WDIFSC
208500160509     C                   CLEAR                   WPRESC
208600160509     C                   CLEAR                   WPESSV
208700160509     C                   CLEAR                   WPESQ
208800160509     C                   CLEAR                   WTITR
208900160509     C                   CLEAR                   WSTITR
209000160509     C                   CLEAR                   WTINO
209100160509     C                   CLEAR                   WSTINO
209200160509     C                   Z-ADD     PESTAS        WPTAS1
209300160509     C                   Z-ADD     1             A
209400160509     C**
209500160509    1C     A             DOWLE     200
209600160509     C     WTROVA        ANDEQ     ' '
209700160509     C     TSCG          ANDGT     0
209800160509     C     A             OCCUR     TARDS
209900160509     C**
210000160509     C* FACCIO LA DIFFERENZA TRA LO SCAGLIONE LETTO ED IL PRECEDENTE
210100160509     C     TSCG          SUB       WPRESC        WDIFSC
210200160509     C**
210300160509     C* SOTRAGGO DAL PESO TASSABILE RIMASTO LA PARTE DA TASSARE ORA
210400160509     C* IN WPTAS1 HO SEMPRE LA PARTE DI PESO TASSABILE ANCORA DA TASSAR
210500160509     C                   SUB       WDIFSC        WPTAS1
210600160509     C**
210700160509     C** SE IL VALORE E' DIVENTATO < DI 0 SIGNIFICA CHE HO TERMINATO LA
210800160509     C**  TASSAZIONE PER CUI USO SOLO LA PARTE RIMASTA DI WPTAS1
210900160509    2C     WPTAS1        IFLE      0
211000160509     C     WDIFSC        ADD       WPTAS1        WPTAS3
211100160509     C* PESO TASSABILE GLOBALE DELLA SPEDIZIONE
211200160509     C                   MOVEL     'S'           WTROVA            1
211300160509     C                   ELSE
211400160509     C**
211500160509     C* PARTE DI PESO DA TASSARE CON QUESTO SCAGLIONE
211600160509     C                   Z-ADD     WDIFSC        WPTAS3
211700160509     C* SALVO LO SCAGLIONE PRECEDENTE
211800160509     C                   Z-ADD     TSCG          WPRESC
211900160509    2C                   ENDIF
212000160509     C**
212100160509     C* IMPOSTO IL VALORE DA MOLTIPLICARE PER LA TARIFFA
212200160509     C                   EXSR      CALQLI
212300160509     C**
212400160509     C* SE LO SCAGLIONE E' APPLICAZIONE TARIFFA FINITA DEVO CONSIDERARE
212500160509     C*  SOLO L'ULTIMO SCAGLIONE COSI' E NON I PRECEDENTI
212600160509    2C     WATA          IFNE      ' '
212700160509     C     WTROVA        ANDEQ     ' '
212800160509     C* MI SALVO IL VALORE MA PER ORA NON LO MOLTIPLICO
212900160509     C                   Z-ADD     PESQLI        WPESQ
213000160509     C                   Z-ADD     TITR          WTITR
213100160509     C                   Z-ADD     TINO          WTINO
213200160509   X2C                   ELSE
213300160509     C**
213400160509     C* SE LO SCAGLIONE TROVATO E' L'ULTIMO MA COMUNQUE TARIFFA FINITA
213500160509     C* DEVO MOLTIPLICARE SOLO QUESTO --> AZZERO UN EVENTUALE PRECEDENT
213600160509     C*  CHE AVEVA LA TASSAZIONE TARIFFA FINITA
213700160509    3C     WTROVA        IFEQ      'S'
213800160509     C     WATA          ANDNE     ' '
213900160509     C                   CLEAR                   WPESQ
214000160509     C                   CLEAR                   WTITR
214100160509     C                   CLEAR                   WTINO
214200160509    3C                   ENDIF
214300160509     C*
214400160509     C* PRIMA DI MOLTIPLICARE LA TARIFFA TROVATA VEDO SE CE NE E'
214500160509     C*  UNA PRECEDENTE TARIFFA FINITA DA CONSIDERARE
214600160509    3C     WPESQ         IFGT      0
214700160509     C* SALVO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
214800160509     C*  APPENA LETTO
214900160509     C                   Z-ADD     PESQLI        WPESSV
215000160509     C                   Z-ADD     TITR          WSTITR
215100160509     C                   Z-ADD     TINO          WSTINO
215200160509     C* IMPOSTO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
215300160509     C*  PRECEDENTE PER IL CALCOLO
215400160509     C                   Z-ADD     WPESQ         PESQLI
215500160509     C                   Z-ADD     WTITR         TITR
215600160509     C                   Z-ADD     WTINO         TINO
215700160509     C                   EXSR      MULTAR
215800160509     C                   ADD       WPOR          TASPOR
215900160509     C                   ADD       WINL          TASINL
216000160509     C                   CLEAR                   WPESQ
216100160509     C                   CLEAR                   WTITR
216200160509     C                   CLEAR                   WTINO
216300160509     C* REIMPOSTO I DATI DELLO SCAGLIONE APPENA LETTO
216400160509     C                   Z-ADD     WPESSV        PESQLI
216500160509     C                   Z-ADD     WSTITR        TITR
216600160509     C                   Z-ADD     WSTINO        TINO
216700160509    3C                   ENDIF
216800160509     C**
216900160509     C* MOLTIPLICAZIONE TARIFFA
217000160509     C                   EXSR      MULTAR
217100160509     C                   ADD       WPOR          TASPOR
217200160509     C                   ADD       WINL          TASINL
217300160509    2C                   ENDIF
217400160509     C                   ADD       1             A
217500160509     C**
217600160509    1C                   ENDDO
217700160509     C**
217800160509     C* APPLICO MINIMO E MASSIMO
217900160509     C                   Z-ADD     TASPOR        WPOR
218000160509     C                   Z-ADD     TASINL        WINL
218100160509     C                   EXSR      MINMAX
218200160509     C                   Z-ADD     WPOR          TASPOR
218300160509     C                   Z-ADD     WINL          TASINL
218400160509     C                   ENDSR
218500160509     C*****************************************************************
218600160509     C* CALCOLO MAX SCAGLIONE PRECEDENTE
218700160509     C*****************************************************************
218800160509     C     MAXSCA        BEGSR
218900160509     C* SOLO SE LO SCAGLIONE APPLICATO NON ERA IL PRIMO
219000160509     C     WA            IFGT      1
219100160509     C* PER CONFRONTARLO CON IL VALORE NORMALE CALCOLATO
219200160509     C* IL CONFRONTO E' DATO DALLA SOMMA TRA PORT E INOLTRO
219300160509     C                   SUB       1             WA
219400160509     C     WA            OCCUR     TARDS
219500160509     C* CALCOLO PESO DA MOLTIPLICARE
219600160509     C                   Z-ADD     TSCG          WPTAS3
219700160509     C                   EXSR      CALQLI
219800160509     C** MOLTIPLICO LA TARIFFA
219900160509     C                   EXSR      MULTAR
220000160509     C** MINIMO E MASSIMO
220100160509     C                   EXSR      MINMAX
220200160509     C**
220300160509     C     TASPOR        ADD       TASINL        T0100            15 3
220400160509     C     WPOR          ADD       WINL          W0100            15 3
220500160509     C** PRENDO DELLE 2 LA TARIFFA PIU' ALTA
220600160509     C     W0100         IFGT      T0100
220700160509     C                   Z-ADD     WPOR          TASPOR
220800160509     C                   Z-ADD     WINL          TASINL
220900160509     C                   ENDIF
221000160509     C                   ENDIF
221100160509     C**
221200160509     C                   ENDSR
221300160509     C*****************************************************************
221400160509     C** MINIMO E MASSIMO DA APPLICARE -SE APPLICATO TUTTO NEL PORTO
221500160509     C*****************************************************************
221600160509     C     MINMAX        BEGSR
221700160509     C** NON ESISTE MINIMO E MASSIMO
221800160509     C     TMIN          IFEQ      0
221900160509     C     TMAX          ANDEQ     0
222000160509     C                   GOTO      DOPOMM
222100160509     C                   END
222200160509     C** MINIMO
222300160509     C     WPOR          ADD       WINL          WMAX             15 3
222400160509     C     TMIN          IFGT      WMAX
222500160509     C                   Z-ADD     TMIN          WPOR
222600160509     C                   Z-ADD     0             WINL
222700160509     C                   Z-ADD     TMIN          WMAX
222800160509     C                   END
222900160509     C** MASSIMO
223000160509     C     TMAX          IFGT      0
223100160509     C     WMAX          IFGT      TMAX
223200160509     C                   Z-ADD     TMAX          WPOR
223300160509     C                   Z-ADD     0             WINL
223400160509     C                   END
223500160509     C                   END
223600160509     C     DOPOMM        ENDSR
223700160509     C*****************************************************************
223800160509     C* CERCO VARIA ISOLA LOCALITA' DISAGIATA CENTRI STORICI
223900160509     c* cerco anche la maggiorazione
224000160509     C*****************************************************************
224100160509     C     CERISO        BEGSR
224200160509     C                   CLEAR                   VAISO
224300160509     C                   CLEAR                   VATSP
224400160509      * vaaggfuel varie che vengono sommate solo all'imponibile fuel e supplemento carburante
224500160509     C                   CLEAR                   VAAGGFUEL
224600160509     C     *LIKE         DEFINE    TASVA1        VAISO
224700160509     C     *LIKE         DEFINE    TASVA1        VATSP
224800160509     C     *LIKE         DEFINE    TASVA1        VAAGGFUEL
224900160509     C                   Z-ADD     1             CC                3 0
225000160509     C     §$2VRJ        LOOKUP    SV(CC)                                 96     ISOLA
225100160509     C   96              ADD       VA(CC)        VAISO
225200160509     C                   Z-ADD     1             CC
225300160509     C     §$2VRK        LOOKUP    SV(CC)                                 96     DISAGIATE
225400160509     C   96              ADD       VA(CC)        VAISO
225500160509     C                   Z-ADD     1             CC
225600160509     C     §$2VRQ        LOOKUP    SV(CC)                                 96     C.STORICI
225700160509     C   96              ADD       VA(CC)        VAISO
225800160509     C                   Z-ADD     1             CC                3 0
225900160509     c* "E" o "H"
226000160509     c                   if        WvariaMag<>' '
226100160509     C     WVariaMag     LOOKUP    SV(CC)                                 96     maggiorazione e o h
226200160509     C   96              z-add     VA(CC)        VAtsp
226300160509     c                   endif
226400160509      * CALCOLO LE VARIE  AGGIUNTIVE AL SOLO IMPONIBILE FUEL
226500160509     C                   Z-ADD     1             CC                3 0
226600160509     C     '='           LOOKUP    SV(CC)                                 96     RESO BANCALI
226700160509     C   96              ADD       VA(CC)        VAAGGFUEL
226800160509     C                   Z-ADD     1             CC                3 0
226900160509     C     'c'           LOOKUP    SV(CC)                                 96     LASCIATO AVVISO
227000160509     C   96              ADD       VA(CC)        VAAGGFUEL
227100160509     C                   Z-ADD     1             CC                3 0
227200160509     C     'A'           LOOKUP    SV(CC)                                 96     APPUNTAMENTO
227300160509     C   96              ADD       VA(CC)        VAAGGFUEL
227400160509     C                   Z-ADD     1             CC                3 0
227500160509     C     'B'           LOOKUP    SV(CC)                                 96     CONSEGNA DDT
227600160509     C   96              ADD       VA(CC)        VAAGGFUEL
227700160509     C                   Z-ADD     1             CC                3 0
227800160509     C     'C'           LOOKUP    SV(CC)                                 96     FACCH. ARR.
227900160509     C   96              ADD       VA(CC)        VAAGGFUEL
228000160509     C                   Z-ADD     1             CC                3 0
228100160509     C     'F'           LOOKUP    SV(CC)                                 96     FUORI MISURA
228200160509     C   96              ADD       VA(CC)        VAAGGFUEL
228300160509     C                   Z-ADD     1             CC                3 0
228400160509     C     'H'           LOOKUP    SV(CC)                                 96     DIRITTO FISSO
228500160509     C   96              ADD       VA(CC)        VAAGGFUEL
228600160509     C                   Z-ADD     1             CC                3 0
228700160509     C     'I'           LOOKUP    SV(CC)                                 96     SPESE DI GIACENZA
228800160509     C   96              ADD       VA(CC)        VAAGGFUEL
228900160509     C                   Z-ADD     1             CC                3 0
229000160509     C     'N'           LOOKUP    SV(CC)                                 96     ANTEPORTO
229100160509     C   96              ADD       VA(CC)        VAAGGFUEL
229200160509     C                   Z-ADD     1             CC                3 0
229300160509     C     'P'           LOOKUP    SV(CC)                                 96     AI PIANI
229400160509     C   96              ADD       VA(CC)        VAAGGFUEL
229500160509     C                   Z-ADD     1             CC                3 0
229600160509     C     'S'           LOOKUP    SV(CC)                                 96     SUPERMERCATO
229700160509     C   96              ADD       VA(CC)        VAAGGFUEL
229800160509     C                   Z-ADD     1             CC                3 0
229900160509     C     'U'           LOOKUP    SV(CC)                                 96     RITIRO
230000160509     C   96              ADD       VA(CC)        VAAGGFUEL
230100160509     C                   Z-ADD     1             CC                3 0
230200160509     C     'X'           LOOKUP    SV(CC)                                 96     CONS.DISAG.
230300160509     C   96              ADD       VA(CC)        VAAGGFUEL
230400160509     C                   Z-ADD     1             CC                3 0
230500160509     C     'p'           LOOKUP    SV(CC)                                 96     PIN CODE
230600160509     C   96              ADD       VA(CC)        VAAGGFUEL
230701170906     C                   Z-ADD     1             CC                3 0
230800160509     C     'z'           LOOKUP    SV(CC)                                 96     EXPO
230900160509     C   96              ADD       VA(CC)        VAAGGFUEL
231001170906     C                   Z-ADD     1             CC                3 0
231100160509     C     't'           LOOKUP    SV(CC)                                 96     Diritto Pren.ORM
231200160509     C   96              ADD       VA(CC)        VAAGGFUEL
231301170906     C                   Z-ADD     1             CC                3 0
231400160509     C     'k'           LOOKUP    SV(CC)                                 96     Packing List
231500160509     C   96              ADD       VA(CC)        VAAGGFUEL
231600160509     c
231700160509     C                   ENDSR
231800160509     C*****************************************************************
231900160509     C* Fuel Surcharge
232000160509     C*****************************************************************
232100160509     c     SR_fuel       BEGSR
232200160509     C                   CLEAR                   SCA_PB
232300160509     C                   CLEAR                   SCA_SPE
232400160509     C                   CLEAR                   SCA_SCA
232500160509      *
232600160509      * verifico se esiste già la varia non la calcolo
232700160509      *
232800160509     c     §$2VFS        lookup    sv                                     96
232900160509     c                   IF        %found
233000160509     c                   goto      Endfuel
233100160509     c                   endif
233200160509
233300160509      * verifico se cliente ha in tariffa il calcolo del FUEL Surcharge
233400160509      * altrimenti non vado a cercare nella cartello e vado a fine
233500160509     C                   MOVEL     'f'           FISO
233600160509     C                   MOVEL     'f'           WFISO
233700160509
233800160509     c                   z-add     1             a
233900160509     c     Wfiso         Lookup    Cp(a)                                  05
234000160509     C  N05              GOTO      ENDFUEL
234100160509      * aggancio la ds della tabella 1P
234200160509     c     A             Occur     DS1P
234300160509
234400160509     C     KISOT         CHAIN     TITPT01L
234500160509      * cliente non ha la tariffa  non faccio calcolo
234600160509     C                   IF        not %FOUND(TITPT01L)
234700160509     c                   goto      Endfuel
234800160509     c                   endif
234900160509      * ds x recupero % minima applicabile
235000160509     c                   eval      DTPT01 = TPTflo
235100160509      *
235200160509      * data prezzo base maggiore della data spedizone non faccio calcolo
235300160509     C                   IF        TPTDPB > TASDSP
235400160509     c                   goto      Endfuel
235500160509     c                   endif
235600160509      * Ricerco il valore minimo applicabile e lo scaglione corrispondente in vigore
235700160509      * alla data spedizione
235800160509      * solo se in tariffa non è stato specificato che non si deve applicare il VMA
235900160509
236000160509     c                   If        §TPTvma = 'N'
236100160509     c                   clear                   dpbvma
236200160509     c                   clear                   dpbsvm
236300160509     c                   else
236400160509      * si applicazione VMA
236500160509     C     TASDSP        SETLL     TIDPB01L
236600160509     C                   READ      TIDPB01L
236700160509     c                   if        not %found(tidpb01l)
236800160509     c                   clear                   dpbvma
236900160509     c                   clear                   dpbsvm
237000160509     c                   endif
237100160509     c                   endif
237200160509
237300160509      * Ricerco scaglione di appartenenza del prezzo base
237400160509
237500160509     C     TPTDPB        SETLL     TIPMG01L
237600160509     C                   READ      TIPMG01L
237700160509     C                   IF        NOT %EOF(TIPMG01L)
237800160509      *
237900160509     C                   Z-ADD     PMGSCA        SCA_PB
238000160509     C                   ENDIF
238100160509
238200160509      * Ricerco scaglione di appartenenza del prezzo del gasolio al momento della spedizione
238300160509
238400160509     C     TASDSP        SETLL     TIPMG01L
238500160509     C                   READ      TIPMG01L
238600160509     C                   IF        NOT %EOF(TIPMG01L)
238700160509      *
238800160509      * recupero il prezzo per ricercare il F.D. in tariffa
238900160509      *
239000160509     c                   z-add     pmgpmg        PMG_SGL
239100160509
239200160509     C                   Z-ADD     PMGSCA        SCA_SPE
239300160509     C                   ENDIF
239400160509      * verifico se il prezzo del gasolio al momento della spedizione è minore del valore minimo
239500160509      * applicabile sostituisco il prezzo del gasolio e il suo scaglione con il valore minimo
239600160509      * applicabile e lo scaglione corrispondente
239700160509
239800160509     c                   if        pmgpmg < dpbvma
239900160509     c                   z-add     dpbvma        PMG_SGL
240000160509     c                   z-add     dpbsvm        SCA_SPE
240100160509     c                   Endif
240200160509
240300160509      * calcolo gli scaglioni di aumento
240400160509
240500160509     C                   EVAL      SCA_SCA = SCA_SPE - SCA_PB
240600160509     C                   IF        SCA_SCA < 0
240700160509     C                   CLEAR                   SCA_SCA
240800160509     C                   ENDIF
240900160509      * calcolo dell'importo da addebitare al cliente
241000160509
241100160509     C                   CLEAR                   VARIA
241200160509     C                   CLEAR                   VARIAfuel_min
241300160509     C                   CLEAR                   IMP_SCA
241400160509
241500160509      * calcolo imponibile FUEL
241600160509      * recupero importi di isola e località disagiate ed espresso + nuove varie introdotte
241700160509      * dal 3/2/2014 dv 2473
241800160509     C                   EXSR      CERISO
241900160509      *
242000160509     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
242100160509     c                                       VAAGGFUEL
242200160509
242300160509      * se gli scaglioni scattati sono maggiore di zero cerco l'incidenza percentuale di
242400160509      * aumento nel dettaglio tariffario
242500160509    1C                   IF        SCA_SCA > 0
242600160509      * cambia con il progetto 666 aumento supplemento carburante in tariffa
242700160509      * per cercare il Fattore demoltiplicatore non usiamo più il numero di scaglioni
242800160509      * di aumento ma il prezzo medio del gasolio della settimana della data di spedizione
242900160509     C****               Z-ADD     SCA_SCA       TPDPES
243000160509     C                   Z-ADD     PMG_SGL       TPDPES
243100160509     C                   EXSR      CALACD
243200160509      * Se la percentuale di incidenza è maggiore di zero
243300160509    2C                   IF        AITR > 0
243400160509      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
243500160509      *                     per abbandonare il concetto di monete diverse dall'EURO)
243600160509     C                   EVAL(H)   VARIA = (IMP_SCA * aitr * sca_sca) / 100
243700160509
243800160509    2c                   Endif
243900160509    1c                   Endif
244000160509      * anche se il cliente ha negato la tariffa verifico
244100160509      * se nella tariffa particolare del cliente è stato espressa la % minima di apllicazione
244200160509      * calcolo l'importo del fuel utilizzando questa percentuale
244300160509
244400160509     c                   if        §tptpma > 0
244500160509     c                   eval(H)   variafuel_min =  (IMP_SCA * §TPTpma)/ 100
244600160509     c                   Endif
244700160509
244800160509      * prendo il valore della varia maggiore tra quella calcolata con la % minima applicabile
244900160509      * e quella calcolata con il FD della tariffa
245000160509
245100160509     c                   if        variafuel_min > Varia
245200160509     c                   eval      Varia =  variafuel_min
245300160509     c                   endif
245400160509
245500160509      **
245600160509    1c                   If        Varia > 0
245700160509     C                   Z-ADD     1             A
245800160509     C     ' '           LOOKUP    SV(A)                                  96
245900160509     C   96              MOVEL     §$2VFS        SV(A)
246000160509     C   96              Z-ADD     VARIA         VA(A)
246100160509     C  N96              ADD       VARIA         TASPOR
246200160509    1C                   ENDIF
246300160509      *
246400160509      *
246500160509     C     ENDFUEL       ENDSR
246600160509     C*****************************************************************
246700160509     C* SUPPLEMENTO CARBURANTE FEDEX
246800160509     C*****************************************************************
246900160509     C     SR_SUPCARB    BEGSR
247000160509      *
247100160509      * verifico se esiste già la varia
247200160509      *
247300160509     c     §$2VSC        lookup    sv                                     96
247400160509     c                   IF        not %found
247500160509
247600160509      * RICERCO IN BASE ALLA DATA SPEDIZIONE LA GIUSTA PERCENTUALE DI
247700160509      * SUPPLEMNENTO
247800160509
247900160509     C                   CLEAR                   VARIA
248000160509     C                   CLEAR                   IMP_SCA
248100160509     C                   CLEAR                   PER_SCA
248200160509     C                   Z-ADD     1             KX
248300160509    1c                   DO        *HIVAL
248400160509      * SCHIERA IN ORDINE DISCENDENTE
248500160509     C                   MOVE      DSCA(KX)      WDSPSC
248600160509      * SE DATA UGUALE A ZERO VADO A FINE
248700160509    2C                   IF        DS_DSC = *ZEROS
248800160509     C                   LEAVE
248900160509    2C                   ENDIF
249000160509      * SE DATA SPEDIZIONE MAGGIORE DI QUELLA IN SCHIERA RECUPERO LA PERCENTUALE DI CALCOLO
249100160509    2C                   IF        TASDSP >=  DS_DSC
249200160509     c                   z-add     DS_psc        PER_SCA
249300160509     C                   LEAVE
249400160509     C                   ELSE
249500160509      * SE DATA MINORE LEGGO LA DATA SUCCESSIVA
249600160509    3C                   IF        KX < 999
249700160509     C                   ADD       1             KX
249800160509     C                   ELSE
249900160509     C                   LEAVE
250000160509    3C                   ENDIF
250100160509    2C                   ENDIF
250200160509
250300160509    1C                   ENDDO
250400160509      * SE PERCENTUALE MAGGIORE DI ZERO ESEGUO IL CALCOLO
250500160509    1C                   IF        PER_SCA > 0
250600160509      * CALCOLO L'IMPONIBILE
250700160509      * recupero importi di isola e località disagiate ed espresso
250800160509     C                   EXSR      CERISO
250900160509      *
251000160509     c* Di fatto qui l'espresso non ci sarà mai!!! bolla estera
251100160509     C                   EVAL      IMP_SCA = TASPOR + TASINL + VAISO+VATSP +
251200160509     c                                       VAAGGFUEL
251300160509      * VARIA CON DECIMALI (non faccio i calcoli anche con la varia senza decimali
251400160509      *                     per abbandonare il concetto di monete diverse dall'EURO)
251500160509     C                   EVAL(H)   VARIA = (IMP_SCA * PER_SCA) / 100
251600160509      **
251700160509     C                   Z-ADD     1             A
251800160509     C     ' '           LOOKUP    SV(A)                                  96
251900160509     C   96              MOVEL     §$2VSC        SV(A)
252000160509     C   96              Z-ADD     VARIA         VA(A)
252100160509     C  N96              ADD       VARIA         TASPOR
252200160509    1C                   ENDIF
252300160509      *
252400160509     C                   ENDIF
252500160509      *
252600160509     C                   ENDSR
252700160509     C*****************************************************************
252800160509     C* TASSAZIONE TARIFFE PARTICOLARI
252900160509     C*****************************************************************
253000160509     C     TASPAR        BEGSR
253100160509     C     *LIKE         DEFINE    TASVA1        TASCP
253200160509     C     *LIKE         DEFINE    TASVA1        TASISO
253300160509     C     *LIKE         DEFINE    TASTC1        TASTC
253400160509     C                   CLEAR                   TASCP
253500160509     C                   CLEAR                   WNOEST
253600160509     C* SE NON ESISTE LA SIGLA DELLA VARIA O E' GIA' IN TASSAZIONE
253700160509     C*  NON RITASSO NE APPLICO L'ESPRESSO
253800160509     C**
253900160509     C                   Z-ADD     1             A
254000160509     C     TASTC         LOOKUP    CP(A)                                  05
254100160509     C  N05              GOTO      ENDPAR
254200160509     C**
254300160509     C     A             OCCUR     DS1P
254400160509     C                   Z-ADD     1             A
254500160509     C     §1PVAR        LOOKUP    SV(A)                                  05
254600160509     C* SIGLA VARIA NON PRESENTE --> CALCOLO
254700160509     C     *IN05         IFEQ      *OFF
254800160509     C                   MOVEL     'S'           WNOEST            1
254900160509     C                   EXSR      CALCP
255000160509     C**
255100160509     C     TASCP         IFGT      0
255200160509      * se sto tassando la tariffa particolare m = avviso affidamento spedizione
255300160509      * ed il flag EMD è uguale a "E" (via sms che mail) raddoppio la varia
255400160509     c                   if        §1pvar = §$2VAD and
255500160509     c                             §asemd = 'E'
255600160509     c                   eval      tascp = tascp * 2
255700160509     c                   endif
255800160509      *
255900160509     C                   Z-ADD     1             A
256000160509     C     ' '           LOOKUP    SV(A)                                  96
256100160509     C   96              MOVEL     §1PVAR        SV(A)
256200160509     C   96              Z-ADD     TASCP         VA(A)
256300160509     C  N96              ADD       TASCP         TASPOR
256400160509     C                   END
256500160509     C                   END
256600160509     C     ENDPAR        ENDSR
256700160509     C******************************************************
256800160509     C** CALCOLO PESO TASSABILE PER TARIFFA A PESO
256900160509     C******************************************************
257000160509     C     CALPT         BEGSR
257100160509     C** MI MEMORIZZO SU BLTAS IL PESO TASSABILE SE
257200160509     C** IL PESO E' SUPERIORE AL MINIMO TASSABILE
257300160509     C** SE ESISTE IL RAPPORTO PESO VOLUME IN TARIFFA  E VOL IN BOLLA
257400160509     C** IL PESO TASSABILE SU BLTAS E' SEMPRE ARROTONDATO AL KG SUPERIO
257500160509     C                   CLEAR                   TASPVL
257600160509     C***  UNOCTR        IFEQ      0
257700160509     C**** UNOCTR        OREQ      3
257800160509      * modifico --- il peso tassabile si stampa e si calcola nei seguenti casi :
257900160509      * Tariffa a quintale "0" il peso da fatturare maggiore del  minimo tassabile
258000160509      *                        (definito nella tabella TR = 100) oppure volume da fatturare
258100160509      *                        maggiore di zero e rapporto peso volume nello scaglione della
258200160509      *                        tariffa maggiore di zero
258300160509      * Tariffa a kg       "3" non controllo il peso da fatturare maggiore del minimo tassabile
258400160509      *                        (definito nella tabella TR = 0,1) ma solo volume da fatturare
258500160509      *                        maggiore di zero e rapporto peso volume nello scaglione della
258600160509      *                        tariffa maggiore di zero
258700160509     C***  TARIFFA A QUINTALE
258800160509
258900160509     C     UNOCTR        IFEQ      0
259000160509
259100160509     C     USAPKF        IFGT      MINTAS
259200160509     C     PESTAS        ADD       0,9           TASPVL
259300160509     C                   ELSE
259400160509     C     TASVLF        IFNE      0
259500160509     C     RAPPV         ANDNE     0
259600160509     C     PESTAS        ADD       0,9           TASPVL
259700160509     C                   ENDIF
259800160509     C                   ENDIF
259900160509     C                   ENDIF
260000160509
260100160509     C***  TARIFFA A KG
260200160509
260300160509     C     UNOCTR        IFEQ      3
260400160509
260500170906     C     USAVLF        IFNE      0
260600160509     C     RAPPV         ANDNE     0
260700160509     C     PESTAS        ADD       0,9           TASPVL
260800160509     C                   ENDIF
260900160509     C                   ENDIF
261000160509
261100160509     C                   ENDSR
261200160509     C******************************************************
261300160509     C** CALCOLA VALORE DA ASSICURARE PER TARIFFA A VALORE
261400160509     C******************************************************
261500160509     C     CALVAS        BEGSR
261600160509     C     *LIKE         DEFINE    TASIAS        IMPVAS
261700160509     C                   Z-ADD     0             IMPVAS
261800160509     C                   MOVEL     'C'           FISO
261900160509     C                   MOVEL     'C'           WFISO             1
262000160509     C**FLAG ASSICURAZIONE X CONTO = C
262100160509     C                   Z-ADD     1             A
262200160509     C     WFISO         LOOKUP    CP(A)                                  05
262300160509     C  N05              GOTO      DOPVA1
262400160509     C**----------------------------------------
262500160509     C     KISOT         CHAIN     TITPT01L                           28
262600160509     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
262700160509     C     *IN28         IFEQ      *ON
262800160509     C     TPTATB        ORNE      ' '
262900160509     C                   GOTO      DOPVA1
263000160509     C                   ENDIF
263100160509     C**
263200160509     C** ESCLUDO GLI ANNULLATI
263300160509     C     TPTFVM        IFEQ      '3'
263400160509     C     TPTVLM        MULT      USAPKF        IMPVAS
263500160509     C                   END
263600160509     C*PESO
263700160509     C     TPTFVM        IFEQ      '1'
263800160509     C     TPTVLM        MULT      TASNCL        IMPVAS
263900160509     C                   END
264000160509     C* COLLI
264100160509     C     TPTFVM        IFEQ      '2'
264200160509     C                   Z-ADD     TPTVLM        IMPVAS
264300160509     C                   END
264400160509     C*SPEDIZIONE
264500160509     C     DOPVA1        ENDSR
264600160509     C******************************************************
264700160509     C** CALCOLA VARIA R  --- ASSICURAZIONE PER CONTO
264800160509     C******************************************************
264900160509     C     CALVR         BEGSR
265000160509     c* Il falg task88 cambia significato:indica se bolla con mandato o no
265100160509     c*
265200160509     c* con mandato significa: x mandato a val variabile--> tutte le bolle
265300160509     c*                        x mandato a val.indicato --> solo bolle con
265400160509     c*                        tasias=0 o con tasias <=al valore del mandat
265500160509     c*
265600160509     c* Imposto task88=N se mandato
265700160509     c* Imposto task88=S se no mandato ma tasias>0
265800160509     c* Imposto task88=X per bolle senza mandato e senza importo
265900160509     c
266000160509     C                   MOVEL     'X'           TASK88
266100160509     C** 88 = SI CALCOLA SOLO SE ESISTE IL VALORE MERCE IN BOLLA
266200160509     C     KSCCOD        IFEQ      8888
266300160509     C     KSCCOD        OREQ      9999
266400160509     C     TASIAS        IFEQ      0
266500160509     C**                 MOVEL     'S'           TASK88
266600160509     C                   GOTO      ENDVR
266700160509     C                   END
266800160509     C                   END
266900160509     C**
267000160509     C** SE TIPO BOLLA CHE NON PREVEDE L'ASSICURAZIONE, ESCO SENZA
267100160509     C**  DARE ERRORE
267200160509     C**   TASIAS        IFEQ      0
267300160509     C**   TASTBL        LOOKUP    TBN                                    05
267400160509     C** 05              MOVEL     'S'           TASK88
267500160509     C** 05              GOTO      ENDVR
267600160509     C**                 ENDIF
267700160509     C**
267800160509     C** SE IMPORTO D'ASSICURARE MAGGIORE DI ZEROS MUOVO 'S' TASK88
267900160509     C** PER STAMPA BOLLE CON ASSICURAZIONE PER TABULATONE DELLA CONSULDANNI
268000160509     C**   TASIAS        IFGT      0
268100160509     C**                 MOVEL     'S'           TASK88
268200160509     C**                 ENDIF
268300160509     C**
268400160509     C                   SETOFF                                       94
268500160509     C     *LIKE         DEFINE    TASIAS        IASLIT
268600160509     C                   Z-ADD     TASIAS        IASLIT
268700160509     C     IASLIT        IFNE      0
268800160509     C                   Z-ADD     1             K
268900160509     C     TASVAS        LOOKUP    CV(K)                                  05
269000160509     C     *IN05         IFEQ      *OFF
269100160509     C                   SETON                                        94
269200160509     C                   MOVEL     MSG(2)        TASMSG
269300160509     C                   GOTO      FINE
269400160509     C                   ENDIF
269500160509     C** NON ESISTE CAMBIO - MANCA TARIFFA
269600160509     C**
269700160509     C* CALCOLO L'IMPORTO D'ASSICURARE NELLA MONETA DELLA TARIFFA SE E' DIVERSA
269800160509     C* DALLA VALUTA DELLA TARIFFA
269900160509     C     §TADIV        IFNE      TASVAS
270000160509     C                   CLEAR                   YEUR
270100160509     C                   MOVEL     TASVAS        YECDVI
270200160509     C                   MOVEL     §TADIV        YECDVO
270300160509     C                   Z-ADD     IASLIT        YECIMI
270400160509     C   12              MOVE      '2'           YECDCO
270500160509     C  N12              MOVE      '0'           YECDCO
270600160509     C*
270700160509     C                   CALL      'YEURCO'
270800160509     C                   PARM                    YEUR
270900160509     C*
271000160509     C     YECESI        IFEQ      ' '
271100160509     C                   Z-ADD     YECIMO        IASLIT
271200160509     C                   END
271300160509     C                   ENDIF
271400160509     C                   ENDIF
271500160509     C**----------------------------------------
271600160509     C                   MOVEL     'C'           FISO
271700160509     C                   MOVEL     'C'           WFISO
271800160509     C**FLAG ASSICURAZIONE X CONTO = C
271900160509     C                   Z-ADD     1             A
272000160509     C     WFISO         LOOKUP    CP(A)                                  05
272100160509     C  N05              GOTO      ENDVR
272200160509      * AGGANCIO LA DS DELA TABELLA 1P
272300160509     C     A             OCCUR     DS1P
272400160509     C**----------------------------------------
272500160509     C     KISOT         CHAIN     TITPT01L                           28
272600160509     C** NON ESISTE TARIFFA ASS.X CONTO CLIENTE
272700160509     C     *IN28         IFEQ      *ON
272800160509     C     TPTATB        ORNE      *BLANKS
272900160509     C                   GOTO      DOPAC1
273000160509     C                   ENDIF
273100160509     C**
273200160509     C** CONTROLLO IL TIPO APPLICAZIONE DELLA TARIFFA ASSIC X CONTO
273300160509     C**  SE INCONGRUENTE CON TIPO BOLLA PRENDO LA CARTELLO
273400160509     C     TPTAPL        IFEQ      'A'
273500160509     C     TIPBOL        ANDEQ     'F'
273600160509     C     TPTAPL        OREQ      'P'
273700160509     C     TIPBOL        ANDEQ     'A'
273800160509     C                   GOTO      DOPAC1
273900160509     C                   ENDIF
274000160509     C**
274100160509     C* VALORE MERCE = 0 --> ESCLUDO SE NON E' UN VERO MANDATO
274200160509     C                   SETOFF                                       94
274300160509    1C     TPTVLM        IFEQ      0
274400160509     C* CHE SIA UN MANDATO FITTIZIO O UN MANDATO A VALORE VARIABILE
274500160509     C*  DEVO SEMPRE SCRIVERE LA BOLLA COME 8888 PERCHE L'ASSICURAZIONE
274600160509     C*  VUOLE VEDERE BOLLA PER BOLLA QUANTO SONO ASSICURATE
274700160509     C****               MOVEL     'S'           TASK88
274800160509     C**
274900160509    2C     IASLIT        IFEQ      0
275000160509     c* MANDATO FITTIZIO
275100160509    3C     TPTTMA        IFEQ      'F'
275200160509     C                   GOTO      ENDVR
275300160509    3C                   ENDIF
275400160509     c**
275500160509     C** ES - non serve: già fatto    sopra!! Ripetizione inutile
275600160509    3C**   TPTAPL        IFEQ      'P'
275700160509     C**   TIPBOL        COMP      'F'                                    94
275800160509    3C**                 END
275900160509    3C**   TPTAPL        IFEQ      'A'
276000160509     C**   TIPBOL        COMP      'A'                                    94
276100160509    3C**                 END
276200160509    3C**   TPTAPL        IFEQ      ' '
276300160509     C**                 SETON                                            94
276400160509    3C**                 END
276500160509     c
276600160509     c
276700160509     c* anche se manca tariffa è comunque con mandato
276800160509     c                   eval      task88='N'
276900160509     c*
277000160509     c* non devo dare manca tariffa se tipo bolla che non prevede
277100160509     c* Assicurazione
277200160509     C     TASTBL        LOOKUP    TBN                                    05
277300160509     c                   if        not *in05
277400160509     c                   eval      *in94='1'
277500160509     C                   MOVEL     MSG(4)        TASMSG
277600160509     C                   GOTO      FINE
277700160509     c                   endif
277800160509     c
277900160509   x2c                   else
278000160509     c* Mandato a valore variabile con importo: se fittizio 8888 altriment
278100160509     c* con mandato
278200160509    3C     TPTTMA        IFEQ      'F'
278300160509     c                   eval      task88='S'
278400160509     c                   else
278500160509     c                   eval      task88='N'
278600160509    3C                   END
278700160509    2C                   END
278800160509     c
278900160509     C** MANCA TARIFFA: MANDATO CON VALORE = 0
279000160509     C**                E NON ESISTE VALORE IN BOLLA (SOLO COD.)
279100160509   X1C                   ELSE
279200160509    2C     IASLIT        IFGT      0
279300160509     C                   Z-ADD     0             VALSPE           15 3
279400160509    3C     TPTFVM        IFEQ      '3'
279500160509     C     TPTVLM        MULT      USAPKF        VALSPE
279600160509    3C                   END
279700160509     C*PESO
279800160509    3C     TPTFVM        IFEQ      '1'
279900160509     C     TPTVLM        MULT      TASNCL        VALSPE
280000160509    3C                   END
280100160509     C* COLLI
280200160509    3C     TPTFVM        IFEQ      '2'
280300160509     C                   Z-ADD     TPTVLM        VALSPE
280400160509    3C                   END
280500160509      *
280600160509      * ARROTONDO AL DECIMALE DELLA TARIFFA
280700160509     C                   CLEAR                   YEUR
280800160509     C                   MOVEL     §TADIV        YECDVI
280900160509     C                   MOVEL     §TADIV        YECDVO
281000160509     C                   Z-ADD     VALSPE        YECIMI
281100160509     C                   CALL      'YEURCO'
281200160509     C                   PARM                    YEUR
281300160509     C*
281400160509     C     YECESI        IFEQ      ' '
281500160509     C                   Z-ADD     YECIMO        VALSPE
281600160509     C                   END
281700160509     C*SPEDIZIONE
281800160509    3C     IASLIT        IFGT      VALSPE
281900160509     C                   GOTO      DOPAC1
282000160509     c                   else
282100160509     c
282200160509     c* Se valore rientra nel mandato, imposto che si tratta di mandato
282300160509     C                   MOVEL     'N'           TASK88
282400160509    3C                   END
282500160509     c
282600160509   x2c                   else
282700160509     c* se senza valore in bolla ma madanto con valore -->ok da mandato
282800160509     c                   eval      task88='N'
282900160509    2C                   END
283000160509    1C                   END
283100160509     c
283200160509     C** SE IL CLIENTE E' 8888 HO IMPOSTATO PRIMA LA TARIFFA DI CARTELL
283300160509     C     KSCCOD        IFEQ      8888
283400160509     C                   MOVEL     'S'           TASK88
283500160509     C                   ENDIF
283600160509     c
283700160509     c** Anche se lo considero con mandato, se non c'e' importo e tipo
283800160509     c**  bolla senza assicurazione, non calcolo varia R
283900160509     C     TASIAS        IFEQ      0
284000160509     C     TASTBL        LOOKUP    TBN                                    05
284100160509     C   05              GOTO      ENDVR
284200160509     C                   ENDIF
284300160509     C**
284400160509     C                   GOTO      DOPAC2
284500160509     C     DOPAC1        TAG
284600160509     C** NON ESISTE TARIFFA AXC DEL CLIENTE
284700160509     C*-----------------------------------------------------------------
284800160509     C* SE USATA SI TRATTA DI TARIFFA DI CARTELLO
284900160509     C***                MOVEL     'S'           TASK88
285000160509     C     IASLIT        CABEQ     0             ENDVR
285100160509     c                   eval      task88='S'
285200160509      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
285300160509     C     KTPTC         CHAIN     TITPT01L                           28
285400160509      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
285500160509     C     *IN28         IFEQ      *ON
285600160509     C     TPTATB        OREQ      'A'
285700160509     C                   GOTO      ENDVR
285800160509     C                   ENDIF
285900160509     C*-----------------
286000160509     C     DOPAC2        TAG
286100160509     C** SE NON SERVE --> ESCO DAL CALCOLO
286200160509     C** TIPO APPLICAZIONE
286300160509     C** P - SOLO PER FRANCHI
286400160509     C** A - SOLO PER ASSEGNATI
286500160509    1C     TPTAPL        IFNE      *BLANKS
286600160509     C     IASLIT        ANDEQ     0
286700160509    2C     TIPBOL        IFEQ      'F'
286800160509    3C     TPTAPL        IFEQ      'A'
286900160509     C                   GOTO      ENDVR
287000160509    3C                   ENDIF
287100160509   X2C                   ELSE
287200160509    3C     TPTAPL        IFEQ      'P'
287300160509     C                   GOTO      ENDVR
287400160509    3C                   ENDIF
287500160509    2C                   END
287600160509    1C                   END
287700160509     C**
287800160509     C     *LIKE         DEFINE    TASIAS        VALVR
287900160509     C     *LIKE         DEFINE    TASIAS        ASCPES
288000160509     C     *LIKE         DEFINE    TASIAS        ASCPE2
288100160509     C                   Z-ADD     0             VALVR
288200160509    1C     IASLIT        IFEQ      0
288300160509    2C     TPTFVM        IFEQ      '3'
288400160509     C     USAPKF        MULT      TPTVLM        VALVR
288500160509    2C                   END
288600160509    2C     TPTFVM        IFEQ      '1'
288700160509     C     TASNCL        MULT      TPTVLM        VALVR
288800160509    2C                   END
288900160509    2C     TPTFVM        IFEQ      '2'
289000160509     C                   Z-ADD     TPTVLM        VALVR
289100160509    2C                   END
289200160509   X1C                   ELSE
289300160509     C                   Z-ADD     IASLIT        VALVR
289400160509    1C                   END
289500160509     C***** OBSOLETO NON RESTITUISCO + IL VALORE IN LIRE MA NELLA DIVISA DELLA TARIFFA
289600160509     C***** PERO' ARROTONDO AL DECIMALE DELLA TARIFFA
289700160509     C                   CLEAR                   YEUR
289800160509     C                   MOVEL     §TADIV        YECDVI
289900160509     C                   MOVEL     §TADIV        YECDVO
290000160509     C                   Z-ADD     VALVR         YECIMI
290100160509     C                   CALL      'YEURCO'
290200160509     C                   PARM                    YEUR
290300160509     C*
290400160509     C     YECESI        IFEQ      ' '
290500160509     C                   Z-ADD     YECIMO        VALVR
290600160509     C                   END
290700160509     C* RESTITUISCO IN OUTPUT L'IMPORTO DA ASSICURARE
290800160509     C                   Z-ADD     VALVR         TASIAL
290900160509     C** VALORE SPEDIZIONE PER CALCOLO ASSICURAZIONE
291000160509     C**
291100160509     C** CALCOLO SOLO SE NON HO IMPOSTATO IL FLAG
291200160509    1C     PARCA         IFEQ      ' '
291300160509     C**
291400160509     C** SE C'E' GIA' LA VARIA R NON LA RICALCOLO
291500160509     C                   Z-ADD     1             A
291600160509     C     §$2VRR        LOOKUP    SV(A)                                  05
291700160509    2C     *IN05         IFEQ      *OFF
291800160509     C                   MOVEL     TPTTPG        WTPG
291900160509     C                   MOVEL     TPTRPV        WRPV
292000160509     C                   CLEAR                   WDET
292100160509     C                   Z-ADD     TPTARL        WARL
292200160509     C                   Z-ADD     TPTARF        WARF
292300160509     C                   Z-ADD     TPTARO        WARO
292400160509     C                   EXSR      CALCOL
292500160509     C**
292600160509     C** A VALORE
292700160509    3C     TPTTPG        IFEQ      'V'
292800160509     C                   Z-ADD     VALVR         ISOPES
292900160509    3C                   END
293000160509     C     *LIKE         DEFINE    TASIAS        TPDPES
293100160509     C**
293200160509     C**CALCOLO IMPORTO ASSICURAZIONE PER CONT
293300160509     C                   Z-ADD     ISOPES        TPDPES
293400160509     C                   EXSR      CALACD
293500160509     C                   Z-ADD     VARIA         WVARIR
293600160509    2C                   ENDIF
293700160509    1C                   ENDIF
293800160509     C     ENDVR         ENDSR
293900160509     C******************************************************
294000160509     C** CALCOLA VARIA d  --- A C BASE
294100160509     C******************************************************
294200160509     c     CALVACB       BEGSR
294300160509     c* Il falg task88 cambia significato:indica se bolla con mandato o no
294400160509     c* Imposto task88=N se mandato
294500160509     c* Lascio  task88=X per bolle senza mandato e senza importo
294600160509
294700160509      **----------------------------------------
294800160509     c                   movel     'd'           Fiso
294900160509     c                   movel     'd'           WFiso
295000160509      ** Flag A C BASE  = d
295100160509     c                   z-add     1             a
295200160509     c     Wfiso         Lookup    Cp(a)                                  05
295300160509     c  N05              Goto      EndVacb
295400160509      * aggancio la ds della tabella 1P
295500160509     c     A             Occur     DS1P
295600160509      **----------------------------------------
295700160509     c     Kisot         Chain     TITPT01L
295800160509      ** esiste Tariffa A C base
295900160509    1c                   If        %found(titpt01l)
296000160509      **
296100160509      ** Controllo il tipo applicazione della tariffa A C Base
296200160509      **
296300160509    2c                   If        (tptapl = 'A' and tipbol = 'A') or
296400160509     c                             (tptapl = 'P' and tipbol = 'F') or
296500160509     c                              tptapl = ' '
296600160509      *
296700160509     c                   eval      task88='N'
296800160509      ** Anche se lo considero con mandato,  tipo
296900160509      **  bolla senza assicurazione, non calcolo varia d
297000160509     c     TAStbl        Lookup    Tbn                                    05
297100160509     c   05              Goto      ENDVacb
297200160509
297300160509      * calcolo valore assicurato
297400160509
297500160509     c     *Like         Define    TASias        VALVacb
297600160509
297700160509     c                   clear                   VALVacb
297800160509
297900160509      * Peso
298000160509    3c                   If        TPTfvm = '3'
298100160509     c     TPTvlm        Mult      USAPkf        Valvacb
298200160509    3c                   Endif
298300160509      * Colli
298400160509    3c                   If        TPTfvm = '1'
298500160509     c     TPTvlm        Mult      TASncl        Valvacb
298600160509    3c                   Endif
298700160509      * Spedizione
298800160509    3c                   If        TPTfvm = '2'
298900160509     c                   Z-add     TPTvlm        Valvacb
299000160509    3c                   Endif
299100160509      *
299200160509      * Arrotondo al decimale della tariffa
299300160509     c                   Clear                   YEUR
299400160509     c                   Movel     §TAdiv        YECdvi
299500160509     c                   Movel     §TAdiv        YECdvo
299600160509     c                   Z-add     VALvacb       YECimi
299700160509     c                   call      'YEURCO'
299800160509     c                   Parm                    YEUR
299900160509      *
300000160509     c                   If        YECesi = ' '
300100160509     c                   Z-add     YECimo        VALvacb
300200160509     c                   Endif
300300160509
300400160509      ** Calcolo solo se non ho impostato il flag
300500160509    3c                   IF        Parca = ' '
300600160509
300700160509      ** Se c'è' già la varia non la ricalcolo
300800160509     c                   Z-add     1             a
300900160509     c     §$2acb        Lookup    sv(a)                                  05
301000160509    4c     *IN05         Ifeq      *off
301100160509     c                   movel     TPTtpg        Wtpg
301200160509     c                   Movel     TPTrpv        Wrpv
301300160509     c                   clear                   Wdet
301400160509     c                   Z-add     TPTarl        Warl
301500160509     c                   Z-add     TPTarf        Warf
301600160509     c                   Z-add     TPTaro        Waro
301700160509     c                   Exsr      Calcol
301800160509
301900160509      ** A Valore
302000160509    5c                   IF        tpttpg =  'V'
302100160509     c                   z-add     valvacb       ISOpes
302200160509    5c                   Endif
302300160509
302400160509     c                   Z-add     ISOpes        TPDpes
302500160509     c                   Exsr      Calacd
302600160509     c                   Z-add     varia         Wvariacb
302700160509    4c                   endif
302800160509
302900160509    3c                   endif
303000160509
303100160509    2c                   endif
303200160509    1c                   Endif
303300160509
303400160509     c     EndVacb       ENDSR
303500160509     C******************************************************
303600160509     C** CALCOLA TARIFFA PARTICOLARE
303700160509     C******************************************************
303800160509     C     CALACD        BEGSR
303900160509     C* CAMPO VARIA CON  DECIMALI
304000160509     C                   Z-ADD     0             VARIA            15 3
304100160509     C* CAMPO VARIA SENZA DECIMALI ALTRIMENTI NON FUNZIONA L'ARROTONDAMENTO
304200160509     C                   Z-ADD     0             VARIAD           15 0
304300160509     C*
304400160509     C                   DO        100           A
304500160509     C     A             OCCUR     ACDS
304600160509     C                   CLEAR                   ACDS
304700160509     C                   END
304800160509     C** PULIZIA DS TARIFFA
304900160509     C*
305000160509     C* SE ESISTE ALMENO UNA RIGA DI DETTAGLIO E NON RIESCO AD APPLICAR
305100160509     C*  QUESTA TAR.PARTICOLARE --> MANCA TARIFFA
305200160509     C* BOLLA IMPORT / ESXPORT
305300160509     C     WBOEST        IFEQ      'E'
305400160509     C     §1PCTE        IFNE      *BLANKS
305500160509     C                   MOVEL     §1PCTE        ISOCTS
305600160509     C                   GOTO      POIAC
305700160509     C                   END
305800160509     C                   ELSE
305900160509     C     §1PCTS        IFNE      *BLANKS
306000160509     C                   MOVEL     §1PCTS        ISOCTS
306100160509     C                   GOTO      POIAC
306200160509     C                   END
306300160509     C                   END
306400160509     C** CODICI TASSAZIONE FISSI DA TABELLA TARIFFE PARTICOLARI
306500160509     C                   MOVEL     TASCTS        ISOCTS            2
306600160509     c
306700160509      * Se assegnato e tariffa FEDEX metto in ISOCTS  quello mittente
306800160509     c                   If        TIPBOL = 'A' and WBOFED = 'S'
306900160509     c                   movel     tasmct        isocts
307000160509     c                   endif
307100160509     c* Se devo utilizzare quello del mittente per via della
307200160509     c*  reversibilità --> lo uso
307300160509     c                   if        applicarevers='S'
307400160509     c                   movel     tasmct        isocts
307500160509     c                   endif
307600160509      *
307700160509     C     KISOD         SETLL     TITPD02L                               28
307800160509     C   28              GOTO      DOPAC3
307900160509     C                   Z-ADD     1             A
308000160509     C     ISOCTS        LOOKUP    CTS(A)                                 05
308100160509     C   05A             OCCUR     DSCT
308200160509     C   05              MOVEL     §CTRAP        ISOCTS
308300160509     C  N05              MOVEL     *BLANKS       ISOCTS
308400160509     C** REGIONE
308500160509     C     KISOD         SETLL     TITPD02L                               28
308600160509     C   28              GOTO      DOPAC3
308700160509     C     WBOEST        IFEQ      'E'
308800160509     C                   MOVEL     'EE'          ISOCTS
308900160509     C                   ELSE
309000160509     C                   MOVEL     'IT'          ISOCTS
309100160509     C                   END
309200160509     C     POIAC         TAG
309300160509     C     KISOD         SETLL     TITPD02L                               28
309400160509     C**N28                GOTO ENDACD
309500160509     C** N 28 --> MANCA TARIFFA
309600160509     C     *IN28         IFEQ      *OFF
309700160509     C* SE NON È PRESENTE NESSUNA RIGA DI DETTAGLIO, NON DO
309800160509     C*  MANCA TARIFFA: SIGNIFICA CHE È INSERITA UNA TASSAZIONE =0
309900160509     C     KISOD2        SETLL     TITPD02L                               28
310000160509     C  N28              GOTO      ENDACD
310100160509     C*
310200160509     C                   SETON                                        94
310300160509     C                   MOVEL     MSG(17)       TASMSG
310400160509     c                   eval      %subst(tasmsg: 47: 2) = TPTFTC
310500160509     C                   GOTO      FINE
310600160509     C                   ENDIF
310700160509     C**
310800160509     C** NON TROVATA TARIFFA
310900160509     C     DOPAC3        TAG
311000160509     C                   DO        *HIVAL        A
311100160509     C     KISOD         READE     TITPD02L                               28
311200160509     C   28              GOTO      DOPAC4
311300160509     C     A             OCCUR     ACDS
311400160509     C* VERIFICO SE STO CARICANDO LA TARIFFA DI CARTELLO
311500160509     C     TPTKSC        IFEQ      CARKSC
311600160509     C     TPTCTR        ANDEQ     CARCTR
311700160509     C     TPTPRG        ANDEQ     CARPRG
311800160509     C* E LA DIVISA DELLA TARIFFA DI CARTELLO E' DIVERSA DALLA DIVISA DELLA
311900160509     C* TARIFFA DEL CLIENTE  CONVERTO I VALORI O SCAGLIONI NELLA DIVISA
312000160509     C* DELLA TARIFFA DEL CLIENTE
312100160509     C     §TADIV        ANDNE     DIVCAR
312200160509     C*
312300160509     C                   EXSR      CONTPD
312400160509     C*
312500160509     C                   ENDIF
312600160509     C*
312700160509     C                   Z-ADD     TPDSGL        ASGL
312800160509     C                   Z-ADD     TPDITR        AITR
312900160509     C                   Z-ADD     TPDMIN        AMIN
313000160509     C                   Z-ADD     TPDMAX        AMAX
313100160509     C                   MOVEL     TPDAIN        AAIN
313200160509     C                   END
313300160509     C     DOPAC4        TAG
313400160509     C                   DO        100           A
313500160509     C     A             OCCUR     ACDS
313600160509     C     ASGL          IFGE      TPDPES
313700160509     C                   GOTO      DOPAC5
313800160509     C                   END
313900160509     C                   END
314000160509     C     ASGL          IFLT      TPDPES
314100160509     C                   SETON                                        94
314200160509     C                   MOVEL     MSG(7)        TASMSG
314300160509     c                   eval      %subst(tasmsg: 47: 2) = TPDFTC
314400160509     C                   GOTO      FINE
314500160509     C                   ENDIF
314600160509     C** NON TROVATO SCAGLIONE ESATTO
314700160509     C     DOPAC5        TAG
314800160509     C*-----------
314900160509     C     ASGL          IFLE      MINTAS
315000160509     C                   Z-ADD     MINTAS        TPDPES
315100160509     C                   END
315200160509     C     TPDPES        IFLE      MINTAS
315300160509     C                   Z-ADD     MINTAS        TPDPES
315400160509     C                   END
315500160509     C                   Z-ADD     0             TPDQLI           18 6
315600160509     C* DIVIDO PER 100 TARIFFA 0 PER AVERE QUINTALI
315700160509     C*                TARIFFA 4 PERCHE' E' TARIFFA %
315800160509     C     WTPG          IFEQ      '0'
315900160509     C     WTPG          OREQ      '4'
316000160509     C     TPDPES        DIV       100           TPDQLI
316100160509     C                   ELSE
316200160509     C                   Z-ADD     TPDPES        TPDQLI
316300160509     C                   END
316400160509     C** PESO TASSABILE
316500160509      * se trovato dettaglio tariffario della tariffa "f" vado a fine routine in quanto il calcolo
316600160509      * viene eseguito nella routine FUEL
316700160509     c                   If        wfiso = 'f'
316800160509     c                   goto      endacd
316900160509     c                   endif
317000160509
317100160509     C   12AITR          MULT(H)   TPDQLI        VARIA
317200160509     C  N12AITR          MULT(H)   TPDQLI        VARIAD
317300160509     C                   MOVEL     AAIN          FLGISO
317400160509     C*
317500160509     C     VARIA         IFGT      0
317600160509     C     *IN12         ANDEQ     *ON
317700160509     C     VARIAD        ORGT      0
317800160509     C     *IN12         ANDEQ     *OFF
317900160509     C*
318000160509     C     WTPG          IFEQ      'V'
318100160509     C     WTPG          OREQ      'T'
318200160509     C   12VARIA         DIV(H)    100           VARIA
318300160509     C  N12VARIAD        DIV(H)    100           VARIAD
318400160509     C                   ENDIF
318500160509     C                   ENDIF
318600160509     C**
318700160509     C* SPOSTO L'IMPORTO DEL CAMPO VARIAD NEL CAMPO VARIA SOLO SE E' STATO USATO
318800160509     C  N12              Z-ADD     VARIAD        VARIA
318900160509     C*
319000160509     C** MINIMO
319100160509     C     AMIN          IFGT      VARIA
319200160509     C                   Z-ADD     AMIN          VARIA
319300160509     C                   END
319400160509     C** MASSIMO
319500160509     C     AMAX          IFGT      0
319600160509     C     AMAX          IFLT      VARIA
319700160509     C                   Z-ADD     AMAX          VARIA
319800160509     C                   END
319900160509     C                   END
320000160509     C** SE VARIA CON ESENZIONE IVA DEVE AVERE SOLO 2 DCEIMALI
320100160509     C* CERCO LA SIGLA VARIA CORRISPONDENTE
320200160509     C                   MOVEL     TPTFTC        WFTC              1
320300160509     C                   Z-ADD     1             A
320400160509     C     WFTC          LOOKUP    CP(A)                                  10
320500160509     C     *IN10         IFEQ      *ON
320600160509     C     A             OCCUR     DS1P
320700160509     C     §1PVAR        LOOKUP    VES                                    10
320800160509     C     *IN10         IFEQ      *ON
320900160509     C                   MOVE      VARIA         W001A             1
321000160509     C     W001A         IFNE      '0'
321100160509     C     VARIA         ADD       0,001         COMOD2           15 2
321200160509     C                   Z-ADD     COMOD2        VARIA
321300160509     C                   ENDIF
321400160509     C                   ENDIF
321500160509     C                   ENDIF
321600160509     C**
321700160509     C     ENDACD        TAG
321800160509     C*
321900160509     C                   ENDSR
322000160509     C/SPACE
322100160509     C******************************************************
322200160509     C** CALCOLO TARIFFE  PARTICOLARI
322300160509     C******************************************************
322400160509     C     CALCP         BEGSR
322500160509     C** CERO LA TARIFFA PARTICOLARE IN TITPT O CARTELLO
322600160509     C                   MOVEL     TASTC         FISO
322700160509     C                   MOVEL     'S'           WCAR
322800160509      ** se sto calcolando la varia "Q" = Centro storico o ZTL
322900160509      ** non devo cercare la tariffa nella Cartello in mancanza
323000160509      ** di quella del cliente
323100160509      ** E' stata aggiuna una tabella che pilota il recupero della
323200160509      ** tariffa di cartello se la filiale del cliente di tassazione
323300160509      ** è presente nella tabella ztl e la data spedizione è maggiore
323400160509      ** uguale alla data attivazione della tassazione
323500160509     c                   clear                   indztl            3 0
323600160509     c                   If        tastc = 'Q'
323700160509     c                   movel     'N'           WCAR
323800160509      * cerco la filiale del cliente nella tabella ZTL
323900160509     c                   eval      indztl = %lookup(kscfil:sfilztl)
324000160509      * se indice =  a zero cerco con la filiale 999
324100160509    2c                   if        indztl = *zeros
324200160509     c                   eval      indztl = %lookup(999:sfilztl)
324300160509    2c                   endif
324400160509      * imposto il flag della ricerca tariffa di cartello = 'S'  se filiale cliente
324500160509      * abilitata e la data spedizione > = data attivazione tassazione
324600160509    2c                   If        indztl > *zeros and
324700160509    3c                             tasdsp >= sdtaztl(indztl)
324800160509     c                   movel     'S'           WCAR
324900160509    3c                   endif
325000160509     c                   endif
325100160509      *
325200160509     C                   EXSR      CERTPT
325300160509     C** SOLO SE L'HO TROVATA
325400160509    1C     WEND          IFEQ      ' '
325500160509     C                   Z-ADD     ISOPES        TPDPES
325600160509     C                   EXSR      CALACD
325700160509     C                   Z-ADD     VARIA         TASCP
325800160509    1C                   ENDIF
325900160509     C*
326000160509     C                   ENDSR
326100160509     C/SPACE
326200160509     C******************************************************
326300160509     C** CALCOLO ADDIZIONALE DI GESTIONE + ISTAT
326400160509     C******************************************************
326500160509     C     ADGIST        BEGSR
326600160509     C**
326700160509     C*
326800160509     C****************
326900160509     C** ADDIZIONALE DI GESTIONE  anni precedenti       varia "Z"
327000160509     C****************
327100160509     C*
327200160509     C     *LIKE         DEFINE    TASVA1        TASADG
327300160509     C     TAMPAG        IFNE      0
327400160509     C                   Z-ADD     1             A
327500160509     C     §$2VRZ        LOOKUP    SV(A)                                  05
327600160509     C  N05              DO
327700160509     C                   Z-ADD     0             TASADG
327800160509     C* TOTALE IMPONIBILE
327900160509     C                   Z-ADD     O17IMV        IMPADG           15 3
328000160509     C**
328100160509     C                   Z-ADD     0             COMADG           15 3
328200160509     C     IMPADG        MULT      TAMPAG        COMADG
328300160509     C     COMADG        IFGT      0
328400160509     C     COMADG        DIV(H)    100           TASADG
328500160509     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
328600160509     C     *IN12         IFEQ      '0'
328700160509     C     TASADG        ADD       0,5           CAMPOW           11 0
328800160509     C                   Z-ADD     CAMPOW        TASADG
328900160509     C                   END
329000160509     C                   END
329100160509     C*
329200160509     c     tasadg        ifgt      0
329300160509      *
329400160509     C                   Z-ADD     1             A
329500160509     C     ' '           LOOKUP    SV(A)                                  96
329600160509     C   96              MOVEL     §$2VRZ        SV(A)
329700160509     C   96              Z-ADD     TASADG        VA(A)
329800160509     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
329900160509     C  N96              ADD       TASADG        TASPOR
330000160509     C*
330100160509     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
330200160509     C*
330300160509     C                   CLEAR                   DSLV16
330400160509     C                   MOVEL     'T'           D17FIM
330500160509     C                   Z-ADD     TASPOR        D17POR
330600160509     C                   CALL      'FNLV16R'
330700160509     C                   PARM                    DSLV16
330800160509     C                   PARM                    DTASV
330900160509      *
331000160509     c                   endif
331100160509     C*
331200160509     C                   END
331300160509     C                   END
331400160509     C*
331500160509     C****************
331600160509     C** ADDIZIONALE DI GESTIONE    anno corrente       varia "b"
331700160509     C****************
331800160509     C*
331900160509     c                   clear                   tasadg
332000160509     c                   clear                   impadg
332100160509     c                   clear                   comadg
332200160509
332300160509     C     TAMPPA        IFNE      0
332400160509     C                   Z-ADD     1             A
332500160509     C     §$2VAG        LOOKUP    SV(A)                                  05
332600160509     C  N05              DO
332700160509     C                   Z-ADD     0             TASADG
332800160509     C* TOTALE IMPONIBILE
332900160509     C                   Z-ADD     O17IMV        IMPADG
333000160509     C**
333100160509     C                   Z-ADD     0             COMADG
333200160509     C     IMPADG        MULT      TAMPPA        COMADG
333300160509     C     COMADG        IFGT      0
333400160509     C     COMADG        DIV(H)    100           TASADG
333500160509     C* SE NON PREVEDE I DECIMALI, PORTO ALL'UNITA
333600160509     C     *IN12         IFEQ      '0'
333700160509     C     TASADG        ADD       0,5           CAMPOW           11 0
333800160509     C                   Z-ADD     CAMPOW        TASADG
333900160509     C                   END
334000160509     C                   END
334100160509     C*
334200160509     c     tasadg        ifgt      0
334300160509      *
334400160509     C                   Z-ADD     1             A
334500160509     C     ' '           LOOKUP    SV(A)                                  96
334600160509     C   96              MOVEL     §$2VAG        SV(A)
334700160509     C   96              Z-ADD     TASADG        VA(A)
334800160509     C** SE LA SCHIERA E' COMPLETA SOMMO AL PORTO
334900160509     C  N96              ADD       TASADG        TASPOR
335000160509     C*
335100160509     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
335200160509     C*
335300160509     C                   CLEAR                   DSLV16
335400160509     C                   MOVEL     'T'           D17FIM
335500160509     C                   Z-ADD     TASPOR        D17POR
335600160509     C                   CALL      'FNLV16R'
335700160509     C                   PARM                    DSLV16
335800160509     C                   PARM                    DTASV
335900160509      *
336000160509     c                   endif
336100160509     C*
336200160509     C                   END
336300160509     C                   END
336400160509     C**
336500160509     C***************
336600160509     C** ISTAT
336700160509     C***************
336800160509     C*
336900160509     C     *LIKE         DEFINE    TASVA1        TASIAC
337000160509     C                   Z-ADD     1             A
337100160509     C     §$2VRV        LOOKUP    SV(A)                                  05
337200160509    1C  N05TAMrct        IFGT      0
337300160509     C                   Z-ADD     0             TASIAC
337400160509     C*
337500160509     C* CALCOLO ISTAT SU TOTALE IMPONIBILE
337600160509     C*
337700160509     C                   Z-ADD     O17IMV        IMPIAC           15 3
337800160509     C                   Z-ADD     0             PUNIAC            7 4
337900160509
338000160509      * richiamo nuovo pgm per calcolo % istat da aggiungere all'imponibile
338100160509     c
338200160509     c                   clear                   trulistds
338300160509     c                   eval      IISTsca = tamrct
338400160509     c                   eval      IISTdsp = tasdsp
338500160509     c                   call      'TRULISTR'
338600160509     c                   parm                    trulistds
338700160509     c                   IF        OISTerr = *blanks
338800160509     c                   eval      puniac = OISTiac
338900160509     c                   ENDIF
339000160509     c
339100160509     C                   Z-ADD     0             PERIAC           15 6
339200160509     C                   Z-ADD     0             PERIAX           15 4
339300160509     C     PUNIAC        MULT      TAMPRI        PERIAX
339400160509    3C     PERIAX        IFNE      0
339500160509     C     PERIAX        DIV       100           PERIAC
339600160509    3C                   END
339700160509     C** PERCENTUALE DI ISTAT DA APPLICARE
339800160509     C** APPLICO I PUNTI X TRIMESTRE SOLO SE
339900160509     C** LA DATA DI SPEDIZIONE E'
340000160509     C** MAGGIORE O UGUALE ALLA DATA DI SCATTO
340100160509     C** DEI PUNTI NEL TRIMESTRE
340200160509    3C     PERIAC        IFNE      0
340300160509     C                   DIV       100           PERIAC
340400160509     C     IMPIAC        MULT(H)   PERIAC        TASIAC
340500160509    3C                   END
340600160509     C*
340700160509    3C     TASIAC        IFGT      0
340800160509     C*  SENZA DECIMALI
340900160509    4C     *IN12         IFEQ      *OFF
341000160509     C     TASIAC        ADD       0,5           CAMPOW           11 0
341100160509     C                   Z-ADD     CAMPOW        TASIAC
341200160509    4C                   END
341300160509     C*
341400160509     C                   Z-ADD     1             A
341500160509     C     ' '           LOOKUP    SV(A)                                  96
341600160509     C   96              MOVEL     §$2VRV        SV(A)
341700160509     C   96              Z-ADD     TASIAC        VA(A)
341800160509     C  N96              ADD       TASIAC        TASPOR
341900160509     C*
342000160509     C* RICALCOLO L'IMPONIBILE SOMMANDO L'ADDIZIOANLE DI GESTIONE
342100160509     C*
342200160509     C                   CLEAR                   DSLV16
342300160509     C                   MOVEL     'T'           D17FIM
342400160509     C                   Z-ADD     TASPOR        D17POR
342500160509     C                   CALL      'FNLV16R'
342600160509     C                   PARM                    DSLV16
342700160509     C                   PARM                    DTASV
342800160509     C*
342900160509    3C                   END
343000160509    1C                   ENDIF
343100160509     C*
343200160509     C                   ENDSR
343300160509     C*******************************************************
343400160509     C** CALCOLO DIRITTO DI FATTURAZIONE  + TOTALE IMPONIBILE
343500160509     C******************************************************
343600160509     C     DIRFAT        BEGSR
343700160509     C**
343800160509      * prima verifico se esiste già
343900160509     C                   Z-ADD     1             A
344000160509     C     §$2VRD        LOOKUP    SV(A)                                  05
344100160509     C* SIGLA VARIA NON PRESENTE --> CALCOLO
344200160509     C     *IN05         IFEQ      *OFF
344300160509     C                   Z-ADD     1             A
344400160509     C     ' '           LOOKUP    SV(A)                                  96
344500160509     C   96              MOVEL     §$2VRD        SV(A)
344600160509     C   96              Z-ADD     TASIVDF       VA(A)
344700160509     C  N96              ADD       TASIVDF       TASPOR
344800160509     C*
344900160509     C* RICALCOLO L'IMPONIBILE SOMMANDO IL DIRITTO DI FATTURAZIONE
345000160509     C*
345100160509     C                   CLEAR                   DSLV16
345200160509     C                   MOVEL     'T'           D17FIM
345300160509     C                   Z-ADD     TASPOR        D17POR
345400160509     C                   CALL      'FNLV16R'
345500160509     C                   PARM                    DSLV16
345600160509     C                   PARM                    DTASV
345700160509     C*
345800160509     C                   Z-ADD     O17IMV        TASIMV
345900160509     C*
346000160509     C*
346100160509     C                   ENDIF
346200160509     C*
346300160509     C                   ENDSR
346400160509     C******************************************************
346500160509     C* CERCO TARIFFA PARTICOLARE
346600160509     C******************************************************
346700160509     C     CERTPT        BEGSR
346800160509     C                   CLEAR                   TASCP
346900160509     C                   CLEAR                   WEND
347000160509     C                   MOVEL     *BLANKS       FLGISO            1            FLAG.APPLICAZI.
347100160509     C                   MOVEL     FISO          WFISO
347200160509     C                   Z-ADD     1             A
347300160509     C     WFISO         LOOKUP    CP(A)                                  05
347400160509     C**
347500160509    1C     *IN05         IFEQ      *ON
347600160509     C     A             OCCUR     DS1P
347700160509     C*-----------------------------------------
347800160509     C     KISOT         CHAIN     TITPT01L                           28
347900160509     C** ESISTE TARIFFA DEL CLIENTE
348000160509     C     *IN28         IFEQ      *OFF
348100160509     C     TPTATB        ANDEQ     *BLANKS
348200160509     C                   GOTO      DOPIS2
348300160509     C                   ENDIF
348400160509     C*-----------------------------------------
348500160509     C** MEMORIZZO DATI DA TARIFFA DI CARTELLO
348600160509     C**  SOLO SE HO DETTO CHE LA POSSO USARE E SE C'E'
348700160509     C*-----------------------------------------
348800160509      * VERIFICO SE ESISTE LA TARIFFA DI CARTELLO
348900160509     C     WCAR          IFEQ      'S'
349000160509     C     KTPTC         CHAIN     TITPT01L                           28
349100160509      * SE NON ESISTE IL RECORD OPPURE E' ANNULLATO VADO A FINE ROUTINE
349200160509     C     *IN28         IFEQ      *ON
349300160509     C     TPTATB        OREQ      'A'
349400160509     C                   MOVEL     'S'           WEND
349500160509     C                   GOTO      DOPIS3
349600160509     C                   ENDIF
349700160509      * E SE NON DEVO CARICARE LA CARTELLO ERRORE NON TROVATA TARIFFA PARTICOLARE
349800160509     C                   ELSE
349900160509     C                   MOVEL     'S'           WEND
350000160509     C                   GOTO      DOPIS3
350100160509     C                   ENDIF
350200160509     C     DOPIS2        TAG
350300160509     C* ARROTONDAMENTI E RPV
350400160509     C                   MOVEL     TPTTPG        WTPG
350500160509      * in caso di pod-immage varia "a" imposto come tipo pagamento a colli
350600160509
350700160509     c                   if        fiso = §$2POD
350800160509     C                   MOVEL     '1'           WTPG
350900160509     c                   endif
351000160509
351100160509     C                   MOVEL     TPTRPV        WRPV
351200160509     C                   CLEAR                   WDET
351300160509     C                   Z-ADD     TPTARL        WARL
351400160509     C                   Z-ADD     TPTARF        WARF
351500160509     C                   Z-ADD     TPTARO        WARO
351600160509     C                   EXSR      CALCOL
351700160509     C** NON TROVATA TARIFFA PARTICOLARE
351800160509   X1C                   ELSE
351900160509     C                   MOVEL     'S'           WEND              1
352000160509    1C                   ENDIF
352100160509     C     DOPIS3        ENDSR
352200160509     C******************************************************
352300160509     C** CALCOLO IL VALORE DA MOLTIPLICARE/RPV/ARROTONDAMENTI
352400160509     C******************************************************
352500160509     C     CALCOL        BEGSR
352600160509     C     *LIKE         DEFINE    TAMARL        ARR
352700160509     C     *LIKE         DEFINE    TASIAS        PESTAS
352800160509     C     *LIKE         DEFINE    TASIAS        ISOPES
352900160509     C     *LIKE         DEFINE    TASIAS        ISOPE2
353000160509     C     *LIKE         DEFINE    TPTRPV        WRPV
353100160509     C     *LIKE         DEFINE    TPTARL        WARL
353200160509     C     *LIKE         DEFINE    TPTARF        WARF
353300160509     C     *LIKE         DEFINE    TPTARO        WARO
353400160509     C                   Z-ADD     0             ISOPES
353500160509     C                   SELECT
353600160509
353700160509     c                   When      wFiso = '='
353800160509     c                   Z-Add     TasBan        Isopes
353900160509
354000160509     C     WTPG          WHENEQ    '0'
354100160509     C     WTPG          OREQ      '3'                                          KG
354200160509     C                   Z-ADD     USAPKF        ISOPES
354300160509     C*
354400160509     C     WTPG          WHENEQ    '1'                                          COLLI
354500160509     C                   Z-ADD     TASNCL        ISOPES
354600160509     C*
354700160509     C     WTPG          WHENEQ    '2'                                          SPEDIZ
354800160509     C                   Z-ADD     1             ISOPES
354900160509     C*
355000160509     C     WTPG          WHENEQ    '4'                                          VALORE
355100160509     C     WTPG          OREQ      '5'                                          Q.TA'
355200160509     C                   Z-ADD     TASQFT        ISOPES
355300160509     C*
355400160509     C     WTPG          WHENEQ    'T'                                          TRASPORTO
355500160509     C                   Z-ADD     TASPOR        ISOPES
355600160509     C                   ADD       TASINL        ISOPES
355700160509     C                   EXSR      CERISO
355800160509     C                   ADD       VAISO         ISOPES
355900160509     C                   ADD       VAtsp         ISOPES
356000160509     C                   ENDSL
356100160509     C** MINIMO TASSABILE STD
356200160509     C                   Z-ADD     0             MINTAS
356300160509     C**
356400160509     C                   Z-ADD     1             A
356500160509     C     WTPG          LOOKUP    CTR(A)                                 05
356600160509     C   05A             OCCUR     DSTR
356700160509     C   05              Z-ADD     §TRATA        MINTAS
356800160509     C**
356900160509     C** SOLO PER TAD (PER TPD NON MI SERVE)
357000160509    1C     WDET          IFEQ      'TAD'
357001170906     C     USAVLF        ANDGT     0
357100160509     C                   CLEAR                   MINTAR
357200160509     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
357300160509     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
357400160509     C   05              Z-ADD     §TRATA        MINTAR
357500160509    2C     TAMATA        IFGT      0
357600160509     C                   Z-ADD     TAMATA        MINTAR
357700160509    2C                   ENDIF
357800160509    1C                   ENDIF
357900160509     C**
358000160509     C** RPV SOLO PER TARIFFA 0 E 3
358100160509    1C     §TRRPV        IFEQ      'S'
358200160509     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
358300160509    2C     WDET          IFEQ      'TAD'
358400160509     C     TASVLF        ANDGT     0
358500160509    3C                   DO        200           A
358600160509     C     A             OCCUR     TARDS
358700160509    4C     TSCG          IFGE      ISOPES
358800160509     C                   GOTO      POITAS
358900160509    4C                   END
359000160509    3C                   END
359100160509     C** RICERCO RPV CON PESO REALE
359200160509    3C     TSCG          IFLT      ISOPES
359300160509     C                   SETON                                        94
359400160509     C                   MOVEL     MSG(5)        TASMSG
359500160509     c                   eval      %subst(tasmsg: 37)= %trim(%editc(ISOPES:'3'))
359600160509     C                   GOTO      FINE
359700160509    3C                   END
359800160509     C     POITAS        TAG
359900160509     C                   Z-ADD     TRPV          WRPV
360000160509     C                   Z-ADD     TRPV          rappv
360100160509    2C                   ENDIF
360200160509     C**
360300170906    2C     USAVLF        IFNE      0
360400160509     C     WRPV          ANDNE     0
360500160509     C                   Z-ADD     0             COMPRV            9 3
360600170906     C     USAVLF        MULT      WRPV          COMPRV
360700160509     C     COMPRV        MULT      100           ISOPE2
360800160509    3C     ISOPE2        IFGT      ISOPES
360900160509     C                   Z-ADD     ISOPE2        ISOPES
361000160509    3C                   ENDIF
361100160509    2C                   ENDIF
361200160509    1C                   ENDIF
361300160509     C**
361400160509     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
361500160509    0C     §TRARR        IFEQ      'S'
361600160509     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
361700160509     C     ISOPES        IFGE      MINTAS
361800160509     C                   Z-ADD     0             ARR
361900160509     C     ISOPES        IFGT      WARL
362000160509     C                   Z-ADD     WARO          ARR
362100160509     C                   ELSE
362200160509     C                   Z-ADD     WARF          ARR
362300160509     C                   END
362400160509     C**
362500160509     C                   Z-ADD     0             RESTO            15 5
362600160509     C                   Z-ADD     0             ISOPEX           13 0
362700160509     C     ARR           IFNE      0
362800160509     C     ISOPES        ANDNE     0
362900160509     C     ISOPES        DIV       ARR           ISOPEX
363000160509     C                   MVR                     RESTO
363100160509     C     RESTO         IFGT      0
363200160509     C     ISOPEX        MULT      ARR           ISOPES
363300160509     C                   ADD       ARR           ISOPES
363400160509     C                   ENDIF
363500160509     C                   ENDIF
363600160509     C                   ENDIF
363700160509     C**
363800160509     C***        GIUARR    TAG
363900160509    0C                   ENDIF
364000160509     C                   ENDSR
364100160509     C******************************************************
364200160509     C** CALCOLA DIRITTO ASSICURATIVO - VARIA E
364300160509     C******************************************************
364400160509     C     CALVE         BEGSR
364500160509     C                   MOVEL     'R'           FISO
364600160509     C                   MOVEL     'N'           WCAR
364700160509     C                   EXSR      CERTPT
364800160509     C     WEND          IFEQ      ' '
364900160509     C** TARIFFA A VALORE TOLGO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DI CONTO
365000160509     C*  E CALCOLO VALORE AL KG (FISSO) PER LA PARTE RESTANTE
365100160509    1C     TPTTPG        IFEQ      'V'
365200160509     C                   CLEAR                   ISOPES
365300160509     C* CONVERTO L'IMPORTO DELLA FRANCHIGIA NELLA MONETA DELLA TARIFFA
365400160509     C*
365500160509     C     *LIKE         DEFINE    §GELRP        WLRP
365600160509     C*
365700160509     C     §TADIV        IFNE      §GEDCN
365800160509     C                   CLEAR                   YEUR
365900160509     C                   MOVEL     §GEDCN        YECDVI
366000160509     C                   MOVEL     §TADIV        YECDVO
366100160509     C                   Z-ADD     §GELRP        YECIMI
366200160509     C   12              MOVE      '3'           YECDCO
366300160509     C  N12              MOVE      '0'           YECDCO
366400160509     C*
366500160509     C                   CALL      'YEURCO'
366600160509     C                   PARM                    YEUR
366700160509     C*
366800160509     C     YECESI        IFEQ      ' '
366900160509     C                   Z-ADD     YECIMO        WLRP
367000160509     C                   END
367100160509     C* SE TARIFFE UGUALI VALORIZZO WLRP
367200160509     C                   ELSE
367300160509     C                   Z-ADD     §GELRP        WLRP
367400160509     C                   ENDIF
367500160509     C*
367600160509    2C     TPTVLM        IFGT      WLRP
367700160509     C     TPTVLM        SUB       WLRP          ISOPES
367800160509     C     ISOPES        MULT      USAPKF        ISOPES
367900160509    2C                   ENDIF
368000160509    1C                   ENDIF
368100160509     C**
368200160509     C                   Z-ADD     ISOPES        TPDPES
368300160509     C                   EXSR      CALACD
368400160509     C                   Z-ADD     VARIA         DIRITT
368500160509     C**
368600160509     C     TPTAPL        IFNE      *BLANKS
368700160509     C     TIPBOL        IFEQ      'F'
368800160509     C     TPTAPL        IFEQ      'A'
368900160509     C                   Z-ADD     0             DIRITT
369000160509     C                   END
369100160509     C                   ELSE
369200160509     C     TPTAPL        IFEQ      'P'
369300160509     C                   Z-ADD     0             DIRITT
369400160509     C                   END
369500160509     C                   END
369600160509     C                   END
369700160509     C** TIPO APPLICAZIONE
369800160509     C** P - SOLO PER FRANCHI
369900160509     C** A - SOLO PER ASSEGNATI
370000160509     C                   ENDIF
370100160509     C                   ENDSR
370200160509     C/SPACE
370300160509     C******************************************************
370400160509     C** CALCOLA MAGGIORAZIONE TIPO SERVIZIO
370500160509     C******************************************************
370600160509     C     CALTS         BEGSR
370700160509     C                   MOVEL     'S'           WCAR              1
370800160509     c
370900160509     C* 24/09/09 --> Ora applichiamo sempre la maggiorazine espresso
371000160509     c
371100160509     C* SE TIPO SERVIZIO DELLA TARIFFA = A TIPO SERVIZIO BOLLA
371200160509     C*  NON PRENDO LA TARIFFA DI CARTELLO
371300160509     C***  TAMTSP        IFEQ      TASTSP
371400160509     C***                MOVEL     'N'           WCAR              1
371500160509     C***                ENDIF
371600160509     C* SIGLA VARIA CORRISPONDENTE
371700160509     C                   MOVEL     §5EFTC        FISO
371800160509     C                   EXSR      CERTPT
371900160509     C     WEND          IFEQ      ' '
372000160509     C**
372100160509     C** A TRASPORTO
372200160509     C     TPTTPG        IFEQ      'T'
372300160509     C                   Z-ADD     IMPON         ISOPES
372400160509     C                   END
372500160509     C**
372600160509     C                   Z-ADD     ISOPES        TPDPES
372700160509     C                   EXSR      CALACD
372800160509     C                   Z-ADD     VARIA         WTSP
372900160509     C                   ENDIF
373000160509     C**
373100160509     C                   ENDSR
373200160509     C******************************************************
373300160509     C** CERCA TARIFFA TNTAM PIU' RELATIVA CARTELLO
373400160509     C******************************************************
373500160509     C     CERTAM        BEGSR
373600160509      *
373700160509     C     *LIKE         DEFINE    TAMPRG        PRG2
373800160509     C     *LIKE         DEFINE    TAMKSC        KSC2
373900160509     C     *LIKE         DEFINE    TAMCTR        CTR2
374000160509      * se in tasflo c'è una data riferimento per il recupero della tariffa la imposto
374100160509      * solo temporaneamente al posto della data spedizione
374200160509     c                   clear                   Sav_dsp
374300160509     c                   if        §asdrt  > *zeros
374400160509     c                   eval      Sav_dsp = Tasdsp
374500160509     c***                eval      tasdsp = §asdrt
374600160509     c                   movel     §asdrt        tasdsp
374700160509     c                   endif
374800160509      *
374900160509     C                   SETOFF                                       180717
375000160509     C                   SETOFF                                       0890
375100160509     C                   Z-ADD     0             PRG2
375200160509     C                   Z-ADD     0             KSC2
375300160509     C                   Z-ADD     0             CTR2
375400160509     C                   Z-ADD     0             TAMKSC
375500160509     C                   Z-ADD     0             TAMPRG
375600160509     C                   Z-ADD     0             TAMCTR
375700160509     C     KTAM          SETLL     TNTAM000                               08
375800160509     C* MANCA TARIFFA CLIENTE
375900160509    1C     *IN08         IFEQ      *OFF
376000160509     C                   MOVEL     MSG(3)        TASMSG
376100160509     C                   SETON                                        94
376200160509     C                   GOTO      ENDTAM
376300160509    1C                   ENDIF
376400160509     C**
376500160509     C     SUTAM         TAG
376600160509     C**
376700160509     C     KTAM          READE     TNTAM000                               07
376800160509     c**
376900160509     C** SE HO LETTO UNA TARIFFA SCADUTA (17) E IL
377000160509     C** RECORD SUCCESSIVO MI ACCENDE FINE FILE
377100160509     C** chaino vecchio progressivo per tassare con tariffa scaduTa
377200160509    1C     *IN07         IFEQ      *ON
377300160509     C   17KTAM2         CHAIN     TNTAM000                           18
377400160509     c**
377500160509     C     *IN17         IFEQ      *OFF
377600160509    2C     *IN18         OREQ      *ON
377700160509     C                   MOVEL     MSG(3)        TASMSG
377800160509     C                   SETON                                        94
377900160509    2C                   ENDIF
378000160509     C**
378100160509     C                   GOTO      ENDTAM
378200160509     C**
378300160509     C                   ELSE
378400160509     C**
378500160509     C** SE NON E' TARIFFA ADATTA PER LA BOLLA (ITALIA INVECE CHE ESTER
378600160509     C**  O VICEVERSA) --> RILEGGO COME SE NON AVESSI LETTO
378700160509     C***  TAMFIE        IFNE      WBOEST
378800160509     C***  TAMFIE        ANDNE     ' '
378900160509     C***                GOTO      SUTAM
379000160509     C***                ENDIF
379100160509    1C                   ENDIF
379200160509     C**
379300160509     C** TARIFFA DA DECORRERE : SEGNALO SE NON C'E' UNA TARIFFA SCADUTA
379400160509     C**    (di fatto non e' possibile perche' se c'e' una da decorrere
379500160509     C**    e ce n'e' una prima e' la tariffa valida dal momento che
379600160509     C**    non e' possibile che esista una buco di decorrenze scadenze
379700160509     C**    tra i vari progressivi)
379800160509    1C     TASDSP        IFLT      TAMDDT
379900160509     C   17KTAM2         CHAIN     TNTAM000                           18
380000160509     C* NON C'E' TARIFFA SCADUTA O NON TROVATA DI NUOVO
380100160509    2C     *IN17         IFEQ      *OFF
380200160509     C     *IN18         OREQ      *ON
380300160509     C                   MOVEL     MSG(10)       TASMSG
380400160509     C                   SETON                                        94
380500160509    2C                   ENDIF
380600160509     C**
380700160509     C                   GOTO      ENDTAM
380800160509    1C                   END
380900160509     C**
381000160509     C** TARIFFA SCADUTA: DATA SPEDIZIONE MAGGIORE DELLA DATA  SCADENZA
381100160509    1C     TASDSP        IFGT      TAMDST
381200160509     C                   SETON                                        17
381300160509     C                   Z-ADD     TAMKSC        KSC2
381400160509     C                   Z-ADD     TAMCTR        CTR2
381500160509     C                   Z-ADD     TAMPRG        PRG2
381600160509     C                   GOTO      SUTAM
381700160509    1C                   ENDIF
381800160509     C*
381900160509     C     ENDTAM        TAG
382000160509     C*
382100160509     C*
382200160509    0C     *IN94         IFEQ      *OFF
382300160509     C                   MOVEL     TAMFLO        DSTA01
382400160509     c                   clear                   tasfie
382500160509      * stabilisco il network della tariffa per la ricerca della cartello
382600160509     c                   select
382700160509      * ITALIA
382800160509     c                   when      tamfie = 'I' and §tadpd = ' '
382900160509     c                   eval      NTWTAM = 'COR'
383000160509     C                   MOVEL     'I'           WBOEST
383100160509      * DPD
383200160509     c                   when      tamfie = 'I' and §tadpd = 'S'
383300160509     c                   eval      NTWTAM = 'DPD'
383400160509     C                   movel     'S'           WBODPD
383500160509     C                   MOVEL     'I'           WBOEST
383600160509      * FEDEX
383700160509     c                   when      tamfie = 'E' and §tafed = 'S'
383800160509     c                   eval      NTWTAM = 'FED'
383900160509     c                   eval      tasfie = 'F'
384000160509     C                   movel     'S'           WBOFED
384100160509     C                   MOVEL     'E'           WBOEST
384200160509      * da aggiungere la differenzazione tariffa merce e documenti
384300160509      *
384400160509      * EEX
384500160509     c                   when      tamfie = 'E' and §tafed = ' '
384600160509     c                   eval      NTWTAM = 'EEX'
384700160509     C                   MOVEL     'E'           WBOEST
384800160509
384900160509     c                   Endsl
385000160509      * verifico se tariffa adatta alla bolla solo per quanto riguarda italia e estero e non
385100160509      * network
385200160509     C                   CLEAR                   TRUL27
385300160509     C                   MOVEL     TASTSP        I27TSP
385400160509     C                   MOVEL     TASLNA        I27LNA
385500160509     C                   MOVEL     TASLNP        I27LNP
385600160509     C                   MOVEL     TASKSC        I27CLI
385700160509     c                   Movel     Tasctr        I27Ctb
385800160509     C                   CALL      'TRUL27R1'
385900160509     C                   PARM                    TRUL27
386000160509      * SE ERRORE (NON PUO' ESSERE DO MANCA TARIFFA)
386100160509     C     O27ERR        IFEQ      *BLANKS
386200160509      * se  o27fie diverso da I e tamfie = 'E' o §tadpd = 'S' tariffa ok
386300160509      * se  o27fie uguale   a I e tamfie = 'I' e §tadpd = ' ' tariffa ok
386400160509      * Bolla italia e tariffa Estera
386500160509     c                   if        o27fie = 'I' and tamfie = 'E'
386600160509     C                   MOVEL     MSG(3)        TASMSG
386700160509     C                   SETON                                        94
386800160509     C                   ENDIF
386900160509      *Bolla italia e tariffa DPD
387000160509     c                   if        o27fie='I' and tamfie='I' and §tadpd='S'
387100160509     C                   MOVEL     MSG(3)        TASMSG
387200160509     C                   SETON                                        94
387300160509     C                   ENDIF
387400160509      *Bolla Estera e tariffa Italia
387500160509     c                   if        o27fie <> 'I' and tamfie='I' and §tadpd=' '
387600160509     C                   MOVEL     MSG(3)        TASMSG
387700160509     C                   SETON                                        94
387800160509     C                   ENDIF
387900160509     c
388000160509     C                   ELSE
388100160509     C                   SETON                                        94
388200160509     C                   MOVEL     O27MSG        TASMSG
388300160509     C                   GOTO      FINE
388400160509     C                   ENDIF
388500160509      *
388600160509     C** CERCO LA CARTELLO RELATIVA ALLA TARIFFA DEL CLIENTE
388700160509     C*  SE NON HO GIA' UNA TARIFFA DI CARTELLO E SE 94 SPENTO
388800160509     C                   SETOFF                                       90
388900160509     C                   Z-ADD     1             T
389000160509     C     TAMKSC        LOOKUP    TAC(T)                                 90
389100160509      *
389200160509      * PER 90 SPENTO VUOL DIRE CHE NON HO TROVATO NESSUNA CARTELLO
389300160509    1C     *IN90         IFEQ      *OFF
389400160509      * imposto il tipo tariffa fedex
389500160509     c                   clear                   tiptam
389600160509     c                   if        NtwTam = 'FED' and
389700160509     c                             o27fie = 'N'
389800160509     c                   eval      TIPTam = 'FD'
389900160509     c                   endif
390000160509
390100160509     c                   if        NtwTam = 'FED' and
390200160509     c                             (o27fie = 'M' or o27fie = 'F')
390300160509     c                   eval      TIPTam = 'FM'
390400160509     c                   endif
390500160509
390600160509     C*
390700160509     C* AGGANCIO LA TABELLA DELLE TARIFFE DI CARTELLO
390800160509     C                   EXSR      CERTCA
390900160509    1C                   ENDIF
391000160509    0C                   ENDIF
391100160509      * se impostato il campo del salvataggio della data spedizione
391200160509     c                   if        Sav_dsp <> *zeros
391300160509     c                   eval      tasdsp = Sav_dsp
391400160509     c                   clear                   Sav_dsp
391500160509     c                   endif
391600160509     C*
391700160509     C*
391800160509     C     ENDTA2        ENDSR
391900160509     C******************************************************
392000160509     C* RICERCA DETTAGLIO SU TUTAD
392100160509     C******************************************************
392200160509     C     RICTAD        BEGSR
392300160509     C     KTAD          SETLL     TITAD000                               09
392400160509     C** 49  TROVATA TARIFFA DEL CAP   DI ARRIVO
392500160509     C   09              SETON                                        49
392600160509     C**
392700160509    1C     *IN09         IFEQ      *OFF
392800160509     C                   MOVEL     *BLANKS       COMNAZ
392900160509     C                   MOVEL     *BLANKS       COMCAP
393000160509     C     KTAD          SETLL     TITAD000                               09
393100160509    2C     *IN09         IFEQ      *OFF
393200160509     C*
393300160509     C                   Z-ADD     1             A
393400160509     C     COMCTS        LOOKUP    CTS(A)                                 05
393500160509     C**
393600160509     C* SE TROVATO COD.TASSAZIONE
393700160509    3C     *IN05         IFEQ      *ON
393800160509     C     A             OCCUR     DSCT
393900160509     C                   MOVEL     §CTRAP        COMCTS
394000160509     C** CARICO REGIONE AL POSTO DEL CODICE DI TASSAZIONE
394100160509     C     KTAD          SETLL     TITAD000                               09
394200160509    3C                   ENDIF
394300160509     C**
394400160509    3C     *IN09         IFEQ      *OFF
394500160509     C*
394600160509     C     WBOEST        IFEQ      'E'
394700160509     C                   MOVEL     'EE'          COMCTS
394800160509     C                   ELSE
394900160509     C                   MOVEL     'IT'          COMCTS
395000160509     C                   END
395100160509     C     KTAD          SETLL     TITAD000                               09
395200160509    3C                   ENDIF
395300160509    2C                   ENDIF
395400160509    1C                   ENDIF
395500160509     C                   ENDSR
395600160509     C/SPACE
395700160509     C******************************************************
395800160509     C** CERCA TARIFFA TITAD DETTAGLIO E CARICA DS
395900160509     C******************************************************
396000160509     C     CARTAR        BEGSR
396100160509C    C                   SETOFF                                       495694
396200160509     C                   MOVEL     TASCAD        COMCAP            9
396300160509     C                   MOVEL     TASNZD        COMNAZ            3
396400160509     C                   MOVEL     TASCTS        COMCTS
396500160509     C                   MOVEL     TASLNP        WLNP
396600160509     C                   EXSR      RICTAD
396700160509     C   09              GOTO      LATAR
396800160509     C**
396900160509     C** SE NON TROVATO TITAD CON LA LINEA DI PARTENZA, CERCO CON LA
397000160509     C**  REGIONE TROVATA DA ORGANIGRANNA DI TASLNP
397100160509     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
397200160509     C     TASLNP        IFNE      SAVLNP
397300160509     C     TASLNP        CHAIN     AZORG01L                           31
397400160509     C   31              CLEAR                   ORGDE7
397500160509     C                   MOVEL     ORGDE7        OG147
397600160509     C                   MOVEL     TASLNP        SAVLNP
397700160509     C                   ENDIF
397800160509     C**
397900160509     C                   MOVEL     TASCAD        COMCAP            9
398000160509     C                   MOVEL     TASNZD        COMNAZ            3
398100160509     C                   MOVEL     TASCTS        COMCTS
398200160509     C                   MOVEL     §OGRET        WLNP
398300160509     C                   EXSR      RICTAD
398400160509     C   09              GOTO      LATAR
398500160509     C**
398600160509     C** SE NON TROVATO TITAD CON LA REGIONE DI TASLNP, CERCO CON LA
398700160509     C**  LINEA ITALIA 950
398800160509     C                   MOVEL     TASCAD        COMCAP            9
398900160509     C                   MOVEL     TASNZD        COMNAZ            3
399000160509     C                   MOVEL     TASCTS        COMCTS
399100160509     C                   MOVEL     950           WLNP
399200160509     C                   EXSR      RICTAD
399300160509     C   09              GOTO      LATAR
399400160509     C*
399500160509     C*--------------------TARIFFA REVERSIBILE----------------------------------
399600160509     C     TIPBOL        CABNE     'A'           MANTAR
399700160509     C** TARIFFA REVERSIBILE SOLO PER RITORNO(TBL=A)
399800160509     C     TASLNP        CABEQ     68            MANTAR
399900160509     C** NO TARIFFA REVERSIBILE PER 68
400000160509     C**** KSCFIL        IFEQ      888
400100160509     C                   Z-ADD     TASLNA        KFIL
400200160509     C*****              ELSE
400300160509     C****               MOVEL     KSCFIL        KFIL
400400160509     C****               ENDIF
400500160509     C**
400600160509     C                   SETON                                        56
400700160509     C                   MOVEL     TASCAM        COMCAP
400800160509     C                   MOVEL     TASNZM        COMNAZ
400900160509     C                   MOVEL     TASMCT        COMCTS
401000160509     C                   MOVEL     KFIL          WLNP
401100160509     C                   EXSR      RICTAD
401200160509     C   09              GOTO      LATAR
401300160509     C*
401400160509     C** SE NON TROVATO TITAD CON LA LINEA CLIENTE, CERCO CON LA
401500160509     C**  REGIONE TROVATA DA ORGANIGRANNA DELLA LIENA CLIENTE
401600160509     C* CONTROLLO LA LINEA DI PARTENZA DA ORGANIGRAMMA
401700160509     C     KFIL          IFNE      SAVLNP
401800160509     C     KFIL          CHAIN     AZORG01L                           31
401900160509     C   31              CLEAR                   ORGDE7
402000160509     C                   MOVEL     ORGDE7        OG147
402100160509     C                   MOVEL     KFIL          SAVLNP
402200160509     C                   ENDIF
402300160509     C*
402400160509     C                   MOVEL     TASCAM        COMCAP
402500160509     C                   MOVEL     TASNZM        COMNAZ
402600160509     C                   MOVEL     TASMCT        COMCTS
402700160509     C                   MOVEL     §OGRET        WLNP
402800160509     C                   EXSR      RICTAD
402900160509     C   09              GOTO      LATAR
403000160509     C**
403100160509     C** SE NON TROVATO TITAD CON LA REGIONE DI KFIL, CERCO CON LA
403200160509     C**  LINEA ITALIA 950
403300160509     C                   MOVEL     TASCAM        COMCAP
403400160509     C                   MOVEL     TASNZM        COMNAZ
403500160509     C                   MOVEL     TASMCT        COMCTS
403600160509     C                   MOVEL     950           WLNP
403700160509     C                   EXSR      RICTAD
403800160509     C   09              GOTO      LATAR
403900160509     C**
404000160509     C**         KTDREV    SETLLTITAD000                 09
404100160509     C** 09                SETON                     49
404200160509     C** 09                GOTO LATAR
404300160509     C**
404400160509     C**                   MOVEL*BLANKS   COMNAZ
404500160509     C**                   MOVEL*BLANKS   COMCAP
404600160509     C**         KTDREV    SETLLTITAD000                 09
404700160509     C** 09                GOTO LATAR
404800160509     C**
404900160509     C**                   Z-ADD1         A
405000160509     C**         COMCTS    LOKUPCTS,A                    05
405100160509     C**N05                GOTO POIT2
405200160509     C**         A         OCUR DSCT
405300160509     C**                   MOVEL§CTRAP    COMCTS
405400160509     C**         KTDREV    SETLLTITAD000                 09
405500160509     C** 09                GOTO LATAR
405600160509     C**         POIT2     TAG
405700160509     C**         WBOEST    IFEQ 'E'
405800160509     C**                   MOVEL'EE'      COMCTS
405900160509     C**                   ELSE
406000160509     C**                   MOVEL'IT'      COMCTS
406100160509     C**                   END
406200160509     C**         KTDREV    SETLLTITAD000                 09
406300160509     C** 09                GOTO LATAR
406400160509     C** NON TROVATO NEMMENO LA TARIFFA REVERSIBILE
406500160509     C     MANTAR        TAG
406600160509     C     *IN09         IFEQ      *OFF
406700160509     C                   SETON                                        94
406800160509     C                   MOVEL     MSG(8)        TASMSG
406900160509     C                   GOTO      FINE
407000160509     C                   ENDIF
407100160509     C** MANCA TARIFFA
407200160509     C     LATAR         TAG
407300160509     C                   EXSR      CARDST
407400160509     C** CARICA DS DELLA TARIFFA
407500160509     C                   ENDSR
407600160509     C/SPACE
407700160509     C******************************************************
407800160509     C** CARICA DS TARIFFA
407900160509     C******************************************************
408000160509     C     CARDST        BEGSR
408100160509     C     *LIKE         DEFINE    TADRPV        RAPPV
408200160509     C**
408300160509     C                   DO        200           A
408400160509     C     A             OCCUR     TARDS
408500160509     C                   CLEAR                   TARDS
408600160509     C                   END
408700160509     C                   Z-ADD     0             RAPPV
408800160509     C                   CLEAR                   WELAMT
408900160509     C**
409000160509    1C                   DO        *HIVAL        A
409100160509     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
409200160509    2C     WELAMT        IFNE      'S'
409300160509     C** 56      KTDREV    READETITAD000                 15
409400160509     C**N56      KTAD      READETITAD000                 15
409500160509     C     KTAD          READE     TITAD000                               15
409600160509     C   15              GOTO      ENDDST
409700160509    2C                   ENDIF
409800160509     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
409900160509    2C     WELAMT        IFEQ      'S'
410000160509     C                   MOVEL     'E'           WELAMT            1
410100160509    2C                   ENDIF
410200160509     C**
410300160509     C     A             OCCUR     TARDS
410400160509     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
410500160509     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
410600160509     C**
410700160509     C                   Z-ADD     TADLNP        TLNP
410800160509     C                   MOVEL     TADCTS        TCTS
410900160509     C                   MOVEL     TADNAZ        TNAZ
411000160509     C                   MOVEL     TADCAP        TCAP
411100160509     C                   Z-ADD     TADCTR        TCTR
411200160509     C                   Z-ADD     TADRPV        TRPV
411300160509     C                   Z-ADD     TADMIN        TMIN
411400160509     C                   Z-ADD     TADMAX        TMAX
411500160509     C***!!!****         Z-ADD     TADRPV        RAPPV
411600160509     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
411700160509    2C     WELAMT        IFNE      'E'
411800160509     C**
411900160509    3C     TADSGL        IFEQ      MINTAS
412000160509     C                   MOVEL     'E'           WELAMT
412100160509   X3C                   ELSE
412200160509     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
412300160509     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
412400160509     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
412500160509     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
412600160509    4C     TADSGL        IFGT      MINTAS
412700160509     C                   MOVEL     'S'           WELAMT
412800160509     C                   Z-ADD     MINTAS        TSCG
412900160509     C**
413000160509     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
413100160509    5C     TAMATA        IFEQ      0
413200160509     C     TAMATA        ORLE      MINTAS
413300160509     C     TADSGL        ORGT      TAMATA
413400160509     C**
413500160509    6C     UNOCTR        IFEQ      4
413600160509     C     UNOCTR        OREQ      0
413700160509     C     MINTAS        DIV(H)    100           WMTAS            15 5
413800160509     C                   ELSE
413900160509     C                   Z-ADD     MINTAS        WMTAS
414000160509    6C                   ENDIF
414100160509     C* TARIFFA
414200160509     C     WMTAS         MULT(H)   TADITR        TITR
414300160509     C     WMTAS         MULT(H)   TADINO        TINO
414400160509     C                   ELSE
414500160509     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
414600160509     C                   Z-ADD     TADITR        TITR
414700160509     C                   Z-ADD     TADINO        TINO
414800160509    5C                   ENDIF
414900160509    4C                   ENDIF
415000160509    3C                   ENDIF
415100160509    2C                   ENDIF
415200160509     C*
415300160509     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
415400160509    2C     WELAMT        IFNE      'S'
415500160509     C                   Z-ADD     TADSGL        TSCG
415600160509     C                   Z-ADD     TADITR        TITR
415700160509     C                   Z-ADD     TADINO        TINO
415800160509    2C                   ENDIF
415900160509    1C                   ENDDO
416000160509     C     ENDDST        ENDSR
416100160509     C/SPACE
416200160509     C******************************************************
416300160509     C** CARICAMENTO TABELLE SOLO LA 1 VOLTA
416400160509     C******************************************************
416500160509     C     CARTAB        BEGSR
416600160509     C     *LIKE         DEFINE    TAMATA        MINTAS
416700160509     C     *LIKE         DEFINE    TAMATA        MINTAR
416800160509     C     *LIKE         DEFINE    §TADIV        DIVCAR
416900160509     C                   CLEAR                   SAVLNP
417000160509     C**
417100160509     C                   OPEN      TABEL00F
417200160509     C**
417300160509     C                   EXSR      CARQT
417400160509     C** CARICAMENTO IAC
417500160509     C                   EXSR      CARTR
417600160509     C** CARICAMENTO TIPI TARIFFA
417700160509     C                   EXSR      CAR$2
417800160509     C** CARICAMENTO SIGLE VARIE
417900160509     C                   EXSR      CARCC
418000160509     C** CARICAMENTO SIGLE VARIE ESENTI
418100160509     C                   EXSR      CARCV
418200160509     C** CARICAMENTO CAMBI
418300160509     C                   EXSR      CARCT
418400160509     C** CARICAMENTO CODIFICI TASSAZIONE
418500160509     C                   EXSR      CAR5E
418600160509     C** CARICA TIPI SERVIZIO
418700160509     C                   EXSR      CAR1A
418800160509     C** CARICA TIPI INCASSO C/A MITTENTI
418900160509     C                   EXSR      CARTB
419000160509     C** CARICA DS TIPI BOLLA CHE NON PREVEDONO ASSICURAZIONE
419100160509     C                   EXSR      CAREST
419200160509     C** CARICA P.O. ESTERI
419300160509     C                   EXSR      CARDIV
419400160509     C** CARICAMENTO MONETA DI CONTO
419500160509     C                   EXSR      CARPAR
419600160509     C** CARICA TARIFFE DI CARTELLO
419700160509     C                   EXSR      CAR3A
419800160509     C** CARICO VARIE DELLE BOLLE CHE ACCETTANO UNA SINGOLA VARIA
419900160509     C                   CLOSE     TABEL00F
420000160509     C**
420100160509     C                   OPEN      TNTBE01L
420200160509     C** caricamento percentuale carburante
420300160509     C                   EXSR      CARPSC
420400160509     C** caricamento filiali addebito lasciato avviso
420500160509     C                   EXSR      CARLAV
420600160509     C** caricamento filiali addebito centro storico da cartello
420700160509     C                   EXSR      CARZTL
420800160509     C**
420900160509     C                   CLOSE     TNTBE01L
421000160509     C**
421100160509     C                   ENDSR
421200160509     C**********************************************************
421300160509     C** CARICAMENTO DS MULTIPLA  CODICI TASSAZIONE
421400160509     C**********************************************************
421500160509     C     CARCT         BEGSR
421600160509     C                   DO        600           A
421700160509     C     A             OCCUR     DSCT
421800160509     C                   CLEAR                   DSCT
421900160509     C                   END
422000160509     C** PULIZIA DS
422100160509     C                   MOVEL     'CT'          COD
422200160509     C     KTAB          SETLL     TABEL                                  07
422300160509     C  N07              SETON                                          H7
422400160509     C   H7              GOTO      FINE
422500160509     C                   Z-ADD     0             K
422600160509     C                   DO        *HIVAL        A
422700160509     C     KTAB          READE     TABEL                                  07
422800160509     C   07              GOTO      ENDCTS
422900160509     C     TBLKEY        IFNE      *BLANKS
423000160509     C                   ADD       1             K
423100160509     C                   MOVEL     TBLKEY        CTS(K)
423200160509     C     K             OCCUR     DSCT
423300160509     C                   MOVEL     TBLUNI        DSCT
423400160509     C                   END
423500160509     C                   END
423600160509     C     ENDCTS        TAG
423700160509     C                   ENDSR
423800160509     C/SPACE
423900160509     C**********************************************************
424000160509     C** CARICAMENTO TIPI SERVIZIO
424100160509     C**********************************************************
424200160509     C     CAR5E         BEGSR
424300160509     C                   DO        10            A
424400160509     C     A             OCCUR     DS5E
424500160509     C                   CLEAR                   DS5E
424600160509     C                   END
424700160509     C** PULIZIA DS
424800160509     C                   MOVEL     '5E'          COD
424900160509     C     KTAB          SETLL     TABEL                                  07
425000160509     C  N07              SETON                                          H7
425100160509     C   H7              GOTO      FINE
425200160509     C                   Z-ADD     0             K
425300160509     C                   DO        *HIVAL        A
425400160509     C     KTAB          READE     TABEL                                  07
425500160509     C   07              GOTO      END5E
425600160509     C     TBLKEY        IFNE      *BLANKS
425700160509     C                   ADD       1             K
425800160509     C                   MOVEL     TBLKEY        TS(K)
425900160509     C     K             OCCUR     DS5E
426000160509     C                   MOVEL     TBLUNI        DS5E
426100160509     C                   END
426200160509     C                   END
426300160509     C     END5E         TAG
426400160509     C                   ENDSR
426500160509     C**********************************************************
426600160509     C** CARICAMENTO TIPI INCASSO C/A
426700160509     C**********************************************************
426800160509     C     CAR1A         BEGSR
426900160509     C                   MOVEA     *ALL'9'       TIC
427000160509     C                   MOVEL     '1A'          COD
427100160509     C                   Z-ADD     0             K
427200160509     C     KTAB          SETLL     TABEL                                  07
427300160509     C     KTAB          READE     TABEL                                  07
427400160509    1C     *IN07         DOWEQ     *OFF
427500160509    2C     TBLKEY        IFNE      *BLANKS
427600160509     C     TBLFLG        ANDEQ     ' '
427700160509     C                   MOVEL     TBLUNI        DS1A
427800160509    3C     §1AFMB        IFEQ      'M'
427900160509     C                   ADD       1             K
428000160509     C                   MOVEL     TBLKEY        TIC(K)
428100160509    3C                   ENDIF
428200160509    2C                   ENDIF
428300160509     C     KTAB          READE     TABEL                                  07
428400160509    1C                   ENDDO
428500160509     C                   ENDSR
428600160509     C**********************************************************
428700160509     C** CARICAMENTO TIPI BOLLA CHE NON PREVEDONO L'ASSICURAZIONE
428800160509     C**********************************************************
428900160509     C     CARTB         BEGSR
429000160509     C                   MOVEL     'TB'          COD
429100160509     C                   Z-ADD     0             K
429200160509     C     KTAB          SETLL     TABEL                                  07
429300160509     C     KTAB          READE     TABEL                                  07
429400160509    1C     *IN07         DOWEQ     *OFF
429500160509    2C     TBLKEY        IFNE      *BLANKS
429600160509     C     TBLFLG        ANDEQ     ' '
429700160509     C                   MOVEL     TBLUNI        DSTB
429800160509    3C     §TBFAS        IFEQ      '0'
429900160509     C                   ADD       1             K
430000160509     C                   MOVEL     TBLKEY        TBN(K)
430100160509    3C                   ENDIF
430200160509    2C                   ENDIF
430300160509     C     KTAB          READE     TABEL                                  07
430400160509    1C                   ENDDO
430500160509     C                   ENDSR
430600160509     C**********************************************************
430700160509     C** CARICAMENTO VARIE PER TIPI BOLLE CHE ACCETTANO UNA SINGOLA VARIA
430800160509     C**********************************************************
430900160509     C     CAR3A         BEGSR
431000160509     C                   MOVEL     '3A'          COD
431100160509     C                   Z-ADD     0             K
431200160509     C     KTAB          SETLL     TABEL                                  07
431300160509     C     KTAB          READE     TABEL                                  07
431400160509    1C     *IN07         DOWEQ     *OFF
431500160509    2C     TBLKEY        IFNE      *BLANKS
431600160509     C     TBLFLG        ANDEQ     ' '
431700160509     C                   MOVEL     TBLUNI        DS3A
431800160509    3C     §3ASVA        IFNE      ' '
431900160509     C                   ADD       1             K
432000160509     C                   MOVEL     §3ASVA        T3A(K)
432100160509    3C                   ENDIF
432200160509    2C                   ENDIF
432300160509     C     KTAB          READE     TABEL                                  07
432400160509    1C                   ENDDO
432500160509     C                   ENDSR
432600160509     C**********************************************************
432700160509     C** CARICAMENTO PO ESTERI
432800160509     C**********************************************************
432900160509     C     CAREST        BEGSR
433000160509     C*
433100160509     C                   CLEAR                   K
433200160509     C                   CLEAR                   KK                4 0
433300160509     C**
433400160509     C                   CLEAR                   CC
433500160509     C     *LOVAL        SETLL     AZORG01L
433600160509     C                   READ      AZORG01L                               07
433700160509    1C     *IN07         DOWEQ     *OFF
433800160509     C** FIL O AGENZIA
433900160509    2C     ORGFAG        IFEQ      'F'
434000160509     C     ORGFAG        OREQ      'A'
434100160509     C* NON ANNULLATO
434200160509    3C     ORGFVA        IFEQ      ' '
434300160509    4C                   MOVEL     ORGDE3        OG143
434400160509     c* p.o. esteri senza dpd
434500160509     C     §OGNTW        IFEQ      'EEX'
434600160509     C     §OGNTW        OREQ      'EUP'
434700160509     C*******    §OGNTW    OREQ 'DPD'
434800160509     C     §OGNTW        OREQ      'FED'
434900160509     C                   ADD       1             K
435000160509     C                   MOVEL     ORGFIL        EST(K)
435100160509    4C                   ENDIF
435200160509     c* p.o. estero totale, anche con DPD
435300160509    4C     §OGNTW        IFEQ      'EEX'
435400160509     C     §OGNTW        OREQ      'EUP'
435500160509     C     §OGNTW        OREQ      'DPD'
435600160509     C     §OGNTW        OREQ      'FED'
435700160509     C                   ADD       1             KK
435800160509     C                   MOVEL     ORGFIL        ESTtot(KK)
435900160509    4C                   ENDIF
436000160509    3C                   ENDIF
436100160509    2C                   ENDIF
436200160509     C**
436300160509     C                   READ      AZORG01L                               07
436400160509    1C                   ENDDO
436500160509     C                   ENDSR
436600160509     C********************************************************
436700160509     C** CARICAMENTO SIGLE VARIE
436800160509     C********************************************************
436900160509     C     CAR$2         BEGSR
437000160509     C                   CLEAR                   DS$2
437100160509     C                   MOVEL     '$2'          COD
437200160509     C                   MOVEL     *BLANKS       KEY
437300160509     C                   MOVEL     '1'           KEY
437400160509     C     KTABC         CHAIN     TABEL                              07
437500160509     C   07              SETON                                          H7
437600160509     C   H7              GOTO      FINE
437700160509     C                   MOVEL     TBLUNI        DS$2
437800160509     C                   ENDSR
437900160509     C********************************************************
438000160509     C** CARICAMENTO SIGLE VARIE  ESENTI IVA
438100160509     C********************************************************
438200160509     C     CARCC         BEGSR
438300160509     C                   CLEAR                   DSCC
438400160509     C                   CLEAR                   KK
438500160509     C                   MOVEL     'CC'          COD
438600160509     C                   MOVEL     *BLANKS       KEY
438700160509     C                   MOVEL     'VARIE'       KEY
438800160509     C     KTABC         SETLL     TABEL                                  07
438900160509     C**N07                GOTO ENDCC
439000160509     C                   DO        *HIVAL
439100160509     C     KTAB          READE     TABEL                                  07
439200160509     C   07              LEAVE
439300160509     C                   MOVEL     TBLUNI        DSCC
439400160509     C* CONTROLLO SE VARIA ESENTE CARICO LA SIGLA IN TABELLA
439500160509     C     §CCJEI        IFNE      *BLANKS
439600160509     C                   ADD       1             K
439700160509     C                   MOVE      TBLKEY        VES(K)
439800160509     C                   ENDIF
439900160509     C* CONTROLLO SE DA RESTITUIRE CON SIGAL VARIA E IMPORTO =0
440000160509     C     §CCSV0        IFEQ      'S'
440100160509     C                   ADD       1             KK                4 0
440200160509     C                   MOVE      TBLKEY        SV0(KK)
440300160509     C                   ENDIF
440400160509     C                   ENDDO
440500160509     C                   Z-ADD     0             K
440600160509     C                   Z-ADD     0             KK
440700160509     C     ENDCC         ENDSR
440800160509     C********************************************************
440900160509     C** CARICAMENTO CAMBI
441000160509     C********************************************************
441100160509     C     CARCV         BEGSR
441200160509     C                   Z-ADD     0             K
441300160509     C                   DO        50            A
441400160509     C     A             OCCUR     DSCV
441500160509     C                   CLEAR                   DSCV
441600160509     C                   END
441700160509     C                   MOVEL     *BLANKS       CV
441800160509     C*
441900160509     C                   MOVEL     'CV'          COD
442000160509     C     KTAB          SETLL     TABEL                                  07
442100160509     C  N07              GOTO      ENDCV
442200160509     C                   DO        *HIVAL        A
442300160509     C     KTAB          READE     TABEL                                  07
442400160509     C   07              GOTO      ENDCV
442500160509     C     TBLKEY        IFNE      *BLANKS
442600160509     C                   ADD       1             K
442700160509     C                   MOVEL     TBLKEY        CV(K)
442800160509     C     K             OCCUR     DSCV
442900160509     C                   MOVEL     TBLUNI        DSCV
443000160509     C                   END
443100160509     C                   END
443200160509     C     ENDCV         ENDSR
443300160509     C******************************************************
443400160509     C** CARICAMENTO TIPI TARIFFA
443500160509     C******************************************************
443600160509     C     CARTR         BEGSR
443700160509     C                   DO        50            A
443800160509     C     A             OCCUR     DSTR
443900160509     C                   CLEAR                   DSTR
444000160509     C                   END
444100160509     C**
444200160509     C                   MOVEL     'TR'          COD
444300160509     C                   Z-ADD     0             K
444400160509     C     KTAB          SETLL     TABEL                                  07
444500160509     C  N07              SETON                                          H7
444600160509     C   H7              GOTO      FINE
444700160509     C                   DO        *HIVAL        A
444800160509     C     KTAB          READE     TABEL                                  07
444900160509     C   07              GOTO      TAB1
445000160509     C     TBLKEY        IFNE      *BLANKS
445100160509     C                   ADD       1             K
445200160509     C                   MOVEL     TBLKEY        CTR(K)
445300160509     C     K             OCCUR     DSTR
445400160509     C                   MOVEL     TBLUNI        DSTR
445500160509     C                   END
445600160509     C                   END
445700160509     C     TAB1          ENDSR
445800160509     C**********************************************************
445900160509     C** MEMORIZZA
446000160509     C** TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
446100160509     C**********************************************************
446200160509     C     CARPAR        BEGSR
446300160509      *
446400160509     C     *LIKE         DEFINE    TAMKSC        CARKSC
446500160509     C     *LIKE         DEFINE    TAMCTR        CARCTR
446600160509     C     *LIKE         DEFINE    TAMPRG        CARPRG
446700160509      *
446800160509      * RICHIAMO IL TRUL27R PER OGNI NETWORK PER CARICARMI LE TARIFFE DI CARTELLO IN SCHIERA
446900160509      *
447000160509     C* CORRIERE BARTOLINI
447100160509     C                   CLEAR                   TRULC7
447200160509     C                   MOVEL     'COR'         IC7TNTW
447300160509     C                   EXSR      SRTRUL
447400160509      * DPD
447500160509     C                   CLEAR                   TRULC7
447600160509     C                   MOVEL     'DPD'         IC7TNTW
447700160509     C                   EXSR      SRTRUL
447800160509      * FEDEX MERCI
447900160509     C                   CLEAR                   TRULC7
448000160509     C                   EVAL      IC7TIP = 'FM'
448100160509     C                   MOVEL     'FED'         IC7TNTW
448200160509     C                   EXSR      SRTRUL
448300160509      * FEDEX DOCUMENTI
448400160509     C                   CLEAR                   TRULC7
448500160509     C                   EVAL      IC7TIP = 'FD'
448600160509     C                   MOVEL     'FED'         IC7TNTW
448700160509     C                   EXSR      SRTRUL
448800160509      * EUROEXPRESS
448900160509     C                   CLEAR                   TRULC7
449000160509     C                   MOVE      'L'           IC7TLA
449100160509     C                   MOVEL     'EEX'         IC7TNTW
449200160509     C                   EXSR      SRTRUL
449300160509      *
449400160509     C                   CLEAR                   CARKSC
449500160509     C                   CLEAR                   CARCTR
449600160509     C                   CLEAR                   CARPRG
449700160509      *
449800160509      ** CARICO LE TARIFFE PARTICOLARI
449900160509     C                   EXSR      CAR1P
450000160509      *
450100160509     C                   ENDSR
450200160509     C**********************************************************
450300160509     C** CARICAMENTO SCHIERA SUPPLEMEMNTO CARBURANTE
450400160509     C**********************************************************
450500160509     C     CARPSC        BEGSR
450600160509
450700160509     C                   CLEAR                   KX
450800160509
450900160509     C                   MOVEL     'PSC'         KEYTBE
451000160509     C     KEYTBE        SETLL     TNTBE01L
451100160509     C                   DO        *HIVAL
451200160509     C     KEYTBE        READE     TNTBE01L
451300160509
451400160509     C                   IF        %EOF(TNTBE01L)
451500160509     C                   LEAVE
451600160509     C                   ENDIF
451700160509
451800160509     C                   IF        TBEATB = ' '
451900160509     C                   ADD       1             KX
452000160509     C                   IF        KX <= 999
452100160509     C                   MOVEL     TBEUNI        DPSC
452200160509     C                   Z-ADD     §PSCDAD       DS_DSC
452300160509     C                   Z-ADD     §PSCPER       DS_PSC
452400160509     C                   MOVE      WDSPSC        DSCA(KX)
452500160509     C                   ENDIF
452600160509     C                   ENDIF
452700160509
452800160509     C                   ENDDO
452900160509
453000160509     C                   SORTA     DSCA
453100160509
453200160509     C                   ENDSR
453300160509      *****************************************************************
453400160509      ** CARICAMENTO SCHIERA FILIALI ABILITATE ADDEBITO LASCIATO AVVISO
453500160509      *****************************************************************
453600160509     C     CARLAV        BEGSR
453700160509
453800160509     C                   CLEAR                   KX
453900160509
454000160509     C                   MOVEL     'LAV'         KEYTBE
454100160509     C     KEYTBE        SETLL     TNTBE01L
454200160509     C                   DO        *HIVAL
454300160509     C     KEYTBE        READE     TNTBE01L
454400160509
454500160509     C                   IF        %EOF(TNTBE01L)
454600160509     C                   LEAVE
454700160509     C                   ENDIF
454800160509
454900160509     C                   IF        TBEATB = ' '
455000160509     C                   ADD       1             KX
455100160509     C                   IF        KX <= 999
455200160509     C                   MOVEL     TBEUNI        DLAV
455300160509     C                   EVAL      SFILLAV(KX) = %dec(tbeke1:3:0)
455400160509     C                   EVAL      SDTALAV(KX) = D§LAVTAS
455500160509     C                   ENDIF
455600160509     C                   ENDIF
455700160509
455800160509     C                   ENDDO
455900160509
456000160509
456100160509     C                   ENDSR
456200160509      ******************************************************************
456300160509      ** CARICAMENTO SCHIERA FILIALI ADDEBITO DA CARTELLO CENTRO STORICO
456400160509      ******************************************************************
456500160509     C     CARZTL        BEGSR
456600160509
456700160509     C                   CLEAR                   KX
456800160509
456900160509     C                   MOVEL     'ZTL'         KEYTBE
457000160509     C     KEYTBE        SETLL     TNTBE01L
457100160509     C                   DO        *HIVAL
457200160509     C     KEYTBE        READE     TNTBE01L
457300160509
457400160509     C                   IF        %EOF(TNTBE01L)
457500160509     C                   LEAVE
457600160509     C                   ENDIF
457700160509
457800160509     C                   IF        TBEATB = ' '
457900160509     C                   ADD       1             KX
458000160509     C                   IF        KX <= 999
458100160509     C                   MOVEL     TBEUNI        DZTL
458200160509     C                   EVAL      SFILZTL(KX) = %dec(tbeke1:3:0)
458300160509     C                   EVAL      SDTAZTL(KX) = D§ZTLTAS
458400160509     C                   ENDIF
458500160509     C                   ENDIF
458600160509
458700160509     C                   ENDDO
458800160509
458900160509
459000160509     C                   ENDSR
459100160509     C*************************************************************
459200160509     C** RICHIAMO DEL TRULC7R
459300160509     C*************************************************************
459400160509     C     SRTRUL        BEGSR
459500160509      *
459600160509     C                   Z-ADD     TASDCT        IC7DTA
459700160509     c******             Movel     TasCtr        I27Ctb
459800160509     C                   CALL      'TRULC7R'
459900160509     C                   PARM                    TRULC7
460000160509      *
460100160509     C     OC7ERR        IFEQ      *BLANKS
460200160509     C     OC7PRGC       ANDNE     *BLANKS
460300160509     C                   ADD       1             T                 2 0
460400160509     C                   MOVE      IC7TNTW       NTW(T)
460500160509     C                   Z-ADD     OC7KSCC       TAC(T)
460600160509     C                   Z-ADD     OC7CTRC       CTC(T)
460700160509     C                   MOVE      OC7PRGC       PRC(T)
460800160509     C                   MOVE      IC7TIP        TIP(T)
460900160509      *
461000160509      * AGGANCIO LA TARIFFA PER RECUPERARE LA DIVISA
461100160509      *
461200160509     C                   Z-ADD     OC7KSCC       CARKSC
461300160509     C                   Z-ADD     OC7CTRC       CARCTR
461400160509     C                   MOVE      OC7PRGC       CARPRG
461500160509     C     KTAMC         CHAIN     TNTAM00L                           94
461600160509      *
461700160509     C     *IN94         IFEQ      *OFF
461800160509     C                   MOVEL     TAMFLO        DSTA01
461900160509     C                   MOVE      §TADIV        DIC(T)
462000160509      * SE C'E ERRORE VADO A FINE PROGRAMMA
462100160509     C                   ELSE
462200160509     C                   MOVEL     MSG(9)        TASMSG
462300160509     C                   ENDIF
462400160509      *
462500160509      * SE C'E ERRORE VADO A FINE PROGRAMMA
462600160509     C                   ELSE
462700160509     C                   SETON                                        94
462800160509     C                   MOVEL     MSG(9)        TASMSG
462900160509     C                   ENDIF
463000160509      *
463100160509     C                   ENDSR
463200160509     C*************************************************************
463300160509     C** CERCO LA TARIFFA DI CARTELLO
463400160509     C*************************************************************
463500160509     C     CERTCA        BEGSR
463600160509     C** DALLE SCHIERE CARICO LA TARIFFA DI CARTELLO
463700160509     C                   SETOFF                                       90
463800160509     C                   Z-ADD     1             T
463900160509      * se ho il tipo tariffa fedex valorizzato vado con questo altrimenti vado
464000160509      * con il network della tariffa
464100160509     c     Tiptam        ifne      '  '
464200160509     c     tiptam        lookup    TIP(T)                                 90
464300160509     c                   else
464400160509     C     NtwTam        LOOKUP    NTW(T)                                 90
464500160509     c                   endif
464600160509     C*  IMPOSSIBILE CHE 90 SIA SPENTO !!!!!!
464700160509     C     *IN90         IFEQ      *OFF
464800160509     C                   MOVEL     MSG(9)        TASMSG
464900160509     C                   SETON                                        94
465000160509     C                   GOTO      FINE
465100160509     C                   ENDIF
465200160509     C*
465300160509     C                   Z-ADD     TAC(T)        CARKSC
465400160509     C                   Z-ADD     CTC(T)        CARCTR
465500160509     C                   Z-ADD     PRC(T)        CARPRG
465600160509     C                   MOVE      DIC(T)        DIVCAR
465700160509      *
465800160509     C                   ENDSR
465900160509     C*************************************************************
466000160509     C** CARICAMENTO CODICI E TARIFFE PARTICOLARI DI CARTELLO
466100160509     C*************************************************************
466200160509     C     CAR1P         BEGSR
466300160509     C*
466400160509     C                   DO        50            A
466500160509     C     A             OCCUR     DS1P
466600160509     C                   CLEAR                   DS1P
466700160509     C                   END
466800160509     C                   MOVEL     *BLANKS       CP
466900160509     C**
467000160509     C                   MOVEL     '1P'          COD
467100160509     C                   Z-ADD     0             K                 5 0
467200160509     C     KTAB          SETLL     TABEL                                  07
467300160509     C  N07              SETON                                          H7
467400160509     C   H7              GOTO      FINE
467500160509     C                   DO        *HIVAL        A
467600160509     C     KTAB          READE     TABEL                                  07
467700160509     C   07              GOTO      TABCP
467800160509     C     TBLKEY        IFNE      *BLANKS
467900160509     C                   MOVEL     TBLUNI        CAMPO            26
468000160509     C                   MOVE      CAMPO         WFCP              1
468100160509     C     WFCP          IFEQ      'S'
468200160509     C                   ADD       1             K
468300160509     C                   MOVEL     TBLKEY        CP(K)
468400160509     C     K             OCCUR     DS1P
468500160509     C                   MOVEL     TBLUNI        DS1P
468600160509     C**
468700160509     C                   END
468800160509     C                   END
468900160509     C                   ENDDO
469000160509     C*
469100160509     C     TABCP         ENDSR
469200160509     C****************************************************
469300160509     C** CARICAMENTO DATI VARI TASSAZIONE
469400160509     C****************************************************
469500160509     C     CARQT         BEGSR
469600160509     C                   MOVEL     'QT'          COD
469700160509     C                   MOVEL     *BLANKS       KEY
469800160509     C                   MOVEL     '1'           KEY
469900160509     C     KTABC         CHAIN     TABEL                              07
470000160509     C   07              SETON                                        H7
470100160509     C   H7              GOTO      FINE
470200160509     C  N07              MOVEL     TBLUNI        DSQT1
470300160509     C*
470400160509     C                   ENDSR
470500160509     C****************************************************
470600160509     C** CARICAMENTO MONETE DI CONTO / IMPORTI STANDARD
470700160509     C****************************************************
470800160509     C     CARDIV        BEGSR
470900160509     C*****
471000160509      *  RICERCA DELLA MONETA DI CONTO
471100160509     C                   CLEAR                   DSBS02
471200160509     C                   CLEAR                   DGED
471300160509     C*
471400160509     C                   MOVEL     'C'           T02MOD
471500160509     C                   MOVEL     KNSIF         T02SIF
471600160509     C                   MOVEL     'GED'         T02COD
471700160509     C                   MOVEL     '1'           T02KE1
471800160509      *
471900160509     C                   CALL      'TIBS02R'
472000160509     C                   PARM                    KPJBA
472100160509     C                   PARM                    DSBS02
472200160509      *
472300160509     C     T02ERR        IFEQ      *BLANKS
472400160509     C                   MOVEL     T02UNI        DGED
472500160509     C                   ENDIF
472600160509      * RICERCA DEGLI IMPORTI STANDARD NELLA MONETA DI CONTO
472700160509     C                   CLEAR                   DSBS02
472800160509     C                   CLEAR                   DGEI
472900160509     C                   MOVEL     'L'           T02TLA
473000160509     C                   MOVEL     'C'           T02MOD
473100160509     C                   MOVEL     KNSIF         T02SIF
473200160509     C                   MOVEL     'GEI'         T02COD
473300160509     C                   MOVEL     §GEDCN        T02KE1
473400160509      *
473500160509     C                   CALL      'TIBS02R'
473600160509     C                   PARM                    KPJBA
473700160509     C                   PARM                    DSBS02
473800160509      *
473900160509     C     T02ERR        IFEQ      *BLANKS
474000160509     C                   MOVEL     T02UNI        DGEI
474100160509     C                   ENDIF
474200160509      *
474300160509      *
474400160509     C                   ENDSR
474500160509     C**********************************************************
474600160509     C** RECUPERO LA DATA DEL GIORNO
474700160509     C**********************************************************
474800160509     C     RUDATE        BEGSR
474900160509      *
475000160509     C                   TIME                    W0140            14 0
475100160509     C* UDATE IN GGMMAAAA
475200160509     C                   MOVE      W0140         WDTGIO            8 0
475300160509     C*
475400160509     C                   Z-ADD     WDTGIO        G02DAT
475500160509     C                   MOVEL     *BLANK        G02ERR
475600160509     C                   CALL      'XSRDA8'
475700160509     C                   PARM                    WLBDAT
475800160509     C*
475900160509     C* UDATE IN AAAAMMGG
476000160509     C                   Z-ADD     G02INV        WDTGIO
476100160509      *
476200160509     C                   ENDSR
476300160509     C**********************************************************
476400160509     C** RECUPERO LA DATA TASSAZIONE DA USARE PER QUESTA BOLLA
476500160509     C**********************************************************
476600160509     C**!!!RECDAT        BEGSR
476700160509      *
476800160509     C**!!!              SETOFF                                       31
476900160509      * VERIFICO SE DATA VALORIZZATA E SOPRATTUTTO NUMERICA
477000160509     C**         §ASDAT    IFNE *BLANK
477100160509     C**         §ASDAT    ANDNE*ZEROS
477200160509      *
477300160509     C**                   TESTN          §ASDAT     31
477400160509      * SE 31 ACCESO VUOL DIRE CHE È NUMERICO MA VERIFICO ANCHE L'ULTIMO CARATTERE
477500160509     C**         *IN31     IFEQ *ON
477600160509     C**                   MOVE §ASDAT    W01A    1
477700160509     C**         W01A      COMP '0'                  31  31
477800160509     C**                   ENDIF
477900160509      *
478000160509     C**                   ENDIF
478100160509      * SE 31 ACCESO DATA VALIDA ALTRIMENTI PRENDO LA DATA DEL GIORNO
478200160509     C**         *IN31     IFEQ *ON
478300160509     C**                   MOVE §ASDAT    DATTAS  80
478400160509     C**                   ELSE
478500160509     C**                   Z-ADDWDTGIO    DATTAS
478600160509     C**                   ENDIF
478700160509     C*
478800160509      *
478900160509     C**!!!              ENDSR
479000160509     C****************************************************
479100160509     C** CARICAMENTO IMPORTI PASSATI DALLE DS
479200160509     C****************************************************
479300160509     C     SAVDS         BEGSR
479400160509     C*
479500160509     C     *LIKE         DEFINE    TASPOR        TASINL
479600160509     C*
479700160509     C* SALVO LE SCHIERE DELLE VARIE (SIGLE VARIE+IMPORTI)
479800160509     C                   MOVEA     SV            SVW
479900160509     C                   MOVEA     VA            VAW
480000160509     C                   CLEAR                   TASINL
480100160509     C                   CLEAR                   TASDIF
480200160509     C*
480300160509     C* RECUPERO L'IMPORTO DELL'INOLTRO SE C'E' NELLA SCHIERA DELLE VARIE
480400160509     C                   Z-ADD     1             X                 2 0
480500160509     C     §$2FIN        LOOKUP    SV(X)                                  30
480600160509     C* SE LO TROVO VALORIZZO TASINL
480700160509     C   30              Z-ADD     VA(X)         TASINL
480800160509     C  N30              Z-ADD     0             X
480900160509     C*
481000160509     C* RECUPERO L'IMPORTO DEL DIRITTO FISSO  NELLA SCHIERA DELLE VARIE
481100160509     C                   Z-ADD     1             Y                 2 0
481200160509     C     §$2VRH        LOOKUP    SV(Y)                                  30
481300160509     C* SE LO TROVO VALORIZZO TASINL
481400160509     C   30              Z-ADD     VA(Y)         TASDIF           11 3
481500160509     C*
481600160509     C                   ENDSR
481700160509     C****************************************************
481800160509     C** CONVERSIONE DELLE VARIE IN MONETA DELLA TARIFFA
481900160509     C****************************************************
482000160509     C     CNVTAS        BEGSR
482100160509     C*
482200160509     C* SE DIVISA PASSATA (DELLA BOLLA) E' DIVERSA DALLA DIVISA DELLA
482300160509     C* TARIFFA CONVERTO IN DIVISA TARIFFA
482400160509     C                   Z-ADD     1             A
482500160509    1C                   DO        20            A
482600160509    2C     VA(A)         IFNE      *ZEROS
482700160509     C                   CLEAR                   YEUR
482800160509     C                   MOVEL     DIVINP        YECDVI
482900160509     C                   MOVEL     DIVOUT        YECDVO
483000160509     C                   Z-ADD     VA(A)         YECIMI
483100160509     C   12              MOVE      '3'           YECDCO
483200160509     C  N12              MOVE      '0'           YECDCO
483300160509     C*
483400160509     C                   CALL      'YEURCO'
483500160509     C                   PARM                    YEUR
483600160509     C*
483700160509    3C     YECESI        IFEQ      ' '
483800160509     C                   Z-ADD     YECIMO        VA(A)
483900160509    3C                   END
484000160509    2C                   END
484100160509    1C                   END
484200160509     C* CONVERTO L'IMPORTO DEL PORTO
484300160509     C* SE PORTO DIVERSO DA 0
484400160509    1C     TASPOR        IFGT      0
484500160509     C                   CLEAR                   YEUR
484600160509     C                   MOVEL     DIVINP        YECDVI
484700160509     C                   MOVEL     DIVOUT        YECDVO
484800160509     C                   Z-ADD     TASPOR        YECIMI
484900160509     C   12              MOVE      '3'           YECDCO
485000160509     C  N12              MOVE      '0'           YECDCO
485100160509     C*
485200160509     C                   CALL      'YEURCO'
485300160509     C                   PARM                    YEUR
485400160509     C*
485500160509    2C     YECESI        IFEQ      ' '
485600160509     C                   Z-ADD     YECIMO        TASPOR
485700160509     C                   ELSE
485800160509     C                   MOVEL     MSG(10)       TASMSG
485900160509     C                   GOTO      FINE
486000160509    2C                   END
486100160509    1C                   ENDIF
486200160509     C* SE INOLTRO DIVERSO DA 0
486300160509    1C     TASINL        IFGT      0
486400160509     C                   CLEAR                   YEUR
486500160509     C                   MOVEL     DIVINP        YECDVI
486600160509     C                   MOVEL     DIVOUT        YECDVO
486700160509     C                   Z-ADD     TASINL        YECIMI
486800160509     C   12              MOVE      '3'           YECDCO
486900160509     C  N12              MOVE      '0'           YECDCO
487000160509     C*
487100160509     C                   CALL      'YEURCO'
487200160509     C                   PARM                    YEUR
487300160509     C*
487400160509    2C     YECESI        IFEQ      ' '
487500160509     C                   Z-ADD     YECIMO        TASINL
487600160509     C                   ELSE
487700160509     C                   MOVEL     MSG(10)       TASMSG
487800160509     C                   GOTO      FINE
487900160509    2C                   END
488000160509    1C                   ENDIF
488100160509     C* SE DIRITTO FISSO DIVERSO DA 0
488200160509    1C     TASDIF        IFGT      0
488300160509     C                   CLEAR                   YEUR
488400160509     C                   MOVEL     DIVINP        YECDVI
488500160509     C                   MOVEL     DIVOUT        YECDVO
488600160509     C                   Z-ADD     TASDIF        YECIMI
488700160509     C   12              MOVE      '3'           YECDCO
488800160509     C  N12              MOVE      '0'           YECDCO
488900160509     C*
489000160509     C                   CALL      'YEURCO'
489100160509     C                   PARM                    YEUR
489200160509     C*
489300160509    2C     YECESI        IFEQ      ' '
489400160509     C                   Z-ADD     YECIMO        TASDIF
489500160509    2C                   END
489600160509    1C                   ENDIF
489700160509     C* SE IMPORTO IMPONIBILE DIVERSO DA ZEROS
489800160509     C* LO RICALCOLO PER SICUREZZA
489900160509    1C     TASIMV        IFGT      0
490000160509     C                   CLEAR                   DSLV16
490100160509     C                   MOVEL     'T'           D17FIM
490200160509     C                   Z-ADD     TASPOR        D17POR
490300160509     C                   CALL      'FNLV16R'
490400160509     C                   PARM                    DSLV16
490500160509     C                   PARM                    DTASV
490600160509     C                   Z-ADD     O17IMV        TASIMV
490700160509    1C                   ENDIF
490800160509     C* SE QUANTITA' DA FATTURARE E' DIVERSA DA 0 E LA TARIFFA E' UNA TARIFFA
490900160509     C* A VALORE SPECIFICO DEVO CONVERTIRE IL CAMPO SE RICHIESTO
491000160509     C*
491100160509    1C     TASQFT        IFGT      0
491200160509     C     TAMFVD        ANDEQ     '4'
491300160509     C     CONQTA        ANDNE     'N'
491400160509     C                   CLEAR                   YEUR
491500160509     C                   MOVEL     DIVINP        YECDVI
491600160509     C                   MOVEL     DIVOUT        YECDVO
491700160509     C                   Z-ADD     TASQFT        YECIMI
491800160509     C   12              MOVE      '3'           YECDCO
491900160509     C  N12              MOVE      '0'           YECDCO
492000160509     C*
492100160509     C                   CALL      'YEURCO'
492200160509     C                   PARM                    YEUR
492300160509     C*
492400160509    2C     YECESI        IFEQ      ' '
492500160509     C                   Z-ADD     YECIMO        TASQFT
492600160509    2C                   END
492700160509    1C                   ENDIF
492800160509     C* SE TASIAL VALORIZZATO DEVO CONVERTIRLO SOLO SE RICHIESTO
492900160509     C*
493000160509    1C     TASIAL        IFGT      0
493100160509     C     CONIAL        ANDNE     'N'
493200160509     C                   CLEAR                   YEUR
493300160509     C                   MOVEL     DIVINP        YECDVI
493400160509     C                   MOVEL     DIVOUT        YECDVO
493500160509     C                   Z-ADD     TASIAL        YECIMI
493600160509     C   12              MOVE      '2'           YECDCO
493700160509     C  N12              MOVE      '0'           YECDCO
493800160509     C*
493900160509     C                   CALL      'YEURCO'
494000160509     C                   PARM                    YEUR
494100160509     C*
494200160509    2C     YECESI        IFEQ      ' '
494300160509     C                   Z-ADD     YECIMO        TASIAL
494400160509    2C                   END
494500160509    1C                   ENDIF
494600160509     C*
494700160509     C                   ENDSR
494800160509     C************************************************************
494900160509     C** CONVERSIONE DEL DETTAGLIO TARIFFA PARTICOLARE DI CARTELLO
495000160509     C************************************************************
495100160509     C     CONTPD        BEGSR
495200160509     C*
495300160509     C                   CLEAR                   YEUR
495400160509     C                   MOVEL     DIVCAR        YECDVI
495500160509     C                   MOVEL     §TADIV        YECDVO
495600160509     C   12              MOVE      '3'           YECDCO
495700160509     C  N12              MOVE      '0'           YECDCO
495800160509     C*
495900160509     C* CONTROLLLO IL TIPO TARIFFA SE E' VALORE OPPURE NO PER CONVERTIRE
496000160509     C* LO SCAGLIONE OPPURE NO
496100160509     C*
496200160509    1C     WTPG          IFEQ      'T'
496300160509     C     WTPG          OREQ      '4'
496400160509     C     WTPG          OREQ      'V'
496500160509     C*
496600160509     C                   Z-ADD     TPDSGL        YECIMI
496700160509     C                   CALL      'YEURCO'
496800160509     C                   PARM                    YEUR
496900160509    2C     YECESI        IFEQ      ' '
497000160509     C                   Z-ADD     YECIMO        TPDSGL
497100160509    2C                   END
497200160509     C* NON CONVERTO L'IMPORTO TARIFFA PERCHE' E' UNA PERCENTUALE
497300160509    1C                   ELSE
497400160509     C*
497500160509     C                   Z-ADD     TPDITR        YECIMI
497600160509     C                   CALL      'YEURCO'
497700160509     C                   PARM                    YEUR
497800160509    2C     YECESI        IFEQ      ' '
497900160509     C                   Z-ADD     YECIMO        TPDITR
498000160509    2C                   END
498100160509    1C                   ENDIF
498200160509     C*
498300160509     C* MINIMO
498400160509     C                   Z-ADD     TPDMIN        YECIMI
498500160509     C                   CALL      'YEURCO'
498600160509     C                   PARM                    YEUR
498700160509    1C     YECESI        IFEQ      ' '
498800160509     C                   Z-ADD     YECIMO        TPDMIN
498900160509    1C                   END
499000160509     C* MASSIMO
499100160509     C                   Z-ADD     TPDMAX        YECIMI
499200160509     C                   CALL      'YEURCO'
499300160509     C                   PARM                    YEUR
499400160509    1C     YECESI        IFEQ      ' '
499500160509     C                   Z-ADD     YECIMO        TPDMAX
499600160509    1C                   END
499700160509     C*
499800160509     C                   ENDSR
499900160509     C************************************************************
500000160509     C** CONTROLLO SE APPLICARE PESO NORMALE O VDL
500100160509     C************************************************************
500200160509     C     CTRPES        BEGSR
500300160509     C                   CLEAR                   USAPKF
500400160509     C**!!!              CLEAR                   §ASSPC
500500160509     C                   CLEAR                   TASSPC
500600160509     C                   CLEAR                   TASPKCN
500700160509     C*
500800160509     C* SE NON C'E' IL PESO VDL, PRENDO QUELLO DA FATTURARE
500900160509     C**!!!§ASPKC        IFEQ      0
501000160509     C     TASPKC        IFEQ      0
501100160509     C                   Z-ADD     TASPKF        USAPKF
501200160509     C                   GOTO      ENDPES
501300160509     C                   ELSE
501400160509     C* SE IL VDL NON + MAI DA APPLICARE --> USO IL DA FATTURARE
501500160509     C     §TATAP        IFEQ      'M'
501600160509     C                   Z-ADD     TASPKF        USAPKF
501700160509     C                   GOTO      ENDPES
501800160509     C                   ENDIF
501900160509     C                   ENDIF
502000160509     C* PESO VDL - TARA
502100160509     C**!!!§QTTPC        MULT      §ASNCP        WTARA
502200160509     C     §QTTPC        MULT      TASNCP        WTARA
502300160509     C**!!!§ASPKC        SUB(h)    WTARA         WNTARA
502400160509     C     TASPKC        SUB(h)    WTARA         WNTARA
502500160509     C* SE PESO DA FATTURARE COMPRESO DA VDL E VDL-TARA
502600160509     C*  APPLICO SEMPRE IL PESO DA FATTURARE
502700160509     C*
502800160509     C     TASPKF        IFGE      WNTARA
502900160509     C**!!!TASPKF        ANDLE     §ASPKC
503000160509     C     TASPKF        ANDLE     TASPKC
503100160509     C                   Z-ADD     TASPKF        USAPKF
503200160509     C                   GOTO      ENDPES
503300160509     C                   ENDIF
503400160509     C* SE VDL-TARA > PESO FATTURARE LO APPLICO SIA PER " " CHE PER "S"
503500160509     C     WNTARA        IFGT      TASPKF
503600160509     C                   Z-ADD     WNTARA        USAPKF
503700160509     C**!!!              MOVEL     'S'           §ASSPC
503800160509     C**!!!              z-add     wntara        §ASpkc
503900160509     C                   MOVEL     'S'           TASSPC
504000160509     C                   z-add     wntara        TASpkcN
504100160509     C                   GOTO      ENDPES
504200160509     C                   ENDIF
504300160509     C* SE VDL-TARA < PESO FATTURARE LO APPLICO SOLO SE TOTALE E SE
504400160509     C* "S"
504500160509     C     WNTARA        IFLT      TASPKF
504600160509     C**!!!§ASNCP        ANDEQ     TASNCL
504700170906     C     TASNCP        ANDEQ     TASNCLB
504800160509     C     §TATAP        ANDEQ     'S'
504900160509     C                   Z-ADD     WNTARA        USAPKF
505000160509     C**!!!              MOVEL     'S'           §ASSPC
505100160509     C**!!!              z-add     wntara        §ASpkc
505200160509     C                   MOVEL     'S'           TASSPC
505300160509     C                   z-add     wntara        TASpkcN
505400160509     C                   GOTO      ENDPES
505500160509     C                   ENDIF
505600160509     C* SE ANCORA A 0 (IMPOSSIBILE) USO IL PESO DA FATTURARE
505700160509     C     USAPKF        IFEQ      0
505800160509     C                   Z-ADD     TASPKF        USAPKF
505900160509     C                   ENDIF
506000160509     C**
506100160509     c     endpes        tag
506200160509      * valorizzo il flag di tipo applicazione volume
506300160509      * serve per escludere le bolle con tariffa mai applicazione VDL
506400160509      * dal calcolo del peso desunto (PRG 852)
506500160509     c                   eval      tastap = §tatap
506600160509
506700160509      * valorizzo il flag di KSC / CTR esclusione calcolo peso desunto
506800160509      * serve per escludere le bolle dei clienti che non vogliono
506900160509      * il  calcolo del peso desunto (PRG 852)
507000160509     c                   if        tamtpr = 'NO'
507100160509     c                   eval      tasepd = 'N'
507200160509     c                   else
507300160509     c                   clear                   tasepd
507400160509     c                   endif
507500160509
507600160509      * valorizzo il progressivo tariffa utilizzato in tassazione
507700160509     c                   movel     tamprg        tasprg
507800160509
507900160509     C                   ENDSR
507901170906     C************************************************************
507902170906     C** CONTROLLO SE APPLICARE VOLUME REALE O VDL
507903170906     C************************************************************
507904170906     C     CTRVOL        BEGSR
507905170906     C                   CLEAR                   USAVLF
507906170906     C                   CLEAR                   TASFVT
507907170906     C                   CLEAR                   TASVLT
507908170906
507909170906      * valorizzo il volume da usare in tassazione uguale al volume da fatturare
507910170906     c                   eval      usavlf = tasvlf
507911170906     c                   eval      tasfvt = tasfvf
507912170906     c                   eval      tasvlt = tasvlf
507913170906
507914170906     C                   SELECT
507915170906      *
507916170906      * se in tariffa il cliente ha come apllicazione VDL "MAI VDL"  utilizzo TASVLF
507917170906     c                   WHEN      §tatap = 'M'
507918170906      *
507919170906      * se volume da fatturare non è reale utilizzo TASVLF
507920170906     c                   WHEN      tasfvf <> 'R'
507921170906      *
507922170906      * se volume da fatturare è reale e non c'è stata rilevazioni sul VDL utilizzo TASVLF
507923170906     c                   WHEN      tasvlc = 0
507924170906      *
507925170906      * se volume da fatturare è reale e c'è stata rilevazioni sul VDL
507926170906      * se rilevazione totale prendo volume VDL
507927170906     c                   WHEN      tasvlc > 0 and
507928170906     c                             tasnclb = tasncr
507929170906     c                   eval      usavlf = tasvlc
507930170906     c                   eval      tasfvt = 'T'
507931170906     c                   eval      tasvlt = tasvlc
507932170906      *
507933170906      * se volume da fatturare è reale e c'è stata rilevazioni sul VDL
507934170906      * se rilevazione parziale prendo volume VDL se maggiore del volume da fatturare
507935170906     c                   WHEN      tasvlc > 0 and
507936170906     c                             tasnclb > tasncr and tasvlc > tasvlf
507937170906     c                   eval      usavlf = tasvlc
507938170906     c                   eval      tasfvt = 'Z'
507939170906     c                   eval      tasvlt = tasvlc
507940170906
507941170906     c                   endsl
507942170906
507943170906     C                   ENDSR
508000160509**
508100160509DIVISA ERRATA per ricerca cambio per il calcolo dell'importo del CONTRASSEGNO  1
508200160509DIVISA ERRATA per ricerca cambio per il calcolo dell'importo da ASSICURARE     2
508300160509Non trovata tariffa cliente                                                    3
508400160509Mancano importo da assicurare in bolla e valore merce in tariffa               4
508500160509Non trovato SCAGLIONE in tariffa                                               5
508600160509Manca la quantita' da fatturare                                                6
508700160509Non trovato SCAGLIONE in tariffa particolare
508800160509Non trovato CODICE TASSAZIONE in tariffa                                       8
508900160509Non trovata tariffa di CARTELLO                                                9
509000160509Tariffa cliente con decorrenza maggiore della data spedizione                 10
509100160509Divisa errata                                                                 11
509200160509Tariffa DPD ma bolla Italia                                                   12
509300160509Bolla import/export DPD  ma  tariffa NON DPD                                  13
509400160509Divisa tariffa del cliente non più utilizzabile                               14
509500160509Tariffa FEDEX ma bolla Italia                                                 15
509600160509Bolla import/export FEDEX ma  tariffa NON FEDEX                               16
509700160509Non trovato COD.TASSAZIONE in tar.PARTICOLARE                                 17
509800160509Non presente tassazione per addebito insoluti                                 18
509900160509Tariffa valida ma imponibile minore di 0,000                                  19
