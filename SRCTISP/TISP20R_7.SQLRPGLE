000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*------------------------------------------------------------------------*
000300000000     F* DATA BASE
000400000000     F*------------------------------------------------------------------------*
000500061017     FTISP20D   CF   E             WORKSTN INFDS(DEVDS1)
000600061017     F                                     SFILE(SP20DMS:MSGULT)
000700061017     Fcap_cap   UF   E             DISK    rename(cap_cap:cap000) prefix(c_)
000800061017     Fcap_loc   UF   E             DISK    rename(cap_loc:loc000) prefix(l_)
000900061017     Fazcpr01l  IF   E           K DISK
001000061017     Fazcpc01l  UF A E           K DISK
001100061017     Fazcpl01l  UF A E           K DISK
001200061024     Fazcpl02l  UF A E           K DISK    rename(azcpl000:azplc002) prefix(l2_)
001300061017     Fazcps01l  UF A E           K DISK
001400061017     Fwfdcpl00f O  A E             DISK    prefix(diff_)
001500061017     FPRTF198   O    F  198        PRINTER
001600061023     FPRTF1982  O    F  198        PRINTER
001700061120     FQSYSPRT   O    F  132        PRINTER
001800000000     D*------------------------------------------------------------------------*
001900000000     D* SCHIERE
002000000000     D*------------------------------------------------------------------------*
002100000000     D*-------------------
002200000000     D* MESSAGGI
002300000000     D*-------------------
002400000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002500000000     D*-------------------
002600000000     D* DESCRIZIONI TASTI FUNZIONALI
002700000000     D*-------------------
002800000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002900000000     D*-------------------
003000000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003100000000     D*-------------------
003200000000     D FUV             S             19    DIM(24)
003300000417     D*-------------------
003400000000     D*------------------------------------------------------------------------*
003500000000     D* INPUT
003600000000     D*------------------------------------------------------------------------*
003700000000     D*-------------------
003800000000     D* RIDEFINIZIONE SCHIERA -FUD-
003900000000     D*-------------------
004000000000     D FUDDS           DS
004100000000     D*                -DESCRIZIONE PARZIALE
004200000000     D  FUDDE1                 1      1
004300000000     D*                -FUNZIONE
004400000000     D  FUDFUN                 2      3
004500000000     D*                -DESCRIZIONE PARZIALE
004600000000     D  FUDDE2                 4     19
004700000000     D*                -FILLER
004800000000     D  FUDFI0                20     34
004900000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005000000000     D  FUDATT                35     35
005100000000     D*                -FILLER
005200000000     D  FUDFI1                36     80
005300000000     D*                -DESCRIZIONE COMPLETA
005400000000     D  FUDDES                 1     19
005500000000     D*                -RIDEFINIZIONE RIGA
005600000000     D  FUDRIG                 1     80
005700000000     D*-------------------
005800000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005900000000     D*-------------------
006000000000     D DSMSG         E DS
006100000000     D*-------------------
006200020917     D* DS REPERIMENTO DATI UTENTE
006300020917     D*------------
006400020917     D TIBS34DS      E DS                                                       *Profili utente
006500020917     D DDATIUTE      E DS                                                       *Dati utente
006600020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006700061017     D*-------------------
006800061017     D* ARCHITETTURA
006900061017     D*-------------------
007000061017     D KPJBA         E DS
007100000000     D*-------------------
007200000000     D* DS PER GESTIONE TASTI FUNZIONALI
007300000000     D*-------------------
007400000000     D DSKEY         E DS
007500000000     D DEVDS1          DS
007600000000     D  KEY                  369    369
007700061017     D*------------------------------------------------------------------------*
007800061017     D* DS
007900061017     D*------------------------------------------------------------------------*
008000061017     D*
008100061017     D DSSI00        E DS                  extname(tisp00ds)                    *ds di procedura
008200061017     D*
008300061017     D ds_cpl_obs    E DS                  extname(azcpl00f) prefix(O_)
008400061017     D*
008500061017     D ds_cpc_k      E DS                  extname(azcpc00f) prefix(K_)
008600061017     D ds_cpl_k      E DS                  extname(azcpl00f) prefix(K_)
008700061017     D ds_cps_k      E DS                  extname(azcps00f) prefix(K_)
008800061017     D*
008900061017     D ds_cap_pt     E DS                  extname(cap_cap)  prefix(wc_)
009000061017     D ds_loc_pt     E DS                  extname(cap_loc)  prefix(wl_)
009100061017     D*
009200061017     D ds_cap_sin    E DS                  extname(cap_sin)  prefix(s_)
009300061017     D*
009400061017     D ds_loc_uniq     DS
009500061017     D   uniq_loc                    35
009600061017     D   uniq_prv                     2
009700061023     D   uniq_pra                     2
009800061017     D   uniq_cnt                     9  0
009900061017     D*
010000061017     D ds_loc_rainer   DS
010100061017     D   rainer_loc                  50
010200061017     D   rainer_cap                   9
010300061017     D   rainer_cnt                   9  0
010400061017     D*
010500061017     D ds_com_pt       DS
010600061017     D   com_com                     50
010700061017     D*
010800061017     D ds_loc          DS
010900061017     D   loc_ver                           like(cplver)
011000061017     D   loc_nar                           like(cplnar)
011100061017     D   loc_cap                           like(cplcap)
011200061017     D   loc_loc                           like(cplloc)
011300061024     D*
011400061024     D ds_prv          DS
011500061024     D   prv_prv                           like(cpcprv)
011600061024     D   prv_pra                           like(cpcpra)
011700061023     D*
011800061023     D tisi95ds      e ds
011900061120     D*
012000061120     D ds_cnt_sin      DS                  inz
012100061120     D   cnt_sin_cap                       like(cpscap)
012200061120     D   cnt_sin_sin                       like(cpssin)
012300061017     D*------------------------------------------------------------------------*
012400061017     D* VARIABILI D WRK
012500061017     D*------------------------------------------------------------------------*
012600061017     D wRiga           s            198    inz
012700061023     D wSI95           s            198    inz
012800061017     D wSQL            s            300    inz
012900061017     D wCPCVER         s                   inz like(cpcver)
013000061017     D wCPCCAP         s                   inz like(cpccap)
013100061017     D wCPLLOC         s                   inz like(cplloc)
013200061017     D wCPLESS         s                   inz like(cpless)
013300061023     D wCPLLIV         s                   inz like(cplliv)
013400061017     D wCPSCAP         s                   inz like(cpscap)
013500061017     D wCPSLOC         s                   inz like(cpsloc)
013600061017     D savcap          S                   inz like(cpccap)
013700061017     D chkobs          S              1    inz('1')
013800061122     D key_sin_loc     S                   inz like(cpsloc)
013900061122     D wrk_error       S              1    inz(*blanks)
014000061122     D w_cnt_sin       S              5  0 inz
014100061122     D*------------------------------------------------------------------------*
014200061122     D* DS E SCHIERA X ANALISI PREVENTIVA SITUAZIONE SINONIMI
014300061122     D*------------------------------------------------------------------------*
014400061122     D ds_dup_sin      DS                  inz
014500061122     D   d_dup_fi1                         inz like(cplfi1)
014600061122     D   d_dup_cap                         inz like(cpscap)
014700061122     D   d_dup_sin                         inz like(cpssin)
014800061122     D   d_dup_loc                         inz like(cpsloc)
014900000000     C*------------------------------------------------------------------------*
015000000000     C* MAIN LINE
015100000000     C*------------------------------------------------------------------------*
015200080317     C*
015300080317     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
015400080317     C
015500080317     C/EXEC SQL
015600080317     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
015700080317     C/END-EXEC
015800000000     C*
015900000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
016000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
016100000000     C*
016200000000     C* CICLO FINO A CHE:
016300000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
016400000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
016500000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
016600000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
016700000000     C     F12L00        OREQ      '1'
016800000000     C     FLFINE        OREQ      '1'
016900000000     C*
017000000000     C* EMETTE FORMATO VIDEO
017100000000     C                   EXSR      BUTFMR
017200000000     C*
017300000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
017400000000     C                   EXSR      FUNCON
017500000000     C*
017600000000     C                   END                                                    <-- 2 --
017700000000     C*
017800000000     C                   END                                                    <-- 1 --
017900000000     C*
018000000000     C                   SETON                                        LR
018100000000     C*------------------------------------------------------------------------*
018200000000     C* EMISSIONE FORMATO A VIDEO
018300000000     C*------------------------------------------------------------------------*
018400000000     C     BUTFMR        BEGSR
018500000000     C*
018600000000     C* WRITE RECORD TESTATA
018700061017     C                   WRITE     SP20D01
018800000000     C*
018900000000     C* VISUALIZZAZIONE MESSAGGIO
019000000000     C                   EXSR      SNDMSG
019100000000     C*
019200000000     C* WRITE RECORD FUNZIONI
019300061017     C                   WRITE     SP20D02
019400991019     C*
019500000417     C* EXFMT RECORD DETTAGLIO E DECODIFICA
019600000417     C                   EXSR      DECODI
019700061017     C                   EXFMT     SP20D03
019800991019     C*
019900000000     C* AZZERO FLAG DI ERRORE LIVELLO
020000000000     C                   MOVEL     '0'           ERRL00
020100000000     C*
020200000000     C* AZZERO FLAG DI ERRORE
020300061017     C                   SETOFF                                       40
020400000000     C*
020500000000     C                   ENDSR
020600000000     C*------------------------------------------------------------------------*
020700000000     C* SNDMSG - MANDA MESSAGGIO
020800000000     C*------------------------------------------------------------------------*
020900000000     C     SNDMSG        BEGSR
021000000000     C*
021100000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
021200061017     C                   MOVEL     '1'           *IN43
021300061017     C                   WRITE     SP20DMC
021400061017     C                   MOVEL     '0'           *IN43
021500000000     C*
021600000000     C* VISUALIZZAZIONE MESSAGGIO
021700000000     C                   Z-ADD     0             MSGULT            4 0
021800000000     C                   CALL      'XMSG'        XMSGPA
021900000000     C     DSMSER        IFEQ      0
022000000000     C                   MOVEL     DSMSKE        MSGKEY
022100000000     C                   END
022200000000     C                   Z-ADD     1             MSGULT
022300061017     C                   WRITE     SP20DMS                                      SFL MESS
022400061017     C                   WRITE     SP20DMC                                      CONTR MESS
022500000000     C*
022600000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
022700000000     C                   MOVEL     *BLANKS       DSMSMS
022800000000     C*
022900000000     C                   ENDSR
023000000000     C*------------------------------------------------------------------------*
023100000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
023200000000     C*------------------------------------------------------------------------*
023300000000     C     ENTGES        BEGSR
023400000000     C*
023500000000     C* CONTROLLA I CAMPI IMMESSI
023600000000     C                   EXSR      DECODI
023700000000     C*
023800000000     C                   ENDSR
023900000000     C*------------------------------------------------------------------------*
024000000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
024100000000     C*------------------------------------------------------------------------*
024200000000     C     F03GES        BEGSR
024300000000     C*
024400000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
024500000000     C                   MOVEL     '1'           F03L00
024600000000     C*
024700000000     C                   ENDSR
024800000000     C*------------------------------------------------------------------------*
024900061017     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
025000000000     C*------------------------------------------------------------------------*
025100991020     C     F06GES        BEGSR
025200991105     C*
025300991105     C                   EXSR      DECODI
025400991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
025500991105     C     ERRL00        IFEQ      '0'
025600991105     C*
025700000414     C* LANCIO LA ROUTINE DI RIEMPIMENTO ARCHIVI CAPPARIO POSTE DA BARTOLINI
025800061017     C                   EXSR      ELABORA
025900061023     C*
026000061023     C* Effettuo lancio TISI95 solo x chiusura
026100061023     C                   CLEAR                   TISI95DS
026200061023     C                   EVAL      I95TLA = 'C'
026300061023     C                   CALL      'TISI95R'
026400061023     C                   PARM                    TISI95DS
026500061023     C*
026600000605     C                   ENDIF
026700000605     C*
026800000000     C                   ENDSR
026900000000     C*------------------------------------------------------------------------*
027000000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
027100000000     C*------------------------------------------------------------------------*
027200000000     C     F12GES        BEGSR
027300000000     C*
027400000000     C* IMPOSTO FLAG DI LIVELLO
027500991103     C                   MOVEL     '1'           F12L00
027600000000     C*
027700000000     C                   ENDSR
027800000000     C*------------------------------------------------------------------------*
027900991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
028000000000     C*------------------------------------------------------------------------*
028100000000     C     F24GES        BEGSR
028200000000     C*
028300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
028400000000     C                   EXSR      FUNGES
028500000000     C*
028600000000     C                   ENDSR
028700000000     C*------------------------------------------------------------------------*
028800000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
028900000000     C*------------------------------------------------------------------------*
029000000000     C     HELGES        BEGSR
029100000000     C                   ENDSR
029200991020     C*------------------------------------------------------------------------*
029300061017     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
029400000000     C*------------------------------------------------------------------------*
029500000000     C     DECODI        BEGSR
029600000104     C*
029700000413     C* CONTROLLO VERSIONE CAPPARIO  BARTOLINI
029800000104     C     ERRL00        IFEQ      '0'
029900000104     C                   EXSR      CHKVER
030000000413     C                   ENDIF
030100000104     C*
030200000000     C                   ENDSR
030300000414     C*------------------------------------------------------------------------*
030400061017     C*  CHKVER - CONTROLLA PRESENZA O MENO DELLA VERSIONE BARTOLINI
030500000104     C*------------------------------------------------------------------------*
030600000104     C     CHKVER        BEGSR
030700000413     C*
030800000413     C                   IF        %trim(VIDVER)='?'
030900000413     C*
031000000413     C* lancia il programma di gestione versioni BAR in ricerca
031100000414     C                   CLEAR                   dssi00                         *pulisce ds
031200000414     C                   MOVEL     'R01'         d00op0                         *livello procedura
031300000414     C                   MOVEL     *blanks       d00op1                         *livello programma
031400000414     C                   MOVEL     '0'           d00f03                         *premuto F03
031500000414     C                   MOVEL     '0'           d00f12                         *premuto F12
031600000414     C                   MOVEL     '0'           d00err                         *errore
031700000414     C                   MOVEL     *blanks       d00msg                         *messaggio ritorno
031800000414     C                   CALL      'TISI46R'                                    *gestione versioni
031900000414     C                   PARM                    kpjba
032000000414     C                   PARM                    dssi00
032100000413     C* ritorno
032200000414     C                   IF        d00err='1'
032300000413     C                   SETON                                        81
032400000413     C                   MOVEL     MSG(5)        DSMSMS
032500000414     C                   ELSE
032600000414     C                   MOVE(p)   d00ver        VIDVER
032700061017     C                   Z-ADD     d00ver        VIHVER            5 0
032800000414     C                   ENDIF
032900000413     C*
033000000413     C                   ELSE
033100000414     C                   call(e)   'ISNUMERIC'
033200000414     C                   PARM      VIDVER        PiStr            30
033300000414     C                   PARM      ','           PiDecChr          1
033400000414     C                   PARM      *ZEROS        PiVal            30 9
033500000414     C                   PARM      '0'           PiInt             1
033600000414     C                   PARM      '0'           PiNum             1
033700061017     C                   IF        NOT %error AND
033800000414     C                             PiInt=*ON
033900000414     C                   Z-ADD     PiVal         VIHVER
034000061017     C     VIHVER        CHAIN     azcpr01l
034100061017     C                   IF        not %found(azcpr01l)
034200061017     C                   SETON                                        40
034300061017     C                   MOVEL     MSG(8)        DSMSMS
034400061017     C                   MOVEL     '1'           ERRL00
034500061023     C                   ELSE
034600061023     C                   Z-ADD     CPRDDE        datcor            8 0
034700061017     C                   ENDIF
034800000417     C                   ELSE
034900061017     C                   MOVEL     '1'           ERRL00
035000000414     C                   ENDIF
035100000413     C                   ENDIF
035200061017     C*
035300061017     C                   IF        ERRL00 <> '1'
035400061017     C                   Z-ADD     VIHVER        NumVER            5 0
035500061017     C                   Z-ADD     *zeros        NumPrecVER        5 0
035600061017     C                   EVAL      NumPrecVER = NumVER - 1
035700061017     C                   ENDIF
035800000105     C*
035900000104     C                   ENDSR
036000000000     C*------------------------------------------------------------------------*
036100061017     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
036200000000     C*------------------------------------------------------------------------*
036300000000     C     AZZVID        BEGSR
036400061018     C*
036500061018     C                   SETOFF                                       919293
036600061018     C                   SETOFF                                       949596
036700061120     C                   SETOFF                                       9798
036800000000     C*
036900061017     C                   MOVEL     *BLANKS       VIDINZ
037000061017     C                   MOVEL     *BLANKS       VIDIMP
037100061017     C                   MOVEL     *BLANKS       VIDCNOP
037200061017     C                   MOVEL     *BLANKS       VIDLNOP
037300061017     C                   MOVEL     *BLANKS       VIDANNS
037400061120     C                   MOVEL     *BLANKS       VIDANNS2
037500061017     C                   MOVEL     *BLANKS       VIDOBSC
037600061023     C                   MOVEL     *BLANKS       VIDSI95
037700061018     C                   MOVEL     *BLANKS       VID01
037800061018     C                   MOVEL     *BLANKS       VID02
037900061018     C                   MOVEL     *BLANKS       VID03
038000061018     C                   MOVEL     *BLANKS       VID04
038100061018     C                   MOVEL     *BLANKS       VID05
038200061018     C                   MOVEL     *BLANKS       VID06
038300061018     C                   MOVEL     *BLANKS       VID07
038400061120     C                   MOVEL     *BLANKS       VID08
038500061017     C*
038600000104     C                   ENDSR
038700000414     C*------------------------------------------------------------------------*
038800061017     C* ELABORA - ROUTINE DI GENERAZIONE ARCHIVI CAPPARIO POSTE DA BARTOLINI
038900000414     C*------------------------------------------------------------------------*
039000061017     C     ELABORA       BEGSR
039100061018     C*
039200061120     C                   IF        VIDINZ  ='X'
039300061120     C                   EVAL      VID01   = 'Da elaborare'
039400061018     C                   ENDIF
039500061120     C                   IF        VIDIMP  ='X'
039600061120     C                   EVAL      VID02   = 'Da elaborare'
039700061018     C                   ENDIF
039800061120     C                   IF        VIDCNOP ='X'
039900061120     C                   EVAL      VID03   = 'Da elaborare'
040000061018     C                   ENDIF
040100061120     C                   IF        VIDLNOP ='X'
040200061120     C                   EVAL      VID04   = 'Da elaborare'
040300061018     C                   ENDIF
040400061120     C                   IF        VIDSI95 <>*blanks
040500061120     C                   EVAL      VID05   = 'Da elaborare'
040600061023     C                   ENDIF
040700061120     C                   IF        VIDOBSC ='X'
040800061120     C                   EVAL      VID06   = 'Da elaborare'
040900061018     C                   ENDIF
041000061120     C                   IF        VIDANNS ='X'
041100061120     C                   EVAL      VID07   = 'Da elaborare'
041200061023     C                   ENDIF
041300061120     C                   IF        VIDANNS2='X'
041400061120     C                   EVAL      VID08   = 'Da elaborare'
041500061120     C                   ENDIF
041600061018     C                   WRITE     SP20D03
041700000606     C*
041800061017     C* => Inizializza versioe da file d wrk
041900061017     C                   IF        VIDINZ='X'
042000061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
042100061018     C                   EXSR      SNDMSG
042200061017     C                   exsr      inzpt
042300000606     C                   ENDIF
042400000606     C*
042500061017     C* => Importa cappario Poste
042600061017     C                   IF        VIDIMP='X'
042700061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
042800061018     C                   EXSR      SNDMSG
042900061024     C                   exsr      updFCPU
043000061017     C                   exsr      execap                                       Elaborazione cap PT
043100061017     C                   exsr      exeloc                                       Elaborazione loc PT
043200000606     C                   ENDIF
043300000605     C*
043400061017     C* => Obsoleta CAP non in CAP Poste
043500061017     C                   IF        VIDCNOP='X'
043600061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
043700061018     C                   EXSR      SNDMSG
043800061017     C                   EXSR      exever1
043900000605     C                   ENDIF
044000061017     C*
044100061017     C* => Obsoleta LOC non in LOC Poste
044200061017     C                   IF        VIDLNOP='X'
044300061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
044400061018     C                   EXSR      SNDMSG
044500061017     C                   EXSR      exever2
044600061017     C                   ENDIF
044700061023     C*
044800061023     C* => Verifica località "?" tramite TISI95R
044900061023     C                   IF        VIDSI95<>*blanks
045000061023     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
045100061023     C                   EXSR      SNDMSG
045200061023     C                   EXSR      exever5
045300061023     C                   ENDIF
045400061023     C*
045500061023     C* => Obsoleta CAP con tutte LOC obsolete
045600061023     C                   IF        VIDOBSC='X'
045700061023     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
045800061023     C                   EXSR      SNDMSG
045900061023     C                   EXSR      exever4
046000061023     C                   ENDIF
046100061023     C*
046200061023     C* => Annulla SIN già LOC (x stesso CAP)
046300061023     C                   IF        VIDANNS='X'
046400061023     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
046500061023     C                   EXSR      SNDMSG
046600061023     C                   EXSR      exever3
046700061123     C*******
046800061123     C* Lasciare exever6 asteriscato! Usato solo la 1° volta x correggere 1 errore.
046900061123     C* Ora correzzione è già nel giro standard
047000061123     C***                EXSR      exever6    (OK asteriscato - usato solo 1° volta - )
047100061123     C*******
047200061023     C                   ENDIF
047300061120     C*
047400061120     C* => Annulla SIN già SIN (x stesso CAP)
047500061120     C                   IF        VIDANNS2='X'
047600061120     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
047700061120     C                   EXSR      SNDMSG
047800061122     C                   EXSR      exever8
047900061120     C                   ENDIF
048000000414     C*
048100000414     C                   ENDSR
048200020917     C*--------------------------------------------------------------------------------------------*
048300020917     C* REPERISCE I DATI UTENTE
048400020917     C*--------------------------------------------------------------------------------------------*
048500020917     C     REPDATIUTE    BEGSR
048600020917     C*
048700020917     C* INIZIALIZZA VARIABILI DI WRK
048800020917     C                   CLEAR                   TIBS34DS
048900020917     C                   CLEAR                   AZUTEDS
049000020917     C                   CLEAR                   DDATIUTE
049100020917     C*
049200020917     C     *DTAARA       DEFINE    §azute        azuteds
049300020917     C     *DTAARA       DEFINE    §datiute      ddatiute
049400020917     C                   IN(E)     *DTAARA
049500061017     C                   IF        %Error
049600020917     c                   EVAL      I34Tla = 'L'
049700020917     C                   CALL      'TIBS34R'
049800020917     C                   PARM                    Tibs34Ds
049900020917     C                   IN        *DTAARA
050000061017     C                   ENDIF
050100020917     C*
050200020917     C                   ENDSR
050300061017     C*--------------------------------------------------------------------------------------------*
050400061017     C* execap - elaborazione cap
050500061017     C*--------------------------------------------------------------------------------------------*
050600061017     C     execap        BEGSR
050700061017     C*
050800061017     C* Scorro tutto il file cap poste
050900061017     C     *start        setll     cap_cap
051000061017     C                   read      cap_cap
051100061017     C                   dow       not %eof(cap_cap)
051200061017     C*
051300061017     C                   setoff                                       50
051400061017     C*
051500061017     C* Verifico se trattasi d cap già esistente
051600061017     C                   eval      K_cpcVER = NumVER
051700061017     C                   eval      K_cpcNAR = *blanks
051800061017     C                   eval      K_cpcCAP = c_cap
051900061017     C     KEYcpc01_C    chain     azcpc01l
052000061017     C                   if        %found(azcpc01l)
052100061024     C*
052200061024     C* Se provincia altrenativa già valorizzata e provincia cap PT corrente diversa da entrambe (prv e pra) => listo
052300061024     C                   if        c_prv  <> cpcPRV  AND
052400061024     C                             c_prv  <> cpcPRA  AND
052500061024     C                             c_prv  <> *blanks AND
052600061024     C                             cpcPRV <> *blanks AND
052700061024     C                             cpcPRA <> *blanks
052800061024     C                   eval      wRiga = cpcCAP + '  '    +
052900061024     C                             '=> tentativo scrittura 3 provincie ' +
053000061024     C                             '('+cpcPRV+' '+cpcPRA+' '+c_prv+')'
053100061024     C                   except    rigaErr
053200061024     C                   else
053300061017     C*
053400061017     C* Se cap già esistente verifico se è rimasto sulla stessa provincia
053500061017     C                   if        c_prv = *blanks OR
053600061017     C                             c_prv = cpcPRV  OR
053700061017     C                             c_prv = cpcPRA
053800061017     C                   seton                                        50
053900061017     C                   else
054000061017     C*
054100061017     C* Se provincia altrenativa già valorizzata => listo il cap x successiva verifica manuale
054200061024     C                   if        cpcPRA <> *blanks and cpcFTR <> *blanks
054300061017     C                   eval      wRiga = cpcCAP + '  '    +
054400061017     C                             '=> provincia alternativa sovrascritta ' +
054500061017     C                             '(' + cpcPRA + ')'
054600061017     C                   except    rigaErr
054700061017     C                   endif
054800061017     C*
054900061017     C* Quindi imposto la "vecchia" provincia come provincia alternativa ed imposto come provincia
055000061017     C* quella "nuova" PT
055100061017     C                   eval      cpcPRA = cpcPRV
055200061017     C                   eval      cpcPRV = c_prv
055300061017     C                   eval      cpcFTR = *blanks
055400061017     C                   update(e) azcpc000
055500061017     C                   seton                                        50
055600061017     C                   endif
055700061024     C*
055800061024     C                   endif
055900061017     C*
056000061017     C                   else
056100061017     C*
056200061017     C* Se cap inesistente flaggo apposito campo FLG2 su file cap PT
056300061017     C                   eval      c_FLG2 = 'N'
056400061017     C                   update    cap000
056500061017     C                   eval      wRiga = c_prv + '  '    +
056600061017     C                                     c_cap + '  '    +
056700061017     C                             '=> CAP inesistente!!!'
056800061017     C                   except    rigaErr
056900061017     C                   endif
057000061017     C*
057100061017     C* Se cap PT è stato in qualche modo trattato flaggo record su file PT
057200061017     C                   if        *in50 = *on
057300061017     C                   eval      c_FLG1 = 'S'
057400061017     C                   update(e) cap000
057500061017     C                   endif
057600061017     C*
057700061017     C* Proseguo con la lettura dei cap PT
057800061017     C                   read      cap_cap
057900061017     C                   enddo
058000061017     C*
058100061017     C                   ENDSR
058200061017     C*--------------------------------------------------------------------------------------------*
058300061017     C* exeloc - elaborazione località
058400061017     C*--------------------------------------------------------------------------------------------*
058500061017     C     exeloc        BEGSR
058600061017     C*
058700061017     C* "Normalizzo" le sintassi delle località BART
058800061017     C                   exsr      normloc
058900061017     C*
059000061017     C* Scorro tutto il file località poste
059100061017     C     *start        setll     cap_loc
059200061017     C                   read      cap_loc
059300061017     C                   dow       not %eof(cap_loc)
059400061017     C*
059500061017     C                   setoff                                       51
059600061017     C                   clear                   wfdcpl00
059700061023     C                   clear                   wCPLLIV
059800061017     C*
059900061017     C* 1° => se cap+loc PT trovati esattamente in file località BART (azcpl)
060000061017     C                   eval      K_cplVER = NumVER
060100061017     C                   eval      K_cplNAR = *blanks
060200061017     C                   eval      K_cplCAP = l_cap
060300061017     C                   eval      K_cplLOC = l_loc
060400061017     C     KEYcpl01_C    chain     azcpl01l
060500061017     C                   if        %found(azcpl01l)
060600061017     C* Se trovata esattamente => eseguo gestione sinonimi
060700061017     C                   exsr      exesin
060800061017     C                   seton                                        51
060900061023     C                   else
061000061023     C*
061100061023     C* Reperisco la provincia dal cap BART
061200061023     C                   eval      K_cpcVER = NumVER
061300061023     C                   eval      K_cpcNAR = *blanks
061400061023     C                   eval      K_cpcCAP = l_cap
061500061023     C     KEYcpc01_C    chain     azcpc01l
061600061023     C                   if        %found(azcpc01l)
061700061017     C*
061800061017     C* 2° => se cap+loc PT nn trovati esattamente in file località BART (azcpl)
061900061017     C*       verifico se la località è univoca nella provincia
062000061017     C                   exsr      rtvlocuniq
062100061017     C*
062200061023     C* Se località univoca nella provincia procedo con elaborazione particolare
062300061017     C                   if        uniq_cnt = 1
062400061017     C*
062500061017     C* Innanzitutto reperisce i dati della prv+loc corrente
062600061017     C                   exsr      rtvloc
062700061017     C*
062800061023     C* 1° obsoletizzo la località BART
062900061017     C                   eval      K_cplVER = loc_ver
063000061017     C                   eval      K_cplNAR = loc_nar
063100061017     C                   eval      K_cplCAP = loc_cap
063200061017     C                   eval      K_cplLOC = loc_loc
063300061017     C     KEYcpl01_C    chain     azcpl01l
063400061017     C                   if        %found(azcpl01l)
063500061023     C                   eval      wCPLLIV = cplLIV
063600061017     C                   movel     'O'           cplFI1
063700061017     C                   movel     *blanks       cplFTR
063800061017     C                   movel     cplESS        wCPLESS
063900061023     C***                movel     *blanks       cplESS
064000061017     C                   update    azcpl000
064100061017     C* Tengo traccia della modifica nel file "diff" - Lato "OLD"
064200061017     C                   eval      wCPCVER = NumPrecVER
064300061017     C                   eval      wCPCCAP = cplCAP
064400061017     C                   exsr      rtvcap
064500061017     C                   eval      diff_cplNAR    = *blanks
064600061017     C                   eval      diff_cplLOC    = cplLOC
064700061017     C                   eval      diff_cpcPRVOLD = cpcPRV
064800061017     C                   eval      diff_cpcPRAOLD = cpcPRA
064900061017     C                   eval      diff_cplCAPOLD = cplCAP
065000061017     C*
065100061017     C                   endif
065200061017     C*
065300061017     C* 2° inserisco nuova loc+cap PT
065400061023     C*
065500061024     C* Se località "oldizzata" era a livello "C" => reperisco i dati d instradamento dal suo cap relativo
065600061024     C                   if        wCPLLIV = 'C'
065700061024     C                   eval      wCPCVER = loc_ver
065800061024     C                   eval      wCPCCAP = loc_cap
065900061024     C                   exsr      cpctocpl
066000061024     C                   endif
066100061023     C*
066200061017     C                   movel     *blanks       cplATB
066300061017     C                   movel     *blanks       cplFI1
066400061017     C                   movel     *blanks       cplFTR
066500061017     C                   movel     wCPLESS       cplESS
066600061023     C* Forzo sempre il nuovo inserimento a livello "L"
066700061023     C                   eval      cplLIV  = 'L'
066800061023     C*
066900061017     C                   eval      cplCAP  = l_cap
067000061017     C                   eval      cplLOC  = l_loc
067100061017     C                   eval      wCPCVER = NumVER
067200061017     C                   eval      wCPCCAP = l_cap
067300061017     C                   exsr      rtvcap
067400061017     C                   if        *in52 = *on
067500061017     C                   write(e)  azcpl000
067600061024     C* Sempre se località "obsoletizzata" gestita a livello "C"  aggiorno il flag sul cap della località corrente
067700061024     C* che indica la presenza d eccezioni a livello d località
067800061024     C                   if        wCPLLIV = 'C' and cpcESL <> 'S'
067900061024     C                   eval      cpcESL = 'S'
068000061024     C                   update(e) azcpc000
068100061024     C                   endif
068200061017     C* Tengo traccia della modifica nel file "diff" - Lato "NEW"
068300061017     C                   eval      diff_cplNAR    = *blanks
068400061017     C                   eval      diff_cplLOC    = l_loc
068500061017     C                   eval      diff_cpcPRVNEW = cpcPRV
068600061017     C                   eval      diff_cpcPRANEW = cpcPRA
068700061017     C                   eval      diff_cplCAPNEW = cplCAP
068800061017     C                   write(e)  wfdcpl00
068900061017     C                   endif
069000061017     C*
069100061017     C* 3° porto i sinonimi della "vecchia" località sulla nuova località
069200061017     C                   eval      K_cpsVER = loc_ver
069300061017     C                   eval      K_cpsNAR = loc_nar
069400061017     C                   eval      K_cpsCAP = loc_cap
069500061017     C                   eval      K_cpsLOC = loc_loc
069600061017     C                   eval      wCPSCAP  = l_cap
069700061017     C                   eval      wCPSLOC  = l_loc
069800061023     C***                exsr      movsin
069900061023     C                   exsr      dupsin
070000061017     C*
070100061017     C* 4° inserisco i sinonimi relativi
070200061017     C                   exsr      exesin
070300061017     C*
070400061017     C                   seton                                        51
070500061017     C*
070600061017     C                   else
070700061017     C*
070800061017     C* Se località nn univoca nella provincia la inserisco a livello d cap
070900061017     C                   if        uniq_cnt > 1
071000061017     C                   exsr      insloc
071100061017     C* ...inserisco anche i sinonimi relativi
071200061017     C                   exsr      exesin
071300061017     C*                  eval      wRiga = l_prv + '  '    +
071400061017     C*                                    l_cap + '  '    +
071500061017     C*                                    l_loc + '     ' +
071600061017     C*                            '=> Località nn univoca nella provincia!'
071700061017     C*                  except    rigaErr
071800061017     C                   endif
071900061017     C*
072000061017     C* Se località nn trovata nella provincia la inserisco a livello d cap
072100061017     C                   if        uniq_cnt = 0
072200061017     C                   exsr      insloc
072300061017     C* ...inserisco anche i sinonimi relativi
072400061017     C                   exsr      exesin
072500061017     C*
072600061017     C*                  eval      wRiga = l_prv + '  '    +
072700061017     C*                                    l_cap + '  '    +
072800061017     C*                                    l_loc + '     ' +
072900061017     C*                            '=> Località nn trovata nella provincia!'
073000061017     C*                  except    rigaErr
073100061017     C                   endif
073200061017     C                   endif
073300061024     C*
073400061024     C                   else
073500061024     C                   eval      l_FLG2 = 'N'
073600061024     C                   update    loc000
073700061024     C                   eval      wRiga = l_prv + '  '    +
073800061024     C                                     l_cap + '  '    +
073900061024     C                                     l_loc + '     ' +
074000061024     C                             '=> Località su cap inesistente!'
074100061024     C                   except    rigaErr
074200061024     C                   endif
074300061017     C                   endif
074400061017     C*
074500061017     C* Se località PT è stata in qualche modo trattata flaggo record su file PT
074600061017     C                   if        *in51 = *on
074700061017     C                   eval      l_FLG1 = 'S'
074800061017     C                   update(e) loc000
074900061017     C                   endif
075000061017     C*
075100061017     C* Proseguo con la lettura delle località PT
075200061017     C                   read      cap_loc
075300061017     C                   enddo
075400061018     C*
075500061018     C                   EVAL      VID02 = 'Fatto'
075600061018     C                   SETOFF                                       92
075700061018     C                   WRITE     SP20D03
075800061017     C*
075900061017     C                   ENDSR
076000061017     C*--------------------------------------------------------------------------------------------*
076100061017     C* insloc - inserimento nuova loc BART da loc PT
076200061017     C*--------------------------------------------------------------------------------------------*
076300061017     C     insloc        BEGSR
076400061017     C*
076500061017     C                   clear                   azcpl000
076600061017     C                   movel     *blanks       cplATB
076700061017     C                   movel     *blanks       cplFI1
076800061017     C                   movel     *blanks       cplFTR
076900061017     C                   eval      cplVER = NumVER
077000061017     C                   eval      cplNAR = *blanks
077100061017     C                   eval      cplCAP = l_cap
077200061017     C                   eval      cplLOC = l_loc
077300061017     C                   eval      cplLIV = 'C'
077400061017     C                   eval      cplDLV = 'N'
077500061017     C                   eval      cplESS = 'S'
077600061017     C                   eval      wCPCVER = NumVER
077700061017     C                   eval      wCPCCAP = l_cap
077800061017     C                   exsr      rtvcap
077900061017     C                   if        *in52 = *on
078000061017     C                   write(e)  azcpl000
078100061017     C                   else
078200061017     C                   eval      l_FLG2 = 'N'
078300061017     C                   update    loc000
078400061017     C                   eval      wRiga = l_prv + '  '    +
078500061017     C                                     l_cap + '  '    +
078600061017     C                                     l_loc + '     ' +
078700061017     C                             '=> Località su cap inesistente!'
078800061017     C                   except    rigaErr
078900061017     C                   endif
079000061017     C*
079100061017     C                   seton                                        51
079200061017     C*
079300061017     C                   ENDSR
079400061017     C*--------------------------------------------------------------------------------------------*
079500061017     C* exesin - gestione sinonimi
079600061017     C*--------------------------------------------------------------------------------------------*
079700061017     C     exesin        BEGSR
079800061017     C*
079900061017     C*
080000061017     C/EXEC SQL
080100061017     C+ declare C1 cursor for
080200061017     C+ select * from cap_sin
080300061017     C+ where PRV = :l_prv and CAP = :l_cap and LOC = :l_loc
080400061017     C+ order by PRV, CAP, LOC, SIN
080500061017     C+ for update of FLG1
080600061017     C/END-EXEC
080700061017     C
080800061017     C/EXEC SQL
080900061017     C+ open C1
081000061017     C/END-EXEC
081100061017     C
081200061017     C/EXEC SQL
081300061017     C+ Fetch next from C1 into :ds_cap_sin
081400061017     C/END-EXEC
081500061017     C*
081600061017     C* Se reperito qualcosa => analizzo
081700061017     C                   dow       sqlcod = *zeros
081800061017     C                   clear                   azcps000
081900061017     C                   eval      cpsVER = NumVER
082000061017     C                   eval      cpsNAR = *blanks
082100061017     C                   eval      cpsCAP = s_cap
082200061017     C                   eval      cpsLOC = s_loc
082300061017     C                   eval      cpsSIN = s_sin
082400061017     C                   eval      cpsFTR = *blanks
082500061017     C                   write(e)  azcps000
082600061017     C
082700061017     C/EXEC SQL
082800061017     C+     update cap_sin
082900061017     C+     set    FLG1 = 'S'
083000061017     C+     where  CURRENT  OF  C1
083100061017     C/END-EXEC
083200061017     C*
083300061017     C*
083400061017     C/EXEC SQL
083500061017     C+ Fetch next from C1 into :ds_cap_sin
083600061017     C/END-EXEC
083700061017     C*
083800061017     C                   enddo
083900061017     C*
084000061017     C/EXEC SQL
084100061017     C+ close C1
084200061017     C/END-EXEC
084300061017     C*
084400061017     C*
084500061017     C                   ENDSR
084600061017     C*--------------------------------------------------------------------------------------------*
084700061017     C* rtvlocuniq - reperimento univocità località x provincia
084800061017     C*--------------------------------------------------------------------------------------------*
084900061017     C     rtvlocuniq    BEGSR
085000061017     C*
085100061017     C/EXEC SQL
085200061017     C+ declare C2 cursor for
085300061024     C+ select cplloc, cpcprv, cpcpra, count(*)
085400061024     C+ from azcpc00f join azcpl00f on
085500061024     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
085600061024     C+ where cpcver = :NumVER and cpcnar = ' '
085700061024     C+ and (cpcprv = :cpcPRV or
085800061024     C+ (cpcprv = :cpcPRA and cpcprv<>'  ') or
085900061024     C+ cpcpra = :cpcPRV or
086000061024     C+ (cpcpra = :cpcPRA and cpcpra <> '  '))
086100061024     C+ and cplloc = :l_loc
086200061024     C+ group by cplloc, cpcprv, cpcpra
086300061017     C+ for read only
086400061017     C/END-EXEC
086500061017     C
086600061017     C/EXEC SQL
086700061017     C+ open C2
086800061017     C/END-EXEC
086900061017     C
087000061017     C/EXEC SQL
087100061017     C+ Fetch next from C2 into :ds_loc_uniq
087200061017     C/END-EXEC
087300061017     C*
087400061017     C* Se contatore > 1 => località nn univoca nella provincia
087500061017     C                   if        sqlcod = *zeros
087600061017     C                   else
087700061017     C                   eval      uniq_cnt = 0
087800061017     C                   endif
087900061017     C*
088000061017     C/EXEC SQL
088100061017     C+ close C2
088200061017     C/END-EXEC
088300061017     C*
088400061017     C*
088500061017     C                   ENDSR
088600061017     C*--------------------------------------------------------------------------------------------*
088700061017     C* rtvloc - reperimento località
088800061017     C*--------------------------------------------------------------------------------------------*
088900061017     C     rtvloc        BEGSR
089000061017     C*
089100061017     C/EXEC SQL
089200061017     C+ declare C3 cursor for
089300061017     C+ select cplver, cplnar, cplcap, cplloc
089400061017     C+ from azcpc00f join azcpl00f on
089500061017     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
089600061017     C+ where cpcver = :NumVER and cpcnar = ' '
089700061023     C+ and cpcprv = :uniq_prv and cpcpra = :uniq_pra
089800061023     C+ and cplloc = :l_loc
089900061017     C+ for read only
090000061017     C/END-EXEC
090100061017     C
090200061017     C/EXEC SQL
090300061017     C+ open C3
090400061017     C/END-EXEC
090500061017     C
090600061017     C/EXEC SQL
090700061017     C+ Fetch next from C3 into :ds_loc
090800061017     C/END-EXEC
090900061017     C
091000061017     C/EXEC SQL
091100061017     C+ close C3
091200061017     C/END-EXEC
091300061017     C*
091400061017     C                   ENDSR
091500061024     C*--------------------------------------------------------------------------------------------*
091600061024     C* rtvlocnew - reperimento nuova località (appena inserita da PT)
091700061024     C*--------------------------------------------------------------------------------------------*
091800061024     C     rtvlocnew     BEGSR
091900061024     C*
092000061024     C/EXEC SQL
092100061024     C+ declare C10 cursor for
092200061024     C+ select cplver, cplnar, cplcap, cplloc
092300061024     C+ from azcpc00f join azcpl00f on
092400061024     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
092500061024     C+ where cpcver = :NumVER and cpcnar = ' '
092600061024     C+ and cpcprv = :prv_prv and cpcpra = :prv_pra
092700061024     C+ and cplloc = :l2_cplLOC and cplfi1 = '  ' and cplftr = ' '
092800061024     C+ for read only
092900061024     C/END-EXEC
093000061024     C
093100061024     C/EXEC SQL
093200061024     C+ open C10
093300061024     C/END-EXEC
093400061024     C
093500061024     C/EXEC SQL
093600061024     C+ Fetch next from C10 into :ds_loc
093700061024     C/END-EXEC
093800061024     C
093900061024     C/EXEC SQL
094000061024     C+ close C10
094100061024     C/END-EXEC
094200061024     C*
094300061024     C                   ENDSR
094400061024     C*--------------------------------------------------------------------------------------------*
094500061024     C* rtvprv - reperimento provincia da cap + loc
094600061024     C*--------------------------------------------------------------------------------------------*
094700061024     C     rtvprv        BEGSR
094800061024     C*
094900061024     C/EXEC SQL
095000061024     C+ declare C9 cursor for
095100061024     C+ select cpcprv, cpcpra
095200061024     C+ from azcpc00f
095300061024     C+ where cpcver = :NumVER and cpcnar = ' '
095400061024     C+ and cpccap = :l2_cplCAP
095500061024     C+ for read only
095600061024     C/END-EXEC
095700061024     C
095800061024     C/EXEC SQL
095900061024     C+ open C9
096000061024     C/END-EXEC
096100061024     C
096200061024     C/EXEC SQL
096300061024     C+ Fetch next from C9 into :ds_prv
096400061024     C/END-EXEC
096500061024     C
096600061024     C/EXEC SQL
096700061024     C+ close C9
096800061024     C/END-EXEC
096900061024     C*
097000061024     C                   ENDSR
097100061017     C*--------------------------------------------------------------------------------------------*
097200061017     C* rtvcap - reperimento cap
097300061017     C*--------------------------------------------------------------------------------------------*
097400061017     C     rtvcap        BEGSR
097500061017     C*
097600061017     C                   setoff                                       52
097700061017     C*
097800061017     C* Verifico se trattasi d cap già esistente
097900061017     C                   eval      K_cpcVER = wCPCVER
098000061017     C                   eval      K_cpcNAR = *blanks
098100061017     C                   eval      K_cpcCAP = wCPCCAP
098200061017     C     KEYcpc01_C    chain     azcpc01l
098300061017     C                   if        %found(azcpc01l)
098400061017     C                   seton                                        52
098500061017     C                   endif
098600061017     C*
098700061017     C                   ENDSR
098800061017     C*--------------------------------------------------------------------------------------------*
098900061017     C* rtvcappt - reperimento cap PT
099000061017     C*--------------------------------------------------------------------------------------------*
099100061017     C     rtvcappt      BEGSR
099200061017     C*
099300061018     C                   setoff                                       54
099400061017     C*
099500061017     C/EXEC SQL
099600061017     C+ declare C4 cursor for
099700061017     C+ select * from cap_cap
099800061018     C+ where cap = :cpcCAP
099900061017     C+ for read only
100000061017     C/END-EXEC
100100061017     C
100200061017     C/EXEC SQL
100300061017     C+ open C4
100400061017     C/END-EXEC
100500061017     C
100600061017     C/EXEC SQL
100700061017     C+ Fetch next from C4 into :ds_cap_pt
100800061017     C/END-EXEC
100900061017     C*
101000061018     C* Se nn trovato => segnalo con indicatore 54
101100061017     C                   if        sqlcod <> *zeros
101200061018     C                   seton                                        54
101300061017     C                   endif
101400061017     C
101500061017     C/EXEC SQL
101600061017     C+ close C4
101700061017     C/END-EXEC
101800061017     C*
101900061017     C                   ENDSR
102000061017     C*--------------------------------------------------------------------------------------------*
102100061017     C* rtvlocpt - reperimento località PT
102200061017     C*--------------------------------------------------------------------------------------------*
102300061017     C     rtvlocpt      BEGSR
102400061017     C*
102500061017     C                   setoff                                       53
102600061017     C*
102700061017     C/EXEC SQL
102800061017     C+ declare C5 cursor for
102900061017     C+ select * from cap_loc
103000061024     C+ where cap = :cplCAP and substr(loc, 1, 35) = :cplLOC
103100061017     C+ for read only
103200061017     C/END-EXEC
103300061017     C
103400061017     C/EXEC SQL
103500061017     C+ open C5
103600061017     C/END-EXEC
103700061017     C
103800061017     C/EXEC SQL
103900061017     C+ Fetch next from C5 into :ds_loc_pt
104000061017     C/END-EXEC
104100061017     C*
104200061017     C* Se nn trovato => segnalo con indicatore 53
104300061017     C                   if        sqlcod <> *zeros
104400061017     C                   seton                                        53
104500061017     C                   endif
104600061017     C
104700061017     C/EXEC SQL
104800061017     C+ close C5
104900061017     C/END-EXEC
105000061017     C*
105100061017     C                   ENDSR
105200061017     C*--------------------------------------------------------------------------------------------*
105300061017     C* rtvcompt - reperimento comune PT
105400061017     C*--------------------------------------------------------------------------------------------*
105500061017     C     rtvcompt      BEGSR
105600061017     C*
105700061017     C                   Z-ADD     *zeros        com_cnt           9 0
105800061017     C*
105900061017     C/EXEC SQL
106000061017     C+ declare C6 cursor for
106100061017     C+ select com from cap_loc
106200061017     C+ where cap = :rainer_cap and loc = com
106300061017     C+ group by com
106400061017     C+ for read only
106500061017     C/END-EXEC
106600061017     C
106700061017     C/EXEC SQL
106800061017     C+ open C6
106900061017     C/END-EXEC
107000061017     C
107100061017     C/EXEC SQL
107200061017     C+ Fetch next from C6 into :ds_com_pt
107300061017     C/END-EXEC
107400061017     C*
107500061017     C* Se reperito qualcosa => analizzo e conto
107600061017     C                   dow       sqlcod = *zeros
107700061017     C                   add       1             com_cnt
107800061017     C*
107900061017     C/EXEC SQL
108000061017     C+ Fetch next from C6 into :ds_com_pt
108100061017     C/END-EXEC
108200061017     C*
108300061017     C                   enddo
108400061017     C*
108500061017     C/EXEC SQL
108600061017     C+ close C6
108700061017     C/END-EXEC
108800061017     C*
108900061017     C                   ENDSR
109000061017     C*--------------------------------------------------------------------------------------------*
109100061017     C* rtvlocrainer - reperimento località x provincia su file "Raineri"
109200061017     C*--------------------------------------------------------------------------------------------*
109300061017     C     rtvlocrainer  BEGSR
109400061017     C*
109500061017     C/EXEC SQL
109600061017     C+ declare C7 cursor for
109700061017     C+ select loc, min(cap), count(*)
109800061017     C+ from cap_rainer
109900061017     C+ where (prv = :cpcPRV or prv = :cpcPRA)
110000061024     C+ and substr(loc, 1, 35) = :cplLOC
110100061017     C+ group by loc
110200061017     C+ for read only
110300061017     C/END-EXEC
110400061017     C
110500061017     C/EXEC SQL
110600061017     C+ open C7
110700061017     C/END-EXEC
110800061017     C
110900061017     C/EXEC SQL
111000061017     C+ Fetch next from C7 into :ds_loc_rainer
111100061017     C/END-EXEC
111200061017     C*
111300061017     C* Se contatore > 1 => località nn univoca nella provincia
111400061017     C                   if        sqlcod = *zeros
111500061017     C                   else
111600061017     C                   eval      rainer_cnt = 0
111700061017     C                   endif
111800061017     C*
111900061017     C/EXEC SQL
112000061017     C+ close C7
112100061017     C/END-EXEC
112200061017     C*
112300061017     C*
112400061017     C                   ENDSR
112500061017     C*--------------------------------------------------------------------------------------------*
112600061017     C* movsin - spostamento sinonimi da "vecchia" località a "nuova" località
112700061017     C*--------------------------------------------------------------------------------------------*
112800061017     C     movsin        BEGSR
112900061017     C*
113000061017     C     KEYcps01_P    setll     azcps01l
113100061017     C                   if        %equal(azcps01l)
113200061017     C     KEYcps01_P    reade     azcps01l
113300061017     C                   dow       not %eof(azcps01l)
113400061017     C                   eval      cpsCAP = wCPSCAP
113500061017     C                   eval      cpsLOC = wCPSLOC
113600061017     C                   eval      cpsFTR = *blanks
113700061023     C* Solo se sinonimo corrente diverso da località sinonimata
113800061023     C                   if        cpsSIN <> cpsLOC
113900061017     C                   update(e) azcps000
114000061023     C                   endif
114100061017     C     KEYcps01_P    reade     azcps01l
114200061017     C                   enddo
114300061017     C                   endif
114400061017     C*
114500061017     C                   ENDSR
114600061017     C*--------------------------------------------------------------------------------------------*
114700061017     C* dupsin - copia sinonimi da "vecchia" località a "nuova" località
114800061017     C*--------------------------------------------------------------------------------------------*
114900061017     C     dupsin        BEGSR
115000061017     C*
115100061017     C     KEYcps01_P    setll     azcps01l
115200061017     C                   if        %equal(azcps01l)
115300061017     C     KEYcps01_P    reade     azcps01l
115400061017     C                   dow       not %eof(azcps01l)
115500061017     C                   eval      cpsCAP = wCPSCAP
115600061017     C                   eval      cpsLOC = wCPSLOC
115700061017     C                   eval      cpsFTR = *blanks
115800061023     C* Solo se sinonimo corrente diverso da località sinonimata
115900061023     C                   if        cpsSIN <> cpsLOC
116000061017     C                   write(e)  azcps000
116100061023     C                   endif
116200061017     C     KEYcps01_P    reade     azcps01l
116300061017     C                   enddo
116400061017     C                   endif
116500061017     C*
116600061017     C                   ENDSR
116700061017     C*--------------------------------------------------------------------------------------------*
116800061017     C* normloc - "normalizzazione" sintassi località BART
116900061017     C*--------------------------------------------------------------------------------------------*
117000061017     C     normloc       BEGSR
117100061017     C*
117200061017     C     NumVER        setll     azcpl01l
117300061017     C                   if        %equal(azcpl01l)
117400061017     C     NumVER        reade     azcpl01l
117500061017     C                   dow       not %eof(azcpl01l)
117600061017     C*
117700061017     C* Verifico presenza caratteri da eliminare
117800061017     C                   setoff                                       45
117900061017     C                   eval      wCPLLOC = cplLOC
118000061017     C                   if        %scan('''':cplLOC) > *zeros
118100061017     C     '''':' '      xlate     cplLOC        cplLOC
118200061017     C                   seton                                        45
118300061017     C                   endif
118400061017     C*
118500061017     C                   if        %scan('"':cplLOC)  > *zeros
118600061017     C     '"':' '       xlate     cplLOC        cplLOC
118700061017     C                   seton                                        45
118800061017     C                   endif
118900061017     C*
119000061017     C                   if        %scan('.':cplLOC)  > *zeros
119100061017     C     '.':' '       xlate     cplLOC        cplLOC
119200061017     C                   seton                                        45
119300061017     C                   endif
119400061017     C*
119500061017     C                   if        %scan('-':cplLOC)  > *zeros
119600061017     C     '-':' '       xlate     cplLOC        cplLOC
119700061017     C                   seton                                        45
119800061017     C                   endif
119900061017     C*
120000061017     C* Se sono state effettuate sostituzioni => gestisco sia località che sinonimi relativi
120100061017     C                   if        *in45 = *on
120200061017     C*
120300061017     C* Compatto gli eventuali spazi ripetuti consecutivamente
120400061017     C                   eval      cplLOC = %trim(cplLOC)
120500061017     C                   z-add     1             wPos              2 0
120600061017     C                   eval      wPos = %scan('  ':cplLOC:wPos)
120700061017     C                   dow       wPos > *zeros AND wPos < %len(%trim(cplLOC))
120800061017     C                   eval      cplLOC = %subst(cplLOC:1:wPos)+
120900061017     C                                      %subst(cplLOC:wPos+2)
121000061017     C                   eval      wPos = %scan('  ':cplLOC:wPos+1)
121100061017     C                   enddo
121200061017     C*
121300061017     C* 1° Sostituisco la località "normalizzata" a quella originale
121400061017     C                   eval      cplFTR = *blanks
121500061017     C                   update(e) azcpl000
121600061017     C*
121700061017     C* 2° Inserisco come sinonimo della località "normalizzata" la località originale
121800061017     C                   clear                   azcps000
121900061017     C                   eval      cpsVER = cplVER
122000061017     C                   eval      cpsNAR = cplNAR
122100061017     C                   eval      cpsCAP = cplCAP
122200061017     C                   eval      cpsLOC = cplLOC
122300061017     C                   eval      cpsSIN = wCPLLOC
122400061017     C                   eval      cpsFTR = *blanks
122500061017     C                   write(e)  azcps000
122600061017     C*
122700061017     C* 3° Sposto i sinonimi della località originale su quella "normalizzata"
122800061017     C                   eval      K_cpsVER = cplVER
122900061017     C                   eval      K_cpsNAR = cplNAR
123000061017     C                   eval      K_cpsCAP = cplCAP
123100061017     C                   eval      K_cpsLOC = wCPLLOC
123200061017     C                   eval      wCPSCAP  = cplCAP
123300061017     C                   eval      wCPSLOC  = cplLOC
123400061018     C                   exsr      movsin
123500061017     C*
123600061017     C                   endif
123700061017     C*
123800061017     C* Proseguo con la lettura delle località BART
123900061017     C     NumVER        reade     azcpl01l
124000061017     C                   enddo
124100061017     C                   endif
124200061017     C*
124300061017     C                   ENDSR
124400061017     C*--------------------------------------------------------------------------------------------*
124500061017     C* exever1 - Esecuzione 1° verifica: cap BART no in cap PT => obsoletare cap BART
124600061017     C*--------------------------------------------------------------------------------------------*
124700061017     C     exever1       BEGSR
124800061018     C*
124900061018     C                   EVAL      VID03 = 'In elaborazione'
125000061018     C                   SETON                                        93
125100061018     C                   WRITE     SP20D03
125200061017     C*
125300061017     C* Scorro tutto il file dei cap BART
125400061018     C                   eval      K_cpcVER = NumVER
125500061018     C                   eval      K_cpcNAR = *blanks
125600061017     C     *start        setll     azcpc01l
125700061018     C     KEYcpc01_P    setll     azcpc01l
125800061018     C     KEYcpc01_P    reade     azcpc01l
125900061017     C                   dow       not %eof(azcpc01l)
126000061017     C*
126100061017     C* Se cap nn già obsoleto effettuo le verifiche
126200061018     C                   if        cpcFI1 = *blanks           AND
126300061018     C                             %subst(cpcCAP:4:2) <> '00' AND
126400061018     C                             %subst(cpcCAP:4:2) <> '  '
126500061017     C*
126600061017     C* Verifico se cap BART è in file cap PT
126700061018     C                   exsr      rtvcappt
126800061017     C*
126900061017     C* Se nn trovata flaggo in maniera "evidente" cap corrente BART
127000061018     C                   if        *in54 = *on
127100061018     C                   eval      cpcFI1 = '?'
127200061017     C                   update(e) azcpc000
127300061017     C                   endif
127400061017     C*
127500061017     C                   endif
127600061017     C*
127700061017     C* Proseguo con la lettura dei cap BART
127800061018     C     KEYcpc01_P    reade     azcpc01l
127900061017     C                   enddo
128000061018     C*
128100061018     C                   EVAL      VID03 = 'Fatto'
128200061018     C                   SETOFF                                       93
128300061018     C                   WRITE     SP20D03
128400061017     C*
128500061017     C                   ENDSR
128600061017     C*--------------------------------------------------------------------------------------------*
128700061017     C* exever2 - Esecuzione 2° verifica: loc BART no in loc PT => obsoletare loc BART
128800061017     C*--------------------------------------------------------------------------------------------*
128900061017     C     exever2       BEGSR
129000061018     C*
129100061018     C                   EVAL      VID04 = 'In elaborazione'
129200061018     C                   SETON                                        94
129300061018     C                   WRITE     SP20D03
129400061017     C*
129500061017     C* Scorro tutto il file delle località BART
129600061017     C     *start        setll     azcpl01l
129700061018     C     NumVER        setll     azcpl01l
129800061017     C     NumVER        reade     azcpl01l
129900061017     C                   dow       not %eof(azcpl01l)
130000061017     C*
130100061017     C                   setoff                                       60
130200061017     C                   setoff                                       61
130300061017     C*
130400061017     C* Se impostato flag "esistenza sinonimi" => verifica che sia vero
130500061017     C                   if        cplESS = 'S'
130600061017     C                   eval      cplESS = *blanks
130700061017     C                   seton                                        70
130800061017     C                   eval      K_cpsVER = NumVER
130900061017     C                   eval      K_cpsNAR = *blanks
131000061017     C                   eval      K_cpsCAP = cplCAP
131100061017     C                   eval      K_cpsLOC = cplLOC
131200061017     C     KEYcps01_P    setll     azcps01l
131300061017     C                   if        %equal(azcps01l)
131400061017     C     KEYcps01_P    reade     azcps01l
131500061017     C                   dow       not %eof(azcps01l)
131600061017     C                   if        cpsATB <> 'A'
131700061017     C                   eval      cplESS = 'S'
131800061017     C                   setoff                                       70
131900061017     C                   leave
132000061017     C                   endif
132100061017     C     KEYcps01_P    reade     azcps01l
132200061017     C                   enddo
132300061017     C                   endif
132400061017     C                   endif
132500061017     C*
132600061017     C* Se località nn già obsoleta effettuo le verifiche
132700061018     C                   if        cplFI1 = *blanks           AND
132800061018     C                             %subst(cplCAP:4:2) <> '00' AND
132900061018     C                             %subst(cplCAP:4:2) <> '  '
133000061017     C*
133100061017     C* Verifico se località BART presente in località PT
133200061017     C                   exsr      rtvlocpt
133300061017     C*
133400061017     C* Se nn trovata
133500061017     C                   if        *in53 = *on
133600061017     C                   seton                                        61
133700061017     C*
133800061017     C* Reperisco la provincia dal cap BART
133900061017     C                   eval      K_cpcVER = NumVER
134000061017     C                   eval      K_cpcNAR = *blanks
134100061017     C                   eval      K_cpcCAP = cplCAP
134200061017     C     KEYcpc01_C    chain     azcpc01l
134300061017     C                   if        %found(azcpc01l)
134400061017     C*
134500061017     C* Reperisco il cap PT dal file "Raineri" associato alla loc BART corrente
134600061017     C                   exsr      rtvlocrainer
134700061017     C*
134800061017     C* Se trovata e univoca procedo
134900061017     C                   if        rainer_cnt = 1
135000061017     C*
135100061017     C* Reperisco il comune PT associato al cap della loc BART corrente
135200061017     C                   exsr      rtvcompt
135300061017     C*
135400061017     C* Se trovata e univoco procedo
135500061017     C                   if        com_cnt = 1
135600061017     C*
135700061017     C                   seton                                        60
135800061017     C*
135900061017     C* Gestisco inserendo la località coerrente come sinonimo del comune cui nel file cap PT
136000061017     C* il cap della località BART corrente è associato, x cui =>
136100061017     C*
136200061017     C* 1° Annullo la località corrente
136300061017     C                   eval      cplFI1 = '!'
136400061017     C                   update(e) azcpl000
136500061017     C*
136600061017     C* Inserisco come sinonimo del comune la località BART corrente (appena annullata)
136700061017     C                   clear                   azcps000
136800061017     C                   eval      cpsVER = cplVER
136900061017     C                   eval      cpsNAR = cplNAR
137000061017     C                   eval      cpsCAP = rainer_cap
137100061017     C                   eval      cpsLOC = com_com
137200061017     C                   eval      cpsSIN = cplLOC
137300061017     C                   eval      cpsFTR = *blanks
137400061017     C                   write(e)  azcps000
137500061017     C*
137600061017     C* Quindi sposto i sinonimi della località originale sul comune appena reperito
137700061017     C                   eval      K_cpsVER = cplVER
137800061017     C                   eval      K_cpsNAR = cplNAR
137900061017     C                   eval      K_cpsCAP = cplCAP
138000061017     C                   eval      K_cpsLOC = cplLOC
138100061017     C                   eval      wCPSCAP  = rainer_cap
138200061017     C                   eval      wCPSLOC  = com_com
138300061018     C***                exsr      movsin
138400061018     C                   exsr      dupsin
138500061017     C*
138600061017     C                   endif
138700061017     C                   endif
138800061017     C                   endif
138900061017     C*
139000061017     C* Se nn trovata flaggo in maniera "evidente" cap+loc corrente BART
139100061017     C                   if        *in60 = *off
139200061017     C                   eval      cplFI1 = '?'
139300061017     C                   endif
139400061017     C*
139500061017     C                   endif
139600061017     C*
139700061017     C                   endif
139800061017     C*
139900061017     C* Se occorre updatare => eseguo
140000061017     C                   if        (*in60 = *off AND *in61 = *on) OR
140100061017     C                             *in70
140200061017     C                   eval      cplFTR = *blanks
140300061017     C                   update(e) azcpl000
140400061017     C                   endif
140500061017     C*
140600061017     C* Proseguo con la lettura delle località BART
140700061017     C     NumVER        reade     azcpl01l
140800061017     C                   enddo
140900061018     C*
141000061018     C                   EVAL      VID04 = 'Fatto'
141100061018     C                   SETOFF                                       94
141200061018     C                   WRITE     SP20D03
141300061017     C*
141400061017     C                   ENDSR
141500061017     C*--------------------------------------------------------------------------------------------*
141600061123     C* exever3 - Esecuzione 3° verifica: eliminare sin BART che sono già loc BART (nn obs) x lo stesso cap
141700061017     C*--------------------------------------------------------------------------------------------*
141800061017     C     exever3       BEGSR
141900061018     C*
142000061023     C                   EVAL      VID07 = 'In elaborazione'
142100061023     C                   SETON                                        97
142200061018     C                   WRITE     SP20D03
142300061017     C*
142400061017     C                   eval      wSQL =
142500061017     C                             'update azcps00f set cpsatb = ''A'', '    +
142600061017     C                             'cpsftr='' '' where cpsver=' + VIDVER     +
142700061017     C                             ' and cpscap!!cpssin in (select '         +
142800061017     C                             'cplcap!!cplloc from azcpl00f '           +
142900061017     C                             'where cplver=' + VIDVER + ' and cplFI1 ' +
143000061123     C                             '= '' '')'
143100061017     C*
143200061017     C/EXEC SQL
143300061017     C+ execute immediate: wSQL
143400061017     C/END-EXEC
143500061017     C*
143600061023     C                   EVAL      VID07 = 'Fatto'
143700061023     C                   SETOFF                                       97
143800061018     C                   WRITE     SP20D03
143900061018     C*
144000061017     C                   ENDSR
144100061017     C*------------------------------------------------------------------------*
144200061017     C* exever4 - Esecuzione 4° verifica: obsoleta cap BART aventi tutte relative loc BART obsolete
144300061017     C*------------------------------------------------------------------------*
144400061017     C     exever4       BEGSR
144500061018     C*
144600061018     C                   EVAL      VID06 = 'In elaborazione'
144700061018     C                   SETON                                        96
144800061018     C                   WRITE     SP20D03
144900061017     C*
145000061017     C/EXEC SQL
145100061017     C+ declare C8 cursor for select * from azcpl00f
145200061017     C+ where cplver = :NumVER and cplnar = ' '
145300061017     C+ order by cplcap, cplloc
145400061017     C+ for fetch only
145500061017     C/END-EXEC
145600061017     C
145700061017     C/EXEC SQL
145800061017     C+ open C8
145900061017     C/END-EXEC
146000061017     C
146100061017     C/EXEC SQL
146200061017     C+ Fetch C8 into :ds_cpl_obs
146300061017     C/END-EXEC
146400061017     C
146500061017     C                   dow       sqlcod=*zeros
146600061017     C*
146700061017     C* ESEGUO CONSIDERAZIONI ED OPERAZIONI DI SCRITTURA ENTRO CICLO
146800061017     C                   if        O_cplcap = savcap
146900061017     C                   if        O_cplfi1 = *blanks
147000061017     C                   movel     '0'           chkobs
147100061017     C                   endif
147200061017     C                   else
147300061017     C                   if        chkobs = '1' and savcap <> *blanks
147400061017     C                   eval      K_cpcVER = NumVER
147500061017     C                   eval      K_cpcNAR = *blanks
147600061017     C                   eval      K_cpcCAP = savcap
147700061017     C     KEYcpc01_C    chain     azcpc01l
147800061017     C                   if        %found(azcpc01l) AND cpcFI1 <> 'O'
147900061017     C                   movel     'O '          cpcFI1
148000061017     C                   movel     *blanks       cpcFTR
148100061017     C                   update(e) azcpc000
148200061017     C                   endif
148300061017     C                   endif
148400061017     C                   movel     O_cplcap      savcap
148500061017     C                   movel     '1'           chkobs
148600061017     C                   if        O_cplfi1 = *blanks
148700061017     C                   movel     '0'           chkobs
148800061017     C                   endif
148900061017     C                   endif
149000061017     C
149100061017     C/EXEC SQL
149200061017     C+ Fetch C8 into :ds_cpl_obs
149300061017     C/END-EXEC
149400061017     C
149500061017     C                   enddo
149600061017     C
149700061017     C/EXEC SQL
149800061017     C+ close C8
149900061017     C/END-EXEC
150000061018     C*
150100061018     C                   EVAL      VID06 = 'Fatto'
150200061018     C                   SETOFF                                       96
150300061018     C                   WRITE     SP20D03
150400061017     C*
150500061017     C                   ENDSR
150600061023     C*--------------------------------------------------------------------------------------------*
150700061023     C* exever5 - Esecuzione 5° verifica: vaglia loc BART "?" tramite TISI95R
150800061023     C*--------------------------------------------------------------------------------------------*
150900061023     C     exever5       BEGSR
151000061023     C*
151100061023     C                   EVAL      VID05 = 'In elaborazione'
151200061023     C                   SETON                                        95
151300061023     C                   WRITE     SP20D03
151400061023     C*
151500061023     C* Scorro tutto il file delle località BART
151600061023     C     *start        setll     azcpl01l
151700061023     C     NumVER        setll     azcpl01l
151800061023     C     NumVER        reade     azcpl01l
151900061023     C                   dow       not %eof(azcpl01l)
152000061024     C*
152100061024     C                   setoff                                       58
152200061023     C*
152300061023     C* Se località già battezzata "?" elaboro
152400061023     C                   if        %trim(cplFI1) = '?'
152500061023     C*
152600061023     C* Reperisco la provincia dal cap BART
152700061023     C                   eval      K_cpcVER = NumVER
152800061023     C                   eval      K_cpcNAR = *blanks
152900061023     C                   eval      K_cpcCAP = cplCAP
153000061023     C     KEYcpc01_C    chain     azcpc01l
153100061023     C                   if        %found(azcpc01l)
153200061023     C*
153300061023     C* Passo al TISI95R loc + prv e se livello affidabilità >= 2 listo e tengo traccia aggiornando la località
153400061023     C                   clear                   TISI95DS
153500061023     C                   eval      I95TCN = '7'
153600061023     C                   z-add     datcor        I95DAT
153700061023     C                   eval      I95LOC = cplLOC
153800061023     C                   eval      I95PRV = cpcPRV
153900061023     C                   call      'TISI95R'
154000061023     C                   parm                    TISI95DS
154100061023     C*
154200061023     C                   if        O95LIA >= '2'
154300061024     C*
154400061024     C                   seton                                        58
154500061023     C*
154600061023     C* 1° Listo in stampa x preventiva verifica manuale
154700061023     C                   eval      wSI95 = 'DA: ' +
154800061023     C                                     cplCAP + ' ' +
154900061023     C                                     cplLOC + ' ' +
155000061023     C                                     cpcPRV + ' ' +
155100061023     C                                     ' => A: ' +
155200061023     C                                     O95CAP + ' ' +
155300061023     C                                     O95LOC + ' ' +
155400061023     C                                     O95PRV + ' ' +
155500061023     C                                     O95SIN + ' ' +
155600061023     C                                     O95LID + ' ' +
155700061023     C                                     O95LIA
155800061023     C                   except    rigaSI95
155900061023     C*
156000061023     C* 2° Marco la località corrente (solo se lancio no in simulazione)
156100061023     C                   if        VIDSI95 = 'X'
156200061023     C                   eval      cplFI1  = 'T'
156300061023     C                   update(e) azcpl000
156400061023     C*
156500061023     C* 3° Sposto i sinonimi della località originale sul comune appena reperito
156600061023     C                   eval      K_cpsVER = cplVER
156700061023     C                   eval      K_cpsNAR = cplNAR
156800061023     C                   eval      K_cpsCAP = cplCAP
156900061023     C                   eval      K_cpsLOC = cplLOC
157000061204     C                   eval      wCPSCAP  = O95CAP
157100061204     C                   eval      wCPSLOC  = O95LOC
157200061023     C***                exsr      movsin
157300061024     C                   exsr      dupsin
157400061023     C                   endif
157500061023     C*
157600061023     C                   endif
157700061023     C                   endif
157800061023     C                   endif
157900061024     C*
158000061024     C* Se x qualunque ragione nn ho elaborato => listo
158100061024     C***                if        *in58 = *off
158200061024     C***                eval      wSI95 = 'DA: ' +
158300061024     C***                                  cplCAP + ' ' +
158400061024     C***                                  cplLOC + ' ' +
158500061024     C***                                  cpcPRV + ' ' +
158600061024     C***                                  ' => N.P. '
158700061024     C***                except    rigaSI95
158800061024     C***                endif
158900061023     C*
159000061023     C* Proseguo con la lettura delle località BART
159100061023     C     NumVER        reade     azcpl01l
159200061023     C                   enddo
159300061023     C*
159400061023     C                   EVAL      VID05 = 'Fatto'
159500061023     C                   SETOFF                                       95
159600061023     C                   WRITE     SP20D03
159700061023     C*
159800061023     C                   ENDSR
159900061024     C*--------------------------------------------------------------------------------------------*
160000061024     C* exever6 - Esecuzione 6° verifica: sistema le LOC BART obsolete e a livello "C" x
160100061024     C*                                   ricongiunzione a nuova località su diverso cap  ma in
160200061024     C*                                   stessa provincia / provincia alternativa
160300061024     C*--------------------------------------------------------------------------------------------*
160400061024     C     exever6       BEGSR
160500061024     C*
160600061024     C                   EVAL      VID07 = 'In elaborazione'
160700061024     C                   SETON                                        97
160800061024     C                   WRITE     SP20D03
160900061024     C*
161000061024     C* Scorro tutto il file delle località BART
161100061024     C     *start        setll     azcpl02l
161200061024     C     NumVER        setll     azcpl02l
161300061024     C     NumVER        reade     azcpl02l
161400061024     C                   dow       not %eof(azcpl02l)
161500061024     C*
161600061024     C* Elaboro località obsolete e a livello "C"
161700061024     C                   if        l2_cplFI1 = 'O ' AND
161800061024     C                             l2_cplLIV = 'C'
161900061024     C*
162000061024     C* Reperisco innanzitutto la provincia / prov alternativa della località corrente
162100061024     C                   clear                   ds_prv
162200061024     C                   exsr      rtvprv
162300061024     C*
162400061024     C* Se reperito qualcosa proseguo con elaborazione
162500061024     C                   if        ds_prv <> *blanks
162600061024     C*
162700061024     C* Quindi reperisco la località precisa
162800061024     C                   clear                   ds_loc
162900061024     C                   exsr      rtvlocnew
163000061024     C*
163100061024     C* Se reperito qualcosa proseguo con elaborazione
163200061024     C                   if        ds_loc <> *blanks
163300061024     C*
163400061024     C* Quindi aggancio la nuova località inserita da PT
163500061024     C                   eval      K_cplVER = loc_ver
163600061024     C                   eval      K_cplNAR = loc_nar
163700061024     C                   eval      K_cplCAP = loc_cap
163800061024     C                   eval      K_cplLOC = loc_loc
163900061024     C     KEYcpl01_C    chain     azcpl01l
164000061024     C                   if        %found(azcpl01l)
164100061024     C*
164200061024     C* Reperisco i dati d instradamento dal cap della relativa località "obsoletizzata"
164300061024     C                   eval      wCPCVER = l2_cplVER
164400061024     C                   eval      wCPCCAP = l2_cplCAP
164500061024     C                   exsr      cpctocpl
164600061024     C*
164700061024     C* Imposto quindi a "L" il livello d gestioine della nuova località inserita da PT
164800061024     C                   eval      cplLIV  = 'L'
164900061024     C*
165000061024     C* Superfluo ma cmq sfleggo il record
165100061024     C                   movel     *blanks       cplFTR
165200061024     C*
165300061024     C* Quindi aggiorno il record della nuova località inserita da PT
165400061024     C                   update(e) azcpl000
165500061024     C*
165600061024     C* A questo punto occorre indicare sul cap della nuova localita inserita da PT la presenza d eccezioni x localita
165700061024     C                   eval      wCPCVER = loc_ver
165800061024     C                   eval      wCPCCAP = loc_cap
165900061024     C                   exsr      rtvcap
166000061024     C                   if        *in52 = *on
166100061024     C                   eval      cpcESL = 'S'
166200061024     C                   update(e) azcpc000
166300061024     C                   endif
166400061024     C                   endif
166500061024     C                   endif
166600061024     C                   endif
166700061024     C                   endif
166800061024     C*
166900061024     C* Proseguo con la lettura delle località BART
167000061024     C     NumVER        reade     azcpl02l
167100061024     C                   enddo
167200061024     C*
167300061024     C                   EVAL      VID07 = 'Fatto'
167400061024     C                   SETOFF                                       97
167500061024     C                   WRITE     SP20D03
167600061024     C*
167700061122     C                   ENDSR
167800061122     C*--------------------------------------------------------------------------------------------*
167900061122     C* exever8 - Esecuzione 8° verifica: annullaamento sinonimi doppi su stesso cap
168000061122     C*--------------------------------------------------------------------------------------------*
168100061122     C     exever8       BEGSR
168200061122     C*
168300061122     C                   EVAL      VID08 = 'In elaborazione'
168400061122     C                   SETON                                        98
168500061122     C                   WRITE     SP20D03
168600061122     C*
168700061122     C* Inizializzo il contatore d stesso sin su stesso cap
168800061122     C                   eval      w_cnt_sin = *zeros
168900061122     C*
169000061122     C/EXEC SQL
169100061122     C+ declare C11 cursor for
169200061122     C+ select cplfi1, cpscap, cpssin, cpsloc from azcps00f join azcpl00f
169300061122     C+ on  cpsver=cplver and cpsnar=cplnar and cpscap=cplcap
169400061122     C+ and cpsloc=cplloc
169500061123     C+ where cpsver=:NumVER
169600061122     C+ order by cpscap, cpssin, cplfi1
169700061122     C+ for read only
169800061122     C/END-EXEC
169900061122     C
170000061122     C/EXEC SQL
170100061122     C+ open C11
170200061122     C/END-EXEC
170300061122     C
170400061122     C/EXEC SQL
170500061122     C+ Fetch next from C11 into :ds_dup_sin
170600061122     C/END-EXEC
170700061122     C*
170800061122     C* Se reperito qualcosa => analizzo
170900061122     C                   dow       sqlcod = *zeros
171000061122     C*
171100061122     C* Verifico se già presente x tesso cap
171200061122     C                   if        d_dup_cap = cnt_sin_cap AND
171300061122     C                             d_dup_sin = cnt_sin_sin
171400061122     C*
171500061122     C* Incremento il contatore d stesso sin su stesso cap
171600061122     C                   eval      w_cnt_sin = w_cnt_sin + 1
171700061122     C*
171800061122     C* Se contatore > 1 => annullo il sin corrente
171900061122     C                   if        w_cnt_sin > 1
172000061122     C                   eval      key_sin_loc = d_dup_loc
172100061122     C                   exsr      annSIN
172200061122     C                   endif
172300061122     C*
172400061122     C* Se nn già presente salvo cap/sin correnti
172500061122     C                   else
172600061122     C*
172700061122     C* Re-imposto a 1 il contatore d sin x stesso cap
172800061122     C                   eval      w_cnt_sin = 1
172900061122     C*
173000061122     C* Quindi salvo il cap/sin corrente
173100061122     C                   eval      cnt_sin_cap = d_dup_cap
173200061122     C                   eval      cnt_sin_sin = d_dup_sin
173300061122     C                   endif
173400061122     C*
173500061122     C* Proseguo con la lettura dei sin
173600061122     C*
173700061122     C/EXEC SQL
173800061122     C+ Fetch next from C11 into :ds_dup_sin
173900061122     C/END-EXEC
174000061122     C*
174100061122     C                   enddo
174200061122     C*
174300061122     C/EXEC SQL
174400061122     C+ close C11
174500061122     C/END-EXEC
174600061122     C*
174700061122     C                   EVAL      VID08 = 'Fatto'
174800061122     C                   SETOFF                                       98
174900061122     C                   WRITE     SP20D03
175000061122     C*
175100061122     C                   ENDSR
175200061120     C*--------------------------------------------------------------------------------------------*
175300061120     C* annSIN - annulla sinonimo
175400061120     C*--------------------------------------------------------------------------------------------*
175500061120     C     annSIN        BEGSR
175600061120     C*
175700061120     C                   eval      K_cpsVER = NumVER
175800061120     C                   eval      K_cpsNAR = *blanks
175900061122     C                   eval      K_cpsCAP = d_dup_cap
176000061120     C                   eval      K_cpsLOC = key_sin_loc
176100061122     C                   eval      K_cpsSIN = d_dup_sin
176200061120     C     KEYcps01_C    chain(e)  azcps01l
176300061120     C                   if        %error
176400061120     C                   eval      wrk_error = 'E'
176500061120     C                   except    rigaSin
176600061120     C                   else
176700061120     C                   if        %found(azcps01l)
176800061120     C                   eval      cpsATB = 'A'
176900061120     C                   eval      cpsFTR = *blanks
177000061120     C                   update    azcps000
177100061120     C                   eval      wrk_error = *blanks
177200061120     C                   except    rigaSin
177300061120     C                   endif
177400061120     C                   endif
177500061120     C*
177600061120     C                   ENDSR
177700061024     C*--------------------------------------------------------------------------------------------*
177800061024     C* updFCPU - aggiornamento provincie alternative
177900061024     C*--------------------------------------------------------------------------------------------*
178000061024     C     updFCPU       BEGSR
178100061024     C*
178200061024     C                   EVAL      VID02 = 'In elaborazione'
178300061024     C                   SETON                                        92
178400061024     C                   WRITE     SP20D03
178500061024     C*
178600061024     C                   eval      wSQL = 'update azcpc00f ' +
178700061024     C                             'set cpcPRV=''FC'', cpcPRA=''FO'' ' +
178800061024     C                             'where cpcver='+VIDVER+' and cpcPRV=''FO'' '+
178900061024     C                             'and cpcPRA=''FC'''
179000061024     C*
179100061024     C/EXEC SQL
179200061024     C+ execute immediate: wSQL
179300061024     C/END-EXEC
179400061024     C*
179500061024     C                   eval      wSQL = 'update azcpc00f ' +
179600061024     C                             'set cpcPRV=''PU'', cpcPRA=''PS'' ' +
179700061024     C                             'where cpcver='+VIDVER+' and cpcPRV=''PS'' '+
179800061024     C                             'and cpcPRA=''PU'''
179900061024     C*
180000061024     C/EXEC SQL
180100061024     C+ execute immediate: wSQL
180200061024     C/END-EXEC
180300061024     C*
180400061024     C                   ENDSR
180500061017     C*--------------------------------------------------------------------------------------------*
180600061017     C* inzpt - inizializzazione flag di elaborazione file PT
180700061017     C*--------------------------------------------------------------------------------------------*
180800061017     C     inzpt         BEGSR
180900061018     C*
181000061018     C                   EVAL      VID01 = 'In elaborazione'
181100061018     C                   SETON                                        91
181200061018     C                   WRITE     SP20D03
181300061017     C*
181400061017     C                   eval      wSQL = 'update cap_cap set FLG1 = '' '', ' +
181500061017     C                                                       'FLG2 = '' '''
181600061017     C*
181700061017     C/EXEC SQL
181800061017     C+ execute immediate: wSQL
181900061017     C/END-EXEC
182000061017     C*
182100061017     C                   eval      wSQL = 'update cap_loc set FLG1 = '' '', ' +
182200061017     C                                                       'FLG2 = '' '''
182300061017     C*
182400061017     C/EXEC SQL
182500061017     C+ execute immediate: wSQL
182600061017     C/END-EXEC
182700061017     C                   eval      wSQL = 'update cap_sin set FLG1 = '' '', ' +
182800061017     C                                                       'FLG2 = '' '''
182900061017     C*
183000061017     C/EXEC SQL
183100061017     C+ execute immediate: wSQL
183200061017     C/END-EXEC
183300061017     C*
183400061017     C                   eval      wSQL = 'delete from azcpc00f where cpcver='+
183500061017     C                                    VIDVER
183600061017     C*
183700061017     C/EXEC SQL
183800061017     C+ execute immediate: wSQL
183900061017     C/END-EXEC
184000061017     C*
184100061017     C                   eval      wSQL = 'delete from azcpl00f where cplver='+
184200061017     C                                    VIDVER
184300061017     C*
184400061017     C/EXEC SQL
184500061017     C+ execute immediate: wSQL
184600061017     C/END-EXEC
184700061017     C                   eval      wSQL = 'delete from azcps00f where cpsver='+
184800061017     C                                    VIDVER
184900061017     C*
185000061017     C/EXEC SQL
185100061017     C+ execute immediate: wSQL
185200061017     C/END-EXEC
185300061017     C*
185400061017     C                   CALL      'TISP20C1'
185500061017     C*
185600061017     C                   eval      wSQL = 'update azcpc00f set cpcFTR = ''T'' '+
185700061017     C                                    'where cpcver='+VIDVER
185800061017     C*
185900061017     C/EXEC SQL
186000061017     C+ execute immediate: wSQL
186100061017     C/END-EXEC
186200061017     C*
186300061017     C                   eval      wSQL = 'update azcpl00f set cplFTR = ''T'' '+
186400061017     C                                    'where cplver='+VIDVER
186500061017     C*
186600061017     C/EXEC SQL
186700061017     C+ execute immediate: wSQL
186800061017     C/END-EXEC
186900061017     C                   eval      wSQL = 'update azcps00f set cpsFTR = ''T'' '+
187000061017     C                                    'where cpsver='+VIDVER
187100061017     C*
187200061017     C/EXEC SQL
187300061017     C+ execute immediate: wSQL
187400061017     C/END-EXEC
187500061018     C*
187600061018     C                   EVAL      VID01 = 'Fatto'
187700061018     C                   SETOFF                                       91
187800061018     C                   WRITE     SP20D03
187900061017     C*
188000061017     C                   ENDSR
188100061024     C*--------------------------------------------------------------------------------------------*
188200061024     C* cpctocpl - passaggio dati da livello "C" a "L"
188300061024     C*--------------------------------------------------------------------------------------------*
188400061024     C     cpctocpl      BEGSR
188500061024     C*
188600061024     C                   exsr      rtvcap
188700061024     C                   if        *in52 = *on
188800061024     C                   eval      cplLNA = cpcLNA
188900061024     C                   eval      cplZNC = cpcZNC
189000061024     C                   eval      cplISO = cpcISO
189100061024     C                   eval      cplZFD = cpcZFD
189200061024     C                   eval      cplLKG = cpcLKG
189300061024     C                   eval      cplLMC = cpcLMC
189400061024     C                   eval      cplLOL = cpcLOL
189500061024     C                   eval      cplZOL = cpcZOL
189600061024     C                   eval      cplLKS = cpcLKS
189700061024     C                   eval      cplLMS = cpcLMS
189800061024     C                   eval      cplLOS = cpcLOS
189900061024     C                   eval      cplZOS = cpcZOS
190000061024     C                   eval      cplLK2 = cpcLK2
190100061024     C                   eval      cplLM2 = cpcLM2
190200061024     C                   eval      cplLO2 = cpcLO2
190300061024     C                   eval      cplZO2 = cpcZO2
190400061024     C                   eval      cplCTS = cpcCTS
190500061024     C                   eval      cplCTE = cpcCTE
190600061024     C                   eval      cplTTC = cpcTTC
190700061024     C                   eval      cplPII = cpcPII
190800061024     C                   eval      cplPID = cpcPID
190900061024     C                   eval      cplPIS = cpcPIS
191000061024     C                   eval      cplGDD = cpcGDD
191100061121     C                   eval      cplFI2 = cpcFI2
191200061024     C                   endif
191300061024     C*
191400061024     C                   ENDSR
191500000000     C*------------------------------------------------------------------------*
191600980403     C* *INZSR - OPERAZIONI INIZIALI
191700000000     C*------------------------------------------------------------------------*
191800980403     C     *INZSR        BEGSR
191900000000     C*
192000000000     C* RICEVIMENTO PARAMETRI
192100000000     C     *ENTRY        PLIST
192200000000     C                   PARM                    KPJBA
192300061017     C*
192400061017     C* Definizione chiavi di lettura
192500061017     C*
192600061017     C* AZCPC01L - Completa
192700061017     C     KEYcpc01_C    KLIST
192800061017     C                   KFLD                    K_cpcVER
192900061017     C                   KFLD                    K_cpcNAR
193000061017     C                   KFLD                    K_cpcCAP
193100061018     C*
193200061018     C* AZCPC01L - Parziale
193300061018     C     KEYcpc01_P    KLIST
193400061018     C                   KFLD                    K_cpcVER
193500061018     C                   KFLD                    K_cpcNAR
193600061017     C*
193700061017     C* AZCPL01L - Completa
193800061017     C     KEYcpl01_C    KLIST
193900061017     C                   KFLD                    K_cplVER
194000061017     C                   KFLD                    K_cplNAR
194100061017     C                   KFLD                    K_cplCAP
194200061017     C                   KFLD                    K_cplLOC
194300061017     C*
194400061017     C* AZCPS01L - Parziale
194500061017     C     KEYcps01_P    KLIST
194600061017     C                   KFLD                    K_cpsVER
194700061017     C                   KFLD                    K_cpsNAR
194800061017     C                   KFLD                    K_cpsCAP
194900061017     C                   KFLD                    K_cpsLOC
195000061120     C*
195100061120     C* AZCPS01L - Completa
195200061120     C     KEYcps01_C    KLIST
195300061120     C                   KFLD                    K_cpsVER
195400061120     C                   KFLD                    K_cpsNAR
195500061120     C                   KFLD                    K_cpsCAP
195600061120     C                   KFLD                    K_cpsLOC
195700061120     C                   KFLD                    K_cpsSIN
195800000104     C*
195900000104     C* AZZERO INIZIALE INDICATORI
196000061017     C                   SETOFF                                       40
196100000000     C*
196200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
196300000000     C     XMSGPA        PLIST
196400000000     C                   PARM                    DSMSMS
196500000000     C                   PARM                    DSMSPG
196600000000     C                   PARM                    DSMSKE
196700000000     C                   PARM                    DSMSER
196800000000     C*
196900000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
197000000000     C                   CALL      'XKEY'
197100000000     C                   PARM                    DSKEY
197200000000     C*
197300000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
197400000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
197500000000     C*
197600000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
197700000000     C*   -NOME PGM CHIAMANTE
197800000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
197900061017     C                   MOVEL     'TISP20R'     DSMSPG
198000061017     C                   MOVEL     'TISP20R'     MSGQUE
198100000000     C                   MOVEL     *BLANKS       MSGKEY
198200000000     C                   MOVEL     *BLANKS       DSMSMS
198300000000     C*
198400000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
198500000000     C                   Z-ADD     0             FUNGRU            5 0
198600000000     C*
198700000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
198800000000     C                   EXSR      FUNGES
198900000000     C*
199000000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
199100000000     C                   MOVEL     '0'           F12L00            1
199200000000     C                   MOVEL     '0'           F03L00            1
199300000000     C                   MOVEL     '0'           FINL00            1
199400000000     C                   MOVEL     '0'           ERRL00            1
199500000000     C                   MOVEL     '0'           FLFINE            1
199600000000     C*
199700000000     C* LANCIO ROUTINE PER VERIFICARE SE
199800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
199900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
200000020917     C                   EXSR      REPDATIUTE
200100000000     C*
200200000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
200300000000     C                   EXSR      AZZVID
200400000000     C*
200500000000     C                   ENDSR
200600000000     C*------------------------------------------------------------------------*
200700000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
200800000000     C*------------------------------------------------------------------------*
200900000000     C     FUNGES        BEGSR
201000000000     C*
201100000000     C* PULISCO CAMPI DI LAVORO
201200000000     C                   Z-ADD     0             W                 5 0
201300000000     C*
201400000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
201500000000     C* E CARICO TABELLA FUNZIONI ATTIVE
201600000000     C     1             DO        24            I
201700000000     C                   MOVEL     FUD(I)        FUDRIG
201800000000     C*
201900000000     C* PULIZIA FUNZIONI PRECEDENTI
202000000000     C                   MOVEL     *BLANKS       FUV(I)
202100000000     C*
202200000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
202300000000     C* (SALTA F24)
202400000000     C     FUDATT        IFEQ      'S'
202500000000     C     FUDFUN        ANDNE     '24'
202600000000     C                   ADD       1             W
202700000000     C                   MOVEL     FUDDES        FUV(W)
202800000000     C                   END
202900000000     C                   END                                                    DO WHILE
203000000000     C*
203100000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
203200000000     C* ALTRIMENTI DISATTIVO F24
203300000000     C     1             DO        24            I
203400000000     C                   MOVEL     FUD(I)        FUDRIG
203500000000     C*
203600000000     C     FUDFUN        IFEQ      '24'
203700000000     C     W             IFGT      6
203800000000     C                   MOVEL     'S'           FUDATT
203900000000     C                   ELSE
204000000000     C                   MOVEL     *BLANKS       FUDATT
204100000000     C                   END                                                    W GT 8
204200000000     C                   MOVEL     FUDRIG        FUD(I)
204300000000     C                   END
204400000000     C                   END                                                    DO WHILE
204500000000     C*
204600000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
204700000000     C     W             DIV       6             I
204800000000     C                   MVR                     Z
204900000000     C*
205000000000     C     Z             IFNE      0
205100000000     C     I             ADD       1             Z
205200000000     C                   ELSE
205300000000     C                   Z-ADD     I             Z                 5 0
205400000000     C                   END
205500000000     C*
205600000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
205700000000     C                   ADD       1             FUNGRU
205800000000     C     FUNGRU        IFGT      Z
205900000000     C     FUNGRU        ORLT      1
206000000000     C                   Z-ADD     1             FUNGRU
206100000000     C                   END
206200000000     C*
206300000000     C* PRIMO GRUPPO DI 6 FUNZIONI
206400000000     C     FUNGRU        IFEQ      1
206500000000     C                   MOVEL     FUV(1)        FUN001
206600000000     C                   MOVEL     FUV(2)        FUN002
206700000000     C                   MOVEL     FUV(3)        FUN003
206800000000     C                   MOVEL     FUV(4)        FUN004
206900000000     C                   MOVEL     FUV(5)        FUN005
207000000000     C                   MOVEL     FUV(6)        FUN006
207100000000     C                   END
207200000000     C*
207300000000     C* SECONDO GRUPPO DI 6 FUNZIONI
207400000000     C     FUNGRU        IFEQ      2
207500000000     C                   MOVEL     FUV(7)        FUN001
207600000000     C                   MOVEL     FUV(8)        FUN002
207700000000     C                   MOVEL     FUV(9)        FUN003
207800000000     C                   MOVEL     FUV(10)       FUN004
207900000000     C                   MOVEL     FUV(11)       FUN005
208000000000     C                   MOVEL     FUV(12)       FUN006
208100000000     C                   END
208200000000     C*
208300000000     C* TERZO GRUPPO DI 6 FUNZIONI
208400000000     C     FUNGRU        IFEQ      3
208500000000     C                   MOVEL     FUV(13)       FUN001
208600000000     C                   MOVEL     FUV(14)       FUN002
208700000000     C                   MOVEL     FUV(15)       FUN003
208800000000     C                   MOVEL     FUV(16)       FUN004
208900000000     C                   MOVEL     FUV(17)       FUN005
209000000000     C                   MOVEL     FUV(18)       FUN006
209100000000     C                   END
209200000000     C*
209300000000     C* QUARTO GRUPPO DI 6 FUNZIONI
209400000000     C     FUNGRU        IFEQ      4
209500000000     C                   MOVEL     FUV(19)       FUN001
209600000000     C                   MOVEL     FUV(20)       FUN002
209700000000     C                   MOVEL     FUV(21)       FUN003
209800000000     C                   MOVEL     FUV(22)       FUN004
209900000000     C                   MOVEL     *BLANKS       FUN005
210000000000     C                   MOVEL     *BLANKS       FUN006
210100000000     C                   END
210200000000     C*
210300000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
210400000000     C                   MOVEL     *BLANKS       FUN007
210500000000     C                   MOVEL     *BLANKS       FUN008
210600000000     C*
210700000000     C     1             DO        24            I
210800000000     C                   MOVEL     FUD(I)        FUDRIG
210900000000     C     FUDFUN        IFEQ      '24'
211000000000     C     FUDATT        ANDEQ     'S'
211100000000     C                   MOVEL     FUDDES        FUN008
211200000000     C                   END
211300000000     C                   END                                                    DO
211400000000     C                   ENDSR
211500000000     C*------------------------------------------------------------------------*
211600000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
211700000000     C*------------------------------------------------------------------------*
211800000000     C     FUNCON        BEGSR
211900000000     C*
212000000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
212100000000     C                   EXSR      FUNVAR
212200000000     C*
212300000000     C* PULISCO CAMPI DI LAVORO
212400000000     C                   Z-ADD     0             I                 5 0
212500000000     C                   Z-ADD     0             W                 5 0
212600000000     C*
212700000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
212800000000     C     FUNKEY        IFNE      *BLANKS
212900000000     C     1             DO        24            I
213000000000     C                   MOVEL     FUD(I)        FUDRIG
213100000000     C*
213200000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
213300000000     C     FUNKEY        IFEQ      FUDFUN
213400000000     C     FUDATT        ANDEQ     'S'
213500000000     C                   Z-ADD     I             W
213600000000     C                   END
213700000000     C                   END                                                    DO WHILE
213800000000     C*
213900000000     C* FUNZIONE NON ATTIVA (ERRORE)
214000000000     C     W             IFEQ      0
214100000000     C*
214200000000     C* EMISSIONE MESSAGGIO DI ERRORE
214300000000     C                   MOVEL     MSG(1)        DSMSMS
214400000000     C*
214500000000     C                   END                                                    W=0
214600000000     C                   END                                                    FUNKEY NE *BLANKS
214700000000     C*
214800000000     C* FUNZIONE ATTIVA (PROCEDI ...)
214900000000     C     W             IFNE      0
215000000000     C     FUNKEY        OREQ      *BLANKS
215100000000     C*
215200000000     C     FUNKEY        IFEQ      *BLANKS
215300000000     C                   EXSR      ENTGES
215400000000     C                   END
215500000000     C     FUNKEY        IFEQ      '03'
215600000000     C                   EXSR      F03GES
215700000000     C                   END
215800000000     C     FUNKEY        IFEQ      '06'
215900000000     C                   EXSR      F06GES
216000000000     C                   END
216100000000     C     FUNKEY        IFEQ      '12'
216200000000     C                   EXSR      F12GES
216300000000     C                   END
216400000000     C     FUNKEY        IFEQ      '24'
216500000000     C                   EXSR      F24GES
216600000000     C                   END
216700000000     C     FUNKEY        IFEQ      'HP'
216800000000     C                   EXSR      HELGES
216900000000     C                   END
217000000000     C*
217100000000     C                   END
217200000000     C*
217300000000     C                   ENDSR
217400000000     C*------------------------------------------------------------------------*
217500000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
217600000000     C*------------------------------------------------------------------------*
217700000000     C     FUNVAR        BEGSR
217800000000     C*
217900000000     C* INIZIALIZZA VARIABILE -FUNKEY-
218000000000     C*
218100000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
218200000000     C     KEY           IFEQ      ENTER
218300000000     C                   MOVEL     *BLANKS       FUNKEY
218400000000     C                   END
218500000000     C     KEY           IFEQ      F1
218600000000     C                   MOVEL     '01'          FUNKEY
218700000000     C                   END
218800000000     C     KEY           IFEQ      F2
218900000000     C                   MOVEL     '02'          FUNKEY
219000000000     C                   END
219100000000     C     KEY           IFEQ      F3
219200000000     C                   MOVEL     '03'          FUNKEY
219300000000     C                   END
219400000000     C     KEY           IFEQ      F4
219500000000     C                   MOVEL     '04'          FUNKEY
219600000000     C                   END
219700000000     C     KEY           IFEQ      F5
219800000000     C                   MOVEL     '05'          FUNKEY
219900000000     C                   END
220000991103     C     KEY           IFEQ      F6
220100000000     C                   MOVEL     '06'          FUNKEY
220200000000     C                   END
220300000000     C     KEY           IFEQ      F7
220400000000     C                   MOVEL     '07'          FUNKEY
220500000000     C                   END
220600000000     C     KEY           IFEQ      F8
220700000000     C                   MOVEL     '08'          FUNKEY
220800000000     C                   END
220900000000     C     KEY           IFEQ      F9
221000000000     C                   MOVEL     '09'          FUNKEY
221100000000     C                   END
221200000000     C     KEY           IFEQ      F10
221300000000     C                   MOVEL     '10'          FUNKEY
221400000000     C                   END
221500000000     C     KEY           IFEQ      F11
221600000000     C                   MOVEL     '11'          FUNKEY
221700000000     C                   END
221800000000     C     KEY           IFEQ      F12
221900000000     C                   MOVEL     '12'          FUNKEY
222000000000     C                   END
222100000000     C     KEY           IFEQ      F13
222200000000     C                   MOVEL     '13'          FUNKEY
222300000000     C                   END
222400000000     C     KEY           IFEQ      F14
222500000000     C                   MOVEL     '14'          FUNKEY
222600000000     C                   END
222700000000     C     KEY           IFEQ      F15
222800000000     C                   MOVEL     '15'          FUNKEY
222900000000     C                   END
223000000000     C     KEY           IFEQ      F16
223100000000     C                   MOVEL     '16'          FUNKEY
223200000000     C                   END
223300000000     C     KEY           IFEQ      F17
223400000000     C                   MOVEL     '17'          FUNKEY
223500000000     C                   END
223600000000     C     KEY           IFEQ      F18
223700000000     C                   MOVEL     '18'          FUNKEY
223800000000     C                   END
223900000000     C     KEY           IFEQ      F19
224000000000     C                   MOVEL     '19'          FUNKEY
224100000000     C                   END
224200000000     C     KEY           IFEQ      F20
224300000000     C                   MOVEL     '20'          FUNKEY
224400000000     C                   END
224500000000     C     KEY           IFEQ      F21
224600000000     C                   MOVEL     '21'          FUNKEY
224700000000     C                   END
224800000000     C     KEY           IFEQ      F22
224900000000     C                   MOVEL     '22'          FUNKEY
225000000000     C                   END
225100000000     C     KEY           IFEQ      F23
225200000000     C                   MOVEL     '23'          FUNKEY
225300000000     C                   END
225400000000     C     KEY           IFEQ      F24
225500000000     C                   MOVEL     '24'          FUNKEY
225600000000     C                   END
225700000000     C     KEY           IFEQ      ROLLUP
225800000000     C                   MOVEL     'RU'          FUNKEY
225900000000     C                   END
226000000000     C     KEY           IFEQ      HELP
226100000000     C                   MOVEL     'HP'          FUNKEY
226200000000     C                   END
226300000000     C                   ENDSR
226400000000     C*------------------------------------------------------------------------*
226500000000     C* NOTE PER COMPILAZIONE SCHIERE
226600000000     C*------------------------------------------------------------------------*
226700000000     C* -MSG-
226800000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
226900000000     C* (NON TOCCARE MSG,1)
227000000000     C*
227100000000     C* -CMD-
227200000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
227300000000     C*
227400000000     C* -FUD-
227500000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
227600000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
227700000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
227800000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
227900000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
228000000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
228100000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
228200000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
228300000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
228400000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
228500000000     C*
228600000000     C*------------------------------------------------------------------------*
228700061017     OPRTF198   E            rigaErr     1
228800061017     O                       wRiga
228900061023     C*------------------------------------------------------------------------*
229000061120     OPRTF1982  E            rigaSI95    1
229100061023     O                       wSI95
229200061120     C*------------------------------------------------------------------------*
229300061120     OQSYSPRT   E            rigaSin     1
229400061122     O                       d_dup_cap           +1
229500061120     O                       key_sin_loc         +1
229600061122     O                       d_dup_sin           +1
229700061120     O                       wrk_error           +1
229800991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
229900061017TISP20R- Tasto funzionale non gestito.                                         1
230000061017TISP20R-                                                                       2
230100061017TISP20R-                                                                       3
230200061017TISP20R-                                                                       4
230300061017TISP20R- Errori nella funzione di richiesta versioni. Avvertire il CED         5
230400061017TISP20R-                                                                       6
230500061017TISP20R-                                                                       7
230600061017TISP20R- Versione Cappario BAR inesistente.                                    8
230700061017TISP20R-                                                                       9
230800061017TISP20R- Versione obbligatoria, inserire                                      10
230900061017TISP20R-                                                                      11
231000061017TISP20R-                                                                      12
231100061017TISP20R-                                                                      13
231200061017TISP20R-                                                                      14
231300061017TISP20R-                                                                      15
231400061017TISP20R-                                                                      16
231500061017TISP20R-                                                                      17
231600061017TISP20R-                                                                      18
231700061017TISP20R-                                                                      19
231800061017TISP20R-                                                                      20
231900000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
232000000000                   -        (S/N)=N                        funz.01¬
232100000000                   -        (S/N)=N                        funz.02¬
232200000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
232300000000                   -        (S/N)=N                        funz.04¬
232400000000                   -        (S/N)=N                        funz.05¬
232500001120F06=Esecuzione     -        (S/N)=S                        funz.06¬
232600000104                   -        (S/N)=N                        funz.07¬
232700000104F08=Ins. Versione  -        (S/N)=N                        funz.08¬
232800000000                   -        (S/N)=N                        funz.09¬
232900000000                   -        (S/N)=N                        funz.10¬
233000000000                   -        (S/N)=N                        funz.11¬
233100991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
233200000000                   -        (S/N)=N                        funz.13¬
233300000000                   -        (S/N)=N                        funz.14¬
233400000000                   -        (S/N)=N                        funz.15¬
233500000000                   -        (S/N)=N                        funz.16¬
233600000000                   -        (S/N)=N                        funz.17¬
233700000000                   -        (S/N)=N                        funz.18¬
233800000000                   -        (S/N)=N                        funz.19¬
233900000000                   -        (S/N)=N                        funz.20¬
234000000000                   -        (S/N)=N                        funz.21¬
234100000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
234200000000                   -        (S/N)=N                        funz.23¬
234300991103F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
