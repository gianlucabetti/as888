000100001003     H DECEDIT('0,') DATEDIT(*DMY.)
000200000929      ***********************************************************************
000300021016      *   Il pgm serve per smistare alle filiali gli - Stati Fedex -        *
000400021016      *   ricevuti in sede                                                  *
000500000929      ***********************************************************************
000600021016     FFIFES00W  IF   E             DISK    usropn
000700021016     f                                     RENAME(FIFES00W:INPUT000)
000800010307     FEDTAB01L  IF   E           K DISK    usropn
000900021016      *---------
001000010123     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100000120      *---------------------------------------------------------------------*
001200000120      * Schiere
001300000120      *---------------------------------------------------------------------*
001400001002      * Tabella PT --> partner
001500001002     D CPT             S             35    DIM(100)
001600000125     D CPTparz         S             35    DIM(100)
001700960528     D DPT             S             90    DIM(100)
001800010123     D DPU             S             90    DIM(100)
001900010123      *---------------------------------------------------------------------*
002000010123      * Comandi
002100010123      *---------------------------------------------------------------------*
002200010123     D ERR             S             78    DIM(5) CTDATA PERRCD(1)
002300010703     D CMD             S             78    DIM(9) CTDATA PERRCD(1)
002400000929      *---------------------------------------------------------------------*
002500001005      * Flag oprativi di work
002600010123     D PrimoDet        S              1    INZ(' ')
002700001005      *---------------------------------------------------------------------*
002800001003     D WCOD            S             35
002900020904     D W035A           S             35
003000010123     D DesErr          S             78
003100010123     D WValor          S             35
003200010413     D WValo1          S             35
003300010123     D WCOM14          S             14
003400010123     D WCOM16          S             16
003500010123     D Sksc            s              7  0 INZ(0)
003600010413     D ALnp            s              3    INZ(' ')
003700010123     D SLnp            s              3  0 INZ(0)
003800001003     D Wptparz         s              1
003900010123     D WptOK           s              1    INZ(' ')
004000010419     D WkeybolOK       s              1    INZ(' ')
004100010115      *---------------------------------------------------------------------*
004200010319     D  MSGDS          S            100
004300010124     D  LENGH          S             15  5
004400010115      *---------------------------------------------------------------------*
004500001003     D X               s              3  0 INZ(0)
004600001005     D Y               s              3  0 INZ(0)
004700010402     D SavX            s              3  0 INZ(0)
004800010115      *---------------------------------------------------------------------*
004900020904     D As400           s              3  0
005000010123      *---------------------------------------------------------------------*
005100010123     D Digits          C                   CONST('0123456789')
005200021016     D FEDEX089        C                   CONST('FEDEX089                    ')
005300000120      *---------------------------------------------------------------------*
005400000120      * DS
005500000120      *---------------------------------------------------------------------*
005600010123     D EDIDSPT       E DS
005700010123     D EDIDSPU       E DS
005800000120      *---------------------------------------------------------------------*
005900020905     D TIBS55DS      E DS
006000000120     D UTEDSE0F      E DS
006100000120     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
006200000120     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
006300010123      *---------------------------------------------------------------------*
006400010123     D DSSPE           DS
006500010123     D  WAAS                   1      4  0
006600010123     D  WLNP                   5      7  0
006700010123     D  WNRS                   8      9  0
006800010123     D  WNSP                  10     16  0
006900010123      *
007000010123     D DSSPE2          DS
007100010123     D  WAAS2                  1      2  0
007200010123     D  WLNP2                  3      5  0
007300010123     D  WNRS2                  6      7  0
007400010123     D  WNSP2                  8     14  0
007500000120      *---------------------------------------------------------------------*
007600000120      * Ciclo principale
007700000120      *---------------------------------------------------------------------*
007800021016      * Controllo esistenza tabella PT
007900021016     c                   exsr      chk_partner
008000021016     c                   clear                   almeno_uno        1
008100021016      *
008200021016      * se non ci sono stati errori ha impostato la linea di partenza
008300021016     c                   if        Slnp >0
008400021016     c                   open      fifes00w
008500001002      * Lettura iniziale sul file
008600021016     C                   READ      fifes00w
008700010123      * Ciclo di Lettura fino a EOF
008800021016     C                   if        not %EOF(fifes00w)
008900021016     c                   move      'S'           almeno_uno
009000021016     c                   end
009100021016     c                   close     fifes00w
009200010123      *
009300021016      * Invia file alla filiale Fedex
009400021016     c                   if        almeno_uno <> *blank
009500050607     c                   exsr      File_Filiale
009600021016     c                   endif
009700021016      *
009800021016     c                   endif
009900021016      *
010000960329     C                   SETON                                        LR
010100010115     C*-------------------------------------------------------------------------------------
010200010123     C*?    Controlla la testata a quale Filiale appartiene il Partner
010300010115     C*-------------------------------------------------------------------------------------
010400021016     C     Chk_Partner   BEGSR
010500010115     C*
010600010115     C* inizializzo i campi di salvataggio codici
010700010123     C                   clear                   SKsc
010800010115     C                   clear                   WCod
010900010115     C                   clear                   EDIDSPT
011000010123     C                   clear                   EDIDSPU
011100010115      *
011200021016      * Imposta la ricerca della PT
011300021016     C                   clear                   WCOD             35
011400021016     C                   Movel     FEDEX089      WCOD
011500021016     C                   clear                   Wqual             4
011600021016     C                   Move      Wqual         WCOD
011700021016      *
011800010115     C                   Z-ADD     1             X
011900010115     C     WCOD          LOOKUP    CPT(X)                                 33
012000010115      *
012100021016      *  se lo trova subito o se non lo trova affatto lo ripulisce
012200010115     c                   clear                   WPTparz
012300010115      *
012400010123      *  Prova a cercarlo con chiave parziale
012500010115     C                   If        *IN33 = *off
012600010123     C                   eval      X = 1
012700021016     C                   eval      W035A = %subst(wcod:1:31)
012800010115     C     W035A         lookup    CPTparz(X)                             33
012900010115      * se lo trova però deve individuarlo con maggior precisione
013000010115      *  in un secondo momento con la nazione del mittente
013100010123      *                           ================
013200010115     c   33              eval      WPTparz = 'S'
013300010123      *                           ================
013400010115     C                   Endif
013500010115      *
013600010115      *  Salva dalla tabella "PT" il cliente solo se è stata presa direttamente
013700010115      *   o in modo parziale.
013800010115     C                   If        *IN33 = *on
013900010115     C                   MOVEL     DPT(X)        EDIDSPT
014000010123     C                   MOVEL     DPU(X)        EDIDSPU
014100010123     C                   MOVEL     §PTLNP        SLnp
014200010123     C                   z-add     §PTKSC        SKsc
014300010123      *
014400010123      *  Solo se non parziale
014500010123     c                   If        WPTparz <> 'S'
014600010123     C                   movel     'S'           WptOK
014700010123     C                   Endif
014800010123      *
014900010123     C                   Else
015000010123      *  Problemi di reperimento del Partner
015100010123     C                   movel     ERR(1)        DesERR
015200010123     C                   movel     Wcod          WValor
015300010413     C                   clear                   WValo1
015400010123     C                   Exsr      STPERR
015500010115     C                   Endif
015600010115      *
015700010123      *  Attenzione da questa routine si può uscire avendo trovato il codice
015800010123      *  Partner in modo parziale e quindi, solo con il dettaglio, è possibile
015900010123      *  capire a quale Nazione appartengono gli stati delle spedizioni di questa
016000010123      *  e poi individuare a quale filiale inviare questa parte di Flat File.
016100010123      *
016200010115     C                   Endsr
016300010123     C*----------------------------------------------------------------
016400010123     C*? STPERR: STAMPA ERRORE
016500010123     C*----------------------------------------------------------------
016600010123     C     STPERR        BEGSR
016700010123     C*
016800010123     C     *INOF         IFEQ      '1'
016900010123     C                   EXCEPT    TESTA
017000010123     C                   EXCEPT    ERRCLI
017100010123     C                   END
017200010413     C*
017300010123     C                   EXCEPT    DETER
017400010413     C*
017500010413     C     Wvalo1        IFne      *blank
017600010413     C                   EXCEPT    MAXDET
017700010413     C                   End
017800010321     C*
017900010123     C                   ENDSR
018000010123     C*----------------------------------------------------------------
018100010123     C*? Chiude il File di Work prima di spedirlo alla Filiale
018200010123     C*----------------------------------------------------------------
018300050607     C     File_Filiale  BEGSR
018400010123      *
018500010124      * ---------------
018600010123     C                   Clear                   AS400
018700010123      * ---------------
018800021016      *   se aveva reperito correttamente il partner può anche
018900021016      *    decodificare la filiale a cui mandare questa parte di Flat file
019000021016     C                   if        WptOK    = 'S'
019100021016      *
019200020904      *   Filiale Elaboratore verrà determinata con la LNP
019300020904      *    utilizzando il nuovo trasmettitore x AS Unico
019400020904     C                   Eval      AS400 = SLnp
019500010123      *
019600010413     C                   else
019700010413      *  Problemi di reperimento del Partner
019800010413     C                   movel     ERR(3)        DesERR
019900010413     C                   movel     Wcod          WValor
020000010413     C                   clear                   WValo1
020100010413     C                   Eval      WValo1 = 'Wptok = *blank'
020200010413     C                   Exsr      STPERR
020300010123     C                   end
020400010123      * ---------------
020500010123     C                   IF        As400 > 0
020600010124      *
020700050607      *   Copia i dati arrivati dal FIFES00W al FIFEST0F della FILTRAGRU
020800050607      *     File Multimembro come era gestito i vecchi STATUS dell'EDI
020900050607     c                   CALL      'TRTCF1C0'
021000050607      *
021100010124     C                   ELSE
021200010124      *
021300021016      *  Se il Partner non è stato trovato sulla tabella PT non devono andar
021400021016      *  perduti i records riportati sul WorkFile.
021500021016     C                   MOVE      Data          Datalfa           6
021600021016     C                   Time                    Oora              6 0
021700021016     C                   MOVE      Oora          Oraalfa           6
021800021016     C                   Clear                   Membro           10
021900021016     C                   eval      Membro= 'M' + %subst(Datalfa:6:1)  +
022000021016     C                             %subst(Datalfa:3:2) + %subst(Datalfa:1:2) +
022100021016     C                             %subst(Oraalfa:1:2) + %subst(Oraalfa:3:2)
022200021016      *
022300010124      *  Li copia in un File di appoggio per controllare l'errore
022400010319     C                   clear                   MSGDS
022500010319     C                   MOVEL     CMD(2)        MSGDS
022600021016      * CPYF FROMFILE(FIFES00W) TOFILE(FIFES00WE)
022700010319     C                   eval      MSGDS = %trim(MSGDS) + %trim(Membro) +
022800010319     C                             ') MBROPT(*ADD) CRTFILE(*YES)'
022900010319     C                   Z-ADD     100           LENGH
023000010124     C                   CALL      'QCMDEXC'
023100010124     C                   PARM                    MSGDS
023200010124     C                   PARM                    LENGH
023300010413      *
023400010703      * SNDPGMMSG di avviso errore TOUSR(EDPAB)
023500010703     C                   clear                   MSGDS
023600010703     C                   MOVEL     CMD(8)        MSGDS
023700010703     C                   eval      MSGDS = %trim(MSGDS) +
023800010905     C                             ' TOUSR('+
023900010905     C                             %trim(USRMSG) +')'
024000010703     C                   Z-ADD     100           LENGH
024100010703     C                   CALL      'QCMDEXC'                            99
024200010703     C                   PARM                    MSGDS
024300010703     C                   PARM                    LENGH
024400010703      *
024500010703      * SNDBRKMSG di avviso errore TOUSR(DSP01)
024600010703     C                   clear                   MSGDS
024700010703     C                   MOVEL     CMD(9)        MSGDS
024800010703     C                   eval      MSGDS = %trim(MSGDS) +
024900010703     C                             ' TOMSGQ(DSP01)'
025000010703     C                   Z-ADD     100           LENGH
025100010703     C                   CALL      'QCMDEXC'                            99
025200010703     C                   PARM                    MSGDS
025300010703     C                   PARM                    LENGH
025400010703      *
025500010413      *  Problemi di reperimento del Partner
025600010413     C                   movel     ERR(4)        DesERR
025700010413     C                   movel     Wcod          WValor
025800010413     C                   clear                   WValo1
025900010413     C                   Eval      WValo1 = 'Vedi membro ' + %trim(Membro)
026000010413     C                   Exsr      STPERR
026100010124      *
026200010123     C                   ENDIF
026300010123      * ---------------
026400010123      *   ed in fine:
026500010123      *    Pulisce i Flags che pilotano delle funzioni interne al ciclo di lettura
026600010123     C                   eval      PrimoDet = *blank
026700010123     C                   eval      WptOK    = *blank
026800010123      *
026900010123     C                   Endsr
027000001003      *----------------------------------------------------------------
027100001003     C*? *INZSR - OPERAZIONI INIZIALI
027200001003     C*----------------------------------------------------------------
027300001003     C     *INZSR        BEGSR
027400001003     C*
027500010320     C* Riceve un parametro per capire se si trova in ambiente di produzione
027600010320     C*  o in ambiente di test
027700010320     C     *entry        plist
027800010320     C                   parm                    flgtst            1
027900010906     C                   parm                    UsrMsg           10
028000001003      *
028100001003      * RECUPERO DATI DELL'UTENTE
028200001003     C                   Z-ADD     1             CODUT
028300001003     C                   CALL      'XPARUT'
028400001003     C                   PARM                    UTEDSE0F
028500010123     C                   MOVEL     RAGUT         RSUT             20
028600010123     C                   Seton                                        OF
028700001003      *
028800001003      * Caricamento Tabella Partner esteri
028900001003      * ------------
029000001003     C                   Z-ADD     0             X
029100010307      *
029200020904      * Punta alle Tabelle in Lista di Librerie
029300010319     C                   clear                   MSGDS
029400010307     C                   MOVEL     CMD(4)        MSGDS
029500010319     C                   Z-ADD     100           LENGH
029600010307     C                   CALL      'QCMDEXC'
029700010307     C                   PARM                    MSGDS
029800010307     C                   PARM                    LENGH
029900010307      *
030000010307      * Carica la parte BARTOLINI
030100010307     C                   OPEN      EDTAB01L
030200001003     C                   MOVEL     'PT'          TABCOD
030300001006     C     TABCOD        SETLL     EDTAB01L
030400001006     C     TABCOD        READE     EDTAB01L
030500001003     C                   DOW       not %EOF(EDTAB01L)
030600001003     C     TABFLG        IFEQ      *BLANKS
030700001003     C                   ADD       1             X
030800001003     C                   MOVEL     TABKEY        CPT(X)
030900001003     C                   eval      CPTparz(X) = %subst(TABKEY:1:30)
031000001003     C                   MOVEL     TABUNI        DPT(X)
031100001003     C                   End
031200001003     C     TABCOD        READE     EDTAB01L
031300001003     C                   ENDdo
031400010402     C                   Z-add     X             SavX
031500010123     C*
031600010123     C                   MOVEL     'PU'          TABCOD
031700010123     C     TABCOD        SETLL     EDTAB01L
031800010123     C     TABCOD        READE     EDTAB01L
031900010123     C                   DOW       not %EOF(EDTAB01L)
032000010123     C     TABFLG        IFEQ      *BLANKS
032100010123     C                   Z-ADD     1             X
032200010123     C     TABKEY        LOOKUP    CPT(X)                                 32
032300010123     C   32              MOVEL     TABUNI        DPU(X)
032400010123     C                   END
032500010123     C     TABCOD        READE     EDTAB01L                               30
032600010123     C                   END
032700010307     C                   CLOSE     EDTAB01L
032800001003     C*
032900010307      * Toglie l'OVRRIDE
033000010319     C                   clear                   MSGDS
033100010307     C                   MOVEL     CMD(3)        MSGDS
033200010319     C                   Z-ADD     100           LENGH
033300010307     C                   CALL      'QCMDEXC'
033400010307     C                   PARM                    MSGDS
033500010307     C                   PARM                    LENGH
033600010307     C*
033700010123     C* RECUPERO DATA E ORA
033800010123     C                   TIME                    WHHDAT           14 0
033900010123     C                   TIME                    WHHD12           12 0
034000010123     C                   MOVE      WHHDAT        DATA8             8 0
034100010123     C                   MOVE      WHHD12        DATA6             6 0
034200010123     C                   MOVEL     DATA6         DATA              6 0
034300010123     C                   TIME                    WORA              6 0
034400010123     C*
034500001003     C                   ENDSR
034600001002      *****************************************************************
034700010123     OQSYSPRT   E            TESTA            01
034800010123     O                       RSUT                20
034900010123     O                                           46 '** ERRORI RICEZIONE Status'
035000021016     O                                              '  F E D E X  in Sede'
035100010123     O                                              ' **'
035200010123     O                       DATA               121 '  /  /  '
035300010123     O                                          127 'Pag.:'
035400010123     O                       PAGE          Z    132
035500010123     O          E            TESTA          1 02
035600010123     O                       WORA               121 '  :  :  '
035700021016     O                                          132 'TRTCF0R'
035800010123     O*------------------------------------
035900010123     O          E            ERRCLI         1
036000010123     O                                           15 'CODICE CLIENTE:'
036100010123     O                       §PTKSC        Z     23
036200010123     O                       WCOD                60
036300010123     O*------------------------------------
036400010123     O          E            DETER          1
036500010123     O                       DESERR              78
036600010123     O                       WVALOR             113
036700010413     O*------------------------------------
036800010413     O          E            MAXDET         1
036900010413     O                       WVALO1             113
037000010123     O*------------------------------------
037100010123** (ERR)
037200010123Il  Partner  NON è stato trovato nella Tabella "PT" con questo codice:
037300021016La Linea di Partenza della filiale non è corretta   filiale con linea:
037400010413Il  Partner  NON è stato trovato nemmeno parzialmente per  il  codice:
037500021016Non  sono  state mandati records alla filiale vedere invio FEDEX089  :
037600010124** (CMD)
037700021016
037800021016CPYF FROMFILE(FIFES00W) TOFILE(FIFES00WE) TOMBR(
037900010402DLTOVR FILE(EDTAB01L)
038000020904OVRDBF FILE(EDTAB01L) TOFILE(*LIBL/EDTAB01L)
038100020904
038200020904
038300020904
038400021016SNDPGMMSG MSG('ATTENZIONE Ricez.FEDEX- Controllare ricezione')
038500021016SNDBRKMSG MSG('ATTENZIONE Ricez.FEDEX- Controllare ricezione')
