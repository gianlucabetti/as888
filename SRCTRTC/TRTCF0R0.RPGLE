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
001500121102     D CPT             S             35    DIM(200)
001600121102     D CPTparz         S             35    DIM(%Elem(CPT))
001700121102     D DPT             S             90    DIM(%Elem(CPT))
001800010123      *---------------------------------------------------------------------*
001900010123      * Comandi
002000010123      *---------------------------------------------------------------------*
002100010123     D ERR             S             78    DIM(5) CTDATA PERRCD(1)
002200010703     D CMD             S             78    DIM(9) CTDATA PERRCD(1)
002300000929      *---------------------------------------------------------------------*
002400001005      * Flag oprativi di work
002500010123     D PrimoDet        S              1    INZ(' ')
002600001005      *---------------------------------------------------------------------*
002700001003     D WCOD            S             35
002800020904     D W035A           S             35
002900010123     D DesErr          S             78
003000010123     D WValor          S             35
003100010413     D WValo1          S             35
003200010123     D WCOM14          S             14
003300010123     D WCOM16          S             16
003400010123     D Sksc            s              7  0 INZ(0)
003500010413     D ALnp            s              3    INZ(' ')
003600010123     D SLnp            s              3  0 INZ(0)
003700001003     D Wptparz         s              1
003800010123     D WptOK           s              1    INZ(' ')
003900010419     D WkeybolOK       s              1    INZ(' ')
004000010115      *---------------------------------------------------------------------*
004100010319     D  MSGDS          S            100
004200010124     D  LENGH          S             15  5
004300010115      *---------------------------------------------------------------------*
004400001003     D X               s              3  0 INZ(0)
004500001005     D Y               s              3  0 INZ(0)
004600010402     D SavX            s              3  0 INZ(0)
004700010115      *---------------------------------------------------------------------*
004800020904     D As400           s              3  0
004900010123      *---------------------------------------------------------------------*
005000010123     D Digits          C                   CONST('0123456789')
005100021016     D FEDEX089        C                   CONST('FEDEX089                    ')
005200000120      *---------------------------------------------------------------------*
005300000120      * DS
005400000120      *---------------------------------------------------------------------*
005500010123     D EDIDSPT       E DS
005600121102     D TRUL0SDS      E DS
005700121102     D KPJBA         E DS
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
011000010115      *
011100021016      * Imposta la ricerca della PT
011200021016     C                   clear                   WCOD             35
011300021016     C                   Movel     FEDEX089      WCOD
011400021016     C                   clear                   Wqual             4
011500021016     C                   Move      Wqual         WCOD
011600021016      *
011700010115     C                   Z-ADD     1             X
011800010115     C     WCOD          LOOKUP    CPT(X)                                 33
011900010115      *
012000021016      *  se lo trova subito o se non lo trova affatto lo ripulisce
012100010115     c                   clear                   WPTparz
012200010115      *
012300010123      *  Prova a cercarlo con chiave parziale
012400010115     C                   If        *IN33 = *off
012500010123     C                   eval      X = 1
012600021016     C                   eval      W035A = %subst(wcod:1:31)
012700010115     C     W035A         lookup    CPTparz(X)                             33
012800010115      * se lo trova però deve individuarlo con maggior precisione
012900010115      *  in un secondo momento con la nazione del mittente
013000010123      *                           ================
013100010115     c   33              eval      WPTparz = 'S'
013200010123      *                           ================
013300010115     C                   Endif
013400010115      *
013500010115      *  Salva dalla tabella "PT" il cliente solo se è stata presa direttamente
013600010115      *   o in modo parziale.
013700010115     C                   If        *IN33 = *on
013800010115     C                   MOVEL     DPT(X)        EDIDSPT
013900010123     C                   MOVEL     §PTLNP        SLnp
014000010123     C                   z-add     §PTKSC        SKsc
014100010123      *
014200010123      *  Solo se non parziale
014300010123     c                   If        WPTparz <> 'S'
014400010123     C                   movel     'S'           WptOK
014500010123     C                   Endif
014600010123      *
014700010123     C                   Else
014800010123      *  Problemi di reperimento del Partner
014900010123     C                   movel     ERR(1)        DesERR
015000010123     C                   movel     Wcod          WValor
015100010413     C                   clear                   WValo1
015200010123     C                   Exsr      STPERR
015300010115     C                   Endif
015400010115      *
015500010123      *  Attenzione da questa routine si può uscire avendo trovato il codice
015600010123      *  Partner in modo parziale e quindi, solo con il dettaglio, è possibile
015700010123      *  capire a quale Nazione appartengono gli stati delle spedizioni di questa
015800010123      *  e poi individuare a quale filiale inviare questa parte di Flat File.
015900010123      *
016000010115     C                   Endsr
016100010123     C*----------------------------------------------------------------
016200010123     C*? STPERR: STAMPA ERRORE
016300010123     C*----------------------------------------------------------------
016400010123     C     STPERR        BEGSR
016500010123     C*
016600010123     C     *INOF         IFEQ      '1'
016700010123     C                   EXCEPT    TESTA
016800010123     C                   EXCEPT    ERRCLI
016900010123     C                   END
017000010413     C*
017100010123     C                   EXCEPT    DETER
017200010413     C*
017300010413     C     Wvalo1        IFne      *blank
017400010413     C                   EXCEPT    MAXDET
017500010413     C                   End
017600010321     C*
017700010123     C                   ENDSR
017800010123     C*----------------------------------------------------------------
017900010123     C*? Chiude il File di Work prima di spedirlo alla Filiale
018000010123     C*----------------------------------------------------------------
018100050607     C     File_Filiale  BEGSR
018200010123      *
018300010124      * ---------------
018400010123     C                   Clear                   AS400
018500010123      * ---------------
018600021016      *   se aveva reperito correttamente il partner può anche
018700021016      *    decodificare la filiale a cui mandare questa parte di Flat file
018800021016     C                   if        WptOK    = 'S'
018900021016      *
019000020904      *   Filiale Elaboratore verrà determinata con la LNP
019100020904      *    utilizzando il nuovo trasmettitore x AS Unico
019200020904     C                   Eval      AS400 = SLnp
019300010123      *
019400010413     C                   else
019500010413      *  Problemi di reperimento del Partner
019600010413     C                   movel     ERR(3)        DesERR
019700010413     C                   movel     Wcod          WValor
019800010413     C                   clear                   WValo1
019900010413     C                   Eval      WValo1 = 'Wptok = *blank'
020000010413     C                   Exsr      STPERR
020100010123     C                   end
020200010123      * ---------------
020300010123     C                   IF        As400 > 0
020400010124      *
020500050607      *   Copia i dati arrivati dal FIFES00W al FIFEST0F della FILTRAGRU
020600050607      *     File Multimembro come era gestito i vecchi STATUS dell'EDI
020700050607     c                   CALL      'TRTCF1C0'
020800050607      *
020900010124     C                   ELSE
021000010124      *
021100021016      *  Se il Partner non è stato trovato sulla tabella PT non devono andar
021200021016      *  perduti i records riportati sul WorkFile.
021300021016     C                   MOVE      Data          Datalfa           6
021400021016     C                   Time                    Oora              6 0
021500021016     C                   MOVE      Oora          Oraalfa           6
021600021016     C                   Clear                   Membro           10
021700021016     C                   eval      Membro= 'M' + %subst(Datalfa:6:1)  +
021800021016     C                             %subst(Datalfa:3:2) + %subst(Datalfa:1:2) +
021900021016     C                             %subst(Oraalfa:1:2) + %subst(Oraalfa:3:2)
022000021016      *
022100010124      *  Li copia in un File di appoggio per controllare l'errore
022200010319     C                   clear                   MSGDS
022300010319     C                   MOVEL     CMD(2)        MSGDS
022400021016      * CPYF FROMFILE(FIFES00W) TOFILE(FIFES00WE)
022500010319     C                   eval      MSGDS = %trim(MSGDS) + %trim(Membro) +
022600010319     C                             ') MBROPT(*ADD) CRTFILE(*YES)'
022700010319     C                   Z-ADD     100           LENGH
022800010124     C                   CALL      'QCMDEXC'
022900010124     C                   PARM                    MSGDS
023000010124     C                   PARM                    LENGH
023100010413      *
023200010703      * SNDPGMMSG di avviso errore TOUSR(EDPAB)
023300010703     C                   clear                   MSGDS
023400010703     C                   MOVEL     CMD(8)        MSGDS
023500010703     C                   eval      MSGDS = %trim(MSGDS) +
023600010905     C                             ' TOUSR('+
023700010905     C                             %trim(USRMSG) +')'
023800010703     C                   Z-ADD     100           LENGH
023900010703     C                   CALL      'QCMDEXC'                            99
024000010703     C                   PARM                    MSGDS
024100010703     C                   PARM                    LENGH
024200010703      *
024300010703      * SNDBRKMSG di avviso errore TOUSR(DSP01)
024400010703     C                   clear                   MSGDS
024500010703     C                   MOVEL     CMD(9)        MSGDS
024600010703     C                   eval      MSGDS = %trim(MSGDS) +
024700010703     C                             ' TOMSGQ(DSP01)'
024800010703     C                   Z-ADD     100           LENGH
024900010703     C                   CALL      'QCMDEXC'                            99
025000010703     C                   PARM                    MSGDS
025100010703     C                   PARM                    LENGH
025200010703      *
025300010413      *  Problemi di reperimento del Partner
025400010413     C                   movel     ERR(4)        DesERR
025500010413     C                   movel     Wcod          WValor
025600010413     C                   clear                   WValo1
025700010413     C                   Eval      WValo1 = 'Vedi membro ' + %trim(Membro)
025800010413     C                   Exsr      STPERR
025900010124      *
026000010123     C                   ENDIF
026100010123      * ---------------
026200010123      *   ed in fine:
026300010123      *    Pulisce i Flags che pilotano delle funzioni interne al ciclo di lettura
026400010123     C                   eval      PrimoDet = *blank
026500010123     C                   eval      WptOK    = *blank
026600010123      *
026700010123     C                   Endsr
026800001003      *----------------------------------------------------------------
026900001003     C*? *INZSR - OPERAZIONI INIZIALI
027000001003     C*----------------------------------------------------------------
027100001003     C     *INZSR        BEGSR
027200001003     C*
027300010320     C* Riceve un parametro per capire se si trova in ambiente di produzione
027400010320     C*  o in ambiente di test
027500010320     C     *entry        plist
027600010320     C                   parm                    flgtst            1
027700010906     C                   parm                    UsrMsg           10
027800001003      *
027900001003      * RECUPERO DATI DELL'UTENTE
028000001003     C                   Z-ADD     1             CODUT
028100001003     C                   CALL      'XPARUT'
028200001003     C                   PARM                    UTEDSE0F
028300010123     C                   MOVEL     RAGUT         RSUT             20
028400010123     C                   Seton                                        OF
028500001003      *
028600001003      * Caricamento Tabella Partner esteri
028700001003      * ------------
028800001003     C                   Z-ADD     0             X
028900010307      *
029000020904      * Punta alle Tabelle in Lista di Librerie
029100010319     C                   clear                   MSGDS
029200010307     C                   MOVEL     CMD(4)        MSGDS
029300010319     C                   Z-ADD     100           LENGH
029400010307     C                   CALL      'QCMDEXC'
029500010307     C                   PARM                    MSGDS
029600010307     C                   PARM                    LENGH
029700010307      *
029800010307      * Carica la parte BARTOLINI
029900010307     C                   OPEN      EDTAB01L
030000001003     C                   MOVEL     'PT'          TABCOD
030100001006     C     TABCOD        SETLL     EDTAB01L
030200001006     C     TABCOD        READE     EDTAB01L
030300001003     C                   DOW       not %EOF(EDTAB01L)
030400001003     C     TABFLG        IFEQ      *BLANKS
030500001003     C                   ADD       1             X
030600001003     C                   MOVEL     TABKEY        CPT(X)
030700001003     C                   eval      CPTparz(X) = %subst(TABKEY:1:30)
030800001003     C                   MOVEL     TABUNI        DPT(X)
030900001003     C                   End
031000001003     C     TABCOD        READE     EDTAB01L
031100001003     C                   ENDdo
031200010402     C                   Z-add     X             SavX
031300121102      *
031400121102      * Controlla riempimento della PT x limiti di schiera
031500121102      *  se deve inviare la mail di alert x limite quasi raggiunto.
031600121102     c                   exsr      ChecK_PT
031700010123     C*
031800010307     C                   CLOSE     EDTAB01L
031900001003     C*
032000010307      * Toglie l'OVRRIDE
032100010319     C                   clear                   MSGDS
032200010307     C                   MOVEL     CMD(3)        MSGDS
032300010319     C                   Z-ADD     100           LENGH
032400010307     C                   CALL      'QCMDEXC'
032500010307     C                   PARM                    MSGDS
032600010307     C                   PARM                    LENGH
032700010307     C*
032800010123     C* RECUPERO DATA E ORA
032900010123     C                   TIME                    WHHDAT           14 0
033000010123     C                   TIME                    WHHD12           12 0
033100010123     C                   MOVE      WHHDAT        DATA8             8 0
033200010123     C                   MOVE      WHHD12        DATA6             6 0
033300010123     C                   MOVEL     DATA6         DATA              6 0
033400010123     C                   TIME                    WORA              6 0
033500010123     C*
033600001003     C                   ENDSR
033700121102      *---------------------------------------------------------------*
033800121102      * *                                                            -*
033900121102      *---------------------------------------------------------------*
034000121102     C     Check_PT      begsr
034100121102     C*
034200121102     c                   clear                   kpjba
034300121102     c* Controllo riempimento schiera
034400121102     c                   clear                   trul0sds
034500121102     c                   eval      i0sski='CPT'
034600121102     c                   eval      i0sele=%elem(CPT)
034700121102     c                   eval      i0spie=x
034800121102     c                   eval      i0sfile='EDTAB00F'
034900121102     c                   eval      i0ssif=knsif
035000121102     c                   eval      i0spgm='TRTCF0R0'
035100121102     c                   movel     trul0sds      kpjbu
035200121102     c                   call      'TRUL0SR'
035300121102     c                   parm                    kpjba
035400121102     C*
035500121102     C                   ENDSR
035600001002      *****************************************************************
035700010123     OQSYSPRT   E            TESTA            01
035800010123     O                       RSUT                20
035900010123     O                                           46 '** ERRORI RICEZIONE Status'
036000021016     O                                              '  F E D E X  in Sede'
036100010123     O                                              ' **'
036200010123     O                       DATA               121 '  /  /  '
036300010123     O                                          127 'Pag.:'
036400010123     O                       PAGE          Z    132
036500010123     O          E            TESTA          1 02
036600010123     O                       WORA               121 '  :  :  '
036700021016     O                                          132 'TRTCF0R'
036800010123     O*------------------------------------
036900010123     O          E            ERRCLI         1
037000010123     O                                           15 'CODICE CLIENTE:'
037100010123     O                       §PTKSC        Z     23
037200010123     O                       WCOD                60
037300010123     O*------------------------------------
037400010123     O          E            DETER          1
037500010123     O                       DESERR              78
037600010123     O                       WVALOR             113
037700010413     O*------------------------------------
037800010413     O          E            MAXDET         1
037900010413     O                       WVALO1             113
038000010123     O*------------------------------------
038100010123** (ERR)
038200010123Il  Partner  NON è stato trovato nella Tabella "PT" con questo codice:
038300021016La Linea di Partenza della filiale non è corretta   filiale con linea:
038400010413Il  Partner  NON è stato trovato nemmeno parzialmente per  il  codice:
038500021016Non  sono  state mandati records alla filiale vedere invio FEDEX089  :
038600010124** (CMD)
038700021016
038800021016CPYF FROMFILE(FIFES00W) TOFILE(FIFES00WE) TOMBR(
038900010402DLTOVR FILE(EDTAB01L)
039000020904OVRDBF FILE(EDTAB01L) TOFILE(*LIBL/EDTAB01L)
039100020904
039200020904
039300020904
039400021016SNDPGMMSG MSG('ATTENZIONE Ricez.FEDEX- Controllare ricezione')
039500021016SNDBRKMSG MSG('ATTENZIONE Ricez.FEDEX- Controllare ricezione')
