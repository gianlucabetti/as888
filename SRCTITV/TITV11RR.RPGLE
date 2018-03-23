000100090317      /TITLE Upload via Internet: traduzione in FIVABWWR. (x SPECCHIASOL Cod. 0433625)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400090330     Ftntbe01l  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021025     FFIVABwwr  O    E             DISK    usropn
000800020409     Ftitv11p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000020409     Ftitv11ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000090330     D dosr          e ds                  inz
002100090330     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
002200090330     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
002300090330     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
002400090330     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
002500090330     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
002600090330     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
002700090330     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
002800090330     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
002900090330     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
003000090330     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
003100090330     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
003200010330     D dscmz         e ds                  inz
003300010330     D psds           sds
003400010330     D  procname         *PROC
003500010330     D tivlrds       e ds                  extname(tivlr00f)
003600090330     D fivabds       e ds                  extname(fivab00f)
003700090330     D tivlrdsPar    e ds                  extname(tivlr00f) prefix(P_)
003800010330     D esito           s              1
003900010330     D prmlit          s             10
004000010330     D prmfir          s             10
004100010330     D wrkesito        s                   like(esito)
004200010330     D wrkdata         s               d
004300010330     D wrkora          s               t
004400010330     D rrnum           s              6  0 INZ(*zeros)
004500010330     D recko           s            150    INZ(*blanks)
004600011113     D depcmd          s            150    INZ(*blanks)
004700090330     D wCTM            s                   LIKE(vabCTM)
004800090330     D wNSR            s              2    INZ
004900090330     D wCRC            s             10    INZ
005000090330     D wKSC            s              7    INZ
005100090330     D wCTR            s              3    INZ
005200090330     D wfivabds        s                   LIKE(fivabds)
005300010330     D*------------------
005400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500010330     D*------------------
005600010330     D WLBDA8          DS                  INZ
005700010330     D  G08DAT                 1      8  0
005800010330     D  G08INV                 9     16  0
005900010330     D  G08ERR                17     17
006000010330     D  G08TGI                18     22  0
006100000313     D*
006200090330     D*------------------
006300090330     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
006400090330     D*------------------
006500090330     D SkCliOSR        s             10    DIM(900)
006600090330     D*
006700090330     D DsCliOSR        ds
006800090330     D  wLnpOSR                       3
006900090330     D  wCliOSR                       7
007000090330     D*
007100090330     D SkLnpNrsOSR     s              5    DIM(900)
007200090330
007300010330
007400010330
007500010330
007600090330     C*
007700090330     C* CARICO LE TABELLE OCCORRENTI
007800090330     C                   exsr      CARTAB
007900090330     C*
008000990915     C                   time                    wrkdata
008100990915     C                   time                    wrkora
008200000913     C                   reset                   rrnum
008300990921     C                   reset                   esito
008400990921     C                   reset                   wrkesito
008500010601     C*
008600010601     C                   exsr      opeini
008700010605     C                   exsr      rwvab
008800010601     C*
008900010601     C                   seton                                        lr
009000010601
009100010601
009200010601
009300010601
009400010601     C*--------------------------------------------------------
009500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009600010601     C*--------------------------------------------------------
009700010601     C     PREELA        BEGSR
009800010601     C*
009900010601     C* SE OCCORRE SPEDIRE IN FILIALE
010000010601     C                   if        invfil <> *zeros and
010100010601     C                             flgGiro = '0'
010200010601     C*
010300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010400010601     C                   eval      flgGiro = '1'
010500000724     C*
010600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010800010601     C     invfil        chain     azorg01l
010900000724     C                   if        %found
011000000724     C                   movel(p)  CMD(1)        depcmd
011100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011200000724     C*
011300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000724     C                   PARM                    depcmd
011700000724     C                   PARM                    LENGH
011800000724     C*
011900000724     C                   endif
012000000616     C*
012100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012200000616     C                   movel(p)  CMD(2)        depcmd
012300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012400000616     C*
012500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012600011113     C                   Z-ADD     150           LENGH            15 5
012700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012800000616     C                   PARM                    depcmd
012900000616     C                   PARM                    LENGH
013000000613     C*
013100020409     C                   if        not %open(titv11ps)
013200020409     C                   open      titv11ps
013300000616     C                   except    testdett
013400000613     C                   endif
013500010601     C*
013600010601     C                   endif
013700010601     C*
013800010601     C                   ENDSR
013900010601     C***
014000010601
014100010601
014200010601
014300010601
014400010601
014500010601
014600010601
014700010601     C*--------------------------------------------------------
014800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014900010601     C*--------------------------------------------------------
015000010601     C     ENDELA        BEGSR
015100990915     C*
015200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015300000613     C*
015400020409     C                   if        %open(titv11ps)
015500000616     C                   except    findett
015600020409     C                   close     titv11ps
015700000613     C                   endif
015800000616     C*
015900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016000011113     C                   Z-ADD     150           LENGH            15 5
016100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016200000616     C                   PARM                    CMD(3)
016300000616     C                   PARM                    LENGH
016400090330     C*
016500090330     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
016600090330     C   54              call(e)   'TITV84R'
016700090330     C                   parm      'C'           InTipl
016800090330     C                   parm                    tivlrdsPar
016900090330     C                   parm                    wfivabds
017000000616     C*
017100010601     C                   ENDSR
017200010601     C***
017300010601
017400010601
017500010601
017600010601
017700000613
017800000613     C*--------------------------------------------------------
017900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018000000613     C*--------------------------------------------------------
018100000621     C     STPR          BEGSR
018200000613     C*
018300020409     C                   if        not %open(titv11p)
018400020409     C                   open      titv11p
018500990915     C                   endif
018600990915     C*
018700990915     C                   except    riepilogo
018800990915     C*
018900020409     C                   if        %open(titv11p)
019000020409     C                   close     titv11p
019100990914     C                   endif
019200000613     C*
019300000613     C                   ENDSR
019400000613     C***
019500000613
019600010330
019700010601
019800010601
019900010601
020000010330     C*--------------------------------------------------------
020100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020200010330     C*--------------------------------------------------------
020300010605     C     RWVAB         BEGSR
020400010330     C*
020500010330     C                   if        not %open(tivin00r)
020600010330     C                   open      tivin00r
020700010330     C                   endif
020800021025     C                   if        not %open(fivabwwr)
020900021025     C                   open      fivabwwr
021000010330     C                   endif
021100010330     C*
021200010604     C                   clear                   §CTROK
021300010604     C                   clear                   §CTRMO
021400010604     C                   clear                   §CTRNO
021500010330     C*
021600010330     C                   DO        *HIVAL
021700010330     C*
021800010330     C                   READ      tivin00r                               70
021900010618     C*
022000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
022100010618     C                   if        vinflg = '1'
022200010618     C                   eval      flgOk = '1'
022300010618     C                   endif
022400010618     C*
022500010330     C                   if        vindta > *blanks
022600010330     C                   add       1             rrnum
022700010330     C*
022800010601     C                   if        *in70 = *off and
022900010330     C                             (vinflg = *blanks
023000010330     C                              or vinflg = '0'
023100010330     C                              or vinflg = '2')
023200010330     C*
023300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023400010711     C                   if        vinflg = *blanks or vinflg = '0'
023500010711     C                   clear                   vinmsg
023600010711     C                   endif
023700010601     C*
023800010330     C                   exsr      inzvar
023900010330     C                   exsr      defcam
024000010605     C                   exsr      impvab
024100010601     C*
024200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024300010605     C                   eval      depfil = VABLNP
024400010601     C                   exsr      repfil
024500010601     C                   if        depfil = invfil
024600021025     C                   if        vlrpoi = 999
024700021025     C                   MOVE(P)   invfil        VABFGS
024800021025     C                   else
024900021025     C                   MOVE(P)   vlrpoi        VABFGS
025000021025     C                   endif
025100010601     C*
025200010601     C                   exsr      PREELA
025300090330     C*
025400090330     C* Se il codice trattamento merce nn viene passato dal cliente lo valorizzo con il valore
025500090330     C* d default passato come parametro dall'anagrafica clienti/tipo file
025600090330     C                   IF        vabCTM = *blanks
025700090330     C                   EVAL      vabCTM = wCTM
025800090330     C                   ENDIF
025900090330     C*
026000090330     C* Se presente particolarità consegna 'RC' e se il codice
026100090330     C* cliente mittente è valorizzato e numero serie ORM nn passata
026200090330     C* nei parametri verifico se il cliente è abilitato in tabella 'OSR'
026300090330     C                   IF        VABGMA = 'RC'
026400090330     C                   IF        VABCCM > *ZEROS
026500090330     C                   IF        wNSR = *BLANKS
026600090330     C                   Z-ADD     1             jOSR
026700090330     C                   move(p)   VABLNP        wLnpOSR
026800090330     C                   move(p)   VABCCM        wCliOSR
026900090330     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
027000090330     C                   IF        %found
027100090330     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
027200090330     C                   ELSE
027300090330     C                   SETON                                        31
027400090330     C                   EVAL      vinmsg = %trimr(vinmsg)
027500090330     C                             + ' ' + 'ORMNRS(tabellaOSR)'
027600090330     C                   ENDIF
027700090330     C                   ENDIF
027800090330     C                   ENDIF
027900090330     C                   ENDIF
028000010601     C*
028100010604     C* Ebbene...
028200010604     C*
028300010604     C  N31              ADD       1             §CTROK            7 0
028400010604     C   32              ADD       1             §CTRMO            7 0
028500010604     C   31              ADD       1             §CTRNO            7 0
028600020205     C*
028700020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
028800021025     C  N31              WRITE     FIVAB000
028900090330     C*
029000090330     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
029100090330     C                   if        *IN31 = *OFF AND
029200090330     C                             vabGMA = 'RC'
029300090330     C*
029400090330     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
029500090330     C                   seton                                        54
029600090330     C*
029700090330     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
029800090330     C                   eval      wfivabds = fivabds
029900090330     C                   eval      tivlrdsPar = tivlrds
030000090330     C                   EVAL      P_vlrppt = *blanks
030100090330     C                   EVAL      %subst(P_vlrppt:4:2) = wNSR
030200090330     C                   call      'TITV84R'
030300090330     C                   parm      *blanks       InTipl            1
030400090330     C                   parm                    tivlrdsPar
030500090330     C                   parm                    wfivabds
030600090330     C*
030700090330     C                   endif
030800010604     C*
030900010604     C                   if        *in31 = *off and
031000010604     C                             *in32 = *off
031100010604     C                   eval      vinflg = '1'
031200010604     C                   else
031300010604     C                   eval      recko = vindta
031400010604     C                   except    rigadett
031500010604     C                   eval      vinflg = '2'
031600010604     C                   endif
031700010604     C                   endif
031800010601     C*
031900010604     C                   endif
032000010604     C*
032100010330     C                   else
032200010330     C                   eval      vinflg = '1'
032300010330     C                   endif
032400010601     C*
032500010601     C  N70              update    tivin000
032600010330     C*
032700010330     C  N70              ENDdo
032800010601     C
032900010601     C*
033000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033100010601     C                   if        cntNonEl = *zeros or
033200010601     C                             flgMulti = '0'
033300010330     C* Se non ci sono record con errori ...
033400010601     C                   if        §ctrno = 0 and
033500010604     C                             §ctrmo = 0 and
033600010601     C                             flgStato <> '2'
033700010330     C* ... restituisco esito OK.
033800010330     C                   eval      wrkesito = '0'
033900010330     C                   else
034000010330     C                   if        §ctrok > 0
034100010330     C                   eval      wrkesito = '1'
034200010330     C                   else
034300010615     C                   if        flgOk = '0'
034400010615     C                   eval      wrkesito = '2'
034500010615     C                   else
034600010615     C                   eval      wrkesito = '6'
034700010615     C                   endif
034800010330     C                   endif
034900010330     C                   endif
035000010601     C                   else
035100010601     C                   eval      wrkesito = '9'
035200010601     C                   endif
035300010330     C*
035400010330     C                   if        %open(tivin00r)
035500010330     C                   close     tivin00r
035600010330     C                   endif
035700021025     C                   if        %open(fivabwwr)
035800021025     C                   close     fivabwwr
035900010330     C                   endif
036000010601     C*
036100010601     C                   if        vlrpoi <> 999
036200010601     C                   eval      invfil = vlrpoi
036300010601     C                   endif
036400010330     C*
036500010330     C                   if        §ctrok > 0
036600010601     C                             and invfil > *zeros
036700010330     C                   exsr      invio
036800010330     C                   endif
036900010601     C*
037000010618     C                   if        flgGiro = '1'
037100010601     C                   exsr      endela
037200010618     C                   endif
037300010330     C*
037400010330     C                   ENDSR
037500010330     C***
037600010330
037700010601
037800010601
037900010601
038000010601
038100010330     C*----------------------------------------------------*
038200010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
038300010330     C*----------------------------------------------------*
038400010330     C     INZVAR        BEGSR
038500010330     C*
038600020204     C                   Z-ADD     *zeros        Num5_0            5 0
038700020213     C                   Z-ADD     *zeros        Num15_3          15 3
038800020204     C                   MOVEL     *blanks       WrkNote          70
038900020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
039000020322     C                   MOVEL     '0'           FlgCAS            1
039100010330     C*
039200010330     C                   ENDSR
039300010330     C*----------------------------------------------------*
039400090317     C*  IMPOSTAZIONE CAMPI COSTANTI
039500010330     C*----------------------------------------------------*
039600010330     C     DEFCAM        BEGSR
039700010330     C*
039800020204     C* Inizializzo il buffer del record da scrivere
039900021025     C                   CLEAR                   FIVAB000
040000020204     C* Imposto i valori di default...
040100020409     C                   EVAL      VABCCM = 0430946
040200020409     C                   EVAL      VABLNP = 043
040300020322     C                   EVAL      VABCTR = 000
040400090330     C                   EVAL      VABGMA = 'RC'
040500020204     C* ... e poi verifico se sono stati passati come parametri
040600020204     C                   IF        vlrppt > *blanks
040700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
040800020204     C                   EXSR      CHKNUM
040900020204     C                   IF        PiInt=*on
041000020204     C                   Z-ADD     PiVal         VABCCM
041100020204     C                   ENDIF
041200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
041300020204     C                   EXSR      CHKNUM
041400020204     C                   IF        PiInt=*on
041500020204     C                   Z-ADD     PiVal         VABLNP
041600020204     C                   ENDIF
041700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
041800020204     C                   EXSR      CHKNUM
041900020204     C                   IF        PiInt=*on
042000020204     C                   Z-ADD     PiVal         VABCTR
042100020204     C                   ENDIF
042200020204     C                   ENDIF
042300020204     C*
042400010330     C                   ENDSR
042500010607     C*----------------------------------------------------*
042600090317     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
042700010607     C*----------------------------------------------------*
042800010607     C     IMPVAB        BEGSR
042900010607     C*
043000010607     C                   SETOFF                                       3132
043100010607     C*
043200010607     C* Reperimento campi ALFA
043300010607     C*
043400010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
043500020409     C                   EVAL      VABRSD=%trim(%subst(vindta:5:40))
043600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
043800020117     C* ==
043900020409     C                   EVAL      VABIND=%trim(%subst(vindta:45:35))
044000020409     C                   EVAL      VABRD2=' Tel.'+%trim(%subst(vindta:80:15))
044100020409     C                   EVAL      VABLOD=%trim(%subst(vindta:100:30))
044200020409     C                   EVAL      VABPRD=%trim(%subst(vindta:130:2))
044300020409     C                   EVAL      VABNZD=*blanks
044400020409     C                   EVAL      VABRMA=%trim(%subst(vindta:307:6))
044500020410     C                   EVAL      VABVCA='EUR'
044600020409     C*
044700020409     C                   MOVEL     DATCOR        VABAAS
044800020409     C                   MOVE      DATCOR        VABMGS
044900010607     C*
045000010607     C* Reperimento campi NUMERICI
045100020409     C                   EVAL      PiStr=%trim(%subst(vindta:177:6))
045200020304     C                   EXSR      CHKNUM
045300020304     C                   IF        PiInt=*on
045400020409     C                   Z-ADD     PiVal         VABNSP
045500020304     C                   Z-ADD     PiVal         VABRMN
045600020304     C                   ELSE
045700020304     C                   SETON                                        32
045800020304     C                   Z-ADD     *zeros        VABNSP
045900020329     C                   Z-ADD     *zeros        VABRMN
046000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
046100020321     C                             + ' ' + 'VABNSP VABRMN'
046200020304     C                   ENDIF
046300020304     C*
046400020409     C                   EVAL      PiStr=%trim(%subst(vindta:95:5))
046500010607     C                   EXSR      CHKNUM
046600010607     C                   IF        PiInt=*on
046700020204     C                   Z-ADD     PiVal         Num5_0
046800020204     C                   MOVEL(p)  Num5_0        VABCAD
046900010607     C                   ELSE
047000010607     C                   SETON                                        32
047100020204     C                   EVAL      VABCAD = *zeros
047200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
047300020204     C                             + ' ' + 'VABCAD'
047400010607     C                   ENDIF
047500010607     C*
047600020409     C                   EVAL      PiStr=%trim(%subst(vindta:153:5))
047700010607     C                   EXSR      CHKNUM
047800010607     C                   IF        PiInt=*on
047900010607     C                   Z-ADD     PiVal         VABNCL
048000010607     C                   ELSE
048100010607     C                   SETON                                        32
048200010607     C                   Z-ADD     *zeros        VABNCL
048300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
048400010607     C                             + ' ' + 'VABNCL'
048500010607     C                   ENDIF
048600020304     C*
048700020409     C                   EVAL      PiStr=%trim(%subst(vindta:158:8))
048800020204     C                   EVAL      PiDecChr = ','
048900010607     C                   EXSR      CHKNUM
049000010607     C                   IF        PiNum=*on
049100020409     C                   Z-ADD(H)  PiVal         VABPKB
049200010607     C                   ELSE
049300010607     C                   SETON                                        32
049400010607     C                   Z-ADD     *zeros        VABPKB
049500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
049600010607     C                             + ' ' + 'VABPKB'
049700010607     C                   ENDIF
049800020304     C*
049900090320     C                   IF        %subst(vindta:268:15) <> *blanks AND
050000090320     C                             %subst(vindta:268:15) <> *all'0'
050100020409     C                   EVAL      PiStr=%trim(%subst(vindta:268:15))
050200020322     C* Detrmino che indipendentemente da giusto o sbagliato l'intenzione del cliente era d
050300020322     C* passare il contrassegno
050400020322     C                   MOVEL     '1'           FlgCAS
050500020322     C*
050600020322     C                   EXSR      CHKNUM
050700020322     C                   IF        PiNum=*on
050800020322     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
050900020409     C* riferisce ad un campo 15,3
051000090318     C***                EVAL(H)   VABCAS = PiVal/1000
051100090318     C                   EVAL      VABCAS = PiVal
051200020322     C                   ELSE
051300020322     C                   SETON                                        32
051400020322     C                   Z-ADD     *zeros        VABCAS
051500020322     C                   EVAL      vinmsg = %trimr(vinmsg)
051600020322     C                             + ' ' + 'VABCAS'
051700020322     C                   ENDIF
051800090320     C                   ENDIF
051900020322     C*
052000020322     C* Considerazioni finali su CBO/CAS
052100020322     C                   IF        FlgCAS = '1'
052200020322     C                   EVAL      VABCBO = '4'
052300020322     C                   ELSE
052400020322     C                   EVAL      VABCBO = '1'
052500020322     C                   ENDIF
052600020204     C*
052700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052800020204     C                   EXSR      CHKIMPDIV
052900020204     C*
053000010607     C                   ENDSR
053100010607     C*----------------------------------------------------*
053200010601
053300020204
053400020204     C*----------------------------------------------------*
053500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053600020204     C*----------------------------------------------------*
053700020204     C     CHKIMPDIV     BEGSR
053800020204     C*
053900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054000020204     C                   Z-ADD     *zeros        wrkDec            9 9
054100020204     C*
054200020204     C* Come prima cosa effettuo considerazioni sulla divisa
054300020204     C                   IF        vabIAS > *zeros
054400020204     C                   IF        vabVAS <> 'EUR'
054500020204     C                   EVAL      vabVAS =  'ITL'
054600020204     C                   ENDIF
054700020204     C                   ENDIF
054800020204     C*
054900020204     C                   IF        vabCAS > *zeros
055000020204     C                   IF        vabVCA <> 'EUR'
055100020204     C                   EVAL      vabVCA =  'ITL'
055200020204     C                   ENDIF
055300020204     C                   ENDIF
055400020204     C*
055500020204     C                   IF        vabVMD > *zeros
055600020321     C                   IF        vabVAD <> 'EUR'
055700020204     C                   EVAL      vabVAD =  'ITL'
055800020204     C                   ENDIF
055900020204     C                   ENDIF
056000020204     C*
056100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056200020204     C                   Z-ADD     vabIAS        wrkDec
056300020204     C                   IF        wrkDec > *zeros
056400020204     C                   IF        vabVAS = 'ITL'
056500020204     C                   EVAL      vabIAS = *zeros
056600020204     C                   ENDIF
056700020204     C                   ENDIF
056800020204     C*
056900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
057000020204     C                   Z-ADD     vabCAS        wrkDec
057100020204     C                   IF        wrkDec > *zeros
057200020204     C                   IF        vabVCA = 'ITL'
057300020204     C                   EVAL      vabCAS = *zeros
057400020204     C                   ENDIF
057500020204     C                   ENDIF
057600020204     C*
057700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057800020204     C                   Z-ADD     vabVMD        wrkDec
057900020204     C                   IF        wrkDec > *zeros
058000020204     C                   IF        vabVAD = 'ITL'
058100020204     C                   EVAL      vabVMD = *zeros
058200020204     C                   ENDIF
058300020204     C                   ENDIF
058400020204     C*
058500020204     C                   ENDSR
058600020204     C***
058700020204
058800010330
058900010330
059000010330     C*----------------------------------------------------*
059100010330     C*  CONTROLLO NUMERICITA' CAMPI
059200010330     C*----------------------------------------------------*
059300010330     C     CHKNUM        BEGSR
059400010330     C*
059500010606     C                   IF        PiDecChr = *blanks
059600010606     C                   EVAL      PiDecChr = ','
059700010606     C                   ENDIF
059800010606     C*
059900010606     C                   CALL(e)   'ISNUMERIC'
060000010330     C                   PARM                    PiStr            30
060100010606     C                   PARM                    PiDecChr          1
060200010330     C                   PARM      *ZEROS        PiVal            30 9
060300010330     C                   PARM      '0'           PiInt             1
060400010330     C                   PARM      '0'           PiNum             1
060500010330     C                   IF        %error
060600010606     C                   EVAL      PiNum=*off
060700010330     C                   ENDIF
060800010330     C*
060900010330     C                   ENDSR
061000010330     C***
061100010330
061200010601
061300010601
061400010601
061500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
061600010601     C     repfil        BEGSR
061700010601     C*
061800010601     C                   if        invfil = *zeros and
061900010601     C                             depfil > *zeros and
062000010629     C                             (vinflg = *blanks or
062100010629     C                              vinflg = *zeros)
062200010601     C
062300010601     C                   eval      invfil = depfil
062400010601     C                   endif
062500010601     C*
062600010601     C                   if        depfil <> invfil and
062700010601     C                             invfil > *zeros
062800010601     C                   eval      flgMulti = '1'
062900010601     C                   if        vinflg = *blanks
063000010601     C                   add       1             cntNonEl
063100010601     C                   endif
063200010601     C                   endif
063300010601     C*
063400010601     C                   if        vinflg = '2'
063500010601     C                   eval      flgStato = '2'
063600010601     C                   endif
063700010601     C*
063800010601     C                   ENDSR
063900010601     C***
064000010601
064100010601
064200010601
064300010330
064400010330
064500010330
064600990920      /TITLE Invio dei dati al punto operativo.
064700000613     C     invio         BEGSR
064800990920     C*
064900990920     C                   reset                   dscmz
065000010601     C                   move      invfil        cmzdst
065100990920     C                   eval      cmzfld = vlrfou
065200990920     C                   eval      cmzmbd = vlrhdl
065300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
065400000710     C                   if        prmfir = *blanks
065500021025     C                   eval      cmzfla = 'FIVAB00F'
065600021025     C                   eval      cmzmba = 'FIVAB00F'
065700000710     C                   else
065800000710     C                   eval      cmzfla = prmfir
065900000710     C                   eval      cmzmba = prmfir
066000000710     C                   endif
066100990920     C                   eval      cmznrr = *zeros
066200990920     C                   move      §ctrok        cmznrr
066300021018     C                   eval      cmzlba = vlrfl1
066400990920     C                   call(e)   'TIS711C'
066500990920     C                   parm                    dscmz
066600990921     C                   parm      *blanks       esito
066700990920     C                   if        %error
066800990920     C                             or cmzerr = '1'
066900990921     C                             or esito  = '1'
067000000710     C                   eval      wrkesito = '3'
067100990920     C                   endif
067200990920     C*
067300000613     C                   ENDSR
067400990910
067500010601
067600010601
067700010601
067800010601
067900010601      /TITLE Invio dei dati al punto operativo.
068000010601     C     opeini        BEGSR
068100010601     C*
068200010601     C* Inizializzo flag e contatori operativi
068300010601     C                   movel     '0'           flgGiro           1
068400010601     C                   movel     '0'           flgMulti          1
068500010601     C                   movel     '1'           flgStato          1
068600010615     C                   movel     '0'           flgOk             1
068700010601     C                   z-add     *zeros        cntNonEl         10 0
068800010601     C                   z-add     *zeros        depfil            3 0
068900010601     C                   z-add     *zeros        invfil            3 0
069000010601     C*
069100010601     C                   ENDSR
069200010601     C***
069300090330
069400090330
069500090330
069600090330     C*--------------------------------------------------------
069700090330     C* CARTAB - CARICAMENTO TABELLE                          *
069800090330     C*--------------------------------------------------------
069900090330     C     CARTAB        BEGSR
070000090330     C*
070100090330     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
070200090330     C                   z-add     *zeros        jOSR              3 0
070300090330     C     'OSR'         setll     tntbe01l
070400090330     C                   if        %equal(tntbe01l)
070500090330     C     'OSR'         reade     tntbe01l
070600090330     C                   dow       not %eof(tntbe01l)
070700090330     C                   eval      DOSR = tbeUNI
070800090330     c*
070900090330     c* Verifica e forzatura x nuovi campi aggiunti
071000090330     c                   if        aD§OSRcl6 = *blanks
071100090330     c                   eval      D§OSRcl6 = *zeros
071200090330     c                   endif
071300090330     c                   if        aD§OSRcl7 = *blanks
071400090330     c                   eval      D§OSRcl7 = *zeros
071500090330     c                   endif
071600090330     c                   if        aD§OSRcl8 = *blanks
071700090330     c                   eval      D§OSRcl8 = *zeros
071800090330     c                   endif
071900090330     c                   if        aD§OSRcl9 = *blanks
072000090330     c                   eval      D§OSRcl9 = *zeros
072100090330     c                   endif
072200090330     c                   if        aD§OSRcl10 = *blanks
072300090330     c                   eval      D§OSRcl10 = *zeros
072400090330     c                   endif
072500090330     c                   if        aD§OSRcl11 = *blanks
072600090330     c                   eval      D§OSRcl11 = *zeros
072700090330     c                   endif
072800090330     c                   if        aD§OSRcl12 = *blanks
072900090330     c                   eval      D§OSRcl12 = *zeros
073000090330     c                   endif
073100090330     c                   if        aD§OSRcl13 = *blanks
073200090330     c                   eval      D§OSRcl13 = *zeros
073300090330     c                   endif
073400090330     c                   if        aD§OSRcl14 = *blanks
073500090330     c                   eval      D§OSRcl14 = *zeros
073600090330     c                   endif
073700090330     c                   if        aD§OSRcl15 = *blanks
073800090330     c                   eval      D§OSRcl15 = *zeros
073900090330     c                   endif
074000090330     c                   if        aD§OSRcl16 = *blanks
074100090330     c                   eval      D§OSRcl16 = *zeros
074200090330     c                   endif
074300090330     c*
074400090330     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
074500090330     C                   if        D§OSRCLI <> *zeros
074600090330     C                   add       1             jOSR
074700090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
074800090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
074900090330     C                   movel     D§OSRCLI      wCliOSR
075000090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
075100090330     C                   endif
075200090330     C*
075300090330     C                   if        D§OSRCL2 <> *zeros
075400090330     C                   add       1             jOSR
075500090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
075600090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
075700090330     C                   movel     D§OSRCL2      wCliOSR
075800090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
075900090330     C                   endif
076000090330     C*
076100090330     C                   if        D§OSRCL3 <> *zeros
076200090330     C                   add       1             jOSR
076300090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
076400090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
076500090330     C                   movel     D§OSRCL3      wCliOSR
076600090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
076700090330     C                   endif
076800090330     C*
076900090330     C                   if        D§OSRCL4 <> *zeros
077000090330     C                   add       1             jOSR
077100090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
077200090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
077300090330     C                   movel     D§OSRCL4      wCliOSR
077400090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
077500090330     C                   endif
077600090330     C*
077700090330     C                   if        D§OSRCL5 <> *zeros
077800090330     C                   add       1             jOSR
077900090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
078000090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
078100090330     C                   movel     D§OSRCL5      wCliOSR
078200090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
078300090330     C                   endif
078400090330     C*
078500090330     C                   if        D§OSRCL6 <> *zeros
078600090330     C                   add       1             jOSR
078700090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
078800090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
078900090330     C                   movel     D§OSRCL6      wCliOSR
079000090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
079100090330     C                   endif
079200090330     C*
079300090330     C                   if        D§OSRCL7 <> *zeros
079400090330     C                   add       1             jOSR
079500090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
079600090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
079700090330     C                   movel     D§OSRCL7      wCliOSR
079800090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
079900090330     C                   endif
080000090330     C*
080100090330     C                   if        D§OSRCL8 <> *zeros
080200090330     C                   add       1             jOSR
080300090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
080400090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
080500090330     C                   movel     D§OSRCL8      wCliOSR
080600090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
080700090330     C                   endif
080800090330     C*
080900090330     C                   if        D§OSRCL9 <> *zeros
081000090330     C                   add       1             jOSR
081100090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
081200090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
081300090330     C                   movel     D§OSRCL9      wCliOSR
081400090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
081500090330     C                   endif
081600090330     C*
081700090330     C                   if        D§OSRCL10 <> *zeros
081800090330     C                   add       1             jOSR
081900090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
082000090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
082100090330     C                   movel     D§OSRCL10     wCliOSR
082200090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
082300090330     C                   endif
082400090330     C*
082500090330     C                   if        D§OSRCL11 <> *zeros
082600090330     C                   add       1             jOSR
082700090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
082800090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
082900090330     C                   movel     D§OSRCL11     wCliOSR
083000090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
083100090330     C                   endif
083200090330     C*
083300090330     C                   if        D§OSRCL12 <> *zeros
083400090330     C                   add       1             jOSR
083500090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
083600090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
083700090330     C                   movel     D§OSRCL12     wCliOSR
083800090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
083900090330     C                   endif
084000090330     C*
084100090330     C                   if        D§OSRCL13 <> *zeros
084200090330     C                   add       1             jOSR
084300090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
084400090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
084500090330     C                   movel     D§OSRCL13     wCliOSR
084600090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
084700090330     C                   endif
084800090330     C*
084900090330     C                   if        D§OSRCL14 <> *zeros
085000090330     C                   add       1             jOSR
085100090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
085200090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
085300090330     C                   movel     D§OSRCL14     wCliOSR
085400090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
085500090330     C                   endif
085600090330     C*
085700090330     C                   if        D§OSRCL15 <> *zeros
085800090330     C                   add       1             jOSR
085900090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
086000090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
086100090330     C                   movel     D§OSRCL15     wCliOSR
086200090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
086300090330     C                   endif
086400090330     C*
086500090330     C                   if        D§OSRCL16 <> *zeros
086600090330     C                   add       1             jOSR
086700090330     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
086800090330     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
086900090330     C                   movel     D§OSRCL16     wCliOSR
087000090330     C                   eval      SkCliOSR(jOSR) = DsCliOSR
087100090330     C                   endif
087200090330     C*
087300090330     C     'OSR'         reade     tntbe01l
087400090330     C                   enddo
087500090330     C                   endif
087600090330     C*
087700090330     C                   ENDSR
087800090330     C***
087900010601
088000010601
088100010330
088200010330
088300000613     C     *inzsr        BEGSR
088400990910     C*
088500990910     C     *entry        plist
088600990920     C                   parm                    tivlrds
088700990921     C                   parm      wrkesito      esito
088800000724     C                   parm                    prmlit
088900000710     C                   parm                    prmfir
089000010330     C*
089100010330     C* CALCOLA LA DATA CORRENTE
089200010330     C                   time                    wn14             14 0
089300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
089400010330     C                   z-add     wn8           g08dat
089500010330     C                   z-add     *zeros        g08inv
089600010330     C                   movel     '0'           g08err
089700010330     C                   call      'XSRDA8'
089800010330     C                   parm                    wlbda8
089900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
090000000613     C*
090100000613     C                   ENDSR
090200000613     C***
090300990908
090400020409     Otitv11p   E            riepilogo         2
090500990915     O                                              'Upload via Internet'
090600990915     O                                           +1 'Traduzione TIVIN00R -'
090700021025     O                                           +1 'FIVABWWR'
090800990915     O                                           +1 'Testate bolle'
090900990915     O          E            riepilogo   2
091000990915     O                       wrkdata
091100990915     O                       wrkora              +1
091200990915     O                       procname            +1
091300990915     O          E            riepilogo   2
091400990915     O                                              'Cliente..................:'
091500010605     O                       VABCCM        z     +1
091600990915     O          E            riepilogo   2
091700990920     O                                              'Riferimento Strategi.....:'
091800990920     O                       vlrhdl              +1
091900990915     O          E            riepilogo   2
092000990915     O                                              'Giusti...................:'
092100971022     O                       §CTROK        2   +  1
092200990915     O          E            riepilogo   2
092300990915     O                                              'Sbagliati e corretti.....:'
092400971022     O                       §CTRMO        2   +  1
092500990915     O          E            riepilogo   2
092600990915     O                                              'Sbagliati e scartati.....:'
092700971022     O                       §CTRNO        2   +  1
092800000613
092900020409     Otitv11ps  E            testdett          2
093000000613     O                                              'Upload via Internet'
093100000613     O                                           +1 'Traduzione TIVIN00R -'
093200021025     O                                           +1 'FIVABWWR'
093300000613     O                                           +1 'Testate bolle'
093400000616     O          E            testdett    3
093500000613     O                                           +2 'N° rec'
093600000613     O                                           +3 'Anteprima contenuto'
093700000616     O          E            rigadett    2
093800000613     O                       rrnum               +2
093900000621     O                       recko               +3
094000000616     O          E            findett     2
094100000613     O                       wrkdata
094200000613     O                       wrkora              +1
094300000613     O                       procname            +1
094400000616     O          E            findett     2
094500000613     O                                              'Cliente..................:'
094600010605     O                       VABCCM        z     +1
094700000616     O          E            findett     2
094800000613     O                                              'Riferimento Strategi.....:'
094900000613     O                       vlrhdl              +1
095000000616     O          E            findett     2
095100000613     O                                              'Giusti...................:'
095200000613     O                       §CTROK        2   +  1
095300000616     O          E            findett     2
095400000613     O                                              'Sbagliati e corretti.....:'
095500000613     O                       §CTRMO        2   +  1
095600000616     O          E            findett     2
095700000613     O                                              'Sbagliati e scartati.....:'
095800000613     O                       §CTRNO        2   +  1
095900000613** CMD - COMANDI CL
096000020409OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV11P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
096100020409OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV11PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
096200020409DLTOVR FILE(TITV11P TITV11PS) LVL(*)
096300010606
096400000613
