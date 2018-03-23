000100051029      /TITLE Upload via Internet: traduzione in FIVABWWR. (x WHIRLPOOL cod.0502867)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700051029     Ftitvn11p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900051029     Ftitvn11ps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300051029     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300010330     D*------------------
003400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003500010330     D*------------------
003600010330     D WLBDA8          DS                  INZ
003700010330     D  G08DAT                 1      8  0
003800010330     D  G08INV                 9     16  0
003900010330     D  G08ERR                17     17
004000010330     D  G08TGI                18     22  0
004100010330
004200010330
004300990915     C                   time                    wrkdata
004400990915     C                   time                    wrkora
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010601     C*
004900010601     C                   exsr      opeini
005000010605     C                   exsr      rwvab
005100051029     C*
005200051029     C* Effettuo lancio TISI95 solo x chiusura
005300051029     C                   CLEAR                   TISI95DS
005400051029     C                   EVAL      I95TLA = 'C'
005500051029     C                   CALL      'TISI95R'
005600051029     C                   PARM                    TISI95DS
005700010601     C*
005800010601     C                   seton                                        lr
005900010601
006000010601
006100010601
006200010601
006300010601     C*--------------------------------------------------------
006400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006500010601     C*--------------------------------------------------------
006600010601     C     PREELA        BEGSR
006700010601     C*
006800010601     C* SE OCCORRE SPEDIRE IN FILIALE
006900010601     C                   if        invfil <> *zeros and
007000010601     C                             flgGiro = '0'
007100010601     C*
007200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007300010601     C                   eval      flgGiro = '1'
007400000724     C*
007500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700010601     C     invfil        chain     azorg01l
007800000724     C                   if        %found
007900000724     C                   movel(p)  CMD(1)        depcmd
008000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008100000724     C*
008200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000724     C                   PARM                    depcmd
008600000724     C                   PARM                    LENGH
008700000724     C*
008800000724     C                   endif
008900000616     C*
009000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100000616     C                   movel(p)  CMD(2)        depcmd
009200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009500011113     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900000613     C*
010000051029     C                   if        not %open(titvn11ps)
010100051029     C                   open      titvn11ps
010200000616     C                   except    testdett
010300000613     C                   endif
010400010601     C*
010500010601     C                   endif
010600010601     C*
010700010601     C                   ENDSR
010800010601     C***
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011400010601     C*--------------------------------------------------------
011500010601     C     ENDELA        BEGSR
011600990915     C*
011700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011800000613     C*
011900051029     C                   if        %open(titvn11ps)
012000000616     C                   except    findett
012100051029     C                   close     titvn11ps
012200000613     C                   endif
012300000616     C*
012400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    CMD(3)
012800000616     C                   PARM                    LENGH
012900000616     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400000613
013500000613     C*--------------------------------------------------------
013600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013700000613     C*--------------------------------------------------------
013800000621     C     STPR          BEGSR
013900000613     C*
014000051029     C                   if        not %open(titvn11p)
014100051029     C                   open      titvn11p
014200990915     C                   endif
014300990915     C*
014400990915     C                   except    riepilogo
014500990915     C*
014600051029     C                   if        %open(titvn11p)
014700051029     C                   close     titvn11p
014800990914     C                   endif
014900000613     C*
015000000613     C                   ENDSR
015100000613     C***
015200000613
015300010601
015400010601
015500010330     C*--------------------------------------------------------
015600050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015700010330     C*--------------------------------------------------------
015800010605     C     RWVAB         BEGSR
015900050408     C*
016000010330     C                   if        not %open(tivin00r)
016100010330     C                   open      tivin00r
016200010330     C                   endif
016300050408     C                   if        not %open(fivabwwr)
016400050408     C                   open      fivabwwr
016500010330     C                   endif
016600010330     C*
016700010604     C                   clear                   §CTROK
016800010604     C                   clear                   §CTRMO
016900010604     C                   clear                   §CTRNO
017000010330     C*
017100010330     C                   DO        *HIVAL
017200010330     C*
017300010330     C                   READ      tivin00r                               70
017400010618     C*
017500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017600010618     C                   if        vinflg = '1'
017700010618     C                   eval      flgOk = '1'
017800010618     C                   endif
017900010618     C*
018000010330     C                   if        vindta > *blanks
018100010330     C                   add       1             rrnum
018200010330     C*
018300010601     C                   if        *in70 = *off and
018400010330     C                             (vinflg = *blanks
018500010330     C                              or vinflg = '0'
018600010330     C                              or vinflg = '2')
018700010330     C*
018800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018900010711     C                   if        vinflg = *blanks or vinflg = '0'
019000010711     C                   clear                   vinmsg
019100010711     C                   endif
019200010601     C*
019300010330     C                   exsr      inzvar
019400010330     C                   exsr      defcam
019500010605     C                   exsr      impvab
019600010601     C*
019700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019800010605     C                   eval      depfil = VABLNP
019900010601     C                   exsr      repfil
020000010601     C                   if        depfil = invfil
020100021025     C                   if        vlrpoi = 999
020200021025     C                   MOVE(P)   invfil        VABFGS
020300021025     C                   else
020400021025     C                   MOVE(P)   vlrpoi        VABFGS
020500021025     C                   endif
020600010601     C*
020700010601     C                   exsr      PREELA
020800010601     C*
020900010604     C* Ebbene...
021000010604     C*
021100010604     C  N31              ADD       1             §CTROK            7 0
021200010604     C   32              ADD       1             §CTRMO            7 0
021300010604     C   31              ADD       1             §CTRNO            7 0
021400020205     C*
021500050408     C* Scarico il buffer del FIVAB
021600050408     C  N31              WRITE     FIVAB000
021700010604     C*
021800020722     C                   endif
021900020722     C*
022000010604     C                   if        *in31 = *off and
022100010604     C                             *in32 = *off
022200010604     C                   eval      vinflg = '1'
022300010604     C                   else
022400010604     C                   eval      recko = vindta
022500010604     C                   except    rigadett
022600010604     C                   eval      vinflg = '2'
022700010604     C                   endif
022800010604     C                   endif
022900010604     C*
023000010330     C                   else
023100010330     C                   eval      vinflg = '1'
023200010330     C                   endif
023300010601     C*
023400010601     C  N70              update    tivin000
023500010330     C*
023600010330     C  N70              ENDdo
023700010601     C*
023800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023900010601     C                   if        cntNonEl = *zeros or
024000010601     C                             flgMulti = '0'
024100010330     C* Se non ci sono record con errori ...
024200010601     C                   if        §ctrno = 0 and
024300010604     C                             §ctrmo = 0 and
024400010601     C                             flgStato <> '2'
024500010330     C* ... restituisco esito OK.
024600010330     C                   eval      wrkesito = '0'
024700010330     C                   else
024800010330     C                   if        §ctrok > 0
024900010330     C                   eval      wrkesito = '1'
025000010330     C                   else
025100010615     C                   if        flgOk = '0'
025200010615     C                   eval      wrkesito = '2'
025300010615     C                   else
025400010615     C                   eval      wrkesito = '6'
025500010615     C                   endif
025600010330     C                   endif
025700010330     C                   endif
025800010601     C                   else
025900010601     C                   eval      wrkesito = '9'
026000010601     C                   endif
026100010330     C*
026200010330     C                   if        %open(tivin00r)
026300010330     C                   close     tivin00r
026400010330     C                   endif
026500050408     C                   if        %open(fivabwwr)
026600050408     C                   close     fivabwwr
026700010330     C                   endif
026800010601     C*
026900010601     C                   if        vlrpoi <> 999
027000010601     C                   eval      invfil = vlrpoi
027100010601     C                   endif
027200010330     C*
027300010330     C                   if        §ctrok > 0
027400010601     C                             and invfil > *zeros
027500010330     C                   exsr      invio
027600010330     C                   endif
027700010601     C*
027800010618     C                   if        flgGiro = '1'
027900010601     C                   exsr      endela
028000010618     C                   endif
028100010330     C*
028200010330     C                   ENDSR
028300010330     C***
028400010330
028500010601
028600010601
028700010601
028800010601
028900010330     C*----------------------------------------------------*
029000020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029100010330     C*----------------------------------------------------*
029200010330     C     INZVAR        BEGSR
029300010330     C*
029400020204     C                   Z-ADD     *zeros        Num5_0            5 0
029500020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029600020322     C                   MOVEL     '0'           FlgCAS            1
029700010330     C*
029800010330     C                   ENDSR
029900010330     C*----------------------------------------------------*
030000020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030100010330     C*----------------------------------------------------*
030200010330     C     DEFCAM        BEGSR
030300010330     C*
030400020204     C* Inizializzo il buffer del record da scrivere
030500050408     C                   CLEAR                   FIVAB000
030600020204     C* Imposto i valori di default...
030700051029     C                   EVAL      VABCCM = 0502867
030800051029     C                   EVAL      VABLNP = 050
030900050921     C                   EVAL      VABCTR = 000
031000050921     C                   EVAL      VABCBO = '1'
031100051029     C                   EVAL      VABCTM = '7Q'
031200051102     C                   EVAL      VABTSP = 'C'
031300020204     C* ... e poi verifico se sono stati passati come parametri
031400020204     C                   IF        vlrppt > *blanks
031500040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031700020204     C                   EXSR      CHKNUM
031800020204     C                   IF        PiInt=*on
031900020204     C                   Z-ADD     PiVal         VABCCM
032000020204     C                   ENDIF
032100040301     C                   ENDIF
032200040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032400020204     C                   EXSR      CHKNUM
032500020204     C                   IF        PiInt=*on
032600020204     C                   Z-ADD     PiVal         VABLNP
032700020204     C                   ENDIF
032800040301     C                   ENDIF
032900040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033100020204     C                   EXSR      CHKNUM
033200020204     C                   IF        PiInt=*on
033300020204     C                   Z-ADD     PiVal         VABCTR
033400040301     C                   ENDIF
033500020204     C                   ENDIF
033600020204     C                   ENDIF
033700020204     C*
033800010330     C                   ENDSR
033900010607     C*----------------------------------------------------*
034000050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034100010607     C*----------------------------------------------------*
034200010607     C     IMPVAB        BEGSR
034300010607     C*
034400010607     C                   SETOFF                                       3132
034500010607     C*
034600010607     C* Reperimento campi ALFA
034700010607     C*
034800010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034900051029     C                   EVAL      VABRSD=%trim(%subst(vindta:11:35))
035000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035200020117     C* ==
035300051029     C                   EVAL      VABIND=%trim(%subst(vindta:46:35))
035400051029     C                   EVAL      VABLOD=%trim(%subst(vindta:81:30))
035500051103     C***                EVAL      VABPRD=%trim(%subst(vindta:111:2))
035600051029     C                   EVAL      VABRMA=%trim(%subst(vindta:1:10))
035700010607     C*
035800010607     C* Reperimento campi NUMERICI
035900020722     C                   MOVEL     DATCOR        VABAAS
036000020722     C                   MOVE      DATCOR        VABMGS
036100050518     C* RMN
036200051029     C                   EVAL      PiStr=%trim(%subst(vindta:1:10))
036300050408     C                   EXSR      CHKNUM
036400050408     C                   IF        PiInt=*on
036500050408     C                   Z-ADD     PiVal         VABRMN
036600050908     C                   Z-ADD     PiVal         VABNSP
036700050408     C                   ELSE
036800051029     C                   SETON                                        31
036900051029     C                   Z-ADD     1             VABRMN
037000050408     C                   EVAL      vinmsg = %trimr(vinmsg)
037100050908     C                             + ' ' + 'VABRMN VABNSP'
037200050408     C                   ENDIF
037300040420     C* CAD
037400051029     C                   EVAL      PiStr=%trim(%subst(vindta:113:5))
037500010607     C                   EXSR      CHKNUM
037600010607     C                   IF        PiInt=*on
037700020204     C                   Z-ADD     PiVal         Num5_0
037800020204     C                   MOVEL(p)  Num5_0        VABCAD
037900010607     C                   ELSE
038000010607     C                   SETON                                        32
038100020204     C                   EVAL      VABCAD = *zeros
038200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038300020204     C                             + ' ' + 'VABCAD'
038400010607     C                   ENDIF
038500040420     C* NCL
038600051029     C                   EVAL      PiStr=%trim(%subst(vindta:128:4))
038700010607     C                   EXSR      CHKNUM
038800010607     C                   IF        PiInt=*on
038900010607     C                   Z-ADD     PiVal         VABNCL
039000010607     C                   ELSE
039100010607     C                   SETON                                        32
039200010607     C                   Z-ADD     *zeros        VABNCL
039300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039400010607     C                             + ' ' + 'VABNCL'
039500010607     C                   ENDIF
039600040420     C* PKB
039700051029     C                   EVAL      PiStr=%trim(%subst(vindta:132:6))
039800010607     C                   EXSR      CHKNUM
039900010607     C                   IF        PiNum=*on
040000051029     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
040100050330     C                   Z-ADD(H)  PiVal         VABPKB
040200010607     C                   ELSE
040300010607     C                   SETON                                        32
040400010607     C                   Z-ADD     *zeros        VABPKB
040500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040600010607     C                             + ' ' + 'VABPKB'
040700010607     C                   ENDIF
040800051029     C*
040900051029     C* Se nn già presente reperisco la provincia dal CAP e dalla località
041000051029     C                   IF        VABCAD <> *blanks AND
041100051029     C                             VABPRD  = *blanks AND
041200051029     C                             VABNZD  = *blanks
041300051029     C                   CLEAR                   TISI95DS
041400051029     C                   EVAL      I95TCN = '3'
041500051029     C                   Z-ADD     datcor        I95DAT
041600051029     C                   EVAL      I95CAP = VABCAD
041700051029     C                   EVAL      I95LOC = VABLOD
041800051029     C                   CALL      'TISI95R'
041900051029     C                   PARM                    TISI95DS
042000051029     C                   EVAL      VABPRD = O95PRV
042100051029     C                   ENDIF
042200020322     C*
042300020322     C* Considerazioni finali su CBO/CAS
042400050908     C                   IF        FlgCAS = '1'
042500040713     C                   IF        VABCBO = '1'
042600020322     C                   EVAL      VABCBO = '4'
042700040713     C                   ENDIF
042800040713     C                   IF        VABCBO = '2'
042900040713     C                   EVAL      VABCBO = '6'
043000040713     C                   ENDIF
043100020322     C                   ENDIF
043200020204     C*
043300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043400020204     C                   EXSR      CHKIMPDIV
043500020204     C*
043600010607     C                   ENDSR
043700010607     C*----------------------------------------------------*
043800010601
043900020204
044000020204     C*----------------------------------------------------*
044100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044200020204     C*----------------------------------------------------*
044300020204     C     CHKIMPDIV     BEGSR
044400020204     C*
044500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044600020204     C                   Z-ADD     *zeros        wrkDec            9 9
044700020204     C*
044800020204     C* Come prima cosa effettuo considerazioni sulla divisa
044900020204     C                   IF        vabIAS > *zeros
045000020204     C                   IF        vabVAS <> 'EUR'
045100020204     C                   EVAL      vabVAS =  'ITL'
045200020204     C                   ENDIF
045300020204     C                   ENDIF
045400020204     C*
045500020204     C                   IF        vabCAS > *zeros
045600020204     C                   IF        vabVCA <> 'EUR'
045700020204     C                   EVAL      vabVCA =  'ITL'
045800020204     C                   ENDIF
045900020204     C                   ENDIF
046000020204     C*
046100020204     C                   IF        vabVMD > *zeros
046200020321     C                   IF        vabVAD <> 'EUR'
046300020204     C                   EVAL      vabVAD =  'ITL'
046400020204     C                   ENDIF
046500020204     C                   ENDIF
046600020204     C*
046700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046800020204     C                   Z-ADD     vabIAS        wrkDec
046900020204     C                   IF        wrkDec > *zeros
047000020204     C                   IF        vabVAS = 'ITL'
047100020204     C                   EVAL      vabIAS = *zeros
047200020204     C                   ENDIF
047300020204     C                   ENDIF
047400020204     C*
047500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047600020204     C                   Z-ADD     vabCAS        wrkDec
047700020204     C                   IF        wrkDec > *zeros
047800020204     C                   IF        vabVCA = 'ITL'
047900020204     C                   EVAL      vabCAS = *zeros
048000020204     C                   ENDIF
048100020204     C                   ENDIF
048200020204     C*
048300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048400020204     C                   Z-ADD     vabVMD        wrkDec
048500020204     C                   IF        wrkDec > *zeros
048600020204     C                   IF        vabVAD = 'ITL'
048700020204     C                   EVAL      vabVMD = *zeros
048800020204     C                   ENDIF
048900020204     C                   ENDIF
049000020204     C*
049100020204     C                   ENDSR
049200020204     C***
049300020204
049400010330
049500010330
049600010330     C*----------------------------------------------------*
049700010330     C*  CONTROLLO NUMERICITA' CAMPI
049800010330     C*----------------------------------------------------*
049900010330     C     CHKNUM        BEGSR
050000010330     C*
050100010606     C                   IF        PiDecChr = *blanks
050200010606     C                   EVAL      PiDecChr = ','
050300010606     C                   ENDIF
050400010606     C*
050500010606     C                   CALL(e)   'ISNUMERIC'
050600010330     C                   PARM                    PiStr            30
050700010606     C                   PARM                    PiDecChr          1
050800010330     C                   PARM      *ZEROS        PiVal            30 9
050900010330     C                   PARM      '0'           PiInt             1
051000010330     C                   PARM      '0'           PiNum             1
051100010330     C                   IF        %error
051200010606     C                   EVAL      PiNum=*off
051300010330     C                   ENDIF
051400010330     C*
051500010330     C                   ENDSR
051600010330     C***
051700010330
051800010601
051900010601
052000010601
052100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052200010601     C     repfil        BEGSR
052300010601     C*
052400010601     C                   if        invfil = *zeros and
052500010601     C                             depfil > *zeros and
052600010629     C                             (vinflg = *blanks or
052700010629     C                              vinflg = *zeros)
052800010601     C
052900010601     C                   eval      invfil = depfil
053000010601     C                   endif
053100010601     C*
053200010601     C                   if        depfil <> invfil and
053300010601     C                             invfil > *zeros
053400010601     C                   eval      flgMulti = '1'
053500010601     C                   if        vinflg = *blanks
053600010601     C                   add       1             cntNonEl
053700010601     C                   endif
053800010601     C                   endif
053900010601     C*
054000010601     C                   if        vinflg = '2'
054100010601     C                   eval      flgStato = '2'
054200010601     C                   endif
054300010601     C*
054400010601     C                   ENDSR
054500010601     C***
054600010601
054700010601
054800010601
054900010330
055000010330
055100010330
055200990920      /TITLE Invio dei dati al punto operativo.
055300000613     C     invio         BEGSR
055400990920     C*
055500990920     C                   reset                   dscmz
055600010601     C                   move      invfil        cmzdst
055700990920     C                   eval      cmzfld = vlrfou
055800990920     C                   eval      cmzmbd = vlrhdl
055900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056000000710     C                   if        prmfir = *blanks
056100050408     C                   eval      cmzfla = 'FIVAB00F'
056200050408     C                   eval      cmzmba = 'FIVAB00F'
056300000710     C                   else
056400000710     C                   eval      cmzfla = prmfir
056500000710     C                   eval      cmzmba = prmfir
056600000710     C                   endif
056700990920     C                   eval      cmznrr = *zeros
056800990920     C                   move      §ctrok        cmznrr
056900021018     C                   eval      cmzlba = vlrfl1
057000990920     C                   call(e)   'TIS711C'
057100990920     C                   parm                    dscmz
057200990921     C                   parm      *blanks       esito
057300990920     C                   if        %error
057400990920     C                             or cmzerr = '1'
057500990921     C                             or esito  = '1'
057600000710     C                   eval      wrkesito = '3'
057700990920     C                   endif
057800990920     C*
057900000613     C                   ENDSR
058000990910
058100010601
058200010601
058300010601
058400010601
058500010601      /TITLE Invio dei dati al punto operativo.
058600010601     C     opeini        BEGSR
058700010601     C*
058800010601     C* Inizializzo flag e contatori operativi
058900010601     C                   movel     '0'           flgGiro           1
059000010601     C                   movel     '0'           flgMulti          1
059100010601     C                   movel     '1'           flgStato          1
059200010615     C                   movel     '0'           flgOk             1
059300010601     C                   z-add     *zeros        cntNonEl         10 0
059400010601     C                   z-add     *zeros        depfil            3 0
059500010601     C                   z-add     *zeros        invfil            3 0
059600010601     C*
059700010601     C                   ENDSR
059800010601     C***
059900010601
060000010601
060100010601
060200010330
060300010330
060400000613     C     *inzsr        BEGSR
060500990910     C*
060600990910     C     *entry        plist
060700990920     C                   parm                    tivlrds
060800990921     C                   parm      wrkesito      esito
060900000724     C                   parm                    prmlit
061000000710     C                   parm                    prmfir
061100010330     C*
061200010330     C* CALCOLA LA DATA CORRENTE
061300010330     C                   time                    wn14             14 0
061400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061500010330     C                   z-add     wn8           g08dat
061600010330     C                   z-add     *zeros        g08inv
061700010330     C                   movel     '0'           g08err
061800010330     C                   call      'XSRDA8'
061900010330     C                   parm                    wlbda8
062000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062100000613     C*
062200000613     C                   ENDSR
062300000613     C***
062400990908
062500051029     Otitvn11p  E            riepilogo         2
062600990915     O                                              'Upload via Internet'
062700990915     O                                           +1 'Traduzione TIVIN00R -'
062800050408     O                                           +1 'FIVABWWR'
062900990915     O                                           +1 'Testate bolle'
063000990915     O          E            riepilogo   2
063100990915     O                       wrkdata
063200990915     O                       wrkora              +1
063300990915     O                       procname            +1
063400990915     O          E            riepilogo   2
063500990915     O                                              'Cliente..................:'
063600010605     O                       VABCCM        z     +1
063700990915     O          E            riepilogo   2
063800990920     O                                              'Riferimento Strategi.....:'
063900990920     O                       vlrhdl              +1
064000990915     O          E            riepilogo   2
064100990915     O                                              'Giusti...................:'
064200971022     O                       §CTROK        2   +  1
064300990915     O          E            riepilogo   2
064400990915     O                                              'Sbagliati e corretti.....:'
064500971022     O                       §CTRMO        2   +  1
064600990915     O          E            riepilogo   2
064700990915     O                                              'Sbagliati e scartati.....:'
064800971022     O                       §CTRNO        2   +  1
064900000613
065000051029     Otitvn11ps E            testdett          2
065100000613     O                                              'Upload via Internet'
065200000613     O                                           +1 'Traduzione TIVIN00R -'
065300050408     O                                           +1 'FIVABWWR'
065400000613     O                                           +1 'Testate bolle'
065500000616     O          E            testdett    3
065600000613     O                                           +2 'N° rec'
065700000613     O                                           +3 'Anteprima contenuto'
065800000616     O          E            rigadett    2
065900000613     O                       rrnum               +2
066000000621     O                       recko               +3
066100000616     O          E            findett     2
066200000613     O                       wrkdata
066300000613     O                       wrkora              +1
066400000613     O                       procname            +1
066500000616     O          E            findett     2
066600000613     O                                              'Cliente..................:'
066700010605     O                       VABCCM        z     +1
066800000616     O          E            findett     2
066900000613     O                                              'Riferimento Strategi.....:'
067000000613     O                       vlrhdl              +1
067100000616     O          E            findett     2
067200000613     O                                              'Giusti...................:'
067300000613     O                       §CTROK        2   +  1
067400000616     O          E            findett     2
067500000613     O                                              'Sbagliati e corretti.....:'
067600000613     O                       §CTRMO        2   +  1
067700000616     O          E            findett     2
067800000613     O                                              'Sbagliati e scartati.....:'
067900000613     O                       §CTRNO        2   +  1
068000000613** CMD - COMANDI CL
068100051029OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITVN11P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068200051029OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITVN11PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068300051029DLTOVR FILE(TITVN11P TITVN11PS) LVL(*)
068400010606
068500000613
