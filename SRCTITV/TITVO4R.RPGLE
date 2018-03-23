000100050921      /TITLE Upload via Internet: traduzione in FIVABWWR. (x MAO OTTICA cod.1320353)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700050921     Ftitvo4p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050921     Ftitvo4ps  O    f  198        PRINTER usropn
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
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011113     D depcmd          s            150    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000010330
004100010330
004200990915     C                   time                    wrkdata
004300990915     C                   time                    wrkora
004400000913     C                   reset                   rrnum
004500990921     C                   reset                   esito
004600990921     C                   reset                   wrkesito
004700010601     C*
004800010601     C                   exsr      opeini
004900010605     C                   exsr      rwvab
005000010601     C*
005100010601     C                   seton                                        lr
005200010601
005300010601
005400010601
005500010601
005600010601     C*--------------------------------------------------------
005700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005800010601     C*--------------------------------------------------------
005900010601     C     PREELA        BEGSR
006000010601     C*
006100010601     C* SE OCCORRE SPEDIRE IN FILIALE
006200010601     C                   if        invfil <> *zeros and
006300010601     C                             flgGiro = '0'
006400010601     C*
006500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006600010601     C                   eval      flgGiro = '1'
006700000724     C*
006800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007000010601     C     invfil        chain     azorg01l
007100000724     C                   if        %found
007200000724     C                   movel(p)  CMD(1)        depcmd
007300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007400000724     C*
007500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007600011113     C                   Z-ADD     150           LENGH            15 5
007700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007800000724     C                   PARM                    depcmd
007900000724     C                   PARM                    LENGH
008000000724     C*
008100000724     C                   endif
008200000616     C*
008300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008400000616     C                   movel(p)  CMD(2)        depcmd
008500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008600000616     C*
008700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008800011113     C                   Z-ADD     150           LENGH            15 5
008900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009000000616     C                   PARM                    depcmd
009100000616     C                   PARM                    LENGH
009200000613     C*
009300050921     C                   if        not %open(titvo4ps)
009400050921     C                   open      titvo4ps
009500000616     C                   except    testdett
009600000613     C                   endif
009700010601     C*
009800010601     C                   endif
009900010601     C*
010000010601     C                   ENDSR
010100010601     C***
010200010601
010300010601
010400010601
010500010601     C*--------------------------------------------------------
010600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010700010601     C*--------------------------------------------------------
010800010601     C     ENDELA        BEGSR
010900990915     C*
011000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011100000613     C*
011200050921     C                   if        %open(titvo4ps)
011300000616     C                   except    findett
011400050921     C                   close     titvo4ps
011500000613     C                   endif
011600000616     C*
011700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011800011113     C                   Z-ADD     150           LENGH            15 5
011900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012000000616     C                   PARM                    CMD(3)
012100000616     C                   PARM                    LENGH
012200000616     C*
012300010601     C                   ENDSR
012400010601     C***
012500010601
012600010601
012700000613
012800000613     C*--------------------------------------------------------
012900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013000000613     C*--------------------------------------------------------
013100000621     C     STPR          BEGSR
013200000613     C*
013300050921     C                   if        not %open(titvo4p)
013400050921     C                   open      titvo4p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900050921     C                   if        %open(titvo4p)
014000050921     C                   close     titvo4p
014100990914     C                   endif
014200000613     C*
014300000613     C                   ENDSR
014400000613     C***
014500000613
014600010601
014700010601
014800010330     C*--------------------------------------------------------
014900050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015000010330     C*--------------------------------------------------------
015100010605     C     RWVAB         BEGSR
015200050408     C*
015300010330     C                   if        not %open(tivin00r)
015400010330     C                   open      tivin00r
015500010330     C                   endif
015600050408     C                   if        not %open(fivabwwr)
015700050408     C                   open      fivabwwr
015800010330     C                   endif
015900010330     C*
016000010604     C                   clear                   §CTROK
016100010604     C                   clear                   §CTRMO
016200010604     C                   clear                   §CTRNO
016300010330     C*
016400010330     C                   DO        *HIVAL
016500010330     C*
016600010330     C                   READ      tivin00r                               70
016700010618     C*
016800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016900010618     C                   if        vinflg = '1'
017000010618     C                   eval      flgOk = '1'
017100010618     C                   endif
017200010618     C*
017300010330     C                   if        vindta > *blanks
017400010330     C                   add       1             rrnum
017500010330     C*
017600010601     C                   if        *in70 = *off and
017700010330     C                             (vinflg = *blanks
017800010330     C                              or vinflg = '0'
017900010330     C                              or vinflg = '2')
018000010330     C*
018100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018200010711     C                   if        vinflg = *blanks or vinflg = '0'
018300010711     C                   clear                   vinmsg
018400010711     C                   endif
018500010601     C*
018600010330     C                   exsr      inzvar
018700010330     C                   exsr      defcam
018800010605     C                   exsr      impvab
018900010601     C*
019000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019100010605     C                   eval      depfil = VABLNP
019200010601     C                   exsr      repfil
019300010601     C                   if        depfil = invfil
019400021025     C                   if        vlrpoi = 999
019500021025     C                   MOVE(P)   invfil        VABFGS
019600021025     C                   else
019700021025     C                   MOVE(P)   vlrpoi        VABFGS
019800021025     C                   endif
019900010601     C*
020000010601     C                   exsr      PREELA
020100010601     C*
020200010604     C* Ebbene...
020300010604     C*
020400010604     C  N31              ADD       1             §CTROK            7 0
020500010604     C   32              ADD       1             §CTRMO            7 0
020600010604     C   31              ADD       1             §CTRNO            7 0
020700020205     C*
020800050408     C* Scarico il buffer del FIVAB
020900050408     C  N31              WRITE     FIVAB000
021000010604     C*
021100020722     C                   endif
021200020722     C*
021300010604     C                   if        *in31 = *off and
021400010604     C                             *in32 = *off
021500010604     C                   eval      vinflg = '1'
021600010604     C                   else
021700010604     C                   eval      recko = vindta
021800010604     C                   except    rigadett
021900010604     C                   eval      vinflg = '2'
022000010604     C                   endif
022100010604     C                   endif
022200010604     C*
022300010330     C                   else
022400010330     C                   eval      vinflg = '1'
022500010330     C                   endif
022600010601     C*
022700010601     C  N70              update    tivin000
022800010330     C*
022900010330     C  N70              ENDdo
023000010601     C*
023100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023200010601     C                   if        cntNonEl = *zeros or
023300010601     C                             flgMulti = '0'
023400010330     C* Se non ci sono record con errori ...
023500010601     C                   if        §ctrno = 0 and
023600010604     C                             §ctrmo = 0 and
023700010601     C                             flgStato <> '2'
023800010330     C* ... restituisco esito OK.
023900010330     C                   eval      wrkesito = '0'
024000010330     C                   else
024100010330     C                   if        §ctrok > 0
024200010330     C                   eval      wrkesito = '1'
024300010330     C                   else
024400010615     C                   if        flgOk = '0'
024500010615     C                   eval      wrkesito = '2'
024600010615     C                   else
024700010615     C                   eval      wrkesito = '6'
024800010615     C                   endif
024900010330     C                   endif
025000010330     C                   endif
025100010601     C                   else
025200010601     C                   eval      wrkesito = '9'
025300010601     C                   endif
025400010330     C*
025500010330     C                   if        %open(tivin00r)
025600010330     C                   close     tivin00r
025700010330     C                   endif
025800050408     C                   if        %open(fivabwwr)
025900050408     C                   close     fivabwwr
026000010330     C                   endif
026100010601     C*
026200010601     C                   if        vlrpoi <> 999
026300010601     C                   eval      invfil = vlrpoi
026400010601     C                   endif
026500010330     C*
026600010330     C                   if        §ctrok > 0
026700010601     C                             and invfil > *zeros
026800010330     C                   exsr      invio
026900010330     C                   endif
027000010601     C*
027100010618     C                   if        flgGiro = '1'
027200010601     C                   exsr      endela
027300010618     C                   endif
027400010330     C*
027500010330     C                   ENDSR
027600010330     C***
027700010330
027800010601
027900010601
028000010601
028100010601
028200010330     C*----------------------------------------------------*
028300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028400010330     C*----------------------------------------------------*
028500010330     C     INZVAR        BEGSR
028600010330     C*
028700020204     C                   Z-ADD     *zeros        Num5_0            5 0
028800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028900020322     C                   MOVEL     '0'           FlgCAS            1
029000010330     C*
029100010330     C                   ENDSR
029200010330     C*----------------------------------------------------*
029300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029400010330     C*----------------------------------------------------*
029500010330     C     DEFCAM        BEGSR
029600010330     C*
029700020204     C* Inizializzo il buffer del record da scrivere
029800050408     C                   CLEAR                   FIVAB000
029900020204     C* Imposto i valori di default...
030000050921     C                   EVAL      VABCCM = 1320353
030100050921     C                   EVAL      VABLNP = 132
030200050921     C                   EVAL      VABCTR = 000
030300050908     C                   EVAL      VABTSP = 'C'
030400050921     C                   EVAL      VABCBO = '1'
030500050921     C                   EVAL      VABCTM = '7K'
030600050921     C                   EVAL      VABIAS = 516
030700050921     C                   EVAL      VABVAS = 'EUR'
030800020204     C* ... e poi verifico se sono stati passati come parametri
030900020204     C                   IF        vlrppt > *blanks
031000040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031200020204     C                   EXSR      CHKNUM
031300020204     C                   IF        PiInt=*on
031400020204     C                   Z-ADD     PiVal         VABCCM
031500020204     C                   ENDIF
031600040301     C                   ENDIF
031700040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031900020204     C                   EXSR      CHKNUM
032000020204     C                   IF        PiInt=*on
032100020204     C                   Z-ADD     PiVal         VABLNP
032200020204     C                   ENDIF
032300040301     C                   ENDIF
032400040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020204     C                   Z-ADD     PiVal         VABCTR
032900040301     C                   ENDIF
033000020204     C                   ENDIF
033100020204     C                   ENDIF
033200020204     C*
033300010330     C                   ENDSR
033400010607     C*----------------------------------------------------*
033500050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033600010607     C*----------------------------------------------------*
033700010607     C     IMPVAB        BEGSR
033800010607     C*
033900010607     C                   SETOFF                                       3132
034000010607     C*
034100010607     C* Reperimento campi ALFA
034200010607     C*
034300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034400050921     C                   EVAL      VABRSD=%trim(%subst(vindta:146:30))
034500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034700020117     C* ==
034800050921     C                   EVAL      VABIND=%trim(%subst(vindta:176:30))
034900050921     C                   EVAL      VABLOD=%trim(%subst(vindta:206:30))
035000050921     C                   EVAL      VABPRD=%trim(%subst(vindta:236:2))
035100050921     C                   EVAL      VABRMA=%trim(%subst(vindta:308:6))
035200010607     C*
035300010607     C* Reperimento campi NUMERICI
035400020722     C                   MOVEL     DATCOR        VABAAS
035500020722     C                   MOVE      DATCOR        VABMGS
035600050518     C* RMN
035700050921     C                   EVAL      PiStr=%trim(%subst(vindta:308:6))
035800050408     C                   EXSR      CHKNUM
035900050408     C                   IF        PiInt=*on
036000050408     C                   Z-ADD     PiVal         VABRMN
036100050908     C                   Z-ADD     PiVal         VABNSP
036200050408     C                   ELSE
036300050518     C                   SETON                                        32
036400050408     C                   Z-ADD     *zeros        VABRMN
036500050408     C                   EVAL      vinmsg = %trimr(vinmsg)
036600050908     C                             + ' ' + 'VABRMN VABNSP'
036700050408     C                   ENDIF
036800040420     C* CAD
036900050921     C                   EVAL      PiStr=%trim(%subst(vindta:420:5))
037000010607     C                   EXSR      CHKNUM
037100010607     C                   IF        PiInt=*on
037200020204     C                   Z-ADD     PiVal         Num5_0
037300020204     C                   MOVEL(p)  Num5_0        VABCAD
037400010607     C                   ELSE
037500010607     C                   SETON                                        32
037600020204     C                   EVAL      VABCAD = *zeros
037700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037800020204     C                             + ' ' + 'VABCAD'
037900010607     C                   ENDIF
038000040420     C* NCL
038100050921     C                   EVAL      PiStr=%trim(%subst(vindta:338:6))
038200010607     C                   EXSR      CHKNUM
038300010607     C                   IF        PiInt=*on
038400010607     C                   Z-ADD     PiVal         VABNCL
038500010607     C                   ELSE
038600010607     C                   SETON                                        32
038700010607     C                   Z-ADD     *zeros        VABNCL
038800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038900010607     C                             + ' ' + 'VABNCL'
039000010607     C                   ENDIF
039100040420     C* PKB
039200050921     C                   EVAL      PiStr=%trim(%subst(vindta:380:8))
039300010607     C                   EXSR      CHKNUM
039400010607     C                   IF        PiNum=*on
039500050921     C                   EVAL      PiVal = PiVal / 10                           * gestisco 1 dec.
039600050330     C                   Z-ADD(H)  PiVal         VABPKB
039700010607     C                   ELSE
039800010607     C                   SETON                                        32
039900010607     C                   Z-ADD     *zeros        VABPKB
040000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040100010607     C                             + ' ' + 'VABPKB'
040200010607     C                   ENDIF
040300020322     C*
040400020322     C* Considerazioni finali su CBO/CAS
040500050908     C                   IF        FlgCAS = '1'
040600040713     C                   IF        VABCBO = '1'
040700020322     C                   EVAL      VABCBO = '4'
040800040713     C                   ENDIF
040900040713     C                   IF        VABCBO = '2'
041000040713     C                   EVAL      VABCBO = '6'
041100040713     C                   ENDIF
041200020322     C                   ENDIF
041300020204     C*
041400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041500020204     C                   EXSR      CHKIMPDIV
041600020204     C*
041700010607     C                   ENDSR
041800010607     C*----------------------------------------------------*
041900010601
042000020204
042100020204     C*----------------------------------------------------*
042200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
042300020204     C*----------------------------------------------------*
042400020204     C     CHKIMPDIV     BEGSR
042500020204     C*
042600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
042700020204     C                   Z-ADD     *zeros        wrkDec            9 9
042800020204     C*
042900020204     C* Come prima cosa effettuo considerazioni sulla divisa
043000020204     C                   IF        vabIAS > *zeros
043100020204     C                   IF        vabVAS <> 'EUR'
043200020204     C                   EVAL      vabVAS =  'ITL'
043300020204     C                   ENDIF
043400020204     C                   ENDIF
043500020204     C*
043600020204     C                   IF        vabCAS > *zeros
043700020204     C                   IF        vabVCA <> 'EUR'
043800020204     C                   EVAL      vabVCA =  'ITL'
043900020204     C                   ENDIF
044000020204     C                   ENDIF
044100020204     C*
044200020204     C                   IF        vabVMD > *zeros
044300020321     C                   IF        vabVAD <> 'EUR'
044400020204     C                   EVAL      vabVAD =  'ITL'
044500020204     C                   ENDIF
044600020204     C                   ENDIF
044700020204     C*
044800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
044900020204     C                   Z-ADD     vabIAS        wrkDec
045000020204     C                   IF        wrkDec > *zeros
045100020204     C                   IF        vabVAS = 'ITL'
045200020204     C                   EVAL      vabIAS = *zeros
045300020204     C                   ENDIF
045400020204     C                   ENDIF
045500020204     C*
045600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
045700020204     C                   Z-ADD     vabCAS        wrkDec
045800020204     C                   IF        wrkDec > *zeros
045900020204     C                   IF        vabVCA = 'ITL'
046000020204     C                   EVAL      vabCAS = *zeros
046100020204     C                   ENDIF
046200020204     C                   ENDIF
046300020204     C*
046400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
046500020204     C                   Z-ADD     vabVMD        wrkDec
046600020204     C                   IF        wrkDec > *zeros
046700020204     C                   IF        vabVAD = 'ITL'
046800020204     C                   EVAL      vabVMD = *zeros
046900020204     C                   ENDIF
047000020204     C                   ENDIF
047100020204     C*
047200020204     C                   ENDSR
047300020204     C***
047400020204
047500010330
047600010330
047700010330     C*----------------------------------------------------*
047800010330     C*  CONTROLLO NUMERICITA' CAMPI
047900010330     C*----------------------------------------------------*
048000010330     C     CHKNUM        BEGSR
048100010330     C*
048200010606     C                   IF        PiDecChr = *blanks
048300010606     C                   EVAL      PiDecChr = ','
048400010606     C                   ENDIF
048500010606     C*
048600010606     C                   CALL(e)   'ISNUMERIC'
048700010330     C                   PARM                    PiStr            30
048800010606     C                   PARM                    PiDecChr          1
048900010330     C                   PARM      *ZEROS        PiVal            30 9
049000010330     C                   PARM      '0'           PiInt             1
049100010330     C                   PARM      '0'           PiNum             1
049200010330     C                   IF        %error
049300010606     C                   EVAL      PiNum=*off
049400010330     C                   ENDIF
049500010330     C*
049600010330     C                   ENDSR
049700010330     C***
049800010330
049900010601
050000010601
050100010601
050200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
050300010601     C     repfil        BEGSR
050400010601     C*
050500010601     C                   if        invfil = *zeros and
050600010601     C                             depfil > *zeros and
050700010629     C                             (vinflg = *blanks or
050800010629     C                              vinflg = *zeros)
050900010601     C
051000010601     C                   eval      invfil = depfil
051100010601     C                   endif
051200010601     C*
051300010601     C                   if        depfil <> invfil and
051400010601     C                             invfil > *zeros
051500010601     C                   eval      flgMulti = '1'
051600010601     C                   if        vinflg = *blanks
051700010601     C                   add       1             cntNonEl
051800010601     C                   endif
051900010601     C                   endif
052000010601     C*
052100010601     C                   if        vinflg = '2'
052200010601     C                   eval      flgStato = '2'
052300010601     C                   endif
052400010601     C*
052500010601     C                   ENDSR
052600010601     C***
052700010601
052800010601
052900010601
053000010330
053100010330
053200010330
053300990920      /TITLE Invio dei dati al punto operativo.
053400000613     C     invio         BEGSR
053500990920     C*
053600990920     C                   reset                   dscmz
053700010601     C                   move      invfil        cmzdst
053800990920     C                   eval      cmzfld = vlrfou
053900990920     C                   eval      cmzmbd = vlrhdl
054000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
054100000710     C                   if        prmfir = *blanks
054200050408     C                   eval      cmzfla = 'FIVAB00F'
054300050408     C                   eval      cmzmba = 'FIVAB00F'
054400000710     C                   else
054500000710     C                   eval      cmzfla = prmfir
054600000710     C                   eval      cmzmba = prmfir
054700000710     C                   endif
054800990920     C                   eval      cmznrr = *zeros
054900990920     C                   move      §ctrok        cmznrr
055000021018     C                   eval      cmzlba = vlrfl1
055100990920     C                   call(e)   'TIS711C'
055200990920     C                   parm                    dscmz
055300990921     C                   parm      *blanks       esito
055400990920     C                   if        %error
055500990920     C                             or cmzerr = '1'
055600990921     C                             or esito  = '1'
055700000710     C                   eval      wrkesito = '3'
055800990920     C                   endif
055900990920     C*
056000000613     C                   ENDSR
056100990910
056200010601
056300010601
056400010601
056500010601
056600010601      /TITLE Invio dei dati al punto operativo.
056700010601     C     opeini        BEGSR
056800010601     C*
056900010601     C* Inizializzo flag e contatori operativi
057000010601     C                   movel     '0'           flgGiro           1
057100010601     C                   movel     '0'           flgMulti          1
057200010601     C                   movel     '1'           flgStato          1
057300010615     C                   movel     '0'           flgOk             1
057400010601     C                   z-add     *zeros        cntNonEl         10 0
057500010601     C                   z-add     *zeros        depfil            3 0
057600010601     C                   z-add     *zeros        invfil            3 0
057700010601     C*
057800010601     C                   ENDSR
057900010601     C***
058000010601
058100010601
058200010601
058300010330
058400010330
058500000613     C     *inzsr        BEGSR
058600990910     C*
058700990910     C     *entry        plist
058800990920     C                   parm                    tivlrds
058900990921     C                   parm      wrkesito      esito
059000000724     C                   parm                    prmlit
059100000710     C                   parm                    prmfir
059200010330     C*
059300010330     C* CALCOLA LA DATA CORRENTE
059400010330     C                   time                    wn14             14 0
059500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059600010330     C                   z-add     wn8           g08dat
059700010330     C                   z-add     *zeros        g08inv
059800010330     C                   movel     '0'           g08err
059900010330     C                   call      'XSRDA8'
060000010330     C                   parm                    wlbda8
060100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060200000613     C*
060300000613     C                   ENDSR
060400000613     C***
060500990908
060600050921     Otitvo4p   E            riepilogo         2
060700990915     O                                              'Upload via Internet'
060800990915     O                                           +1 'Traduzione TIVIN00R -'
060900050408     O                                           +1 'FIVABWWR'
061000990915     O                                           +1 'Testate bolle'
061100990915     O          E            riepilogo   2
061200990915     O                       wrkdata
061300990915     O                       wrkora              +1
061400990915     O                       procname            +1
061500990915     O          E            riepilogo   2
061600990915     O                                              'Cliente..................:'
061700010605     O                       VABCCM        z     +1
061800990915     O          E            riepilogo   2
061900990920     O                                              'Riferimento Strategi.....:'
062000990920     O                       vlrhdl              +1
062100990915     O          E            riepilogo   2
062200990915     O                                              'Giusti...................:'
062300971022     O                       §CTROK        2   +  1
062400990915     O          E            riepilogo   2
062500990915     O                                              'Sbagliati e corretti.....:'
062600971022     O                       §CTRMO        2   +  1
062700990915     O          E            riepilogo   2
062800990915     O                                              'Sbagliati e scartati.....:'
062900971022     O                       §CTRNO        2   +  1
063000000613
063100050921     Otitvo4ps  E            testdett          2
063200000613     O                                              'Upload via Internet'
063300000613     O                                           +1 'Traduzione TIVIN00R -'
063400050408     O                                           +1 'FIVABWWR'
063500000613     O                                           +1 'Testate bolle'
063600000616     O          E            testdett    3
063700000613     O                                           +2 'N° rec'
063800000613     O                                           +3 'Anteprima contenuto'
063900000616     O          E            rigadett    2
064000000613     O                       rrnum               +2
064100000621     O                       recko               +3
064200000616     O          E            findett     2
064300000613     O                       wrkdata
064400000613     O                       wrkora              +1
064500000613     O                       procname            +1
064600000616     O          E            findett     2
064700000613     O                                              'Cliente..................:'
064800010605     O                       VABCCM        z     +1
064900000616     O          E            findett     2
065000000613     O                                              'Riferimento Strategi.....:'
065100000613     O                       vlrhdl              +1
065200000616     O          E            findett     2
065300000613     O                                              'Giusti...................:'
065400000613     O                       §CTROK        2   +  1
065500000616     O          E            findett     2
065600000613     O                                              'Sbagliati e corretti.....:'
065700000613     O                       §CTRMO        2   +  1
065800000616     O          E            findett     2
065900000613     O                                              'Sbagliati e scartati.....:'
066000000613     O                       §CTRNO        2   +  1
066100000613** CMD - COMANDI CL
066200050921OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVO4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
066300050921OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVO4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
066400050921DLTOVR FILE(TITVO4P TITVO4PS) LVL(*)
066500010606
066600000613
