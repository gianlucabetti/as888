000100030929      /TITLE Upload via Internet: traduzione in FIVABWWR. (x LORENZONI - Cod. 1320031)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700030929     Ftitv68p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030929     Ftitv68ps  O    f  198        PRINTER usropn
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
004000000313     D*
004100010330
004200010330
004300010330
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100010605     C                   exsr      rwvab
005200010601     C*
005300010601     C                   seton                                        lr
005400010601
005500010601
005600010601
005700010601
005800010601     C*--------------------------------------------------------
005900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006000010601     C*--------------------------------------------------------
006100010601     C     PREELA        BEGSR
006200010601     C*
006300010601     C* SE OCCORRE SPEDIRE IN FILIALE
006400010601     C                   if        invfil <> *zeros and
006500010601     C                             flgGiro = '0'
006600010601     C*
006700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006800010601     C                   eval      flgGiro = '1'
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200010601     C     invfil        chain     azorg01l
007300000724     C                   if        %found
007400000724     C                   movel(p)  CMD(1)        depcmd
007500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000724     C                   PARM                    depcmd
008100000724     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000613     C*
009500030929     C                   if        not %open(titv68ps)
009600030929     C                   open      titv68ps
009700000616     C                   except    testdett
009800000613     C                   endif
009900010601     C*
010000010601     C                   endif
010100010601     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011300010601     C*--------------------------------------------------------
011400010601     C     ENDELA        BEGSR
011500990915     C*
011600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011700000613     C*
011800030929     C                   if        %open(titv68ps)
011900000616     C                   except    findett
012000030929     C                   close     titv68ps
012100000613     C                   endif
012200000616     C*
012300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012400011113     C                   Z-ADD     150           LENGH            15 5
012500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    CMD(3)
012700000616     C                   PARM                    LENGH
012800000616     C*
012900010601     C                   ENDSR
013000010601     C***
013100010601
013200010601
013300010601
013400010601
013500000613
013600000613     C*--------------------------------------------------------
013700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013800000613     C*--------------------------------------------------------
013900000621     C     STPR          BEGSR
014000000613     C*
014100030929     C                   if        not %open(titv68p)
014200030929     C                   open      titv68p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700030929     C                   if        %open(titv68p)
014800030929     C                   close     titv68p
014900990914     C                   endif
015000000613     C*
015100000613     C                   ENDSR
015200000613     C***
015300000613
015400010330
015500010601
015600010601
015700010601
015800010330     C*--------------------------------------------------------
015900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600021025     C                   if        not %open(fivabwwr)
016700021025     C                   open      fivabwwr
016800010330     C                   endif
016900010330     C*
017000010604     C                   clear                   §CTROK
017100010604     C                   clear                   §CTRMO
017200010604     C                   clear                   §CTRNO
017300010330     C*
017400010330     C                   DO        *HIVAL
017500010330     C*
017600010330     C                   READ      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200010618     C*
018300010330     C                   if        vindta > *blanks
018400010330     C                   add       1             rrnum
018500010330     C*
018600010601     C                   if        *in70 = *off and
018700010330     C                             (vinflg = *blanks
018800010330     C                              or vinflg = '0'
018900010330     C                              or vinflg = '2')
019000010330     C*
019100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019200010711     C                   if        vinflg = *blanks or vinflg = '0'
019300010711     C                   clear                   vinmsg
019400010711     C                   endif
019500010601     C*
019600010330     C                   exsr      inzvar
019700010330     C                   exsr      defcam
019800010605     C                   exsr      impvab
019900010601     C*
020000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020100010605     C                   eval      depfil = VABLNP
020200010601     C                   exsr      repfil
020300010601     C                   if        depfil = invfil
020400021025     C                   if        vlrpoi = 999
020500021025     C                   MOVE(P)   invfil        VABFGS
020600021025     C                   else
020700021025     C                   MOVE(P)   vlrpoi        VABFGS
020800021025     C                   endif
020900010601     C*
021000010601     C                   exsr      PREELA
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             §CTROK            7 0
021500010604     C   32              ADD       1             §CTRMO            7 0
021600010604     C   31              ADD       1             §CTRNO            7 0
021700021025     C  N31              WRITE     FIVAB000
021800010604     C*
021900010604     C                   if        *in31 = *off and
022000010604     C                             *in32 = *off
022100010604     C                   eval      vinflg = '1'
022200010604     C                   else
022300010604     C                   eval      recko = vindta
022400010604     C                   except    rigadett
022500010604     C                   eval      vinflg = '2'
022600010604     C                   endif
022700010604     C                   endif
022800010601     C*
022900010604     C                   endif
023000010604     C*
023100010330     C                   else
023200010330     C                   eval      vinflg = '1'
023300010330     C                   endif
023400010601     C*
023500010601     C  N70              update    tivin000
023600010330     C*
023700010330     C  N70              ENDdo
023800010601     C
023900010601     C*
024000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024100010601     C                   if        cntNonEl = *zeros or
024200010601     C                             flgMulti = '0'
024300010330     C* Se non ci sono record con errori ...
024400010601     C                   if        §ctrno = 0 and
024500010604     C                             §ctrmo = 0 and
024600010601     C                             flgStato <> '2'
024700010330     C* ... restituisco esito OK.
024800010330     C                   eval      wrkesito = '0'
024900010330     C                   else
025000010330     C                   if        §ctrok > 0
025100010330     C                   eval      wrkesito = '1'
025200010330     C                   else
025300010615     C                   if        flgOk = '0'
025400010615     C                   eval      wrkesito = '2'
025500010615     C                   else
025600010615     C                   eval      wrkesito = '6'
025700010615     C                   endif
025800010330     C                   endif
025900010330     C                   endif
026000010601     C                   else
026100010601     C                   eval      wrkesito = '9'
026200010601     C                   endif
026300010330     C*
026400010330     C                   if        %open(tivin00r)
026500010330     C                   close     tivin00r
026600010330     C                   endif
026700021025     C                   if        %open(fivabwwr)
026800021025     C                   close     fivabwwr
026900010330     C                   endif
027000010601     C*
027100010601     C                   if        vlrpoi <> 999
027200010601     C                   eval      invfil = vlrpoi
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        §ctrok > 0
027600010601     C                             and invfil > *zeros
027700010330     C                   exsr      invio
027800010330     C                   endif
027900010601     C*
028000010618     C                   if        flgGiro = '1'
028100010601     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600010601
028700010601
028800010601
028900010330     C*----------------------------------------------------*
029000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029100010330     C*----------------------------------------------------*
029200010330     C     INZVAR        BEGSR
029300010330     C*
029400020204     C                   Z-ADD     *zeros        Num5_0            5 0
029500030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029600030606     C                   MOVEL     '0'           FlgCAS            1
029700010330     C*
029800010330     C                   ENDSR
029900010330     C*----------------------------------------------------*
030000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030100010330     C*----------------------------------------------------*
030200010330     C     DEFCAM        BEGSR
030300010330     C*
030400020204     C* Inizializzo il buffer del record da scrivere
030500021025     C                   CLEAR                   FIVAB000
030600020204     C* Imposto i valori di default...
030700030929     C                   EVAL      VABCCM = 1320031
030800030929     C                   EVAL      VABLNP = 132
030900030929     C                   EVAL      VABCBO = '1 '
031000030929     C                   EVAL      VABTSP = 'C'
031100030929     C                   EVAL      VABCTM = '7K'
031200030526     C                   EVAL      VABCTR = 000
031300020204     C* ... e poi verifico se sono stati passati come parametri
031400020204     C                   IF        vlrppt > *blanks
031500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031600020204     C                   EXSR      CHKNUM
031700020204     C                   IF        PiInt=*on
031800020204     C                   Z-ADD     PiVal         VABCCM
031900020204     C                   ENDIF
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABLNP
032400020204     C                   ENDIF
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020204     C                   Z-ADD     PiVal         VABCTR
032900020204     C                   ENDIF
033000020204     C                   ENDIF
033100020204     C*
033200010330     C                   ENDSR
033300010607     C*----------------------------------------------------*
033400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033500010607     C*----------------------------------------------------*
033600010607     C     IMPVAB        BEGSR
033700010607     C*
033800010607     C                   SETOFF                                       3132
033900030515     C*
034000010607     C                   MOVEL     datcor        VABAAS
034100010607     C                   MOVE      datcor        VABMGS
034200010607     C*
034300010607     C* Reperimento campi ALFA
034400010607     C*
034500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034600030929     C                   EVAL      VABRSD=%trim(%subst(vindta:24:30))
034700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034900020117     C* ==
035000030929     C                   EVAL      VABRD2=%trim(%subst(vindta:54:35))
035100030929     C                   EVAL      VABIND=%trim(%subst(vindta:114:35))
035200030929     C                   EVAL      VABLOD=%trim(%subst(vindta:158:28))
035300030929     C                   EVAL      VABPRD=%trim(%subst(vindta:186:2))
035400030929     C                   EVAL      VABRMA=%trim(%subst(vindta:14:5))
035500010607     C*
035600010607     C* Reperimento campi NUMERICI
035700020604     C*
035800030515     C* CAD
035900030929     C                   EVAL      PiStr=%trim(%subst(vindta:149:5))
036000010607     C                   EXSR      CHKNUM
036100010607     C                   IF        PiInt=*on
036200020204     C                   Z-ADD     PiVal         Num5_0
036300020204     C                   MOVEL(p)  Num5_0        VABCAD
036400010607     C                   ELSE
036500010607     C                   SETON                                        32
036600020204     C                   EVAL      VABCAD = *zeros
036700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036800020204     C                             + ' ' + 'VABCAD'
036900010607     C                   ENDIF
037000030515     C* RMN/NSP
037100030929     C                   EVAL      PiStr=%trim(%subst(vindta:14:5))
037200011023     C                   EXSR      CHKNUM
037300011023     C                   IF        PiInt=*on
037400030225     C                   Z-ADD     PiVal         VABRMN
037500030225     C                   Z-ADD     PiVal         VABNSP
037600011023     C                   ELSE
037700020205     C                   EVAL      VABRMN = 1
037800011023     C                   ENDIF
037900030515     C* NCL
038000030929     C                   EVAL      PiStr=%trim(%subst(vindta:195:5))
038100010607     C                   EXSR      CHKNUM
038200010607     C                   IF        PiInt=*on
038300010607     C                   Z-ADD     PiVal         VABNCL
038400010607     C                   ELSE
038500010607     C                   SETON                                        32
038600010607     C                   Z-ADD     *zeros        VABNCL
038700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038800010607     C                             + ' ' + 'VABNCL'
038900010607     C                   ENDIF
039000030515     C* PKB
039100030929     C                   EVAL      PiStr=%trim(%subst(vindta:204:7))
039200030929     C                   EVAL      PiDecChr = ','
039300010607     C                   EXSR      CHKNUM
039400010607     C                   IF        PiNum=*on
039500010607     C                   Z-ADD(H)  PiVal         VABPKB
039600010607     C                   ELSE
039700010607     C                   SETON                                        32
039800010607     C                   Z-ADD     *zeros        VABPKB
039900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040000010607     C                             + ' ' + 'VABPKB'
040100010607     C                   ENDIF
040200030929     C*
040300030929     C* Considerazioni finali su CBO/CAS
040400030929     C                   IF        FlgCAS = '1'
040500030929     C                   EVAL      VABCBO = '4'
040600030929     C                   ELSE
040700030929     C                   EVAL      VABCBO = '1'
040800030929     C                   ENDIF
040900020204     C*
041000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041100020204     C                   EXSR      CHKIMPDIV
041200020204     C*
041300010607     C                   ENDSR
041400010607     C*----------------------------------------------------*
041500010601
041600020204
041700020204     C*----------------------------------------------------*
041800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
041900020204     C*----------------------------------------------------*
042000020204     C     CHKIMPDIV     BEGSR
042100020204     C*
042200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
042300020204     C                   Z-ADD     *zeros        wrkDec            9 9
042400020204     C*
042500020204     C* Come prima cosa effettuo considerazioni sulla divisa
042600020204     C                   IF        vabIAS > *zeros
042700020204     C                   IF        vabVAS <> 'EUR'
042800020204     C                   EVAL      vabVAS =  'ITL'
042900020204     C                   ENDIF
043000020204     C                   ENDIF
043100020204     C*
043200020204     C                   IF        vabCAS > *zeros
043300020204     C                   IF        vabVCA <> 'EUR'
043400020204     C                   EVAL      vabVCA =  'ITL'
043500020204     C                   ENDIF
043600020204     C                   ENDIF
043700020204     C*
043800020204     C                   IF        vabVMD > *zeros
043900020204     C                   IF        vabVAD <> 'EUR'
044000020204     C                   EVAL      vabVAD =  'ITL'
044100020204     C                   ENDIF
044200020204     C                   ENDIF
044300020204     C*
044400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
044500020204     C                   Z-ADD     vabIAS        wrkDec
044600020204     C                   IF        wrkDec > *zeros
044700020204     C                   IF        vabVAS = 'ITL'
044800020204     C                   EVAL      vabIAS = *zeros
044900020204     C                   ENDIF
045000020204     C                   ENDIF
045100020204     C*
045200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
045300020204     C                   Z-ADD     vabCAS        wrkDec
045400020204     C                   IF        wrkDec > *zeros
045500020204     C                   IF        vabVCA = 'ITL'
045600020204     C                   EVAL      vabCAS = *zeros
045700020204     C                   ENDIF
045800020204     C                   ENDIF
045900020204     C*
046000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
046100020204     C                   Z-ADD     vabVMD        wrkDec
046200020204     C                   IF        wrkDec > *zeros
046300020204     C                   IF        vabVAD = 'ITL'
046400020204     C                   EVAL      vabVMD = *zeros
046500020204     C                   ENDIF
046600020204     C                   ENDIF
046700020204     C*
046800020204     C                   ENDSR
046900020204     C***
047000020204
047100010330
047200010330
047300010330     C*----------------------------------------------------*
047400010330     C*  CONTROLLO NUMERICITA' CAMPI
047500010330     C*----------------------------------------------------*
047600010330     C     CHKNUM        BEGSR
047700010330     C*
047800010606     C                   IF        PiDecChr = *blanks
047900030606     C                   EVAL      PiDecChr = ','
048000010606     C                   ENDIF
048100010606     C*
048200010606     C                   CALL(e)   'ISNUMERIC'
048300010330     C                   PARM                    PiStr            30
048400010606     C                   PARM                    PiDecChr          1
048500010330     C                   PARM      *ZEROS        PiVal            30 9
048600010330     C                   PARM      '0'           PiInt             1
048700010330     C                   PARM      '0'           PiNum             1
048800010330     C                   IF        %error
048900010606     C                   EVAL      PiNum=*off
049000010330     C                   ENDIF
049100010330     C*
049200010330     C                   ENDSR
049300010330     C***
049400010330
049500010601
049600010601
049700010601
049800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
049900010601     C     repfil        BEGSR
050000010601     C*
050100010601     C                   if        invfil = *zeros and
050200010601     C                             depfil > *zeros and
050300010629     C                             (vinflg = *blanks or
050400010629     C                              vinflg = *zeros)
050500010601     C
050600010601     C                   eval      invfil = depfil
050700010601     C                   endif
050800010601     C*
050900010601     C                   if        depfil <> invfil and
051000010601     C                             invfil > *zeros
051100010601     C                   eval      flgMulti = '1'
051200010601     C                   if        vinflg = *blanks
051300010601     C                   add       1             cntNonEl
051400010601     C                   endif
051500010601     C                   endif
051600010601     C*
051700010601     C                   if        vinflg = '2'
051800010601     C                   eval      flgStato = '2'
051900010601     C                   endif
052000010601     C*
052100010601     C                   ENDSR
052200010601     C***
052300010601
052400010601
052500010601
052600010330
052700010330
052800010330
052900990920      /TITLE Invio dei dati al punto operativo.
053000000613     C     invio         BEGSR
053100990920     C*
053200990920     C                   reset                   dscmz
053300010601     C                   move      invfil        cmzdst
053400990920     C                   eval      cmzfld = vlrfou
053500990920     C                   eval      cmzmbd = vlrhdl
053600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
053700000710     C                   if        prmfir = *blanks
053800021025     C                   eval      cmzfla = 'FIVAB00F'
053900021025     C                   eval      cmzmba = 'FIVAB00F'
054000000710     C                   else
054100000710     C                   eval      cmzfla = prmfir
054200000710     C                   eval      cmzmba = prmfir
054300000710     C                   endif
054400990920     C                   eval      cmznrr = *zeros
054500990920     C                   move      §ctrok        cmznrr
054600021018     C                   eval      cmzlba = vlrfl1
054700990920     C                   call(e)   'TIS711C'
054800990920     C                   parm                    dscmz
054900990921     C                   parm      *blanks       esito
055000990920     C                   if        %error
055100990920     C                             or cmzerr = '1'
055200990921     C                             or esito  = '1'
055300000710     C                   eval      wrkesito = '3'
055400990920     C                   endif
055500990920     C*
055600000613     C                   ENDSR
055700990910
055800010601
055900010601
056000010601
056100010601
056200010601      /TITLE Invio dei dati al punto operativo.
056300010601     C     opeini        BEGSR
056400010601     C*
056500010601     C* Inizializzo flag e contatori operativi
056600010601     C                   movel     '0'           flgGiro           1
056700010601     C                   movel     '0'           flgMulti          1
056800010601     C                   movel     '1'           flgStato          1
056900010615     C                   movel     '0'           flgOk             1
057000010601     C                   z-add     *zeros        cntNonEl         10 0
057100010601     C                   z-add     *zeros        depfil            3 0
057200010601     C                   z-add     *zeros        invfil            3 0
057300010601     C*
057400010601     C                   ENDSR
057500010601     C***
057600010601
057700010601
057800010601
057900010330
058000010330
058100000613     C     *inzsr        BEGSR
058200990910     C*
058300990910     C     *entry        plist
058400990920     C                   parm                    tivlrds
058500990921     C                   parm      wrkesito      esito
058600000724     C                   parm                    prmlit
058700000710     C                   parm                    prmfir
058800010330     C*
058900010330     C* CALCOLA LA DATA CORRENTE
059000010330     C                   time                    wn14             14 0
059100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059200010330     C                   z-add     wn8           g08dat
059300010330     C                   z-add     *zeros        g08inv
059400010330     C                   movel     '0'           g08err
059500010330     C                   call      'XSRDA8'
059600010330     C                   parm                    wlbda8
059700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059800000613     C*
059900000613     C                   ENDSR
060000000613     C***
060100990908
060200030929     Otitv68p   E            riepilogo         2
060300990915     O                                              'Upload via Internet'
060400990915     O                                           +1 'Traduzione TIVIN00R -'
060500021025     O                                           +1 'FIVABWWR'
060600990915     O                                           +1 'Testate bolle'
060700990915     O          E            riepilogo   2
060800990915     O                       wrkdata
060900990915     O                       wrkora              +1
061000990915     O                       procname            +1
061100990915     O          E            riepilogo   2
061200990915     O                                              'Cliente..................:'
061300010605     O                       VABCCM        z     +1
061400990915     O          E            riepilogo   2
061500990920     O                                              'Riferimento Strategi.....:'
061600990920     O                       vlrhdl              +1
061700990915     O          E            riepilogo   2
061800990915     O                                              'Giusti...................:'
061900971022     O                       §CTROK        2   +  1
062000990915     O          E            riepilogo   2
062100990915     O                                              'Sbagliati e corretti.....:'
062200971022     O                       §CTRMO        2   +  1
062300990915     O          E            riepilogo   2
062400990915     O                                              'Sbagliati e scartati.....:'
062500971022     O                       §CTRNO        2   +  1
062600000613
062700030929     Otitv68ps  E            testdett          2
062800000613     O                                              'Upload via Internet'
062900000613     O                                           +1 'Traduzione TIVIN00R -'
063000021025     O                                           +1 'FIVABWWR'
063100000613     O                                           +1 'Testate bolle'
063200000616     O          E            testdett    3
063300000613     O                                           +2 'N° rec'
063400000613     O                                           +3 'Anteprima contenuto'
063500000616     O          E            rigadett    2
063600000613     O                       rrnum               +2
063700000621     O                       recko               +3
063800000616     O          E            findett     2
063900000613     O                       wrkdata
064000000613     O                       wrkora              +1
064100000613     O                       procname            +1
064200000616     O          E            findett     2
064300000613     O                                              'Cliente..................:'
064400010605     O                       VABCCM        z     +1
064500000616     O          E            findett     2
064600000613     O                                              'Riferimento Strategi.....:'
064700000613     O                       vlrhdl              +1
064800000616     O          E            findett     2
064900000613     O                                              'Giusti...................:'
065000000613     O                       §CTROK        2   +  1
065100000616     O          E            findett     2
065200000613     O                                              'Sbagliati e corretti.....:'
065300000613     O                       §CTRMO        2   +  1
065400000616     O          E            findett     2
065500000613     O                                              'Sbagliati e scartati.....:'
065600000613     O                       §CTRNO        2   +  1
065700000613** CMD - COMANDI CL
065800030929OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV68P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
065900030929OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV68PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
066000030929DLTOVR FILE(TITV68P TITV68PS) LVL(*)
066100010606
066200000613
