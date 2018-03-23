000100040116      /TITLE Upload via Internet: traduzione in FIVABWWR. (x HAIR COMANY Cod. 2493052/0540424)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040116     Ftitv88p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040116     Ftitv88ps  O    f  198        PRINTER usropn
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
009500040116     C                   if        not %open(titv88ps)
009600040116     C                   open      titv88ps
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
011800040116     C                   if        %open(titv88ps)
011900000616     C                   except    findett
012000040116     C                   close     titv88ps
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
014100040116     C                   if        not %open(titv88p)
014200040116     C                   open      titv88p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700040116     C                   if        %open(titv88p)
014800040116     C                   close     titv88p
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
018300040122     C                   if        vindta > *blanks AND
018400040122     C                             %subst(vindta:14:8) > *blanks
018500010330     C                   add       1             rrnum
018600010330     C*
018700010601     C                   if        *in70 = *off and
018800010330     C                             (vinflg = *blanks
018900010330     C                              or vinflg = '0'
019000010330     C                              or vinflg = '2')
019100010330     C*
019200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019300010711     C                   if        vinflg = *blanks or vinflg = '0'
019400010711     C                   clear                   vinmsg
019500010711     C                   endif
019600010601     C*
019700010330     C                   exsr      inzvar
019800010330     C                   exsr      defcam
019900010605     C                   exsr      impvab
020000010601     C*
020100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020200010605     C                   eval      depfil = VABLNP
020300010601     C                   exsr      repfil
020400010601     C                   if        depfil = invfil
020500021025     C                   if        vlrpoi = 999
020600021025     C                   MOVE(P)   invfil        VABFGS
020700021025     C                   else
020800021025     C                   MOVE(P)   vlrpoi        VABFGS
020900021025     C                   endif
021000010601     C*
021100010601     C                   exsr      PREELA
021200010601     C*
021300010604     C* Ebbene...
021400010604     C*
021500010604     C  N31              ADD       1             §CTROK            7 0
021600010604     C   32              ADD       1             §CTRMO            7 0
021700010604     C   31              ADD       1             §CTRNO            7 0
021800021025     C  N31              WRITE     FIVAB000
021900010604     C*
022000010604     C                   if        *in31 = *off and
022100010604     C                             *in32 = *off
022200010604     C                   eval      vinflg = '1'
022300010604     C                   else
022400010604     C                   eval      recko = vindta
022500010604     C                   except    rigadett
022600010604     C                   eval      vinflg = '2'
022700010604     C                   endif
022800010604     C                   endif
022900010601     C*
023000010604     C                   endif
023100010604     C*
023200010330     C                   else
023300010330     C                   eval      vinflg = '1'
023400010330     C                   endif
023500010601     C*
023600010601     C  N70              update    tivin000
023700010330     C*
023800010330     C  N70              ENDdo
023900010601     C
024000010601     C*
024100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024200010601     C                   if        cntNonEl = *zeros or
024300010601     C                             flgMulti = '0'
024400010330     C* Se non ci sono record con errori ...
024500010601     C                   if        §ctrno = 0 and
024600010604     C                             §ctrmo = 0 and
024700010601     C                             flgStato <> '2'
024800010330     C* ... restituisco esito OK.
024900010330     C                   eval      wrkesito = '0'
025000010330     C                   else
025100010330     C                   if        §ctrok > 0
025200010330     C                   eval      wrkesito = '1'
025300010330     C                   else
025400010615     C                   if        flgOk = '0'
025500010615     C                   eval      wrkesito = '2'
025600010615     C                   else
025700010615     C                   eval      wrkesito = '6'
025800010615     C                   endif
025900010330     C                   endif
026000010330     C                   endif
026100010601     C                   else
026200010601     C                   eval      wrkesito = '9'
026300010601     C                   endif
026400010330     C*
026500010330     C                   if        %open(tivin00r)
026600010330     C                   close     tivin00r
026700010330     C                   endif
026800021025     C                   if        %open(fivabwwr)
026900021025     C                   close     fivabwwr
027000010330     C                   endif
027100010601     C*
027200010601     C                   if        vlrpoi <> 999
027300010601     C                   eval      invfil = vlrpoi
027400010601     C                   endif
027500010330     C*
027600010330     C                   if        §ctrok > 0
027700010601     C                             and invfil > *zeros
027800010330     C                   exsr      invio
027900010330     C                   endif
028000010601     C*
028100010618     C                   if        flgGiro = '1'
028200010601     C                   exsr      endela
028300010618     C                   endif
028400010330     C*
028500010330     C                   ENDSR
028600010330     C***
028700010601
028800010601
028900010601
029000010330     C*----------------------------------------------------*
029100010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029200010330     C*----------------------------------------------------*
029300010330     C     INZVAR        BEGSR
029400010330     C*
029500020204     C                   Z-ADD     *zeros        Num5_0            5 0
029600030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029700030606     C                   MOVEL     '0'           FlgCAS            1
029800010330     C*
029900010330     C                   ENDSR
030000010330     C*----------------------------------------------------*
030100010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030200010330     C*----------------------------------------------------*
030300010330     C     DEFCAM        BEGSR
030400010330     C*
030500020204     C* Inizializzo il buffer del record da scrivere
030600021025     C                   CLEAR                   FIVAB000
030700020204     C* Imposto i valori di default...
030800031013     C                   EVAL      VABCTR = 000
030900020204     C* ... e poi verifico se sono stati passati come parametri
031000020204     C                   IF        vlrppt > *blanks
031100040116     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:3))
031200020204     C                   EXSR      CHKNUM
031300020204     C                   IF        PiInt=*on
031400020204     C                   Z-ADD     PiVal         VABCTR
031500020204     C                   ENDIF
031600040202     C                   EVAL      PiStr=%trim(%subst(vlrppt:4:6))
031700040202     C                   EXSR      CHKNUM
031800040202     C                   IF        PiInt=*on
031900040202     C                   Z-ADD     PiVal         VABLNP
032000040202     C                   ENDIF
032100020204     C                   ENDIF
032200020204     C*
032300010330     C                   ENDSR
032400010607     C*----------------------------------------------------*
032500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032600010607     C*----------------------------------------------------*
032700010607     C     IMPVAB        BEGSR
032800010607     C*
032900010607     C                   SETOFF                                       3132
033000030515     C*
033100010607     C                   MOVEL     datcor        VABAAS
033200010607     C                   MOVE      datcor        VABMGS
033300010607     C*
033400010607     C* Reperimento campi ALFA
033500010607     C*
033600010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
033700040116     C                   EVAL      VABRSD=%trim(%subst(vindta:83:35))
033800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034000020117     C* ==
034100040116     C                   EVAL      VABRD2=%trim(%subst(vindta:118:24))+
034200040116     C                                    %trim(%subst(vindta:72:11))
034300040121     C     ':':'.'       XLATE     VABRD2        VABRD2
034400040121     C*
034500040116     C                   EVAL      VABIND=%trim(%subst(vindta:173:35))
034600040116     C                   EVAL      VABLOD=%trim(%subst(vindta:273:35))
034700040116     C                   EVAL      VABPRD=%trim(%subst(vindta:353:2))
034800040116     C                   EVAL      VABRMA=%trim(%subst(vindta:47:5))+' '+
034900040116     C                                    %trim(%subst(vindta:434:9))
035000010607     C*
035100010607     C* Reperimento campi NUMERICI
035200020604     C*
035300040202     C* CCM
035400040116     C                   IF        %subst(vindta:17:1) = '/'
035500040116     C                   EVAL      PiStr=%subst(vindta:14:3)+
035600040116     C                                   %subst(vindta:18:4)
035700040116     C                   ELSE
035800040116     C                   EVAL      PiStr=%subst(vindta:14:7)
035900040116     C                   ENDIF
036000040116     C                   EXSR      CHKNUM
036100040116     C                   IF        PiInt=*on
036200040116     C                   Z-ADD     PiVal         VABCCM
036300040116     C                   ELSE
036400040116     C                   SETON                                        31
036500040116     C                   EVAL      VABCCM = *zeros
036600040116     C                   EVAL      vinmsg = %trimr(vinmsg)
036700040202     C                             + ' ' + 'VABCCM'
036800040116     C                   ENDIF
036900031029     C* CAD
037000040116     C                   EVAL      PiStr=%trim(%subst(vindta:253:5))
037100010607     C                   EXSR      CHKNUM
037200010607     C                   IF        PiInt=*on
037300020204     C                   Z-ADD     PiVal         Num5_0
037400020204     C                   MOVEL(p)  Num5_0        VABCAD
037500010607     C                   ELSE
037600010607     C                   SETON                                        32
037700020204     C                   EVAL      VABCAD = *zeros
037800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037900020204     C                             + ' ' + 'VABCAD'
038000010607     C                   ENDIF
038100030515     C* RMN/NSP
038200040116     C                   EVAL      PiStr=%trim(%subst(vindta:62:6))
038300011023     C                   EXSR      CHKNUM
038400011023     C                   IF        PiInt=*on
038500030225     C                   Z-ADD     PiVal         VABRMN
038600031013     C                   MOVE(P)   VABRMN        VABNSP
038700011023     C                   ELSE
038800020205     C                   EVAL      VABRMN = 1
038900011023     C                   ENDIF
039000030515     C* NCL
039100040116     C                   EVAL      PiStr=%trim(%subst(vindta:373:6))
039200010607     C                   EXSR      CHKNUM
039300010607     C                   IF        PiInt=*on
039400010607     C                   Z-ADD     PiVal         VABNCL
039500010607     C                   ELSE
039600010607     C                   SETON                                        32
039700010607     C                   Z-ADD     *zeros        VABNCL
039800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039900010607     C                             + ' ' + 'VABNCL'
040000010607     C                   ENDIF
040100031013     C* PKB
040200040116     C                   EVAL      PiStr=%trim(%subst(vindta:379:13))
040300031013     C                   EXSR      CHKNUM
040400031017     C                   IF        PiNum=*on
040500040116     C                   Z-ADD(H)  PiVal         VABPKB
040600031013     C                   ELSE
040700031013     C                   SETON                                        32
040800031013     C                   Z-ADD     *zeros        VABPKB
040900031013     C                   EVAL      vinmsg = %trimr(vinmsg)
041000031013     C                             + ' ' + 'VABPKB'
041100031013     C                   ENDIF
041200040116     C* VLB
041300040116     C                   EVAL      PiStr=%trim(%subst(vindta:392:13))
041400040116     C                   EXSR      CHKNUM
041500040116     C                   IF        PiNum=*on
041600040116     C                   Z-ADD(H)  PiVal         VABVLB
041700040116     C                   ELSE
041800040116     C                   SETON                                        32
041900040116     C                   Z-ADD     *zeros        VABVLB
042000040116     C                   EVAL      vinmsg = %trimr(vinmsg)
042100040116     C                             + ' ' + 'VABVLB'
042200040116     C                   ENDIF
042300040116     C* CAS
042400040116     C                   IF        %subst(vindta:405:13) <> '00000000,0000'
042500040116     C                   EVAL      FlgCAS = '1'
042600040116     C                   EVAL      PiStr=%trim(%subst(vindta:405:13))
042700040116     C                   EXSR      CHKNUM
042800040116     C                   IF        PiNum=*on
042900040116     C                   Z-ADD     PiVal         VABCAS
043000040116     C                   MOVEL     'EUR'         VABVCA
043100040116     C                   ELSE
043200040116     C                   SETON                                        32
043300040116     C                   Z-ADD     *zeros        VABCAS
043400040116     C                   EVAL      vinmsg = %trimr(vinmsg)
043500040116     C                             + ' ' + 'VABCAS'
043600040116     C                   ENDIF
043700040116     C                   ENDIF
043800030929     C*
043900030929     C* Considerazioni finali su CBO/CAS
044000030929     C                   IF        FlgCAS = '1'
044100030929     C                   EVAL      VABCBO = '4'
044200030929     C                   ELSE
044300030929     C                   EVAL      VABCBO = '1'
044400030929     C                   ENDIF
044500020204     C*
044600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044700020204     C                   EXSR      CHKIMPDIV
044800020204     C*
044900010607     C                   ENDSR
045000010607     C*----------------------------------------------------*
045100010601
045200020204
045300020204     C*----------------------------------------------------*
045400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045500020204     C*----------------------------------------------------*
045600020204     C     CHKIMPDIV     BEGSR
045700020204     C*
045800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045900020204     C                   Z-ADD     *zeros        wrkDec            9 9
046000020204     C*
046100020204     C* Come prima cosa effettuo considerazioni sulla divisa
046200020204     C                   IF        vabIAS > *zeros
046300020204     C                   IF        vabVAS <> 'EUR'
046400020204     C                   EVAL      vabVAS =  'ITL'
046500020204     C                   ENDIF
046600020204     C                   ENDIF
046700020204     C*
046800020204     C                   IF        vabCAS > *zeros
046900020204     C                   IF        vabVCA <> 'EUR'
047000020204     C                   EVAL      vabVCA =  'ITL'
047100020204     C                   ENDIF
047200020204     C                   ENDIF
047300020204     C*
047400020204     C                   IF        vabVMD > *zeros
047500020204     C                   IF        vabVAD <> 'EUR'
047600020204     C                   EVAL      vabVAD =  'ITL'
047700020204     C                   ENDIF
047800020204     C                   ENDIF
047900020204     C*
048000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048100020204     C                   Z-ADD     vabIAS        wrkDec
048200020204     C                   IF        wrkDec > *zeros
048300020204     C                   IF        vabVAS = 'ITL'
048400020204     C                   EVAL      vabIAS = *zeros
048500020204     C                   ENDIF
048600020204     C                   ENDIF
048700020204     C*
048800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
048900020204     C                   Z-ADD     vabCAS        wrkDec
049000020204     C                   IF        wrkDec > *zeros
049100020204     C                   IF        vabVCA = 'ITL'
049200020204     C                   EVAL      vabCAS = *zeros
049300020204     C                   ENDIF
049400020204     C                   ENDIF
049500020204     C*
049600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049700020204     C                   Z-ADD     vabVMD        wrkDec
049800020204     C                   IF        wrkDec > *zeros
049900020204     C                   IF        vabVAD = 'ITL'
050000020204     C                   EVAL      vabVMD = *zeros
050100020204     C                   ENDIF
050200020204     C                   ENDIF
050300020204     C*
050400020204     C                   ENDSR
050500020204     C***
050600020204
050700010330
050800010330
050900010330     C*----------------------------------------------------*
051000010330     C*  CONTROLLO NUMERICITA' CAMPI
051100010330     C*----------------------------------------------------*
051200010330     C     CHKNUM        BEGSR
051300010330     C*
051400010606     C                   IF        PiDecChr = *blanks
051500030606     C                   EVAL      PiDecChr = ','
051600010606     C                   ENDIF
051700010606     C*
051800010606     C                   CALL(e)   'ISNUMERIC'
051900010330     C                   PARM                    PiStr            30
052000010606     C                   PARM                    PiDecChr          1
052100010330     C                   PARM      *ZEROS        PiVal            30 9
052200010330     C                   PARM      '0'           PiInt             1
052300010330     C                   PARM      '0'           PiNum             1
052400010330     C                   IF        %error
052500010606     C                   EVAL      PiNum=*off
052600010330     C                   ENDIF
052700010330     C*
052800010330     C                   ENDSR
052900010330     C***
053000010330
053100010601
053200010601
053300010601
053400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053500010601     C     repfil        BEGSR
053600010601     C*
053700010601     C                   if        invfil = *zeros and
053800010601     C                             depfil > *zeros and
053900010629     C                             (vinflg = *blanks or
054000010629     C                              vinflg = *zeros)
054100010601     C
054200010601     C                   eval      invfil = depfil
054300010601     C                   endif
054400010601     C*
054500010601     C                   if        depfil <> invfil and
054600010601     C                             invfil > *zeros
054700010601     C                   eval      flgMulti = '1'
054800010601     C                   if        vinflg = *blanks
054900010601     C                   add       1             cntNonEl
055000010601     C                   endif
055100010601     C                   endif
055200010601     C*
055300010601     C                   if        vinflg = '2'
055400010601     C                   eval      flgStato = '2'
055500010601     C                   endif
055600010601     C*
055700010601     C                   ENDSR
055800010601     C***
055900010601
056000010601
056100010601
056200010330
056300010330
056400010330
056500990920      /TITLE Invio dei dati al punto operativo.
056600000613     C     invio         BEGSR
056700990920     C*
056800990920     C                   reset                   dscmz
056900010601     C                   move      invfil        cmzdst
057000990920     C                   eval      cmzfld = vlrfou
057100990920     C                   eval      cmzmbd = vlrhdl
057200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
057300000710     C                   if        prmfir = *blanks
057400021025     C                   eval      cmzfla = 'FIVAB00F'
057500021025     C                   eval      cmzmba = 'FIVAB00F'
057600000710     C                   else
057700000710     C                   eval      cmzfla = prmfir
057800000710     C                   eval      cmzmba = prmfir
057900000710     C                   endif
058000990920     C                   eval      cmznrr = *zeros
058100990920     C                   move      §ctrok        cmznrr
058200021018     C                   eval      cmzlba = vlrfl1
058300990920     C                   call(e)   'TIS711C'
058400990920     C                   parm                    dscmz
058500990921     C                   parm      *blanks       esito
058600990920     C                   if        %error
058700990920     C                             or cmzerr = '1'
058800990921     C                             or esito  = '1'
058900000710     C                   eval      wrkesito = '3'
059000990920     C                   endif
059100990920     C*
059200000613     C                   ENDSR
059300990910
059400010601
059500010601
059600010601
059700010601
059800010601      /TITLE Invio dei dati al punto operativo.
059900010601     C     opeini        BEGSR
060000010601     C*
060100010601     C* Inizializzo flag e contatori operativi
060200010601     C                   movel     '0'           flgGiro           1
060300010601     C                   movel     '0'           flgMulti          1
060400010601     C                   movel     '1'           flgStato          1
060500010615     C                   movel     '0'           flgOk             1
060600010601     C                   z-add     *zeros        cntNonEl         10 0
060700010601     C                   z-add     *zeros        depfil            3 0
060800010601     C                   z-add     *zeros        invfil            3 0
060900010601     C*
061000010601     C                   ENDSR
061100010601     C***
061200010601
061300010601
061400010601
061500010330
061600010330
061700000613     C     *inzsr        BEGSR
061800990910     C*
061900990910     C     *entry        plist
062000990920     C                   parm                    tivlrds
062100990921     C                   parm      wrkesito      esito
062200000724     C                   parm                    prmlit
062300000710     C                   parm                    prmfir
062400010330     C*
062500010330     C* CALCOLA LA DATA CORRENTE
062600010330     C                   time                    wn14             14 0
062700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062800010330     C                   z-add     wn8           g08dat
062900010330     C                   z-add     *zeros        g08inv
063000010330     C                   movel     '0'           g08err
063100010330     C                   call      'XSRDA8'
063200010330     C                   parm                    wlbda8
063300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063400000613     C*
063500000613     C                   ENDSR
063600000613     C***
063700990908
063800040116     Otitv88p   E            riepilogo         2
063900990915     O                                              'Upload via Internet'
064000990915     O                                           +1 'Traduzione TIVIN00R -'
064100021025     O                                           +1 'FIVABWWR'
064200990915     O                                           +1 'Testate bolle'
064300990915     O          E            riepilogo   2
064400990915     O                       wrkdata
064500990915     O                       wrkora              +1
064600990915     O                       procname            +1
064700990915     O          E            riepilogo   2
064800990915     O                                              'Cliente..................:'
064900010605     O                       VABCCM        z     +1
065000990915     O          E            riepilogo   2
065100990920     O                                              'Riferimento Strategi.....:'
065200990920     O                       vlrhdl              +1
065300990915     O          E            riepilogo   2
065400990915     O                                              'Giusti...................:'
065500971022     O                       §CTROK        2   +  1
065600990915     O          E            riepilogo   2
065700990915     O                                              'Sbagliati e corretti.....:'
065800971022     O                       §CTRMO        2   +  1
065900990915     O          E            riepilogo   2
066000990915     O                                              'Sbagliati e scartati.....:'
066100971022     O                       §CTRNO        2   +  1
066200000613
066300040116     Otitv88ps  E            testdett          2
066400000613     O                                              'Upload via Internet'
066500000613     O                                           +1 'Traduzione TIVIN00R -'
066600021025     O                                           +1 'FIVABWWR'
066700000613     O                                           +1 'Testate bolle'
066800000616     O          E            testdett    3
066900000613     O                                           +2 'N° rec'
067000000613     O                                           +3 'Anteprima contenuto'
067100000616     O          E            rigadett    2
067200000613     O                       rrnum               +2
067300000621     O                       recko               +3
067400000616     O          E            findett     2
067500000613     O                       wrkdata
067600000613     O                       wrkora              +1
067700000613     O                       procname            +1
067800000616     O          E            findett     2
067900000613     O                                              'Cliente..................:'
068000010605     O                       VABCCM        z     +1
068100000616     O          E            findett     2
068200000613     O                                              'Riferimento Strategi.....:'
068300000613     O                       vlrhdl              +1
068400000616     O          E            findett     2
068500000613     O                                              'Giusti...................:'
068600000613     O                       §CTROK        2   +  1
068700000616     O          E            findett     2
068800000613     O                                              'Sbagliati e corretti.....:'
068900000613     O                       §CTRMO        2   +  1
069000000616     O          E            findett     2
069100000613     O                                              'Sbagliati e scartati.....:'
069200000613     O                       §CTRNO        2   +  1
069300000613** CMD - COMANDI CL
069400040116OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV88P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
069500040116OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV88PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
069600040116DLTOVR FILE(TITV88P TITV88PS) LVL(*)
069700010606
069800000613
