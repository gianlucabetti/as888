000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR. (x IMEC - Cod. 0930895)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700010913     Ftis7imp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010913     Ftis7imps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
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
003100011123     D depcmd          s            150    INZ(*blanks)
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
007500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011123     C                   Z-ADD     150           LENGH            15 5
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
009000011123     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000613     C*
009500010913     C                   if        not %open(tis7imps)
009600010913     C                   open      tis7imps
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
011800010913     C                   if        %open(tis7imps)
011900000616     C                   except    findett
012000010913     C                   close     tis7imps
012100000613     C                   endif
012200000616     C*
012300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012400011123     C                   Z-ADD     150           LENGH            15 5
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
014100010913     C                   if        not %open(tis7imp)
014200010913     C                   open      tis7imp
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700010913     C                   if        %open(tis7imp)
014800010913     C                   close     tis7imp
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
015900021113     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600021113     C                   if        not %open(fivabwwr)
016700021113     C                   open      fivabwwr
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
020400021113     C                   if        vlrpoi = 999
020500021113     C                   MOVE(P)   invfil        VABFGS
020600021113     C                   else
020700021113     C                   MOVE(P)   vlrpoi        VABFGS
020800021113     C                   endif
020900010601     C*
021000010601     C                   exsr      PREELA
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             §CTROK            7 0
021500010604     C   32              ADD       1             §CTRMO            7 0
021600010604     C   31              ADD       1             §CTRNO            7 0
021700021113     C  N31              WRITE     FIVAB000
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
026700021113     C                   if        %open(fivabwwr)
026800021113     C                   close     fivabwwr
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
028100010618     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600010330
028700010601
028800010601
028900010601
029000010601
029100010330     C*----------------------------------------------------*
029200010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029300010330     C*----------------------------------------------------*
029400010330     C     INZVAR        BEGSR
029500010330     C*
029600010330     C*
029700010330     C                   ENDSR
029800010330     C*----------------------------------------------------*
029900010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030000010330     C*----------------------------------------------------*
030100010330     C     DEFCAM        BEGSR
030200010330     C*
030300021113     C                   CLEAR                   FIVAB000
030400060721     C                   EVAL      VABCCM = 0932854
030500010913     C                   EVAL      VABLNP = 093
030600010605     C                   EVAL      VABCTR = 000
030700010330     C*
030800010330     C                   ENDSR
030900010330     C*----------------------------------------------------*
031000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031100010330     C*----------------------------------------------------*
031200010605     C     IMPVAB        BEGSR
031300010330     C*
031400010604     C                   SETOFF                                       3132
031500010605     C                   MOVEL     datcor        VABAAS
031600010607     C                   MOVE      datcor        VABMGS
031700010330     C*
031800010330     C* Reperimento campi ALFA
031900010913     C                   IF        %trim(%subst(vindta:16:1)) = '1'
032000010913     C                   EVAL      VABFFD = 'S'
032100010913     C                   ENDIF
032200010913     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
032300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032500020117     C* ==
032600010913     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
032700010913     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
032800010913     C                   EVAL      VABCAD=%trim(%subst(vindta:107:5))
032900010913     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
033000010913     C                   EVAL      VABRMA=%trim(%subst(vindta:319:22))
033100010913     C                   EVAL      VABRD2=%trim(%subst(vindta:92:15))
033200010913     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
033300010913     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
033400010913     C                   EVAL      VABNT2=%trim(%subst(vindta:232:35))
033500010330     C*
033600010913     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
033700011115     C                   EVAL      PiDecChr='.'
033800010330     C                   EXSR      CHKNUM
033900010330     C                   IF        PiInt=*on
034000010605     C                   Z-ADD     PiVal         VABNCL
034100010330     C                   ELSE
034200010605     C                   SETON                                        32
034300010605     C                   Z-ADD     *zeros        VABNCL
034400010330     C                   EVAL      vinmsg = %trimr(vinmsg)
034500010605     C                             + ' ' + 'VABNCL'
034600010330     C                   ENDIF
034700010605     C*
034800010913     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
034900011115     C                   EVAL      PiDecChr='.'
035000010605     C                   EXSR      CHKNUM
035100010605     C                   IF        PiNum=*on
035200010606     C                   Z-ADD(H)  PiVal         VABPKB
035300010605     C                   ELSE
035400010605     C                   SETON                                        32
035500010605     C                   Z-ADD     *zeros        VABPKB
035600010605     C                   EVAL      vinmsg = %trimr(vinmsg)
035700010605     C                             + ' ' + 'VABPKB'
035800010605     C                   ENDIF
035900010913     C*
036000010913     C                   IF        %subst(vindta:296:7) = *blanks
036100010913     C                   EVAL      PiStr=*all'0'
036200010913     C                   ELSE
036300010913     C                   EVAL      PiStr=%trim(%subst(vindta:296:7))
036400010913     C                   ENDIF
036500011115     C                   EVAL      PiDecChr='.'
036600010913     C                   EXSR      CHKNUM
036700010913     C                   IF        PiNum=*on
036800010913     C                   Z-ADD     PiVal         VABVLB
036900010913     C                   ELSE
037000010913     C                   SETON                                        32
037100010913     C                   Z-ADD     *zeros        VABVLB
037200010913     C                   EVAL      vinmsg = %trimr(vinmsg)
037300010913     C                             + ' ' + 'VABVLB'
037400010913     C                   ENDIF
037500010913     C*
037600010913     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
037700011115     C                   EVAL      PiDecChr='.'
037800010605     C                   EXSR      CHKNUM
037900010605     C                   IF        PiInt=*on
038000010606     C                   Z-ADD     PiVal         VABRMN
038100010605     C                   ELSE
038200010605     C                   SETON                                        32
038300010605     C                   Z-ADD     *zeros        VABRMN
038400010605     C                   EVAL      vinmsg = %trimr(vinmsg)
038500010720     C                             + ' ' + 'VABRMN'
038600010605     C                   ENDIF
038700010605     C*
038800010913     C                   IF        %subst(vindta:277:9) = *blanks
038900010913     C                   EVAL      PiStr=*all'0'
039000010913     C                   ELSE
039100010913     C                   EVAL      PiStr=%trim(%subst(vindta:277:9))
039200010913     C                   ENDIF
039300011115     C                   EVAL      PiDecChr=','
039400010605     C                   EXSR      CHKNUM
039500010605     C                   IF        PiNum=*on
039600010605     C                   Z-ADD     PiVal         VABCAS
039700010605     C                   ELSE
039800010605     C                   SETON                                        32
039900010605     C                   Z-ADD     *zeros        VABCAS
040000010605     C                   EVAL      vinmsg = %trimr(vinmsg)
040100010605     C                             + ' ' + 'VABCAS'
040200010605     C                   ENDIF
040300010913     C*
040400010913     C                   IF        %subst(vindta:286:9) = *blanks
040500010913     C                   EVAL      PiStr=*all'0'
040600010913     C                   ELSE
040700010913     C                   EVAL      PiStr=%trim(%subst(vindta:286:9))
040800010913     C                   ENDIF
040900011115     C                   EVAL      PiDecChr=','
041000010913     C                   EXSR      CHKNUM
041100010913     C                   IF        PiNum=*on
041200010913     C                   Z-ADD     PiVal         VABIAS
041300010913     C                   ELSE
041400010913     C                   SETON                                        32
041500010913     C                   Z-ADD     *zeros        VABIAS
041600010913     C                   EVAL      vinmsg = %trimr(vinmsg)
041700010913     C                             + ' ' + 'VABIAS'
041800010913     C                   ENDIF
041900010330     C*
042000010605     C* Considerazioni finali sui valori sopra ricavati
042100010913     C                   IF        (VABCAS = *zeros)
042200010720     C                   EVAL      VABCBO = '1 '
042300010913     C                   ELSE
042400010913     C                   EVAL      VABCBO = '4 '
042500011115     C                   IF        %trim(%subst(vindta:341:4)) = 'EURO'
042600011115     C                   EVAL      VABVCA = 'EUR'
042700010913     C                   ELSE
042800011115     C                   EVAL      VABVCA = 'ITL'
042900010913     C                   ENDIF
043000010913     C                   ENDIF
043100010913     C*
043200010913     C                   IF        (VABIAS > *zeros)
043300011115     C                   IF        %trim(%subst(vindta:341:4)) = 'EURO'
043400011115     C                   EVAL      VABVAS = 'EUR'
043500010913     C                   ELSE
043600011115     C                   EVAL      VABVAS = 'ITL'
043700010913     C                   ENDIF
043800010913     C                   ENDIF
043900011115     C*
044000011115     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044100011115     C                   EXSR      CHKIMPDIV
044200010330     C*
044300010330     C                   ENDSR
044400990920
044500010601
044600010601
044700010330
044800010330
044900010330     C*----------------------------------------------------*
045000010330     C*  CONTROLLO NUMERICITA' CAMPI
045100010330     C*----------------------------------------------------*
045200010330     C     CHKNUM        BEGSR
045300010330     C*
045400010606     C                   IF        PiDecChr = *blanks
045500010913     C                   EVAL      PiDecChr = '.'
045600010606     C                   ENDIF
045700010606     C*
045800010606     C                   CALL(e)   'ISNUMERIC'
045900010330     C                   PARM                    PiStr            30
046000010606     C                   PARM                    PiDecChr          1
046100010330     C                   PARM      *ZEROS        PiVal            30 9
046200010330     C                   PARM      '0'           PiInt             1
046300010330     C                   PARM      '0'           PiNum             1
046400010330     C                   IF        %error
046500010606     C                   EVAL      PiNum=*off
046600010330     C                   ENDIF
046700010330     C*
046800010330     C                   ENDSR
046900010330     C***
047000010330
047100011115
047200011115
047300011115     C*----------------------------------------------------*
047400011115     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047500011115     C*----------------------------------------------------*
047600011115     C     CHKIMPDIV     BEGSR
047700011115     C*
047800011115     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047900011115     C                   Z-ADD     *zeros        wrkDec            9 9
048000011115     C*
048100011115     C* Come prima cosa effettuo considerazioni sulla divisa
048200011115     C                   IF        vabIAS > *zeros
048300011115     C                   IF        vabVAS <> 'EUR'
048400011115     C                   EVAL      vabVAS =  'ITL'
048500011115     C                   ENDIF
048600011115     C                   ENDIF
048700011115     C*
048800011115     C                   IF        vabCAS > *zeros
048900011115     C                   IF        vabVCA <> 'EUR'
049000011115     C                   EVAL      vabVCA =  'ITL'
049100011115     C                   ENDIF
049200011115     C                   ENDIF
049300011115     C*
049400011115     C                   IF        vabVMD > *zeros
049500011115     C                   IF        vabVAD <> 'EUR'
049600011115     C                   EVAL      vabVAD =  'ITL'
049700011115     C                   ENDIF
049800011115     C                   ENDIF
049900011115     C*
050000011115     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050100011115     C                   Z-ADD     vabIAS        wrkDec
050200011115     C                   IF        wrkDec > *zeros
050300011115     C                   IF        vabVAS = 'ITL'
050400011115     C                   EVAL      vabIAS = *zeros
050500011115     C                   ENDIF
050600011115     C                   ENDIF
050700011115     C*
050800011115     C* Stabilisco se il contrasegno ha decimali valorizzati
050900011115     C                   Z-ADD     vabCAS        wrkDec
051000011115     C                   IF        wrkDec > *zeros
051100011115     C                   IF        vabVCA = 'ITL'
051200011115     C                   EVAL      vabCAS = *zeros
051300011115     C                   ENDIF
051400011115     C                   ENDIF
051500011115     C*
051600011115     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051700011115     C                   Z-ADD     vabVMD        wrkDec
051800011115     C                   IF        wrkDec > *zeros
051900011115     C                   IF        vabVAD = 'ITL'
052000011115     C                   EVAL      vabVMD = *zeros
052100011115     C                   ENDIF
052200011115     C                   ENDIF
052300011115     C*
052400011115     C                   ENDSR
052500011115     C***
052600011115
052700011115
052800010601
052900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053000010601     C     repfil        BEGSR
053100010601     C*
053200010601     C                   if        invfil = *zeros and
053300010601     C                             depfil > *zeros and
053400010628     C                             (vinflg = *blanks or
053500010628     C                              vinflg = *zeros)
053600010601     C
053700010601     C                   eval      invfil = depfil
053800010601     C                   endif
053900010601     C*
054000010601     C                   if        depfil <> invfil and
054100010601     C                             invfil > *zeros
054200010601     C                   eval      flgMulti = '1'
054300010601     C                   if        vinflg = *blanks
054400010601     C                   add       1             cntNonEl
054500010601     C                   endif
054600010601     C                   endif
054700010601     C*
054800010601     C                   if        vinflg = '2'
054900010601     C                   eval      flgStato = '2'
055000010601     C                   endif
055100010601     C*
055200010601     C                   ENDSR
055300010601     C***
055400010601
055500010601
055600010601
055700010330
055800010330
055900010330
056000990920      /TITLE Invio dei dati al punto operativo.
056100000613     C     invio         BEGSR
056200990920     C*
056300990920     C                   reset                   dscmz
056400010601     C                   move      invfil        cmzdst
056500990920     C                   eval      cmzfld = vlrfou
056600990920     C                   eval      cmzmbd = vlrhdl
056700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056800000710     C                   if        prmfir = *blanks
056900021113     C                   eval      cmzfla = 'FIVAB00F'
057000021113     C                   eval      cmzmba = 'FIVAB00F'
057100000710     C                   else
057200000710     C                   eval      cmzfla = prmfir
057300000710     C                   eval      cmzmba = prmfir
057400000710     C                   endif
057500990920     C                   eval      cmznrr = *zeros
057600990920     C                   move      §ctrok        cmznrr
057700021025     C                   eval      cmzlba = vlrfl1
057800990920     C                   call(e)   'TIS711C'
057900990920     C                   parm                    dscmz
058000990921     C                   parm      *blanks       esito
058100990920     C                   if        %error
058200990920     C                             or cmzerr = '1'
058300990921     C                             or esito  = '1'
058400000710     C                   eval      wrkesito = '3'
058500990920     C                   endif
058600990920     C*
058700000613     C                   ENDSR
058800990910
058900010601
059000010601
059100010601
059200010601
059300010601      /TITLE Invio dei dati al punto operativo.
059400010601     C     opeini        BEGSR
059500010601     C*
059600010601     C* Inizializzo flag e contatori operativi
059700010601     C                   movel     '0'           flgGiro           1
059800010601     C                   movel     '0'           flgMulti          1
059900010601     C                   movel     '1'           flgStato          1
060000010615     C                   movel     '0'           flgOk             1
060100010601     C                   z-add     *zeros        cntNonEl         10 0
060200010601     C                   z-add     *zeros        depfil            3 0
060300010601     C                   z-add     *zeros        invfil            3 0
060400010601     C*
060500010601     C                   ENDSR
060600010601     C***
060700010601
060800010601
060900010601
061000010330
061100010330
061200000613     C     *inzsr        BEGSR
061300990910     C*
061400990910     C     *entry        plist
061500990920     C                   parm                    tivlrds
061600990921     C                   parm      wrkesito      esito
061700000724     C                   parm                    prmlit
061800000710     C                   parm                    prmfir
061900010330     C*
062000010330     C* CALCOLA LA DATA CORRENTE
062100010330     C                   time                    wn14             14 0
062200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062300010330     C                   z-add     wn8           g08dat
062400010330     C                   z-add     *zeros        g08inv
062500010330     C                   movel     '0'           g08err
062600010330     C                   call      'XSRDA8'
062700010330     C                   parm                    wlbda8
062800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062900000613     C*
063000000613     C                   ENDSR
063100000613     C***
063200990908
063300010913     Otis7imp   E            riepilogo         2
063400990915     O                                              'Upload via Internet'
063500990915     O                                           +1 'Traduzione TIVIN00R -'
063600021113     O                                           +1 'FIVABWWR'
063700990915     O                                           +1 'Testate bolle'
063800990915     O          E            riepilogo   2
063900990915     O                       wrkdata
064000990915     O                       wrkora              +1
064100990915     O                       procname            +1
064200990915     O          E            riepilogo   2
064300990915     O                                              'Cliente..................:'
064400010605     O                       VABCCM        z     +1
064500990915     O          E            riepilogo   2
064600990920     O                                              'Riferimento Strategi.....:'
064700990920     O                       vlrhdl              +1
064800990915     O          E            riepilogo   2
064900990915     O                                              'Giusti...................:'
065000971022     O                       §CTROK        2   +  1
065100990915     O          E            riepilogo   2
065200990915     O                                              'Sbagliati e corretti.....:'
065300971022     O                       §CTRMO        2   +  1
065400990915     O          E            riepilogo   2
065500990915     O                                              'Sbagliati e scartati.....:'
065600971022     O                       §CTRNO        2   +  1
065700000613
065800010913     Otis7imps  E            testdett          2
065900000613     O                                              'Upload via Internet'
066000000613     O                                           +1 'Traduzione TIVIN00R -'
066100021113     O                                           +1 'FIVABWWR'
066200000613     O                                           +1 'Testate bolle'
066300000616     O          E            testdett    3
066400000613     O                                           +2 'N° rec'
066500000613     O                                           +3 'Anteprima contenuto'
066600000616     O          E            rigadett    2
066700000613     O                       rrnum               +2
066800000621     O                       recko               +3
066900000616     O          E            findett     2
067000000613     O                       wrkdata
067100000613     O                       wrkora              +1
067200000613     O                       procname            +1
067300000616     O          E            findett     2
067400000613     O                                              'Cliente..................:'
067500010605     O                       VABCCM        z     +1
067600000616     O          E            findett     2
067700000613     O                                              'Riferimento Strategi.....:'
067800000613     O                       vlrhdl              +1
067900000616     O          E            findett     2
068000000613     O                                              'Giusti...................:'
068100000613     O                       §CTROK        2   +  1
068200000616     O          E            findett     2
068300000613     O                                              'Sbagliati e corretti.....:'
068400000613     O                       §CTRMO        2   +  1
068500000616     O          E            findett     2
068600000613     O                                              'Sbagliati e scartati.....:'
068700000613     O                       §CTRNO        2   +  1
068800000613** CMD - COMANDI CL
068900011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7IMP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
069000011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7IMPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
069100010913DLTOVR FILE(TIS7IMP TIS7IMPS) LVL(*)
069200010606
069300000613
