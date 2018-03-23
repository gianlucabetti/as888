000100040324      /TITLE Upload via Internet: traduzione in FIVABWWR. (MORELLATO S.p.A. - Cod. 0062332)
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020913     FFIVABwwr  O    E             DISK    usropn
000700040324     Ftitva8p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040324     Ftitva8ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990908
001200000512     D*------------
001300000512     D* COMANDI
001400000512     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000512     D*------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500990915     D wrkdata         s               d
002600990915     D wrkora          s               t
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800000621     D recko           s            150    INZ(*blanks)
002900011123     D depcmd          s            150    INZ(*blanks)
003000010606     D*------------------
003100010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200010606     D*------------------
003300010606     D WLBDA8          DS                  INZ
003400010606     D  G08DAT                 1      8  0
003500010606     D  G08INV                 9     16  0
003600010606     D  G08ERR                17     17
003700010606     D  G08TGI                18     22  0
003800010606     D*
003900990908
004000990915     C                   time                    wrkdata
004100990915     C                   time                    wrkora
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500010530     C*
004600010530     C                   exsr      opeini
004700010530     C                   exsr      rwvab
004800010530     C*
004900010530     C                   seton                                        lr
005000010530
005100010530
005200010530
005300010530
005400010530     C*--------------------------------------------------------
005500010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005600010530     C*--------------------------------------------------------
005700010530     C     PREELA        BEGSR
005800000724     C*
005900000724     C* SE OCCORRE SPEDIRE IN FILIALE
006000010530     C                   if        invfil <> *zeros and
006100010530     C                             flgGiro = '0'
006200010530     C*
006300010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006400010530     C                   eval      flgGiro = '1'
006500000724     C*
006600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006800010530     C     invfil        chain     azorg01l
006900000724     C                   if        %found
007000000616     C                   movel(p)  CMD(1)        depcmd
007100020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007200000616     C*
007300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007400011123     C                   Z-ADD     150           LENGH            15 5
007500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007600000616     C                   PARM                    depcmd
007700000616     C                   PARM                    LENGH
007800000724     C*
007900000724     C                   endif
008000000616     C*
008100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008200000616     C                   movel(p)  CMD(2)        depcmd
008300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008400000616     C*
008500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008600011123     C                   Z-ADD     150           LENGH            15 5
008700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008800000616     C                   PARM                    depcmd
008900000616     C                   PARM                    LENGH
009000000616     C*
009100040324     C                   if        not %open(titva8ps)
009200040324     C                   open      titva8ps
009300030407     C   50              except    testdett
009400000613     C                   endif
009500010530     C*
009600010530     C                   endif
009700010530     C*
009800010530     C                   ENDSR
009900010530     C***
010000010530
010100010530
010200010530
010300010530
010400010530     C*--------------------------------------------------------
010500010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010600010530     C*--------------------------------------------------------
010700010530     C     ENDELA        BEGSR
010800010530     C*
010900010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011000000613     C*
011100040324     C                   if        %open(titva8ps)
011200030407     C   50              except    findett
011300040324     C                   close     titva8ps
011400000613     C                   endif
011500000616     C*
011600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011700011123     C                   Z-ADD     150           LENGH            15 5
011800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
011900000616     C                   PARM                    CMD(3)
012000000616     C                   PARM                    LENGH
012100010530     C*
012200010530     C                   ENDSR
012300010530     C***
012400000613
012500010530
012600010530
012700000613
012800000613     C*--------------------------------------------------------
012900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013000000613     C*--------------------------------------------------------
013100000621     C     STPR          BEGSR
013200000613     C*
013300040324     C                   if        not %open(titva8p)
013400040324     C                   open      titva8p
013500990915     C                   endif
013600990915     C*
013700030407     C   50              except    riepilogo
013800990915     C*
013900040324     C                   if        %open(titva8p)
014000040324     C                   close     titva8p
014100990914     C                   endif
014200990910     C*
014300000613     C                   ENDSR
014400000613     C***
014500990908
014600010530
014700010530
014800010530
014900910830     C*--------------------------------------------------------
015000020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
015100910830     C*--------------------------------------------------------
015200941229     C     RWVAB         BEGSR
015300990910     C*
015400990914     C                   if        not %open(tivin00r)
015500990908     C                   open      tivin00r
015600990914     C                   endif
015700020913     C                   if        not %open(fivabwwr)
015800020913     C                   open      fivabwwr
015900990914     C                   endif
016000990910     C*
016100990910     C                   clear                   §CTROK
016200990910     C                   clear                   §CTRMO
016300990910     C                   clear                   §CTRNO
016400990910     C*
016500921023     C                   DO        *HIVAL
016600990913     C*
016700990915     C                   READ      tivin00r                               70
016800010618     C*
016900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017000010618     C                   if        vinflg = '1'
017100010618     C                   eval      flgOk = '1'
017200010618     C                   endif
017300010705     C*
017400010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
017500010705     C                   if        %subst(vindta:1:1) = '='
017600010705     C                   eval      %subst(vindta:1:1) = *blanks
017700010705     C                   endif
017800010705     C*
017900000905     C                   if        vindta > *blanks
018000000613     C                   add       1             rrnum
018100010601     C* ...elimino eventuali segni
018200010601     C     '+':' '       xlate     vindta        vindta
018300990913     C*
018400010601     C                   if        *in70 = *off and
018500990913     C                             (vinflg = *blanks
018600010711     C                              or vinflg = '0'
018700010711     C                              or vinflg = '2')
018800990913     C*
018900020913     C                   clear                   FIVAB000
019000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019100010711     C                   if        vinflg = *blanks or vinflg = '0'
019200010711     C                   clear                   vinmsg
019300010711     C                   endif
019400040316     C*
019500040316     C                   exsr      inzvar
019600040316     C                   exsr      defcam
019700040316     C                   exsr      impvab
019800010530     C*
019900010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020000010530     C                   eval      depfil = VABLNP
020100010530     C                   exsr      repfil
020200010601     C                   if        depfil = invfil
020300021014     C                   if        vlrpoi = 999
020400021010     C                   MOVE(P)   invfil        VABFGS
020500021014     C                   else
020600021014     C                   MOVE(P)   vlrpoi        VABFGS
020700021014     C                   endif
020800010530     C*
020900010530     C                   exsr      PREELA
021000930409     C*
021100010604     C  N31              ADD       1             §CTROK            7 0
021200010604     C   32              ADD       1             §CTRMO            7 0
021300010604     C   31              ADD       1             §CTRNO            7 0
021400020913     C  N31              WRITE     FIVAB000
021500990910     C*
021600010601     C                   if        *in31 = *off and
021700010601     C                             *in32 = *off
021800990910     C                   eval      vinflg = '1'
021900990910     C                   else
022000000621     C                   eval      recko = vindta
022100040324     C                   if        %open(titva8ps)
022200030407     C   50              except    rigadett
022300010612     C                   endif
022400990910     C                   eval      vinflg = '2'
022500990910     C                   endif
022600991022     C                   endif
022700010601     C*
022800010601     C                   endif
022900000905     C*
023000000905     C                   else
023100000905     C                   eval      vinflg = '1'
023200000905     C                   endif
023300000905     C*
023400000905     C  N70              update    tivin000
023500991022     C*
023600991022     C  N70              ENDdo
023700010530     C
023800010530     C*
023900010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024000010530     C                   if        cntNonEl = *zeros or
024100010530     C                             flgMulti = '0'
024200990910     C* Se non ci sono record con errori ...
024300010530     C                   if        §ctrno = 0 and
024400010601     C                             §ctrmo = 0 and
024500010530     C                             flgStato <> '2'
024600990910     C* ... restituisco esito OK.
024700990921     C                   eval      wrkesito = '0'
024800990910     C                   else
024900010601     C                   if        §ctrok > 0
025000990921     C                   eval      wrkesito = '1'
025100000710     C                   else
025200010615     C                   if        flgOk = '0'
025300010615     C                   eval      wrkesito = '2'
025400010615     C                   else
025500010615     C                   eval      wrkesito = '6'
025600010615     C                   endif
025700990910     C                   endif
025800000710     C                   endif
025900010530     C                   else
026000010530     C                   eval      wrkesito = '9'
026100010530     C                   endif
026200990910     C*
026300990914     C                   if        %open(tivin00r)
026400990908     C                   close     tivin00r
026500990914     C                   endif
026600020913     C                   if        %open(fivabwwr)
026700020913     C                   close     fivabwwr
026800990914     C                   endif
026900010530     C*
027000010530     C                   if        vlrpoi <> 999
027100010530     C                   eval      invfil = vlrpoi
027200010530     C                   endif
027300990910     C*
027400990920     C                   if        §ctrok > 0
027500010530     C                             and invfil > *zeros
027600000613     C                   exsr      invio
027700990920     C                   endif
027800010612     C*
027900010612     C                   if        flgGiro = '1'
028000010612     C                   exsr      endela
028100010612     C                   endif
028200990920     C*
028300910830     C                   ENDSR
028400000613     C***
028500040316
028600040316
028700040316
028800040316
028900040316
029000040316     C*----------------------------------------------------*
029100040316     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029200040316     C*----------------------------------------------------*
029300040316     C     INZVAR        BEGSR
029400040316     C*
029500040316     C                   Z-ADD     *zeros        Num5_0            5 0
029600040316     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029700040316     C                   MOVEL     '0'           FlgCAS            1
029800040316     C*
029900040316     C                   ENDSR
030000040316     C*----------------------------------------------------*
030100040316     C*  IMPOSTAZIONE CAMPI COSTANTI
030200040316     C*----------------------------------------------------*
030300040316     C     DEFCAM        BEGSR
030400040316     C*
030500040316     C* Inizializzo il buffer del record da scrivere
030600040316     C                   CLEAR                   FIVAB000
030700040316     C*
030800040316     C* Valorizzo i campi costanti
030900040324     C                   eval      VABCCM = 0062332
031000040324     C                   eval      VABLNP = 006
031100040316     C                   eval      VABCTR = 000
031200040923     C                   eval      VABCTM = '7Q'
031300040316     C* ... e poi verifico se sono stati passati come parametri
031400040316     C                   IF        vlrppt > *blanks
031500040324     C                   IF        %trim(%subst(vlrppt:2:7)) <> *blanks
031600040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:2:7))
031700040316     C                   EXSR      CHKNUM
031800040316     C                   IF        PiInt=*on
031900040316     C                   Z-ADD     PiVal         VABCCM
032000040316     C                   ENDIF
032100040316     C                   ENDIF
032200040324     C                   IF        %trim(%subst(vlrppt:9:3)) <> *blanks
032300040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:9:3))
032400040316     C                   EXSR      CHKNUM
032500040316     C                   IF        PiInt=*on
032600040316     C                   Z-ADD     PiVal         VABLNP
032700040316     C                   ENDIF
032800040316     C                   ENDIF
032900040324     C                   IF        %trim(%subst(vlrppt:12:3)) <> *blanks
033000040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:12:3))
033100040316     C                   EXSR      CHKNUM
033200040316     C                   IF        PiInt=*on
033300040316     C                   Z-ADD     PiVal         VABCTR
033400040316     C                   ENDIF
033500040316     C                   ENDIF
033600041022     C                   IF        %trim(%subst(vlrppt:15:2)) <> *blanks
033700041022     C                   EVAL      VABCTM=%trim(%subst(vlrppt:15:2))
033800041022     C                   ENDIF
033900040316     C                   ENDIF
034000040316     C*
034100040316     C                   ENDSR
034200040316     C*----------------------------------------------------*
034300040316     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034400040316     C*----------------------------------------------------*
034500040316     C     IMPVAB        BEGSR
034600040316     C*
034700040316     C                   SETOFF                                       3132
034800040316     C*
034900040316     C* Reperimento campi ALFA
035000040316     C*
035100040316     C* Considerazioni sulla ragione sociale del destinatario da indicare
035200040317     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
035300040316     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035400040316     C     '@':'A'       XLATE     VABRSD        VABRSD
035500040316     C* ==
035600040324     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
035700040324     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
035800040324     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
035900040324     C                   EVAL      VABRMA=%trim(%subst(vindta:108:7))
036000040324     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
036100040324     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35-1:60-35))
036200040316     C*
036300040316     C* Reperimento campi NUMERICI
036400040316     C                   MOVEL     DATCOR        VABAAS
036500040316     C                   MOVE      DATCOR        VABMGS
036600040316     C* NSP/RMN
036700040324     C                   EVAL      PiStr=%trim(%subst(vindta:108:7))
036800040316     C                   EXSR      CHKNUM
036900040316     C                   IF        PiInt=*on
037000040316     C                   Z-ADD     PiVal         VABNSP
037100040316     C                   Z-ADD     PiVal         VABRMN
037200040316     C                   ELSE
037300040316     C                   SETON                                        31
037400040316     C                   Z-ADD     *zeros        VABNSP
037500040316     C                   Z-ADD     *zeros        VABRMN
037600040316     C                   EVAL      vinmsg = %trimr(vinmsg)
037700040316     C                             + ' ' + 'VABNSP VABRMN'
037800040316     C                   ENDIF
037900040316     C* CAD
038000040324     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
038100040316     C                   EXSR      CHKNUM
038200040316     C                   IF        PiInt=*on
038300040316     C                   Z-ADD     PiVal         Num5_0
038400040316     C                   MOVEL(p)  Num5_0        VABCAD
038500040316     C                   ELSE
038600040316     C                   SETON                                        32
038700040316     C                   EVAL      vinmsg = %trimr(vinmsg)
038800040316     C                             + ' ' + 'VABCAD'
038900040316     C                   ENDIF
039000040316     C* NCL
039100040324     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
039200040316     C                   EXSR      CHKNUM
039300040316     C                   IF        PiInt=*on
039400040316     C                   Z-ADD     PiVal         VABNCL
039500040316     C                   ELSE
039600040316     C                   SETON                                        32
039700040316     C                   Z-ADD     *zeros        VABNCL
039800040316     C                   EVAL      vinmsg = %trimr(vinmsg)
039900040316     C                             + ' ' + 'VABNCL'
040000040316     C                   ENDIF
040100040316     C* PKB
040200040324     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
040300040316     C                   EXSR      CHKNUM
040400040316     C                   IF        PiNum=*on
040500040317     C                   Z-ADD     PiVal         VABPKB
040600040316     C                   ELSE
040700040316     C                   SETON                                        32
040800040316     C                   Z-ADD     *zeros        VABPKB
040900040316     C                   EVAL      vinmsg = %trimr(vinmsg)
041000040316     C                             + ' ' + 'VABPKB'
041100040316     C                   ENDIF
041200040317     C* CAS
041300040324     C                   IF        %subst(vindta:137:10) <> *all'0'
041400040317     C                   EVAL      FlgCAS = '1'
041500040324     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
041600040317     C                   EXSR      CHKNUM
041700040317     C                   IF        PiNum=*on
041800040317     C                   Z-ADD     PiVal         VABCAS
041900040324     C                   EVAL      VABVCA = %subst(vindta:208:3)
042000040317     C                   ELSE
042100040317     C                   SETON                                        32
042200040317     C                   Z-ADD     *zeros        VABCAS
042300040317     C                   EVAL      vinmsg = %trimr(vinmsg)
042400040317     C                             + ' ' + 'VABCAS'
042500040317     C                   ENDIF
042600040317     C                   ENDIF
042700040316     C*
042800040316     C* Considerazioni finali su CBO/CAS
042900040316     C                   IF        FlgCAS = '1'
043000040316     C                   EVAL      VABCBO = '4'
043100040316     C                   ELSE
043200040316     C                   EVAL      VABCBO = '1'
043300040316     C                   ENDIF
043400040316     C*
043500040316     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043600040316     C                   EXSR      CHKIMPDIV
043700040316     C*
043800040316     C                   ENDSR
043900040316     C*----------------------------------------------------*
044000040316
044100040316
044200040316     C*----------------------------------------------------*
044300040316     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044400040316     C*----------------------------------------------------*
044500040316     C     CHKIMPDIV     BEGSR
044600040316     C*
044700040316     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044800040316     C                   Z-ADD     *zeros        wrkDec            9 9
044900040316     C*
045000040316     C* Come prima cosa effettuo considerazioni sulla divisa
045100040316     C                   IF        vabIAS > *zeros
045200040316     C                   IF        vabVAS <> 'EUR'
045300040316     C                   EVAL      vabVAS =  'ITL'
045400040316     C                   ENDIF
045500040316     C                   ENDIF
045600040316     C*
045700040316     C                   IF        vabCAS > *zeros
045800040316     C                   IF        vabVCA <> 'EUR'
045900040316     C                   EVAL      vabVCA =  'ITL'
046000040316     C                   ENDIF
046100040316     C                   ENDIF
046200040316     C*
046300040316     C                   IF        vabVMD > *zeros
046400040316     C                   IF        vabVAD <> 'EUR'
046500040316     C                   EVAL      vabVAD =  'ITL'
046600040316     C                   ENDIF
046700040316     C                   ENDIF
046800040316     C*
046900040316     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047000040316     C                   Z-ADD     vabIAS        wrkDec
047100040316     C                   IF        wrkDec > *zeros
047200040316     C                   IF        vabVAS = 'ITL'
047300040316     C                   EVAL      vabIAS = *zeros
047400040316     C                   ENDIF
047500040316     C                   ENDIF
047600040316     C*
047700040316     C* Stabilisco se il contrasegno ha decimali valorizzati
047800040316     C                   Z-ADD     vabCAS        wrkDec
047900040316     C                   IF        wrkDec > *zeros
048000040316     C                   IF        vabVCA = 'ITL'
048100040316     C                   EVAL      vabCAS = *zeros
048200040316     C                   ENDIF
048300040316     C                   ENDIF
048400040316     C*
048500040316     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048600040316     C                   Z-ADD     vabVMD        wrkDec
048700040316     C                   IF        wrkDec > *zeros
048800040316     C                   IF        vabVAD = 'ITL'
048900040316     C                   EVAL      vabVMD = *zeros
049000040316     C                   ENDIF
049100040316     C                   ENDIF
049200040316     C*
049300040316     C                   ENDSR
049400040316     C***
049500040316
049600040316
049700040316
049800040316     C*----------------------------------------------------*
049900040316     C*  CONTROLLO NUMERICITA' CAMPI
050000040316     C*----------------------------------------------------*
050100040316     C     CHKNUM        BEGSR
050200040316     C*
050300040316     C                   IF        PiDecChr = *blanks
050400040316     C                   EVAL      PiDecChr = ','
050500040316     C                   ENDIF
050600040316     C*
050700040316     C                   CALL(e)   'ISNUMERIC'
050800040316     C                   PARM                    PiStr            30
050900040316     C                   PARM                    PiDecChr          1
051000040316     C                   PARM      *ZEROS        PiVal            30 9
051100040316     C                   PARM      '0'           PiInt             1
051200040316     C                   PARM      '0'           PiNum             1
051300040316     C                   IF        %error
051400040316     C                   EVAL      PiNum=*off
051500040316     C                   ENDIF
051600040316     C*
051700040316     C                   ENDSR
051800040316     C***
051900990920
052000010530
052100010530
052200010530
052300010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052400010530     C     repfil        BEGSR
052500010530     C*
052600010530     C                   if        invfil = *zeros and
052700010530     C                             depfil > *zeros and
052800010613     C                             (vinflg = *blanks or
052900010613     C                              vinflg = *zeros)
053000010530     C
053100010530     C                   eval      invfil = depfil
053200010530     C                   endif
053300010530     C*
053400010530     C                   if        depfil <> invfil and
053500010530     C                             invfil > *zeros
053600010530     C                   eval      flgMulti = '1'
053700010530     C                   if        vinflg = *blanks
053800010530     C                   add       1             cntNonEl
053900010530     C                   endif
054000010530     C                   endif
054100010530     C*
054200010530     C                   if        vinflg = '2'
054300010530     C                   eval      flgStato = '2'
054400010530     C                   endif
054500010530     C*
054600010530     C                   ENDSR
054700010530     C***
054800010530
054900010530
055000010530
055100010530
055200990920      /TITLE Invio dei dati al punto operativo.
055300000613     C     invio         BEGSR
055400990920     C*
055500990920     C                   reset                   dscmz
055600010530     C                   move      invfil        cmzdst
055700990920     C                   eval      cmzfld = vlrfou
055800990920     C                   eval      cmzmbd = vlrhdl
055900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056000000710     C                   if        prmfir = *blanks
056100020913     C                   eval      cmzfla = 'FIVAB00F'
056200020913     C                   eval      cmzmba = 'FIVAB00F'
056300000710     C                   else
056400000710     C                   eval      cmzfla = prmfir
056500000710     C                   eval      cmzmba = prmfir
056600000710     C                   endif
056700990920     C                   eval      cmznrr = *zeros
056800990920     C                   move      §ctrok        cmznrr
056900021003     C                   eval      cmzlba = vlrfl1
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
058000000613     C***
058100010530
058200010530
058300010530
058400010530
058500010530      /TITLE Invio dei dati al punto operativo.
058600010530     C     opeini        BEGSR
058700010530     C*
058800010530     C* Inizializzo flag e contatori operativi
058900010530     C                   movel     '0'           flgGiro           1
059000010530     C                   movel     '0'           flgMulti          1
059100010530     C                   movel     '1'           flgStato          1
059200010615     C                   movel     '0'           flgOk             1
059300010530     C                   z-add     *zeros        cntNonEl         10 0
059400010530     C                   z-add     *zeros        depfil            3 0
059500010530     C                   z-add     *zeros        invfil            3 0
059600030407     C*
059700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
059800030407     C                   if        %subst(vlrppt:1:1) = 'S'
059900030407     C                   seton                                        50
060000030407     C                   else
060100030407     C                   setoff                                       50
060200030407     C                   endif
060300010530     C*
060400010530     C                   ENDSR
060500010530     C***
060600010530
060700010530
060800010530
060900990910
061000000613     C     *inzsr        BEGSR
061100990910     C*
061200990910     C     *entry        plist
061300990920     C                   parm                    tivlrds
061400990921     C                   parm      wrkesito      esito
061500000724     C                   parm                    prmlit
061600000710     C                   parm                    prmfir
061700010606     C*
061800010606     C* CALCOLA LA DATA CORRENTE
061900010606     C                   time                    wn14             14 0
062000010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062100010606     C                   z-add     wn8           g08dat
062200010606     C                   z-add     *zeros        g08inv
062300010606     C                   movel     '0'           g08err
062400010606     C                   call      'XSRDA8'
062500010606     C                   parm                    wlbda8
062600010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062700000613     C*
062800000613     C                   ENDSR
062900000613     C***
063000990908
063100040324     Otitva8p   E            riepilogo         2
063200990915     O                                              'Upload via Internet'
063300990915     O                                           +1 'Traduzione TIVIN00R -'
063400020913     O                                           +1 'FIVABWWR'
063500990915     O                                           +1 'Testate bolle'
063600990915     O          E            riepilogo   2
063700990915     O                       wrkdata
063800990915     O                       wrkora              +1
063900990915     O                       procname            +1
064000990915     O          E            riepilogo   2
064100990915     O                                              'Cliente..................:'
064200990915     O                       VABCCM        z     +1
064300990915     O          E            riepilogo   2
064400990920     O                                              'Riferimento Strategi.....:'
064500990920     O                       vlrhdl              +1
064600990915     O          E            riepilogo   2
064700990915     O                                              'Giusti...................:'
064800971022     O                       §CTROK        2   +  1
064900990915     O          E            riepilogo   2
065000990915     O                                              'Sbagliati e corretti.....:'
065100971022     O                       §CTRMO        2   +  1
065200990915     O          E            riepilogo   2
065300990915     O                                              'Sbagliati e scartati.....:'
065400971022     O                       §CTRNO        2   +  1
065500000613
065600040324     Otitva8ps  E            testdett          2
065700000613     O                                              'Upload via Internet'
065800000613     O                                           +1 'Traduzione TIVIN00R -'
065900020913     O                                           +1 'FIVABWWR'
066000000613     O                                           +1 'Testate bolle'
066100000616     O          E            testdett    3
066200000613     O                                           +2 'N° rec'
066300000613     O                                           +3 'Anteprima contenuto'
066400000616     O          E            rigadett    2
066500000613     O                       rrnum               +2
066600000621     O                       recko               +3
066700000616     O          E            findett     2
066800000613     O                       wrkdata
066900000613     O                       wrkora              +1
067000000613     O                       procname            +1
067100000616     O          E            findett     2
067200000613     O                                              'Cliente..................:'
067300000613     O                       VABCCM        z     +1
067400000616     O          E            findett     2
067500000613     O                                              'Riferimento Strategi.....:'
067600000613     O                       vlrhdl              +1
067700000616     O          E            findett     2
067800000613     O                                              'Giusti...................:'
067900000613     O                       §CTROK        2   +  1
068000000616     O          E            findett     2
068100000613     O                                              'Sbagliati e corretti.....:'
068200000613     O                       §CTRMO        2   +  1
068300000616     O          E            findett     2
068400000613     O                                              'Sbagliati e scartati.....:'
068500000613     O                       §CTRNO        2   +  1
068600000512** CMD - COMANDI CL
068700040324OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068800040324OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068900040324DLTOVR FILE(TITVA8P TITVA8PS) LVL(*)
069000000512
069100000512
