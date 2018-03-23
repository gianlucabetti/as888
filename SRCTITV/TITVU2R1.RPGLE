000100071022      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600071022     FEDIVABWR  O    E             DISK    usropn
000700060710     Ftitvu2p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900060710     Ftitvu2ps  O    f  198        PRINTER usropn
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
009300060710     C                   if        not %open(titvu2ps)
009400060710     C                   open      titvu2ps
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
011200060710     C                   if        %open(titvu2ps)
011300000616     C                   except    findett
011400060710     C                   close     titvu2ps
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
013300060710     C                   if        not %open(titvu2p)
013400060710     C                   open      titvu2p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900060710     C                   if        %open(titvu2p)
014000060710     C                   close     titvu2p
014100990914     C                   endif
014200000613     C*
014300000613     C                   ENDSR
014400000613     C***
014500000613
014600010601
014700010601
014800010330     C*--------------------------------------------------------
014900071022     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
015000010330     C*--------------------------------------------------------
015100010605     C     RWVAB         BEGSR
015200050408     C*
015300010330     C                   if        not %open(tivin00r)
015400010330     C                   open      tivin00r
015500010330     C                   endif
015600071022     C                   if        not %open(edivabwr)
015700071022     C                   open      edivabwr
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
020100071022     C*
020200071022     C* VALORIZZO CAMPI RELATIVI AL "CMR"
020300071022     C                   eval      VABCMR = 'INTERCOMP'
020400071022     C                   eval      VABDCM = DATCOR
020500071022     C                   eval      VABDTS = DATCOR
020600071022     C                   eval      VABHMS = ORACOR
020700071022     C                   eval      VABCNT = 1
020800010601     C*
020900010604     C* Ebbene...
021000010604     C*
021100010604     C  N31              ADD       1             §CTROK            7 0
021200010604     C   32              ADD       1             §CTRMO            7 0
021300010604     C   31              ADD       1             §CTRNO            7 0
021400020205     C*
021500071022     C* Scarico il buffer del EDIVAB
021600071022     C  N31              WRITE     EDIVAB00
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
026500071022     C                   if        %open(edivabwr)
026600071022     C                   close     edivabwr
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
030500071022     C                   CLEAR                   EDIVAB00
030600020204     C* Imposto i valori di default...
030700060710     C                   EVAL      VABCCM = 0430287
030800060629     C                   EVAL      VABLNP = 277
030900060710     C                   EVAL      VABCTR = 000
031000020204     C* ... e poi verifico se sono stati passati come parametri
031100020204     C                   IF        vlrppt > *blanks
031200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031400020204     C                   EXSR      CHKNUM
031500020204     C                   IF        PiInt=*on
031600020204     C                   Z-ADD     PiVal         VABCCM
031700020204     C                   ENDIF
031800040301     C                   ENDIF
031900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABLNP
032400020204     C                   ENDIF
032500040301     C                   ENDIF
032600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABCTR
033100040301     C                   ENDIF
033200020204     C                   ENDIF
033300020204     C                   ENDIF
033400020204     C*
033500010330     C                   ENDSR
033600010607     C*----------------------------------------------------*
033700071022     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
033800010607     C*----------------------------------------------------*
033900010607     C     IMPVAB        BEGSR
034000010607     C*
034100010607     C                   SETOFF                                       3132
034200010607     C*
034300010607     C* Reperimento campi ALFA
034400010607     C*
034500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034600060710     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
034700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034900020117     C* ==
035000060710     C                   EVAL      VABRD2=%trim(%subst(vindta:92:15))
035100060710     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
035200060710     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
035300060710     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
035400060710     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
035500060710     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
035600060710     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
035700060710     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
035800060710     C                   IF        %trim(%subst(vindta:390:1)) = 'A'
035900060630     C                   EVAL      VABCBO='2'
036000060629     C                   ELSE
036100060630     C                   EVAL      VABCBO='1'
036200060629     C                   ENDIF
036300060710     C                   IF        %trim(%subst(vindta:16:1)) = '1'
036400060710     C                   EVAL      VABFFD='S'
036500060710     C                   ENDIF
036600060710     C                   IF        %trim(%subst(vindta:307:1)) = '1'
036700060710     C                   EVAL      FlgCAS='1'
036800060710     C                   ENDIF
036900060510     C*
037000060510     C* Reperimento campi NUMERICI
037100060510     C                   MOVEL     DATCOR        VABAAS
037200060510     C                   MOVE      DATCOR        VABMGS
037300060710     C* NSP/RMN
037400060710     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
037500060510     C                   EXSR      CHKNUM
037600060510     C                   IF        PiInt=*on
037700060510     C                   Z-ADD     PiVal         VABNSP
037800060510     C                   Z-ADD     PiVal         VABRMN
037900060404     C                   ELSE
038000060404     C                   SETON                                        32
038100060510     C                   Z-ADD     *zeros        VABNSP
038200060510     C                   Z-ADD     1             VABRMN
038300060404     C                   EVAL      vinmsg = %trimr(vinmsg)
038400060510     C                             + ' ' + 'VABNSP VABRMN'
038500060404     C                   ENDIF
038600040420     C* CAD
038700060710     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
038800010607     C                   EXSR      CHKNUM
038900010607     C                   IF        PiInt=*on
039000020204     C                   Z-ADD     PiVal         Num5_0
039100020204     C                   MOVEL(p)  Num5_0        VABCAD
039200010607     C                   ELSE
039300010607     C                   SETON                                        32
039400020204     C                   EVAL      VABCAD = *zeros
039500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039600020204     C                             + ' ' + 'VABCAD'
039700060316     C                   ENDIF
039800040420     C* NCL
039900060710     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
040000010607     C                   EXSR      CHKNUM
040100010607     C                   IF        PiInt=*on
040200010607     C                   Z-ADD     PiVal         VABNCL
040300010607     C                   ELSE
040400010607     C                   SETON                                        32
040500010607     C                   Z-ADD     *zeros        VABNCL
040600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040700010607     C                             + ' ' + 'VABNCL'
040800010607     C                   ENDIF
040900040420     C* PKB
041000060710     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
041100010607     C                   EXSR      CHKNUM
041200010607     C                   IF        PiNum=*on
041300060404     C                   Z-ADD(H)  PiVal         VABPKB
041400010607     C                   ELSE
041500010607     C                   SETON                                        32
041600010607     C                   Z-ADD     *zeros        VABPKB
041700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041800010607     C                             + ' ' + 'VABPKB'
041900010607     C                   ENDIF
042000060516     C* CAS
042100060710     C                   IF        %subst(vindta:280:12) <> *blanks
042200060710     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
042300060710     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
042400060516     C                   EXSR      CHKNUM
042500060516     C                   IF        PiNum=*on
042600060516     C                   Z-ADD     PiVal         VABCAS
042700060516     C                   ELSE
042800060516     C                   SETON                                        32
042900060629     C                   Z-ADD     *zeros        VABCAS
043000060516     C                   EVAL      vinmsg = %trimr(vinmsg)
043100060516     C                             + ' ' + 'VABCAS'
043200060516     C                   ENDIF
043300060710     C                   ENDIF
043400060629     C* IAS
043500060710     C                   IF        %subst(vindta:295:12) <> *blanks
043600060710     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
043700060710     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
043800060629     C                   EXSR      CHKNUM
043900060629     C                   IF        PiNum=*on
044000060629     C                   Z-ADD     PiVal         VABIAS
044100060629     C                   ELSE
044200060629     C                   SETON                                        32
044300060629     C                   Z-ADD     *zeros        VABIAS
044400060629     C                   EVAL      vinmsg = %trimr(vinmsg)
044500060629     C                             + ' ' + 'VABIAS'
044600060629     C                   ENDIF
044700060710     C                   ENDIF
044800020322     C*
044900020322     C* Considerazioni finali su CBO/CAS
045000050908     C                   IF        FlgCAS = '1'
045100040713     C                   IF        VABCBO = '1'
045200020322     C                   EVAL      VABCBO = '4'
045300040713     C                   ENDIF
045400040713     C                   IF        VABCBO = '2'
045500040713     C                   EVAL      VABCBO = '6'
045600040713     C                   ENDIF
045700020322     C                   ENDIF
045800020204     C*
045900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046000020204     C                   EXSR      CHKIMPDIV
046100020204     C*
046200010607     C                   ENDSR
046300010607     C*----------------------------------------------------*
046400010601
046500020204
046600020204     C*----------------------------------------------------*
046700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046800020204     C*----------------------------------------------------*
046900020204     C     CHKIMPDIV     BEGSR
047000020204     C*
047100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047200020204     C                   Z-ADD     *zeros        wrkDec            9 9
047300020204     C*
047400020204     C* Come prima cosa effettuo considerazioni sulla divisa
047500020204     C                   IF        vabIAS > *zeros
047600020204     C                   IF        vabVAS <> 'EUR'
047700020204     C                   EVAL      vabVAS =  'ITL'
047800020204     C                   ENDIF
047900020204     C                   ENDIF
048000020204     C*
048100020204     C                   IF        vabCAS > *zeros
048200020204     C                   IF        vabVCA <> 'EUR'
048300020204     C                   EVAL      vabVCA =  'ITL'
048400020204     C                   ENDIF
048500020204     C                   ENDIF
048600020204     C*
048700020204     C                   IF        vabVMD > *zeros
048800020321     C                   IF        vabVAD <> 'EUR'
048900020204     C                   EVAL      vabVAD =  'ITL'
049000020204     C                   ENDIF
049100020204     C                   ENDIF
049200020204     C*
049300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049400020204     C                   Z-ADD     vabIAS        wrkDec
049500020204     C                   IF        wrkDec > *zeros
049600020204     C                   IF        vabVAS = 'ITL'
049700020204     C                   EVAL      vabIAS = *zeros
049800020204     C                   ENDIF
049900020204     C                   ENDIF
050000020204     C*
050100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
050200020204     C                   Z-ADD     vabCAS        wrkDec
050300020204     C                   IF        wrkDec > *zeros
050400020204     C                   IF        vabVCA = 'ITL'
050500020204     C                   EVAL      vabCAS = *zeros
050600020204     C                   ENDIF
050700020204     C                   ENDIF
050800020204     C*
050900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051000020204     C                   Z-ADD     vabVMD        wrkDec
051100020204     C                   IF        wrkDec > *zeros
051200020204     C                   IF        vabVAD = 'ITL'
051300020204     C                   EVAL      vabVMD = *zeros
051400020204     C                   ENDIF
051500020204     C                   ENDIF
051600020204     C*
051700020204     C                   ENDSR
051800020204     C***
051900020204
052000010330
052100010330
052200010330     C*----------------------------------------------------*
052300010330     C*  CONTROLLO NUMERICITA' CAMPI
052400010330     C*----------------------------------------------------*
052500010330     C     CHKNUM        BEGSR
052600010330     C*
052700010606     C                   IF        PiDecChr = *blanks
052800060710     C                   EVAL      PiDecChr = '.'
052900010606     C                   ENDIF
053000010606     C*
053100010606     C                   CALL(e)   'ISNUMERIC'
053200010330     C                   PARM                    PiStr            30
053300010606     C                   PARM                    PiDecChr          1
053400010330     C                   PARM      *ZEROS        PiVal            30 9
053500010330     C                   PARM      '0'           PiInt             1
053600010330     C                   PARM      '0'           PiNum             1
053700010330     C                   IF        %error
053800010606     C                   EVAL      PiNum=*off
053900010330     C                   ENDIF
054000010330     C*
054100010330     C                   ENDSR
054200010330     C***
054300010330
054400010601
054500010601
054600010601
054700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
054800010601     C     repfil        BEGSR
054900010601     C*
055000010601     C                   if        invfil = *zeros and
055100010601     C                             depfil > *zeros and
055200010629     C                             (vinflg = *blanks or
055300010629     C                              vinflg = *zeros)
055400010601     C
055500010601     C                   eval      invfil = depfil
055600010601     C                   endif
055700010601     C*
055800010601     C                   if        depfil <> invfil and
055900010601     C                             invfil > *zeros
056000010601     C                   eval      flgMulti = '1'
056100010601     C                   if        vinflg = *blanks
056200010601     C                   add       1             cntNonEl
056300010601     C                   endif
056400010601     C                   endif
056500010601     C*
056600010601     C                   if        vinflg = '2'
056700010601     C                   eval      flgStato = '2'
056800010601     C                   endif
056900010601     C*
057000010601     C                   ENDSR
057100010601     C***
057200010601
057300010601
057400010601
057500010330
057600010330
057700010330
057800990920      /TITLE Invio dei dati al punto operativo.
057900000613     C     invio         BEGSR
058000990920     C*
058100990920     C                   reset                   dscmz
058200010601     C                   move      invfil        cmzdst
058300990920     C                   eval      cmzfld = vlrfou
058400990920     C                   eval      cmzmbd = vlrhdl
058500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
058600000710     C                   if        prmfir = *blanks
058700071022     C                   eval      cmzfla = 'EDIVAB0F'
058800071022     C                   eval      cmzmba = 'EDIVAB0F'
058900000710     C                   else
059000000710     C                   eval      cmzfla = prmfir
059100000710     C                   eval      cmzmba = prmfir
059200000710     C                   endif
059300990920     C                   eval      cmznrr = *zeros
059400990920     C                   move      §ctrok        cmznrr
059500021018     C                   eval      cmzlba = vlrfl1
059600990920     C                   call(e)   'TIS711C'
059700990920     C                   parm                    dscmz
059800990921     C                   parm      *blanks       esito
059900990920     C                   if        %error
060000990920     C                             or cmzerr = '1'
060100990921     C                             or esito  = '1'
060200000710     C                   eval      wrkesito = '3'
060300990920     C                   endif
060400990920     C*
060500000613     C                   ENDSR
060600990910
060700010601
060800010601
060900010601
061000010601
061100010601      /TITLE Invio dei dati al punto operativo.
061200010601     C     opeini        BEGSR
061300010601     C*
061400010601     C* Inizializzo flag e contatori operativi
061500010601     C                   movel     '0'           flgGiro           1
061600010601     C                   movel     '0'           flgMulti          1
061700010601     C                   movel     '1'           flgStato          1
061800010615     C                   movel     '0'           flgOk             1
061900010601     C                   z-add     *zeros        cntNonEl         10 0
062000010601     C                   z-add     *zeros        depfil            3 0
062100010601     C                   z-add     *zeros        invfil            3 0
062200010601     C*
062300010601     C                   ENDSR
062400010601     C***
062500010601
062600010601
062700010601
062800010330
062900010330
063000000613     C     *inzsr        BEGSR
063100990910     C*
063200990910     C     *entry        plist
063300990920     C                   parm                    tivlrds
063400990921     C                   parm      wrkesito      esito
063500000724     C                   parm                    prmlit
063600000710     C                   parm                    prmfir
063700010330     C*
063800010330     C* CALCOLA LA DATA CORRENTE
063900010330     C                   time                    wn14             14 0
064000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064100071022     C                   movel     wn14          oracor            6 0          *ORA
064200010330     C                   z-add     wn8           g08dat
064300010330     C                   z-add     *zeros        g08inv
064400010330     C                   movel     '0'           g08err
064500010330     C                   call      'XSRDA8'
064600010330     C                   parm                    wlbda8
064700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064800000613     C*
064900000613     C                   ENDSR
065000000613     C***
065100990908
065200060710     Otitvu2p   E            riepilogo         2
065300990915     O                                              'Upload via Internet'
065400990915     O                                           +1 'Traduzione TIVIN00R -'
065500071022     O                                           +1 'EDIVABWR'
065600990915     O                                           +1 'Testate bolle'
065700990915     O          E            riepilogo   2
065800990915     O                       wrkdata
065900990915     O                       wrkora              +1
066000990915     O                       procname            +1
066100990915     O          E            riepilogo   2
066200990915     O                                              'Cliente..................:'
066300010605     O                       VABCCM        z     +1
066400990915     O          E            riepilogo   2
066500990920     O                                              'Riferimento Strategi.....:'
066600990920     O                       vlrhdl              +1
066700990915     O          E            riepilogo   2
066800990915     O                                              'Giusti...................:'
066900971022     O                       §CTROK        2   +  1
067000990915     O          E            riepilogo   2
067100990915     O                                              'Sbagliati e corretti.....:'
067200971022     O                       §CTRMO        2   +  1
067300990915     O          E            riepilogo   2
067400990915     O                                              'Sbagliati e scartati.....:'
067500971022     O                       §CTRNO        2   +  1
067600000613
067700060710     Otitvu2ps  E            testdett          2
067800000613     O                                              'Upload via Internet'
067900000613     O                                           +1 'Traduzione TIVIN00R -'
068000071022     O                                           +1 'EDIVABWR'
068100000613     O                                           +1 'Testate bolle'
068200000616     O          E            testdett    3
068300000613     O                                           +2 'N° rec'
068400000613     O                                           +3 'Anteprima contenuto'
068500000616     O          E            rigadett    2
068600000613     O                       rrnum               +2
068700000621     O                       recko               +3
068800000616     O          E            findett     2
068900000613     O                       wrkdata
069000000613     O                       wrkora              +1
069100000613     O                       procname            +1
069200000616     O          E            findett     2
069300000613     O                                              'Cliente..................:'
069400010605     O                       VABCCM        z     +1
069500000616     O          E            findett     2
069600000613     O                                              'Riferimento Strategi.....:'
069700000613     O                       vlrhdl              +1
069800000616     O          E            findett     2
069900000613     O                                              'Giusti...................:'
070000000613     O                       §CTROK        2   +  1
070100000616     O          E            findett     2
070200000613     O                                              'Sbagliati e corretti.....:'
070300000613     O                       §CTRMO        2   +  1
070400000616     O          E            findett     2
070500000613     O                                              'Sbagliati e scartati.....:'
070600000613     O                       §CTRNO        2   +  1
070700000613** CMD - COMANDI CL
070800060710OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVU2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070900060710OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVU2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
071000060710DLTOVR FILE(TITVU2P TITVU2PS) LVL(*)
071100010606
071200000613
