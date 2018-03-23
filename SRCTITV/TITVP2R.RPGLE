000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000041210     D parccm          s              8    INZ(*blanks)
002100010202     D parmbr          s             10    INZ(*blanks)
002200010202     D paropz          s              1    INZ(*blanks)
002300010202     D chkcall         s              1    INZ(*blanks)
002400000830
002500000830     D*------------------
002600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
002700000830     D*------------------
002800000830     D WLBDA8          DS                  INZ
002900000830     D  G08DAT                 1      8  0
003000000830     D  G08INV                 9     16  0
003100000830     D  G08ERR                17     17
003200000830     D  G08TGI                18     22  0
003300041025     D*------------------
003400990908
003500010201
003600010201
003700000913     C                   reset                   rrnum
003800990921     C                   reset                   esito
003900990921     C                   reset                   wrkesito
004000000613     C*
004100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004200000613     C*
004300010202     C* Effettuo la chiamata al CLLE preposto
004400040506     C                   call(e)   'TITVVTC'
004500010202     C                   parm                    parccm
004600010202     C                   parm                    parmbr
004700010202     C                   parm      '2'           paropz
004800000616     C*
004900010201     C                   seton                                        LR
005000041210
005100041210
005200041210
005300910830     C*--------------------------------------------------------
005400040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
005500910830     C*--------------------------------------------------------
005600040526     C     RWFILE        BEGSR
005700990910     C*
005800990914     C                   if        not %open(tivin00r)
005900990908     C                   open      tivin00r
006000990914     C                   endif
006100021113     C                   if        not %open(fivabwwr)
006200021113     C                   open      fivabwwr
006300990914     C                   endif
006400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
006500020305     C                   exsr      prevat
006600010201     C*
006700010202     C                   if        chkcall = '0'
006800010202     C*
006900021113     C                   if        not %open(fivatwwr)
007000021113     C                   open      fivatwwr
007100010201     C                   endif
007200990910     C*
007300010201     C                   clear                   §CTROKVB          5 0
007400020305     C                   clear                   §CTROKVT          5 0
007500000801     C                   clear                   §CTRMO            5 0
007600000801     C                   clear                   §CTRNO            5 0
007700040910     C*
007800921023     C                   DO        *HIVAL
007900990913     C*
008000990915     C                   READ      tivin00r                               70
008100040910     C                   if        vindta > *blanks
008200000613     C                   add       1             rrnum
008300000801     C*
008400000801     C                   if        *in70 = *off
008500000801     C                             and
008600000801     C                             (vinflg = *blanks
008700000801     C                              or vinflg = '0'
008800000801     C                              or vinflg = '2')
008900000801     C*
009000000801     C                   clear                   vinmsg
009100000801     C                   eval      vinflg = '1'
009200040910     C*
009300040910     C* Eseguo routine d traduzione
009400040910     C                   exsr      impvabvat
009500051122     C*
009600051122     C* Se no errori bloccanti =>
009700051122     C                   if        errore = *zeros
009800051122     C                   WRITE     FIVAB000
009900051122     C                   endif
010000040802     C*
010100010305     C                   endif
010200000905     C*
010300000905     C                   else
010400000905     C                   eval      vinflg = '1'
010500000905     C                   endif
010600000905     C*
010700000905     C  N70              update    tivin000
010800000905     C*
010900991022     C  N70              ENDdo
011000010202     C*
011100010202     C                   endif
011200990910
011300990910     C* Se non ci sono record con errori ...
011400000710     C                   if        §ctrno = 0
011500990910     C* ... restituisco esito OK.
011600990921     C                   eval      wrkesito = '0'
011700990910     C                   else
011800010201     C                   if        §ctrokvb > 0
011900990921     C                   eval      wrkesito = '1'
012000000710     C                   else
012100000710     C                   eval      wrkesito = '2'
012200990910     C                   endif
012300000710     C                   endif
012400990910     C*
012500990914     C                   if        %open(tivin00r)
012600990908     C                   close     tivin00r
012700990914     C                   endif
012800021113     C                   if        %open(fivabwwr)
012900021113     C                   close     fivabwwr
013000990914     C                   endif
013100021113     C                   if        %open(fivatwwr)
013200021113     C                   close     fivatwwr
013300010201     C                   endif
013400990910     C*
013500010201     C                   if        §ctrokvb > 0
013600000724     C                             and vlrpoi <> *zeros
013700010202     C                   exsr      invio
013800990920     C                   endif
013900990920     C*
014000910830     C                   ENDSR
014100000613     C***
014200990920
014300000801     C*----------------------------------------------------*
014400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
014500000801     C*----------------------------------------------------*
014600010201     C     INZVAR        BEGSR
014700000801     C*
014800040802     C                   Z-ADD     *zeros        Num5_0            5 0
014900040802     C                   MOVEL     '0'           FlgCAS            1
015000000801     C*
015100000801     C                   ENDSR
015200000801     C*----------------------------------------------------*
015300040910     C*  IMPOSTAZIONE CAMPI COSTANTI
015400000801     C*----------------------------------------------------*
015500000801     C     DEFCAM        BEGSR
015600000801     C*
015700021113     C                   CLEAR                   FIVAB000
015800040802     C                   CLEAR                   FIVAT000
015900020619     C* Imposto i valori di default...
016000051122     C                   Z-ADD     0893018       VABCCM
016100051122     C                   Z-ADD     0893018       VATCCM
016200051122     C                   Z-ADD     089           VABLNP
016300051122     C                   Z-ADD     089           VATLNP
016400051122     C                   Z-ADD     000           VABCTR
016500051122     C                   Z-ADD     1             VABNCL
016600051122     C                   MOVEL     '1'           VABCBO
016700051122     C                   MOVEL     'C'           VABTSP
016800051122     C                   MOVEL     '7Q'          VABCTM
016900051122     C                   EVAL      VABNAS = 'MAT. AUDIOVISIVO'
017000020619     C* ... e poi verifico se sono stati passati come parametri
017100020619     C                   IF        vlrppt > *blanks
017200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017400020619     C                   EXSR      CHKNUM
017500020619     C                   IF        PiInt=*on
017600020619     C                   Z-ADD     PiVal         VABCCM
017700020619     C                   Z-ADD     PiVal         VATCCM
017800020619     C                   ENDIF
017900040506     C                   ENDIF
018000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018200020619     C                   EXSR      CHKNUM
018300020619     C                   IF        PiInt=*on
018400020619     C                   Z-ADD     PiVal         VABLNP
018500020619     C                   Z-ADD     PiVal         VATLNP
018600040506     C                   ENDIF
018700020619     C                   ENDIF
018800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
018900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019000020619     C                   EXSR      CHKNUM
019100020619     C                   IF        PiInt=*on
019200020619     C                   Z-ADD     PiVal         VABCTR
019300040506     C                   ENDIF
019400020619     C                   ENDIF
019500041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
019600041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
019700041210     C                   ENDIF
019800020619     C                   ENDIF
019900000801     C*
020000000801     C                   ENDSR
020100000801     C*----------------------------------------------------*
020200040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
020300000801     C*----------------------------------------------------*
020400040910     C     IMPVABVAT     BEGSR
020500041210     C*
020600041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
020700041210     C                   Z-ADD     *zeros        errore            1 0
020800040910     C*
020900051122     C* ......inizializzazioni
021000040910     C                   EXSR      INZVAR
021100040910     C                   EXSR      DEFCAM
021200040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
021300040928     C                   MOVEL     datcor        VABAAS
021400040928     C                   MOVEL     datcor        VATAAS
021500040928     C                   MOVE      datcor        VABMGS
021600040928     C                   MOVE(P)   vlrpoi        VABFGS
021700040928     C                   MOVE(P)   vlrpoi        VATFGS
021800050805     C* ......VABRMA
021900051122     C                   EVAL      VABRMA=%trim(%subst(vindta:541:70))
022000051122     C* ......VABRSD
022100051122     C                   EVAL      VABRSD=%trim(%subst(vindta:218:35))
022200051122     C* ......VABRD2
022300051122     C                   EVAL      VABRD2=%trim(%subst(vindta:254:35))
022400050805     C* ......VABIND
022500051123     C                   EVAL      VABIND=%trim(%subst(vindta:290:50))
022600050805     C* ......VABLOD
022700051123     C                   EVAL      VABLOD=%trim(%subst(vindta:341:50))
022800051122     C* ......VABPRD
022900051123     C                   EVAL      VABPRD=%trim(%subst(vindta:392:2))
023000050805     C* ......VABCAD
023100051122     C                   EVAL      PiStr=%trim(%subst(vindta:395:5))
023200050805     C                   EXSR      CHKNUM
023300050805     C                   IF        PiInt=*on
023400050805     C                   Z-ADD     PiVal         Num5_0
023500050805     C                   MOVEL(P)  Num5_0        VABCAD
023600050805     C                   ELSE
023700050805     C                   ADD       1             errore
023800050805     C                   EVAL      vinmsg = %trimr(vinmsg)
023900050805     C                             + ' ' + 'VABCAD'
024000050805     C                   ENDIF
024100050805     C* ......VABCAS
024200051123     C                   IF        %subst(vindta:458:11) <> *blanks
024300051122     C                   MOVEL     '1'           FlgCAS
024400051123     C                   EVAL      PiStr=%trim(%subst(vindta:458:11))
024500050805     C                   EXSR      CHKNUM
024600050805     C                   IF        PiNum=*on
024700050805     C                   Z-ADD     PiVal         VABCAS
024800051123     C                   MOVEL     *blanks       VABTIC
024900051123     C                   MOVEL     'EUR'         VABVCA
025000050805     C                   ELSE
025100050805     C                   ADD       1             errore
025200050805     C                   EVAL      vinmsg = %trimr(vinmsg)
025300050805     C                             + ' ' + 'VABCAS'
025400051122     C                   ENDIF
025500050805     C                   ENDIF
025600051122     C* ......VABPKB
025700051122     C                   EVAL      PiStr=%trim(%subst(vindta:534:6))
025800050805     C                   EXSR      CHKNUM
025900050805     C                   IF        PiNum=*on
026000050805     C                   Z-ADD     PiVal         VABPKB
026100050805     C                   ELSE
026200050805     C                   ADD       1             errore
026300050805     C                   EVAL      vinmsg = %trimr(vinmsg)
026400050805     C                             + ' ' + 'VABPKB'
026500050805     C                   ENDIF
026600051123     C* ......VABRMN/VABNSP/VATNSP
026700051123     C                   EVAL      PiStr=%trim(%subst(vindta:541:70))
026800051123     C                   EXSR      CHKNUM
026900051123     C                   IF        PiInt=*on
027000051123     C                   Z-ADD     PiVal         VABRMN
027100051123     C                   Z-ADD     PiVal         VABNSP
027200051123     C                   Z-ADD     PiVal         VATNSP
027300051123     C                   ELSE
027400051123     C                   Z-ADD     1             VABRMN
027500051123     C                   Z-ADD     1             VABNSP
027600051123     C                   Z-ADD     1             VATNSP
027700051123     C                   ADD       1             errore
027800051123     C                   EVAL      vinmsg = %trimr(vinmsg)
027900051123     C                             + ' ' + 'VABRMN'
028000051123     C                   ENDIF
028100051122     C* ......VATNOT/VATTRC = B
028200051122     C                   EVAL      VATTRC='B'
028300051122     C                   EVAL      VATNOT=%trim(%subst(vindta:401:20))
028400051122     C                   WRITE     FIVAT000
028500051122     C* ......VATNOT/VATTRC = E
028600050805     C                   EVAL      VATTRC='E'
028700051122     C                   EVAL      VATNOT=%trim(%subst(vindta:541:70))
028800051122     C                   WRITE     FIVAT000
028900040910     C*
029000040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
029100040910     C                   IF        FlgCAS <> '0'
029200040929     C                   IF        VABCBO = '1'
029300040910     C                   EVAL      VABCBO = '4'
029400040910     C                   ELSE
029500040929     C                   EVAL      VABCBO = '6'
029600040910     C                   ENDIF
029700040929     C                   ENDIF
029800040910     C*
029900040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
030000040910     C                   EXSR      CHKIMPDIV
030100010202     C*
030200000801     C* Ebbene...
030300000801     C                   ADD       1             §CTRMO
030400010201     C                   IF        errore <> *zeros
030500000801     C                   ADD       1             §CTRNO
030600000801     C                   EVAL      vinflg = '2'
030700000801     C                   ELSE
030800010201     C                   ADD       1             §CTROKVB
030900000801     C                   ENDIF
031000000801     C*
031100000801     C                   ENDSR
031200010202     C*----------------------------------------------------*
031300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
031400010202     C*----------------------------------------------------*
031500020305     C     PREVAT        BEGSR
031600010202     C*
031700021113     C* Compongo il nome del membro da dare al FIVATWWR
031800010202     C                   eval      parmbr = vlrhdl
031900010202     C                   movel     'M'           parmbr
032000041210     C                   eval      parccm = vlrksc
032100010202     C                   eval      paropz = '1'
032200010202     C* Effettuo la chiamata al CLLE preposto
032300040506     C                   call(e)   'TITVVTC'
032400010202     C                   parm                    parccm
032500010202     C                   parm                    parmbr
032600010202     C                   parm                    paropz
032700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
032800010202     C                   if        %error
032900010202     C                   movel     '1'           chkcall
033000010202     C                   else
033100010202     C                   movel     '0'           chkcall
033200010202     C                   endif
033300010202     C*
033400010202     C                   ENDSR
033500000801     C*----------------------------------------------------*
033600000801     C*  CONTROLLO NUMERICITA' CAMPI
033700000801     C*----------------------------------------------------*
033800000801     C     CHKNUM        BEGSR
033900000801     C*
034000000801     C                   call(e)   'ISNUMERIC'
034100000801     C                   PARM                    PiStr            30
034200051122     C                   PARM      ','           PiDecChr          1
034300000801     C                   PARM      *ZEROS        PiVal            30 9
034400000801     C                   PARM      '0'           PiInt             1
034500000801     C                   PARM      '0'           PiNum             1
034600000801     C                   IF        %error
034700000801     C                   EVAL      PiInt=*off
034800000801     C                   ENDIF
034900000801     C*
035000000801     C                   ENDSR
035100000801     C***
035200000801
035300011113
035400011113     C*----------------------------------------------------*
035500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
035600011113     C*----------------------------------------------------*
035700011113     C     CHKIMPDIV     BEGSR
035800011113     C*
035900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
036000011113     C                   Z-ADD     *zeros        wrkDec            9 9
036100011113     C*
036200011113     C* Come prima cosa effettuo considerazioni sulla divisa
036300011113     C                   IF        vabIAS > *zeros
036400011113     C                   IF        vabVAS <> 'EUR'
036500011113     C                   EVAL      vabVAS =  'ITL'
036600011113     C                   ENDIF
036700011113     C                   ENDIF
036800011113     C*
036900011113     C                   IF        vabCAS > *zeros
037000011113     C                   IF        vabVCA <> 'EUR'
037100011113     C                   EVAL      vabVCA =  'ITL'
037200011113     C                   ENDIF
037300011113     C                   ENDIF
037400011113     C*
037500011113     C                   IF        vabVMD > *zeros
037600020305     C                   IF        vabVAD <> 'EUR'
037700011113     C                   EVAL      vabVAD =  'ITL'
037800011113     C                   ENDIF
037900011113     C                   ENDIF
038000011113     C*
038100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
038200011113     C                   Z-ADD     vabIAS        wrkDec
038300011113     C                   IF        wrkDec > *zeros
038400011113     C                   IF        vabVAS = 'ITL'
038500011113     C                   EVAL      vabIAS = *zeros
038600011113     C                   ENDIF
038700011113     C                   ENDIF
038800011113     C*
038900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
039000011113     C                   Z-ADD     vabCAS        wrkDec
039100011113     C                   IF        wrkDec > *zeros
039200011113     C                   IF        vabVCA = 'ITL'
039300011113     C                   EVAL      vabCAS = *zeros
039400011113     C                   ENDIF
039500011113     C                   ENDIF
039600011113     C*
039700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
039800011113     C                   Z-ADD     vabVMD        wrkDec
039900011113     C                   IF        wrkDec > *zeros
040000011113     C                   IF        vabVAD = 'ITL'
040100011113     C                   EVAL      vabVMD = *zeros
040200011113     C                   ENDIF
040300011113     C                   ENDIF
040400011113     C*
040500011113     C                   ENDSR
040600011113     C***
040700011113
040800011113
040900000801
041000000801
041100990920      /TITLE Invio dei dati al punto operativo.
041200010202     C     invio         BEGSR
041300990920     C*
041400021113     C* 1° invio FIVAT
041500010201     C                   reset                   dscmz
041600010201     C                   move      vlrpoi        cmzdst
041700021113     C                   eval      cmzfld = 'FIVATWWR'
041800010201     C                   eval      cmzmbd = vlrhdl
041900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
042000021009     C***                if        prmfir = *blanks
042100021113     C                   eval      cmzfla = 'FIVAT00F'
042200021113     C                   eval      cmzmba = 'FIVAT00F'
042300021009     C***                else
042400021009     C***                eval      cmzfla = prmfir
042500021009     C***                eval      cmzmba = prmfir
042600021009     C***                endif
042700010201     C                   eval      cmznrr = *zeros
042800020305     C                   move      §ctrokvt      cmznrr
042900021018     C                   eval      cmzlba = vlrfl1
043000010201     C                   call(e)   'TIS711C'
043100010201     C                   parm                    dscmz
043200010201     C                   parm      *blanks       esito
043300010205     C                   if        %error
043400010205     C                             or cmzerr = '1'
043500010205     C                             or esito  = '1'
043600010205     C                   eval      wrkesito = '3'
043700010205     C                   else
043800010201     C*
043900021113     C* 2° invio FIVAB
044000010201     C                   reset                   dscmz
044100010201     C                   move      vlrpoi        cmzdst
044200010201     C                   eval      cmzfld = vlrfou
044300010201     C                   eval      cmzmbd = vlrhdl
044400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
044500021009     C***                if        prmfir = *blanks
044600021113     C                   eval      cmzfla = 'FIVAB00F'
044700021113     C                   eval      cmzmba = 'FIVAB00F'
044800021009     C***                else
044900021009     C***                eval      cmzfla = prmfir
045000021009     C***                eval      cmzmba = prmfir
045100021009     C***                endif
045200010201     C                   eval      cmznrr = *zeros
045300010201     C                   move      §ctrokvb      cmznrr
045400021018     C                   eval      cmzlba = vlrfl1
045500010201     C                   call(e)   'TIS711C'
045600010201     C                   parm                    dscmz
045700010201     C                   parm      *blanks       esito
045800010201     C                   if        %error
045900010201     C                             or cmzerr = '1'
046000010201     C                             or esito  = '1'
046100010201     C                   eval      wrkesito = '3'
046200010201     C                   endif
046300010205     C                   endif
046400990920     C*
046500000613     C                   ENDSR
046600000613     C***
046700070411
046800070411     C     *pssr         BEGSR
046900070411     C*
047000070411     C                   if        %open(tivin00r)
047100070411     C                   close     tivin00r
047200070411     C                   endif
047300070411     C                   if        %open(fivabwwr)
047400070411     C                   close     fivabwwr
047500070411     C                   endif
047600070411     C                   if        %open(fivatwwr)
047700070411     C                   close     fivatwwr
047800070411     C                   endif
047900070411     C*
048000070411     C* Effettuo la chiamata al CLLE preposto
048100070411     C                   call(e)   'TITVVTC'
048200070411     C                   parm                    parccm
048300070411     C                   parm                    parmbr
048400070411     C                   parm      '2'           paropz
048500070411     C*
048600070411     C                   eval      wrkesito = '2'
048700070411     C*
048800070411     C                   seton                                        LR
048900070411     C*
049000070411     C                   ENDSR     '*CANCL'
049100070411     C***
049200070411
049300990910
049400000613     C     *inzsr        BEGSR
049500990910     C*
049600990910     C     *entry        plist
049700990920     C                   parm                    tivlrds
049800990921     C                   parm      wrkesito      esito
049900000724     C                   parm                    prmlit
050000000710     C                   parm                    prmfir
050100000613     C*
050200000830     C* CALCOLA LA DATA CORRENTE
050300000830     C                   time                    wn14             14 0
050400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
050500000830     C                   z-add     wn8           g08dat
050600000830     C                   z-add     *zeros        g08inv
050700000830     C                   movel     '0'           g08err
050800000830     C                   call      'XSRDA8'
050900000830     C                   parm                    wlbda8
051000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
051100000830     C*
051200000613     C                   ENDSR
051300000613     C***
