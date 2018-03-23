000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700120319     FTIVGD00F  O    E             DISK
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600070719     D tisi95ds      e ds
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600120319     D wNomeFile       s             30    INZ(*blanks)
002700000830
002800041025     D*------------------
002900041025     D* DS REPERIMENTO NUMERATORE
003000041025     D*------------------
003100041025     D trul33ds      e ds                  inz
003200041025     D*------------------
003300041025     D* DS ARCHITETTURA
003400041025     D*------------------
003500041025     D kpjba         e ds                  inz
003600041025     D*------------------
003700990908
003800010201
003900010201
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300000613     C*
004400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004500070719     C*
004600070719     C* Esegue lancio TISI95R solo x chiusura
004700070719     C                   CLEAR                   TISI95DS
004800070719     C                   EVAL      I95TLA = 'C'
004900070719     C                   CALL      'TISI95R'
005000070719     C                   PARM                    TISI95DS
005100000613     C*
005200010202     C* Effettuo la chiamata al CLLE preposto
005300040506     C                   call(e)   'TITVVTC'
005400010202     C                   parm                    parccm
005500010202     C                   parm                    parmbr
005600010202     C                   parm      '2'           paropz
005700000616     C*
005800010201     C                   seton                                        LR
005900990908
006000000801
006100910830     C*--------------------------------------------------------
006200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006300910830     C*--------------------------------------------------------
006400040526     C     RWFILE        BEGSR
006500990910     C*
006600990914     C                   if        not %open(tivin00r)
006700990908     C                   open      tivin00r
006800990914     C                   endif
006900021113     C                   if        not %open(fivabwwr)
007000021113     C                   open      fivabwwr
007100990914     C                   endif
007200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007300020305     C                   exsr      prevat
007400010201     C*
007500010202     C                   if        chkcall = '0'
007600010202     C*
007700021113     C                   if        not %open(fivatwwr)
007800021113     C                   open      fivatwwr
007900010201     C                   endif
008000990910     C*
008100010201     C                   clear                   §CTROKVB          5 0
008200020305     C                   clear                   §CTROKVT          5 0
008300000801     C                   clear                   §CTRMO            5 0
008400000801     C                   clear                   §CTRNO            5 0
008500040910     C*
008600921023     C                   DO        *HIVAL
008700990913     C*
008800990915     C                   READ      tivin00r                               70
008900040910     C                   if        vindta > *blanks
009000000613     C                   add       1             rrnum
009100000801     C*
009200000801     C                   if        *in70 = *off
009300000801     C                             and
009400000801     C                             (vinflg = *blanks
009500000801     C                              or vinflg = '0'
009600000801     C                              or vinflg = '2')
009700000801     C*
009800000801     C                   clear                   vinmsg
009900000801     C                   eval      vinflg = '1'
010000040910     C*
010100040910     C* Eseguo routine d traduzione
010200040910     C                   exsr      impvabvat
010300040802     C*
010400010305     C                   endif
010500000905     C*
010600000905     C                   else
010700000905     C                   eval      vinflg = '1'
010800000905     C                   endif
010900000905     C*
011000000905     C  N70              update    tivin000
011100000905     C*
011200991022     C  N70              ENDdo
011300010202     C*
011400010202     C                   endif
011500990910
011600990910     C* Se non ci sono record con errori ...
011700000710     C                   if        §ctrno = 0
011800990910     C* ... restituisco esito OK.
011900990921     C                   eval      wrkesito = '0'
012000990910     C                   else
012100010201     C                   if        §ctrokvb > 0
012200990921     C                   eval      wrkesito = '1'
012300000710     C                   else
012400000710     C                   eval      wrkesito = '2'
012500990910     C                   endif
012600000710     C                   endif
012700990910     C*
012800990914     C                   if        %open(tivin00r)
012900990908     C                   close     tivin00r
013000990914     C                   endif
013100021113     C                   if        %open(fivabwwr)
013200021113     C                   close     fivabwwr
013300990914     C                   endif
013400021113     C                   if        %open(fivatwwr)
013500021113     C                   close     fivatwwr
013600010201     C                   endif
013700990910     C*
013800010201     C                   if        §ctrokvb > 0
013900000724     C                             and vlrpoi <> *zeros
014000010202     C                   exsr      invio
014100990920     C                   endif
014200990920     C*
014300910830     C                   ENDSR
014400000613     C***
014500990920
014600000801     C*----------------------------------------------------*
014700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
014800000801     C*----------------------------------------------------*
014900010201     C     INZVAR        BEGSR
015000000801     C*
015100040802     C                   Z-ADD     *zeros        Num5_0            5 0
015200040802     C                   MOVEL     '0'           FlgCAS            1
015300000801     C*
015400000801     C                   ENDSR
015500000801     C*----------------------------------------------------*
015600040910     C*  IMPOSTAZIONE CAMPI COSTANTI
015700000801     C*----------------------------------------------------*
015800000801     C     DEFCAM        BEGSR
015900000801     C*
016000021113     C                   CLEAR                   FIVAB000
016100040802     C                   CLEAR                   FIVAT000
016200020619     C* Imposto i valori di default...
016300100714     C                   Z-ADD     0895660       VABCCM
016400100714     C                   Z-ADD     0895660       VATCCM
016500100714     C                   Z-ADD     089           VABLNP
016600100714     C                   Z-ADD     089           VATLNP
016700070531     C                   Z-ADD     000           VABCTR
016800070531     C                   MOVEL     '7Q'          VABCTM
016900040823     C                   MOVEL     '1'           VABCBO
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
019500060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
019600060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
019700060202     C                   ENDIF
019800020619     C                   ENDIF
019900000801     C*
020000000801     C                   ENDSR
020100000801     C*----------------------------------------------------*
020200040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
020300000801     C*----------------------------------------------------*
020400040910     C     IMPVABVAT     BEGSR
020500040910     C*
020600040910     C* Traduzione relativa ai tipi record del file del cliente
020700040910     C*
020800071210     C*
020900071210     C***
021000071210     C* ...tipo record 'EBE' (testata)
021100071210     C                   IF        %subst(vindta:1:3) = 'EBE'
021200071210     C* Resetto indicatore d anomalia sul singolo record
021300071210     C                   eval      vinflg = '1'
021400071210     C* ......inizializzazioni iniziali e formati record file Bartolini
021500071210     C                   EXSR      INZVAR
021600071210     C                   EXSR      DEFCAM
021700071210     C*
021800071210     C                   Z-ADD     *zeros        errore            1 0
021900071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
022000071210     C                   MOVEL     datcor        VABAAS
022100071210     C                   MOVEL     datcor        VATAAS
022200071210     C                   MOVE      datcor        VABMGS
022300071210     C                   MOVE(P)   vlrpoi        VABFGS
022400071210     C                   MOVE(P)   vlrpoi        VATFGS
022500071210     C* ......VABNSP/VATNSP
022600071210     C* NSP => Stacco un numeratore da AZNUM
022700071210     C                   clear                   TRUL33DS
022800071210     C                   eval      I33OPE = *zeros
022900071210     C                   eval      I33CNU = 302
023000071210     C                   eval      I33NUM = 1
023100071210     C                   movel     TRUL33DS      KPJBU
023200071210     C                   call      'TRUL33R'
023300071210     C                   parm                    KPJBA
023400071210     C                   movel     KPJBU         TRUL33DS
023500071210     C                   if        O33ERR = *zeros
023600071210     C                   z-add     O33NRF        VABNSP
023700071210     C                   z-add     O33NRF        VATNSP
023800071210     C                   else
023900071210     C                   Z-ADD     1             errore
024000071210     C                   EVAL      vinmsg = %trimr(vinmsg)
024100071210     C                             + ' ' + 'VABNSP VATNSP'
024200071210     C                   endif
024300080211     C* ......VABNT2
024400080211     C                   EVAL      VABNT2='CUST ACCOUNT #: ' +
024500080211     C                                    %trim(%subst(vindta:9:13))
024600060202     C* ......VABRMN
024700071210     C                   EVAL      PiStr=%trim(%subst(vindta:22:15))
024800060202     C                   EXSR      CHKNUM
024900060202     C                   IF        PiInt=*on
025000060202     C                   Z-ADD     PiVal         VABRMN
025100060202     C                   ELSE
025200060202     C                   ADD       1             errore
025300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
025400060202     C                             + ' ' + 'VABRMN'
025500060202     C                   ENDIF
025600100714     C* ......VABNCL
025700100714     C                   EVAL      PiStr=%trim(%subst(vindta:275:5))
025800100714     C                   EXSR      CHKNUM
025900100714     C                   IF        PiInt=*on
026000100714     C                   Z-ADD     PiVal         VABNCL
026100100714     C                   ELSE
026200100714     C                   ADD       1             errore
026300100714     C                   EVAL      vinmsg = %trimr(vinmsg)
026400100714     C                             + ' ' + 'VABNCL'
026500100714     C                   ENDIF
026600060202     C* ......VABPKB
026700100714     C                   EVAL      PiStr=%trim(%subst(vindta:280:9))
026800060202     C                   EXSR      CHKNUM
026900060202     C                   IF        PiNum=*on
027000070726     C                   EVAL      PiVal = PiVal / 1000                         da grammi a kg
027100070726     C                   Z-ADD(H)  PiVal         VABPKB
027200060202     C                   ELSE
027300060202     C                   ADD       1             errore
027400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
027500060202     C                             + ' ' + 'VABPKB'
027600060202     C                   ENDIF
027700071210     C* ......VABVLB
027800100714     C*                  EVAL      PiStr=%trim(%subst(vindta:289:9))
027900071210     C*                  EXSR      CHKNUM
028000071210     C*                  IF        PiNum=*on
028100071210     C*                  EVAL      PiVal = PiVal / 1000                         da dm3 a m3
028200071210     C*                  Z-ADD(H)  PiVal         VABVLB
028300071210     C*                  ELSE
028400071210     C*                  ADD       1             errore
028500071210     C*                  EVAL      vinmsg = %trimr(vinmsg)
028600071210     C*                            + ' ' + 'VABVLB'
028700071210     C*                  ENDIF
028800071210     C* ......VABRMA
028900071210     C                   EVAL      VABRMA=%trim(%subst(vindta:22:15))
029000060202     C* ......VABRSD
029100071210     C                   EVAL      VABRSD=%trim(%subst(vindta:53:35))
029200100714     C* ......VABIND
029300100714     C                   EVAL      VABIND=%trim(%subst(vindta:88:35))
029400060202     C* ......VABLOD
029500070719     C                   EVAL      VABLOD=%trim(%subst(vindta:237:35))
029600060202     C* ......VABCAD
029700070719     C                   EVAL      PiStr=%trim(%subst(vindta:228:9))
029800060202     C                   EXSR      CHKNUM
029900060202     C                   IF        PiInt=*on
030000060202     C                   Z-ADD     PiVal         Num5_0
030100060202     C                   MOVEL(P)  Num5_0        VABCAD
030200060202     C                   ELSE
030300060202     C                   ADD       1             errore
030400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
030500060202     C                             + ' ' + 'VABCAD'
030600060202     C                   ENDIF
030700060202     C* ......VABPRD
030800070719     C* Reperisco la provincia dal CAP e dalla località
030900070719     C                   IF        VABPRD  = *blanks AND
031000070719     C                             VABCAD <> *blanks
031100070719     C                   CLEAR                   TISI95DS
031200070719     C                   EVAL      I95TCN = '3'
031300070719     C                   Z-ADD     datcor        I95DAT
031400070719     C                   EVAL      I95CAP = VABCAD
031500070719     C                   EVAL      I95LOC = VABLOD
031600070719     C                   CALL      'TISI95R'
031700070719     C                   PARM                    TISI95DS
031800070719     C                   EVAL      VABPRD = O95PRV
031900070719     C                   ENDIF
032000071210     C*
032100060202     C*
032200060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
032300060202     C                   IF        FlgCAS <> '0'
032400060202     C                   IF        VABCBO = '1'
032500060202     C                   EVAL      VABCBO = '4'
032600060202     C                   ELSE
032700060202     C                   EVAL      VABCBO = '6'
032800060202     C                   ENDIF
032900060202     C                   ENDIF
033000060202     C*
033100060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
033200060202     C                   EXSR      CHKIMPDIV
033300060202     C* ...e in fine...
033400060202     C                   WRITE     FIVAB000
033500120319     C                   exsr      wrivgd
033600040910     C*
033700040910     C                   ENDIF
033800040910     C***
033900070719     C* ...tipo record 'EBC' (dettaglio barcode segnacooli)
034000070719     C                   IF        %subst(vindta:1:3) = 'EBC'
034100070719     C* Resetto indicatore d anomalia sul singolo record
034200070719     C                   eval      vinflg = '1'
034300040910     C* ......VATNOT
034400100715     C                   EVAL      VATNOT=%trim(%subst(vindta:45:30))           * sarebbe 32 char!!
034500040910     C                   EVAL      VATTRC = 'E'
034600070719     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
034700060202     C                   IF        VATNOT <> *blanks
034800040910     C                   WRITE     FIVAT000
034900060202     C                   ENDIF
035000040910     C*
035100040910     C                   ENDIF
035200010202     C*
035300000801     C* Ebbene...
035400000801     C                   ADD       1             §CTRMO
035500010201     C                   IF        errore <> *zeros
035600000801     C                   ADD       1             §CTRNO
035700000801     C                   EVAL      vinflg = '2'
035800000801     C                   ELSE
035900010201     C                   ADD       1             §CTROKVB
036000000801     C                   ENDIF
036100000801     C*
036200000801     C                   ENDSR
036300010202     C*----------------------------------------------------*
036400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
036500010202     C*----------------------------------------------------*
036600020305     C     PREVAT        BEGSR
036700010202     C*
036800021113     C* Compongo il nome del membro da dare al FIVATWWR
036900010202     C                   eval      parmbr = vlrhdl
037000010202     C                   movel     'M'           parmbr
037100060113     C                   eval      parccm = vlrksc
037200010202     C                   eval      paropz = '1'
037300010202     C* Effettuo la chiamata al CLLE preposto
037400040506     C                   call(e)   'TITVVTC'
037500010202     C                   parm                    parccm
037600010202     C                   parm                    parmbr
037700010202     C                   parm                    paropz
037800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
037900010202     C                   if        %error
038000010202     C                   movel     '1'           chkcall
038100010202     C                   else
038200010202     C                   movel     '0'           chkcall
038300010202     C                   endif
038400010202     C*
038500010202     C                   ENDSR
038600000801     C*----------------------------------------------------*
038700000801     C*  CONTROLLO NUMERICITA' CAMPI
038800000801     C*----------------------------------------------------*
038900000801     C     CHKNUM        BEGSR
039000000801     C*
039100000801     C                   call(e)   'ISNUMERIC'
039200000801     C                   PARM                    PiStr            30
039300040714     C                   PARM      ','           PiDecChr          1
039400000801     C                   PARM      *ZEROS        PiVal            30 9
039500000801     C                   PARM      '0'           PiInt             1
039600000801     C                   PARM      '0'           PiNum             1
039700000801     C                   IF        %error
039800000801     C                   EVAL      PiInt=*off
039900000801     C                   ENDIF
040000000801     C*
040100000801     C                   ENDSR
040200000801     C***
040300000801
040400011113
040500011113     C*----------------------------------------------------*
040600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040700011113     C*----------------------------------------------------*
040800011113     C     CHKIMPDIV     BEGSR
040900011113     C*
041000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041100011113     C                   Z-ADD     *zeros        wrkDec            9 9
041200011113     C*
041300011113     C* Come prima cosa effettuo considerazioni sulla divisa
041400011113     C                   IF        vabIAS > *zeros
041500011113     C                   IF        vabVAS <> 'EUR'
041600011113     C                   EVAL      vabVAS =  'ITL'
041700011113     C                   ENDIF
041800011113     C                   ENDIF
041900011113     C*
042000011113     C                   IF        vabCAS > *zeros
042100011113     C                   IF        vabVCA <> 'EUR'
042200011113     C                   EVAL      vabVCA =  'ITL'
042300011113     C                   ENDIF
042400011113     C                   ENDIF
042500011113     C*
042600011113     C                   IF        vabVMD > *zeros
042700020305     C                   IF        vabVAD <> 'EUR'
042800011113     C                   EVAL      vabVAD =  'ITL'
042900011113     C                   ENDIF
043000011113     C                   ENDIF
043100011113     C*
043200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043300011113     C                   Z-ADD     vabIAS        wrkDec
043400011113     C                   IF        wrkDec > *zeros
043500011113     C                   IF        vabVAS = 'ITL'
043600011113     C                   EVAL      vabIAS = *zeros
043700011113     C                   ENDIF
043800011113     C                   ENDIF
043900011113     C*
044000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
044100011113     C                   Z-ADD     vabCAS        wrkDec
044200011113     C                   IF        wrkDec > *zeros
044300011113     C                   IF        vabVCA = 'ITL'
044400011113     C                   EVAL      vabCAS = *zeros
044500011113     C                   ENDIF
044600011113     C                   ENDIF
044700011113     C*
044800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044900011113     C                   Z-ADD     vabVMD        wrkDec
045000011113     C                   IF        wrkDec > *zeros
045100011113     C                   IF        vabVAD = 'ITL'
045200011113     C                   EVAL      vabVMD = *zeros
045300011113     C                   ENDIF
045400011113     C                   ENDIF
045500011113     C*
045600011113     C                   ENDSR
045700011113     C***
045800011113
045900011113
046000000801
046100000801
046200990920      /TITLE Invio dei dati al punto operativo.
046300010202     C     invio         BEGSR
046400990920     C*
046500021113     C* 1° invio FIVAT
046600010201     C                   reset                   dscmz
046700010201     C                   move      vlrpoi        cmzdst
046800021113     C                   eval      cmzfld = 'FIVATWWR'
046900010201     C                   eval      cmzmbd = vlrhdl
047000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
047100021009     C***                if        prmfir = *blanks
047200021113     C                   eval      cmzfla = 'FIVAT00F'
047300021113     C                   eval      cmzmba = 'FIVAT00F'
047400021009     C***                else
047500021009     C***                eval      cmzfla = prmfir
047600021009     C***                eval      cmzmba = prmfir
047700021009     C***                endif
047800010201     C                   eval      cmznrr = *zeros
047900020305     C                   move      §ctrokvt      cmznrr
048000021018     C                   eval      cmzlba = vlrfl1
048100010201     C                   call(e)   'TIS711C'
048200010201     C                   parm                    dscmz
048300010201     C                   parm      *blanks       esito
048400010205     C                   if        %error
048500010205     C                             or cmzerr = '1'
048600010205     C                             or esito  = '1'
048700010205     C                   eval      wrkesito = '3'
048800010205     C                   else
048900010201     C*
049000021113     C* 2° invio FIVAB
049100010201     C                   reset                   dscmz
049200010201     C                   move      vlrpoi        cmzdst
049300010201     C                   eval      cmzfld = vlrfou
049400010201     C                   eval      cmzmbd = vlrhdl
049500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
049600021009     C***                if        prmfir = *blanks
049700021113     C                   eval      cmzfla = 'FIVAB00F'
049800021113     C                   eval      cmzmba = 'FIVAB00F'
049900021009     C***                else
050000021009     C***                eval      cmzfla = prmfir
050100021009     C***                eval      cmzmba = prmfir
050200021009     C***                endif
050300010201     C                   eval      cmznrr = *zeros
050400010201     C                   move      §ctrokvb      cmznrr
050500021018     C                   eval      cmzlba = vlrfl1
050600010201     C                   call(e)   'TIS711C'
050700010201     C                   parm                    dscmz
050800010201     C                   parm      *blanks       esito
050900010201     C                   if        %error
051000010201     C                             or cmzerr = '1'
051100010201     C                             or esito  = '1'
051200010201     C                   eval      wrkesito = '3'
051300010201     C                   endif
051400010205     C                   endif
051500990920     C*
051600000613     C                   ENDSR
051700000613     C***
051800070411
051900070411     C     *pssr         BEGSR
052000070411     C*
052100070411     C                   if        %open(tivin00r)
052200070411     C                   close     tivin00r
052300070411     C                   endif
052400070411     C                   if        %open(fivabwwr)
052500070411     C                   close     fivabwwr
052600070411     C                   endif
052700070411     C                   if        %open(fivatwwr)
052800070411     C                   close     fivatwwr
052900070411     C                   endif
053000070411     C*
053100070411     C* Effettuo la chiamata al CLLE preposto
053200070411     C                   call(e)   'TITVVTC'
053300070411     C                   parm                    parccm
053400070411     C                   parm                    parmbr
053500070411     C                   parm      '2'           paropz
053600070411     C*
053700070411     C                   eval      wrkesito = '2'
053800070411     C*
053900070411     C                   seton                                        LR
054000070411     C*
054100070411     C                   ENDSR     '*CANCL'
054200070411     C***
054300120319
054400120319
054500120319
054600120319      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
054700120319     C     wriVGD        BEGSR
054800120319     C*
054900120319     C                   clear                   tivgd000
055000120319     C                   eval      vgdDTA = %trim(wNomeFile)       +';'+
055100120319     C                                      %trim(vabRSD)          +';'+
055200120319     C                                      %trim(vabIND)          +';'+
055300120319     C                                      %trim(vabLOD)          +';'+
055400120319     C                                      %trim(vabCAD)          +';'+
055500120319     C                                      %trim(vabRMA)          +';'+
055600120319     C                                      %trim(%char(vabNCL))   +';'+
055700120319     C                                      %trim(%editw(vabPKB:'    0 , '))
055800120319     C                   eval      vgdTIP = 'EB'
055900120319     C                   eval      vgdKSU = vlrKSC
056000120319     C                   eval      vgdTSC = 'WW'
056100120319     C                   eval      vgdDAT = datcor
056200120319     C                   eval      vgdPGM = 'TITV1Y6R'
056300120319     C                   write     tivgd000
056400120319     C*
056500120319     C                   ENDSR
056600120319     C*------------------------------------------------------------------------*
056700120319
056800070411
056900990910
057000000613     C     *inzsr        BEGSR
057100990910     C*
057200990910     C     *entry        plist
057300990920     C                   parm                    tivlrds
057400990921     C                   parm      wrkesito      esito
057500000724     C                   parm                    prmlit
057600000710     C                   parm                    prmfir
057700120319     C*
057800120319     C                   MOVE(P)   vlrMSG        wNomeFile
057900120319     C*
058000120319     C* CALCOLA LA DATA CORRENTE
058100120319     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
058200120319     C                   eval      datcor = %dec(%date() : *iso)
058300000830     C*
058400000613     C                   ENDSR
058500000613     C***
