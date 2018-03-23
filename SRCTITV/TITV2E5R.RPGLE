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
001500070719     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500000830
002600041025     D*------------------
002700041025     D* DS REPERIMENTO NUMERATORE
002800041025     D*------------------
002900041025     D trul33ds      e ds                  inz
003000041025     D*------------------
003100041025     D* DS ARCHITETTURA
003200041025     D*------------------
003300041025     D kpjba         e ds                  inz
003400041025     D*------------------
003500990908
003600010201
003700010201
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100000613     C*
004200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004300070719     C*
004400070719     C* Esegue lancio TISI95R solo x chiusura
004500070719     C                   CLEAR                   TISI95DS
004600070719     C                   EVAL      I95TLA = 'C'
004700070719     C                   CALL      'TISI95R'
004800070719     C                   PARM                    TISI95DS
004900000613     C*
005000010202     C* Effettuo la chiamata al CLLE preposto
005100040506     C                   call(e)   'TITVVTC'
005200010202     C                   parm                    parccm
005300010202     C                   parm                    parmbr
005400010202     C                   parm      '2'           paropz
005500000616     C*
005600010201     C                   seton                                        LR
005700990908
005800000801
005900910830     C*--------------------------------------------------------
006000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006100910830     C*--------------------------------------------------------
006200040526     C     RWFILE        BEGSR
006300990910     C*
006400990914     C                   if        not %open(tivin00r)
006500990908     C                   open      tivin00r
006600990914     C                   endif
006700021113     C                   if        not %open(fivabwwr)
006800021113     C                   open      fivabwwr
006900990914     C                   endif
007000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007100020305     C                   exsr      prevat
007200010201     C*
007300010202     C                   if        chkcall = '0'
007400010202     C*
007500021113     C                   if        not %open(fivatwwr)
007600021113     C                   open      fivatwwr
007700010201     C                   endif
007800990910     C*
007900010201     C                   clear                   §CTROKVB          5 0
008000020305     C                   clear                   §CTROKVT          5 0
008100000801     C                   clear                   §CTRMO            5 0
008200000801     C                   clear                   §CTRNO            5 0
008300100729     C*
008400100729     C                   z-add     1             wGiro             1 0
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
011300100722     C*
011400100722     C* Scarico i buffer testata ancora "in canna"
011500100722     C                   WRITE     FIVAB000
011600010202     C*
011700010202     C                   endif
011800990910
011900990910     C* Se non ci sono record con errori ...
012000000710     C                   if        §ctrno = 0
012100990910     C* ... restituisco esito OK.
012200990921     C                   eval      wrkesito = '0'
012300990910     C                   else
012400010201     C                   if        §ctrokvb > 0
012500990921     C                   eval      wrkesito = '1'
012600000710     C                   else
012700000710     C                   eval      wrkesito = '2'
012800990910     C                   endif
012900000710     C                   endif
013000990910     C*
013100990914     C                   if        %open(tivin00r)
013200990908     C                   close     tivin00r
013300990914     C                   endif
013400021113     C                   if        %open(fivabwwr)
013500021113     C                   close     fivabwwr
013600990914     C                   endif
013700021113     C                   if        %open(fivatwwr)
013800021113     C                   close     fivatwwr
013900010201     C                   endif
014000990910     C*
014100010201     C                   if        §ctrokvb > 0
014200000724     C                             and vlrpoi <> *zeros
014300010202     C                   exsr      invio
014400990920     C                   endif
014500990920     C*
014600910830     C                   ENDSR
014700000613     C***
014800990920
014900000801     C*----------------------------------------------------*
015000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015100000801     C*----------------------------------------------------*
015200010201     C     INZVAR        BEGSR
015300000801     C*
015400040802     C                   Z-ADD     *zeros        Num5_0            5 0
015500040802     C                   MOVEL     '0'           FlgCAS            1
015600000801     C*
015700000801     C                   ENDSR
015800000801     C*----------------------------------------------------*
015900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
016000000801     C*----------------------------------------------------*
016100000801     C     DEFCAM        BEGSR
016200000801     C*
016300021113     C                   CLEAR                   FIVAB000
016400040802     C                   CLEAR                   FIVAT000
016500020619     C* Imposto i valori di default...
016600110425     C                   Z-ADD     0056113       VABCCM
016700110425     C                   Z-ADD     0056113       VATCCM
016800110425     C                   Z-ADD     005           VABLNP
016900110425     C                   Z-ADD     005           VATLNP
017000070531     C                   Z-ADD     000           VABCTR
017100070531     C                   MOVEL     '7Q'          VABCTM
017200040823     C                   MOVEL     '1'           VABCBO
017300110428     C                   EVAL      VABRMO = 'SATURN ON LINE SHOP'
017400020619     C* ... e poi verifico se sono stati passati come parametri
017500020619     C                   IF        vlrppt > *blanks
017600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017800020619     C                   EXSR      CHKNUM
017900020619     C                   IF        PiInt=*on
018000020619     C                   Z-ADD     PiVal         VABCCM
018100020619     C                   Z-ADD     PiVal         VATCCM
018200020619     C                   ENDIF
018300040506     C                   ENDIF
018400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018600020619     C                   EXSR      CHKNUM
018700020619     C                   IF        PiInt=*on
018800020619     C                   Z-ADD     PiVal         VABLNP
018900020619     C                   Z-ADD     PiVal         VATLNP
019000040506     C                   ENDIF
019100020619     C                   ENDIF
019200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
019300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019400020619     C                   EXSR      CHKNUM
019500020619     C                   IF        PiInt=*on
019600020619     C                   Z-ADD     PiVal         VABCTR
019700040506     C                   ENDIF
019800020619     C                   ENDIF
019900060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
020000060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
020100060202     C                   ENDIF
020200020619     C                   ENDIF
020300000801     C*
020400000801     C                   ENDSR
020500000801     C*----------------------------------------------------*
020600040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
020700000801     C*----------------------------------------------------*
020800040910     C     IMPVABVAT     BEGSR
020900040910     C*
021000040910     C* Traduzione relativa ai tipi record del file del cliente
021100040910     C*
021200071210     C*
021300071210     C***
021400110425     C* ...tipo record '101' (testata documento)
021500110425     C                   IF        %subst(vindta:1:3) = '101'
021600100722     C*
021700100722     C* Se nn primo giro => scarico il buffer precedente
021800100722     C                   if        wGiro = 1
021900100722     C                   eval      wGiro = 2
022000100722     C                   else
022100100722     C                   WRITE     FIVAB000
022200100722     C                   endif
022300100722     C*
022400071210     C* Resetto indicatore d anomalia sul singolo record
022500071210     C                   eval      vinflg = '1'
022600071210     C* ......inizializzazioni iniziali e formati record file Bartolini
022700071210     C                   EXSR      INZVAR
022800071210     C                   EXSR      DEFCAM
022900071210     C*
023000071210     C                   Z-ADD     *zeros        errore            1 0
023100071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
023200071210     C                   MOVEL     datcor        VABAAS
023300071210     C                   MOVEL     datcor        VATAAS
023400071210     C                   MOVE      datcor        VABMGS
023500071210     C                   MOVE(P)   vlrpoi        VABFGS
023600071210     C                   MOVE(P)   vlrpoi        VATFGS
023700071210     C* ......VABNSP/VATNSP
023800071210     C* NSP => Stacco un numeratore da AZNUM
023900071210     C                   clear                   TRUL33DS
024000071210     C                   eval      I33OPE = *zeros
024100071210     C                   eval      I33CNU = 302
024200071210     C                   eval      I33NUM = 1
024300071210     C                   movel     TRUL33DS      KPJBU
024400071210     C                   call      'TRUL33R'
024500071210     C                   parm                    KPJBA
024600071210     C                   movel     KPJBU         TRUL33DS
024700071210     C                   if        O33ERR = *zeros
024800071210     C                   z-add     O33NRF        VABNSP
024900071210     C                   z-add     O33NRF        VATNSP
025000071210     C                   else
025100071210     C                   Z-ADD     1             errore
025200071210     C                   EVAL      vinmsg = %trimr(vinmsg)
025300071210     C                             + ' ' + 'VABNSP VATNSP'
025400071210     C                   endif
025500060202     C* ......VABRMN
025600110425     C                   EVAL      PiStr=%trim(%subst(vindta:60:8))
025700060202     C                   EXSR      CHKNUM
025800060202     C                   IF        PiInt=*on
025900060202     C                   Z-ADD     PiVal         VABRMN
026000060202     C                   ELSE
026100060202     C                   ADD       1             errore
026200060202     C                   EVAL      vinmsg = %trimr(vinmsg)
026300060202     C                             + ' ' + 'VABRMN'
026400060202     C                   ENDIF
026500110207     C* ......VABNCL
026600110425     C                   EVAL      PiStr=%trim(%subst(vindta:227:10))
026700100714     C                   EXSR      CHKNUM
026800100714     C                   IF        PiInt=*on
026900110211     C                   Z-ADD     PiVal         VABNCL
027000100714     C                   ELSE
027100100714     C                   ADD       1             errore
027200100714     C                   EVAL      vinmsg = %trimr(vinmsg)
027300100714     C                             + ' ' + 'VABNCL'
027400100714     C                   ENDIF
027500060202     C* ......VABPKB
027600110425     C                   EVAL      PiStr=%trim(%subst(vindta:199:14))
027700060202     C                   EXSR      CHKNUM
027800060202     C                   IF        PiNum=*on
027900110425     C                   EVAL      PiVal = PiVal / 10000                        gest. 4 dec.
028000070726     C                   Z-ADD(H)  PiVal         VABPKB
028100060202     C                   ELSE
028200060202     C                   ADD       1             errore
028300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028400060202     C                             + ' ' + 'VABPKB'
028500060202     C                   ENDIF
028600110425     C* ......VABVLB
028700110425     C                   IF        %subst(vindta:237:10) <> *blanks AND
028800110425     C                             %subst(vindta:237:10) <> *zeros
028900110425     C                   EVAL      PiStr=%trim(%subst(vindta:237:10))
029000110425     C                   EXSR      CHKNUM
029100110425     C                   IF        PiNum=*on
029200110425     C                   EVAL      PiVal = PiVal / 10000                        gest. 4 dec.
029300110425     C                   Z-ADD(H)  PiVal         VABVLB
029400110425     C                   ELSE
029500110425     C                   ADD       1             errore
029600110425     C                   EVAL      vinmsg = %trimr(vinmsg)
029700110425     C                             + ' ' + 'VABVLB'
029800110425     C                   ENDIF
029900110425     C                   ENDIF
030000110211     C* ......VABRMA
030100110425     C                   EVAL      VABRMA=%trim(%subst(vindta:315:15))
030200060202     C* ......VABRSD
030300110425     C                   EVAL      VABRSD=%trim(%subst(vindta:88:30))
030400100714     C* ......VABIND
030500110425     C                   EVAL      VABIND=%trim(%subst(vindta:118:30))
030600060202     C* ......VABLOD
030700110425     C                   EVAL      VABLOD=%trim(%subst(vindta:153:30))
030800060202     C* ......VABCAD
030900110425     C                   EVAL      VABCAD=%trim(%subst(vindta:148:5))
031000060202     C* ......VABPRD
031100110425     C                   EVAL      VABPRD=%trim(%subst(vindta:183:2))
031200070719     C* Reperisco la provincia dal CAP e dalla località
031300070719     C                   IF        VABPRD  = *blanks AND
031400070719     C                             VABCAD <> *blanks
031500070719     C                   CLEAR                   TISI95DS
031600070719     C                   EVAL      I95TCN = '3'
031700070719     C                   Z-ADD     datcor        I95DAT
031800070719     C                   EVAL      I95CAP = VABCAD
031900070719     C                   EVAL      I95LOC = VABLOD
032000070719     C                   CALL      'TISI95R'
032100070719     C                   PARM                    TISI95DS
032200070719     C                   EVAL      VABPRD = O95PRV
032300070719     C                   ENDIF
032400110425     C* ......VABDCR
032500110425     C                   IF        %subst(vindta:185:8) <> *blanks AND
032600110425     C                             %subst(vindta:185:8) <> *zeros
032700110425     C                   EVAL      PiStr=%trim(%subst(vindta:185:8))
032800110425     C                   EXSR      CHKNUM
032900110425     C                   IF        PiInt=*on
033000110425     C                   Z-ADD     PiVal         VABDCR
033100110425     C                   ELSE
033200110425     C                   ADD       1             errore
033300110425     C                   EVAL      vinmsg = %trimr(vinmsg)
033400110425     C                             + ' ' + 'VABDCR'
033500110425     C                   ENDIF
033600110425     C                   ENDIF
033700110425     C* ......VABHCR
033800110425     C                   IF        %subst(vindta:193:4) <> *blanks AND
033900110425     C                             %subst(vindta:193:4) <> *zeros
034000110425     C                   EVAL      PiStr=%trim(%subst(vindta:193:4))
034100110425     C                   EXSR      CHKNUM
034200110425     C                   IF        PiInt=*on
034300110425     C                   Z-ADD     PiVal         VABHCR
034400110425     C                   ELSE
034500110425     C                   ADD       1             errore
034600110425     C                   EVAL      vinmsg = %trimr(vinmsg)
034700110425     C                             + ' ' + 'VABHCR'
034800110425     C                   ENDIF
034900110425     C                   ENDIF
035000110207     C* CAS
035100110425     C                   IF        %trim(%subst(vindta:247:11)) <> *blanks AND
035200110425     C                             %trim(%subst(vindta:247:11)) <> *zeros
035300110207     C                   MOVEL     '1'           FlgCAS
035400110207     C                   EVAL      VABVCA = 'EUR'
035500110425     C                   EVAL      PiStr=%trim(%subst(vindta:247:11))
035600110207     C                   EXSR      CHKNUM
035700110207     C                   IF        PiNum=*on
035800110207     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
035900110207     C                   Z-ADD     PiVal         VABCAS
036000110207     C                   ELSE
036100110207     C                   ADD       1             errore
036200110207     C                   Z-ADD     *zeros        VABCAS
036300110207     C                   EVAL      vinmsg = %trimr(vinmsg)
036400110207     C                             + ' ' + 'VABCAS'
036500110207     C                   ENDIF
036600110207     C                   ENDIF
036700060202     C*
036800060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036900060202     C                   IF        FlgCAS <> '0'
037000060202     C                   IF        VABCBO = '1'
037100060202     C                   EVAL      VABCBO = '4'
037200060202     C                   ELSE
037300060202     C                   EVAL      VABCBO = '6'
037400060202     C                   ENDIF
037500060202     C                   ENDIF
037600060202     C*
037700060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037800060202     C                   EXSR      CHKIMPDIV
037900040910     C*
038000040910     C                   ENDIF
038100110425     C***
038200110425     C* ...tipo record '103' (informazioni aggiuntive di consegna)
038300110425     C                   IF        %subst(vindta:1:3) = '103'
038400110425     C* Resetto indicatore d anomalia sul singolo record
038500110425     C                   eval      vinflg = '1'
038600110425     C*
038700110425     C* Verifico il tipo d informazione corrente
038800110425     C                   SELECT
038900110425     C*
039000110425     C                   WHEN      %trim(%subst(vindta:138:20)) = 'TELEF'
039100110425     C* VATNOT "B"
039200110425     C                   EVAL      VATNOT=%trim(%subst(vindta:68:70))
039300110425     C                   EVAL      VATTRC = 'B'
039400110425     C                   IF        VATNOT <> *blanks
039500110425     C                   ADD       1             §CTROKVT
039600110425     C                   WRITE     FIVAT000
039700110425     C                   ENDIF
039800110425     C*
039900110425     C                   WHEN      %trim(%subst(vindta:138:20)) = 'SHIP'
040000110425     C*
040100110425     C                   SELECT
040200110425     C*
040300110425     C                   WHEN      %trim(%subst(vindta:68:20)) = 'APPNT'
040400110425     C                   EVAL      VABTC1='A'
040500110425     C*
040600110425     C                   WHEN      %trim(%subst(vindta:68:20)) = 'TIME1'
040700110425     C                   EVAL      VABTSP='E'
040800110425     C*
040900110425     C                   WHEN      %trim(%subst(vindta:68:20)) = 'FLOOR'
041000110425     C                   EVAL      VABTC2='P'
041100110425     C*
041200110425     C                   ENDSL
041300110425     C*
041400110425     C                   WHEN      %trim(%subst(vindta:138:20)) = 'VALASS'
041500110425     C* IAS
041600110425     C                   IF        %trim(%subst(vindta:68:70)) <> *blanks AND
041700110425     C                             %trim(%subst(vindta:68:70)) <> *zeros
041800110425     C                   EVAL      VABVAS = 'EUR'
041900110425     C                   EVAL      PiStr=%trim(%subst(vindta:68:70))
042000110425     C                   EXSR      CHKNUM
042100110425     C                   IF        PiNum=*on
042200110425     C                   Z-ADD     PiVal         VABIAS
042300110425     C                   ELSE
042400110425     C                   ADD       1             errore
042500110425     C                   Z-ADD     *zeros        VABIAS
042600110425     C                   EVAL      vinmsg = %trimr(vinmsg)
042700110425     C                             + ' ' + 'VABIAS'
042800110425     C                   ENDIF
042900110425     C                   ENDIF
043000110425     C*
043100110425     C                   WHEN      %trim(%subst(vindta:138:20)) = 'NOTE'
043200110425     C* NOT / NT2
043300110425     C                   EVAL      VABNOT=%trim(%subst(vindta:68:35))
043400110425     C                   EVAL      VABNT2=%trim(%subst(vindta:68+35:70-35))
043500110425     C*
043600110425     C                   ENDSL
043700110425     C*
043800110425     C                   ENDIF
043900040910     C***
044000110425     C* ...tipo record '109' (dettaglio barcode segnacooli)
044100110425     C                   IF        %subst(vindta:1:3) = '109'
044200070719     C* Resetto indicatore d anomalia sul singolo record
044300070719     C                   eval      vinflg = '1'
044400100722     C* ......VATNOT
044500110425     C                   EVAL      VATNOT=%trim(%subst(vindta:68:30))
044600100722     C                   EVAL      VATTRC = 'E'
044700110425     C* x ogni tipo record '109' devo scaricare il buffer del file FIVAT
044800100722     C                   IF        VATNOT <> *blanks
044900100722     C                   ADD       1             §CTROKVT
045000100722     C                   WRITE     FIVAT000
045100100722     C                   ENDIF
045200100722     C*
045300100722     C                   ENDIF
045400010202     C*
045500000801     C* Ebbene...
045600000801     C                   ADD       1             §CTRMO
045700010201     C                   IF        errore <> *zeros
045800000801     C                   ADD       1             §CTRNO
045900000801     C                   EVAL      vinflg = '2'
046000000801     C                   ELSE
046100010201     C                   ADD       1             §CTROKVB
046200000801     C                   ENDIF
046300000801     C*
046400000801     C                   ENDSR
046500010202     C*----------------------------------------------------*
046600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046700010202     C*----------------------------------------------------*
046800020305     C     PREVAT        BEGSR
046900010202     C*
047000021113     C* Compongo il nome del membro da dare al FIVATWWR
047100010202     C                   eval      parmbr = vlrhdl
047200010202     C                   movel     'M'           parmbr
047300060113     C                   eval      parccm = vlrksc
047400010202     C                   eval      paropz = '1'
047500010202     C* Effettuo la chiamata al CLLE preposto
047600040506     C                   call(e)   'TITVVTC'
047700010202     C                   parm                    parccm
047800010202     C                   parm                    parmbr
047900010202     C                   parm                    paropz
048000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048100010202     C                   if        %error
048200010202     C                   movel     '1'           chkcall
048300010202     C                   else
048400010202     C                   movel     '0'           chkcall
048500010202     C                   endif
048600010202     C*
048700010202     C                   ENDSR
048800000801     C*----------------------------------------------------*
048900000801     C*  CONTROLLO NUMERICITA' CAMPI
049000000801     C*----------------------------------------------------*
049100000801     C     CHKNUM        BEGSR
049200000801     C*
049300000801     C                   call(e)   'ISNUMERIC'
049400000801     C                   PARM                    PiStr            30
049500040714     C                   PARM      ','           PiDecChr          1
049600000801     C                   PARM      *ZEROS        PiVal            30 9
049700000801     C                   PARM      '0'           PiInt             1
049800000801     C                   PARM      '0'           PiNum             1
049900000801     C                   IF        %error
050000000801     C                   EVAL      PiInt=*off
050100000801     C                   ENDIF
050200000801     C*
050300000801     C                   ENDSR
050400000801     C***
050500000801
050600011113
050700011113     C*----------------------------------------------------*
050800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050900011113     C*----------------------------------------------------*
051000011113     C     CHKIMPDIV     BEGSR
051100011113     C*
051200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051300011113     C                   Z-ADD     *zeros        wrkDec            9 9
051400011113     C*
051500011113     C* Come prima cosa effettuo considerazioni sulla divisa
051600011113     C                   IF        vabIAS > *zeros
051700011113     C                   IF        vabVAS <> 'EUR'
051800011113     C                   EVAL      vabVAS =  'ITL'
051900011113     C                   ENDIF
052000011113     C                   ENDIF
052100011113     C*
052200011113     C                   IF        vabCAS > *zeros
052300011113     C                   IF        vabVCA <> 'EUR'
052400011113     C                   EVAL      vabVCA =  'ITL'
052500011113     C                   ENDIF
052600011113     C                   ENDIF
052700011113     C*
052800011113     C                   IF        vabVMD > *zeros
052900020305     C                   IF        vabVAD <> 'EUR'
053000011113     C                   EVAL      vabVAD =  'ITL'
053100011113     C                   ENDIF
053200011113     C                   ENDIF
053300011113     C*
053400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053500011113     C                   Z-ADD     vabIAS        wrkDec
053600011113     C                   IF        wrkDec > *zeros
053700011113     C                   IF        vabVAS = 'ITL'
053800011113     C                   EVAL      vabIAS = *zeros
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054300011113     C                   Z-ADD     vabCAS        wrkDec
054400011113     C                   IF        wrkDec > *zeros
054500011113     C                   IF        vabVCA = 'ITL'
054600011113     C                   EVAL      vabCAS = *zeros
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055100011113     C                   Z-ADD     vabVMD        wrkDec
055200011113     C                   IF        wrkDec > *zeros
055300011113     C                   IF        vabVAD = 'ITL'
055400011113     C                   EVAL      vabVMD = *zeros
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C                   ENDSR
055900011113     C***
056000011113
056100011113
056200000801
056300000801
056400990920      /TITLE Invio dei dati al punto operativo.
056500010202     C     invio         BEGSR
056600990920     C*
056700021113     C* 1° invio FIVAT
056800010201     C                   reset                   dscmz
056900010201     C                   move      vlrpoi        cmzdst
057000021113     C                   eval      cmzfld = 'FIVATWWR'
057100010201     C                   eval      cmzmbd = vlrhdl
057200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057300021009     C***                if        prmfir = *blanks
057400021113     C                   eval      cmzfla = 'FIVAT00F'
057500021113     C                   eval      cmzmba = 'FIVAT00F'
057600021009     C***                else
057700021009     C***                eval      cmzfla = prmfir
057800021009     C***                eval      cmzmba = prmfir
057900021009     C***                endif
058000010201     C                   eval      cmznrr = *zeros
058100020305     C                   move      §ctrokvt      cmznrr
058200021018     C                   eval      cmzlba = vlrfl1
058300010201     C                   call(e)   'TIS711C'
058400010201     C                   parm                    dscmz
058500010201     C                   parm      *blanks       esito
058600010205     C                   if        %error
058700010205     C                             or cmzerr = '1'
058800010205     C                             or esito  = '1'
058900010205     C                   eval      wrkesito = '3'
059000010205     C                   else
059100010201     C*
059200021113     C* 2° invio FIVAB
059300010201     C                   reset                   dscmz
059400010201     C                   move      vlrpoi        cmzdst
059500010201     C                   eval      cmzfld = vlrfou
059600010201     C                   eval      cmzmbd = vlrhdl
059700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059800021009     C***                if        prmfir = *blanks
059900021113     C                   eval      cmzfla = 'FIVAB00F'
060000021113     C                   eval      cmzmba = 'FIVAB00F'
060100021009     C***                else
060200021009     C***                eval      cmzfla = prmfir
060300021009     C***                eval      cmzmba = prmfir
060400021009     C***                endif
060500010201     C                   eval      cmznrr = *zeros
060600010201     C                   move      §ctrokvb      cmznrr
060700021018     C                   eval      cmzlba = vlrfl1
060800010201     C                   call(e)   'TIS711C'
060900010201     C                   parm                    dscmz
061000010201     C                   parm      *blanks       esito
061100010201     C                   if        %error
061200010201     C                             or cmzerr = '1'
061300010201     C                             or esito  = '1'
061400010201     C                   eval      wrkesito = '3'
061500010201     C                   endif
061600010205     C                   endif
061700990920     C*
061800000613     C                   ENDSR
061900000613     C***
062000070411
062100070411     C     *pssr         BEGSR
062200070411     C*
062300070411     C                   if        %open(tivin00r)
062400070411     C                   close     tivin00r
062500070411     C                   endif
062600070411     C                   if        %open(fivabwwr)
062700070411     C                   close     fivabwwr
062800070411     C                   endif
062900070411     C                   if        %open(fivatwwr)
063000070411     C                   close     fivatwwr
063100070411     C                   endif
063200070411     C*
063300070411     C* Effettuo la chiamata al CLLE preposto
063400070411     C                   call(e)   'TITVVTC'
063500070411     C                   parm                    parccm
063600070411     C                   parm                    parmbr
063700070411     C                   parm      '2'           paropz
063800070411     C*
063900070411     C                   eval      wrkesito = '2'
064000070411     C*
064100070411     C                   seton                                        LR
064200070411     C*
064300070411     C                   ENDSR     '*CANCL'
064400070411     C***
064500070411
064600990910
064700000613     C     *inzsr        BEGSR
064800990910     C*
064900990910     C     *entry        plist
065000990920     C                   parm                    tivlrds
065100990921     C                   parm      wrkesito      esito
065200000724     C                   parm                    prmlit
065300000710     C                   parm                    prmfir
065400000613     C*
065500000830     C* CALCOLA LA DATA CORRENTE
065600100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
065700100722     C                   eval      datcor = %dec(%date() : *ISO)
065800000830     C*
065900000613     C                   ENDSR
066000000613     C***
