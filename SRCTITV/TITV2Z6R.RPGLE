000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200140120     H dftactgrp(*no) actgrp(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500140117     FEDIVABwr  O    E             DISK    usropn
000600140117     FEDIVATwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070719     D tisi95ds      e ds
001600140117     D edivabds      e ds                  extname(edivab0f)
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600000830
002700041025     D*------------------
002800041025     D* DS REPERIMENTO NUMERATORE
002900041025     D*------------------
003000041025     D trul33ds      e ds                  inz
003100041025     D*------------------
003200041025     D* DS ARCHITETTURA
003300041025     D*------------------
003400041025     D kpjba         e ds                  inz
003500041025     D*------------------
003600990908
003700010201
003800010201
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200000613     C*
004300150213     C                   EXSR      DEFCAM
004400150213     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004500070719     C*
004600070719     C* Esegue lancio TISI95R solo x chiusura
004700070719     C                   CLEAR                   TISI95DS
004800070719     C                   EVAL      I95TLA = 'C'
004900070719     C                   CALL      'TISI95R'
005000070719     C                   PARM                    TISI95DS
005100000613     C*
005200010202     C* Effettuo la chiamata al CLLE preposto
005300140922     C                   call(e)   'TITVEVTC'
005400140922     C                   parm                    parccm
005500140922     C                   parm                    parmbr
005600140922     C                   parm      '2'           paropz
005700170731     C*
005800170731     C                   if        *in53
005900170731     C                   call      'TIS7P2SER'
006000170731     C                   parm      'C'           pIn_Opz           1
006100170731     C                   parm                    tivlrds
006200170731     C                   parm                    EDIVABDS
006300170731     C                   parm      *blanks       pIn_CdIdAz        2
006400170731     C                   parm      *blanks       pIn_MaskPDF      50
006500170731     C                   parm      *blanks       pOut_Esito        1
006600170731     C                   endif
006700000616     C*
006800010201     C                   seton                                        LR
006900990908
007000000801
007100910830     C*--------------------------------------------------------
007200140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007300910830     C*--------------------------------------------------------
007400040526     C     RWFILE        BEGSR
007500990910     C*
007600990914     C                   if        not %open(tivin00r)
007700990908     C                   open      tivin00r
007800990914     C                   endif
007900140117     C                   if        not %open(edivabwr)
008000140117     C                   open      edivabwr
008100990914     C                   endif
008200140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008300020305     C                   exsr      prevat
008400010201     C*
008500010202     C                   if        chkcall = '0'
008600010202     C*
008700140117     C                   if        not %open(edivatwr)
008800140117     C                   open      edivatwr
008900010201     C                   endif
009000990910     C*
009100010201     C                   clear                   §CTROKVB          5 0
009200020305     C                   clear                   §CTROKVT          5 0
009300000801     C                   clear                   §CTRMO            5 0
009400000801     C                   clear                   §CTRNO            5 0
009500140117     C*
009600140902     C                   if        *in53
009700140117     C                   call      'TIS7P2SER'
009800140117     C                   parm      'O'           pIn_Opz           1
009900140117     C                   parm                    tivlrds
010000140117     C                   parm                    EDIVABDS
010100140117     C                   parm      *blanks       pIn_CdIdAz        2
010200140117     C                   parm      *blanks       pIn_MaskPDF      50
010300140117     C                   parm      *blanks       pOut_Esito        1
010400140902     C                   endif
010500161003     C*
010600130708     C                   SETOFF                                       31
010700921023     C                   DO        *HIVAL
010800990913     C*
010900990915     C                   READ      tivin00r                               70
011000040910     C                   if        vindta > *blanks
011100000613     C                   add       1             rrnum
011200000801     C*
011300000801     C                   if        *in70 = *off
011400000801     C                             and
011500000801     C                             (vinflg = *blanks
011600000801     C                              or vinflg = '0'
011700000801     C                              or vinflg = '2')
011800000801     C*
011900000801     C                   clear                   vinmsg
012000000801     C                   eval      vinflg = '1'
012100040910     C*
012200040910     C* Eseguo routine d traduzione
012300040910     C                   exsr      impvabvat
012400161014     C*
012500161014     C* Scarico buffer
012600161014     C  N31              exsr      CHKWRI
012700161014     C  N31              WRITE     EDIVAB00
012800161014     C                   SETOFF                                       31
012900040802     C*
013000010305     C                   endif
013100000905     C*
013200000905     C                   else
013300000905     C                   eval      vinflg = '1'
013400000905     C                   endif
013500000905     C*
013600000905     C  N70              update    tivin000
013700000905     C*
013800991022     C  N70              ENDdo
013900010202     C*
014000010202     C                   endif
014100990910
014200990910     C* Se non ci sono record con errori ...
014300000710     C                   if        §ctrno = 0
014400990910     C* ... restituisco esito OK.
014500990921     C                   eval      wrkesito = '0'
014600990910     C                   else
014700010201     C                   if        §ctrokvb > 0
014800990921     C                   eval      wrkesito = '1'
014900000710     C                   else
015000000710     C                   eval      wrkesito = '2'
015100990910     C                   endif
015200000710     C                   endif
015300990910     C*
015400990914     C                   if        %open(tivin00r)
015500990908     C                   close     tivin00r
015600990914     C                   endif
015700140117     C                   if        %open(edivabwr)
015800140117     C                   close     edivabwr
015900990914     C                   endif
016000140117     C                   if        %open(edivatwr)
016100140117     C                   close     edivatwr
016200010201     C                   endif
016300990910     C*
016400010201     C                   if        §ctrokvb > 0
016500000724     C                             and vlrpoi <> *zeros
016600010202     C                   exsr      invio
016700990920     C                   endif
016800990920     C*
016900910830     C                   ENDSR
017000000613     C***
017100140922
017200140922     C*----------------------------------------------------*
017300140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
017400140922     C*----------------------------------------------------*
017500140922     C     CHKWRI        BEGSR
017600140922     C*
017700140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
017800161014     C                   IF        FlgCAS <> '0' OR
017900161014     C                             VABCAS > *zeros
018000140922     C                   IF        VABCBO = '1'
018100140922     C                   EVAL      VABCBO = '4'
018200140922     C                   ELSE
018300140922     C                   EVAL      VABCBO = '6'
018400140922     C                   ENDIF
018500140922     C                   ENDIF
018600140922     C*
018700140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
018800140922     C                   EXSR      CHKIMPDIV
018900140922     C*
019000140922     C                   ENDSR
019100990920
019200000801     C*----------------------------------------------------*
019300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019400000801     C*----------------------------------------------------*
019500010201     C     INZVAR        BEGSR
019600000801     C*
019700040802     C                   Z-ADD     *zeros        Num5_0            5 0
019800040802     C                   MOVEL     '0'           FlgCAS            1
019900000801     C*
020000000801     C                   ENDSR
020100000801     C*----------------------------------------------------*
020200040910     C*  IMPOSTAZIONE CAMPI COSTANTI
020300000801     C*----------------------------------------------------*
020400000801     C     DEFCAM        BEGSR
020500000801     C*
020600140117     C                   CLEAR                   EDIVAB00
020700140117     C                   CLEAR                   EDIVAT00
020800020619     C* Imposto i valori di default...
020900170418     C                   Z-ADD     1282234       VABCCM
021000170418     C                   Z-ADD     1282234       VATCCM
021100170418     C                   Z-ADD     128           VABLNP
021200170418     C                   Z-ADD     128           VATLNP
021300160509     C                   Z-ADD     000           VABCTR
021400151119     C                   MOVEL     '7Q'          VABCTM
021500040823     C                   MOVEL     '1'           VABCBO
021600020619     C* ... e poi verifico se sono stati passati come parametri
021700020619     C                   IF        vlrppt > *blanks
021800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022000020619     C                   EXSR      CHKNUM
022100020619     C                   IF        PiInt=*on
022200020619     C                   Z-ADD     PiVal         VABCCM
022300020619     C                   Z-ADD     PiVal         VATCCM
022400020619     C                   ENDIF
022500040506     C                   ENDIF
022600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022800020619     C                   EXSR      CHKNUM
022900020619     C                   IF        PiInt=*on
023000020619     C                   Z-ADD     PiVal         VABLNP
023100020619     C                   Z-ADD     PiVal         VATLNP
023200040506     C                   ENDIF
023300020619     C                   ENDIF
023400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023600020619     C                   EXSR      CHKNUM
023700020619     C                   IF        PiInt=*on
023800020619     C                   Z-ADD     PiVal         VABCTR
023900040506     C                   ENDIF
024000020619     C                   ENDIF
024100060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
024200060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
024300060202     C                   ENDIF
024400140902     C                   IF        %subst(vlrppt:16:1) = 'S'
024500140902     C                   SETON                                        53
024600140902     C                   ELSE
024700140902     C                   SETOFF                                       53
024800140902     C                   ENDIF
024900020619     C                   ENDIF
025000000801     C*
025100000801     C                   ENDSR
025200000801     C*----------------------------------------------------*
025300140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
025400000801     C*----------------------------------------------------*
025500040910     C     IMPVABVAT     BEGSR
025600040910     C*
025700161014     C* Creo e salvo il numero CMR del file corrente
025800151119     C                   MOVEL     *blanks       savCMR           35
025900170418     C                   EVAL      savCMR = %char(%date())
026000100722     C*
026100071210     C* Resetto indicatore d anomalia sul singolo record
026200071210     C                   eval      vinflg = '1'
026300161014     C*
026400071210     C* ......inizializzazioni iniziali e formati record file Bartolini
026500071210     C                   EXSR      INZVAR
026600071210     C                   EXSR      DEFCAM
026700071210     C*
026800071210     C                   Z-ADD     *zeros        errore            1 0
026900071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
027000071210     C                   MOVEL     datcor        VABAAS
027100071210     C                   MOVEL     datcor        VATAAS
027200071210     C                   MOVE      datcor        VABMGS
027300071210     C                   MOVE(P)   vlrpoi        VABFGS
027400071210     C                   MOVE(P)   vlrpoi        VATFGS
027500140903     C*
027600140903     C* Dai "fissi"
027700140903     C                   EVAL      VABCMR = savCMR
027800140903     C                   EVAL      VABDCM = datcor
027900140903     C                   EVAL      VABDTS = datcor
028000140903     C                   EVAL      VABHMS = oracor
028100140903     C                   EVAL      VABCNT = 1
028200140903     C*
028300071210     C* ......VABNSP/VATNSP
028400071210     C* NSP => Stacco un numeratore da AZNUM
028500160609     C                   clear                   TRUL33DS
028600160609     C                   eval      I33OPE = *zeros
028700160609     C                   eval      I33CNU = 302
028800160609     C                   eval      I33NUM = 1
028900160609     C                   movel     TRUL33DS      KPJBU
029000160609     C                   call      'TRUL33R'
029100160609     C                   parm                    KPJBA
029200160609     C                   movel     KPJBU         TRUL33DS
029300160609     C                   if        O33ERR = *zeros
029400160609     C                   z-add     O33NRF        VABNSP
029500160609     C                   z-add     O33NRF        VATNSP
029600160609     C                   else
029700160609     C                   Z-ADD     1             errore
029800160609     C                   EVAL      vinmsg = %trimr(vinmsg)
029900160609     C                             + ' ' + 'VABNSP VATNSP'
030000160609     C                   endif
030100161014     C* ......VABRMA
030200170418     C                   EVAL      VABRMA=%trim(%subst(vindta:3:10))
030300161014     C* ......VABRSD
030400170418     C                   EVAL      VABRSD=%trim(%subst(vindta:45:40))
030500170524     C* ......VABFFD
030600170524     C                   MOVEL     *blanks       wCIVICO          10
030700170524     C                   IF        %trim(%subst(vindta:145:10)) = 'FD'
030800170524     C                   EVAL      VABFFD = 'S'
030900170524     C                   ELSE
031000170524     C                   EVAL      wCIVICO = %trim(%subst(vindta:145:10))
031100170524     C                   ENDIF
031200170418     C* ......VABIND / RD2
031300170418     C                   MOVEL     *blanks       wIND             70
031400170524     C                   EVAL      wIND = %trim(%subst(vindta:85:60))+
031500170524     C                                    ' '+wCIVICO
031600170418     C                   EVAL      VABIND=%trim(%subst(wIND:1:35))
031700170418     C                   EVAL      VABRD2=%trim(%subst(wIND:36:35))
031800150611     C* ......VABLOD
031900170418     C                   EVAL      VABLOD=%trim(%subst(vindta:160:40))
032000161014     C* ......VABCAD
032100170418     C                   EVAL      VABCAD=%trim(%subst(vindta:155:5))
032200150611     C* ......VABPRD
032300170418     C                   EVAL      VABPRD=%trim(%subst(vindta:200:3))
032400170418     C* Se vuota reperisco la provincia dal CAP e dalla località
032500150611     C                   IF        VABPRD  = *blanks AND
032600150611     C                             VABLOD <> *blanks AND
032700150611     C                             VABCAD <> *blanks AND
032800150611     C                             VABNZD  = *blanks
032900150611     C                   CLEAR                   TISI95DS
033000150611     C                   EVAL      I95TCN = '3'
033100150611     C                   Z-ADD     datcor        I95DAT
033200150611     C                   EVAL      I95CAP = VABCAD
033300150611     C                   EVAL      I95LOC = VABLOD
033400150611     C                   CALL      'TISI95R'
033500150611     C                   PARM                    TISI95DS
033600150611     C                   EVAL      VABPRD = O95PRV
033700150611     C                   ENDIF
033800170418     C* ......VABNZD
033900170418     C                   EVAL      VABNZD=%trim(%subst(vindta:876:3))
034000170418     C                   IF        VABNZD='IT' OR
034100170418     C                             VABNZD='SM'
034200170418     C                   EVAL      VABNZD=*blanks
034300170418     C                   ENDIF
034400161014     C* ......VABTSP
034500170418     C***                IF        %subst(vindta:546:1) = '1'
034600170418     C***                EVAL      VABTSP = 'E'
034700170418     C***                ENDIF
034800170418     C* ......VABNOT / NT2
034900170418     C                   MOVEL     *blanks       wNOTE            70
035000170418     C                   EVAL      wNOTE = %trim(%subst(vindta:266:132))
035100170418     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
035200170418     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:35))
035300160609     C* ......VABRMN
035400170418     C                   EVAL      PiStr=%trim(%subst(vindta:3:10))
035500160617     C                   EXSR      CHKNUM
035600160617     C                   IF        PiInt=*on
035700160617     C                   Z-ADD     PiVal         VABRMN
035800160617     C                   ELSE
035900160617     C                   Z-ADD     1             VABRMN
036000160617     C                   EVAL      vinmsg = %trimr(vinmsg)
036100160617     C                             + ' ' + 'VABRMN'
036200160617     C                   ENDIF
036300170518     C* ......VABTSP
036400170518     C                   SELECT
036500170518     C                   WHEN      %subst(vindta:783:2) = '03'
036600170518     C                   EVAL      VABTSP = 'C'
036700170518     C                   WHEN      %subst(vindta:783:2) = '21'
036800170518     C                   EVAL      VABTSP = 'E'
036900170518     C                   WHEN      %subst(vindta:783:2) = '19'
037000170518     C                   EVAL      VABTSP = 'H'
037100170518     C                   ENDSL
037200150611     C* ......VATNOT_B
037300170418     C                   EVAL      VATNOT=%trim(%subst(vindta:398:15))
037400161014     C                   IF        VATNOT <> *blanks
037500161014     C                   EVAL      VATCMR = VABCMR
037600161014     C                   EVAL      VATCNT = VABCNT
037700161014     C                   EVAL      VATTRC = 'B'
037800161014     C                   ADD       1             §CTROKVT
037900161014     C                   WRITE     EDIVAT00
038000161014     C                   ENDIF
038100160321     C* ......VATNOT_S
038200170418     C                   EVAL      VATNOT=%trim(%subst(vindta:413:15))
038300170418     C                   IF        VATNOT <> *blanks
038400170418     C                   EVAL      VATCMR = VABCMR
038500170418     C                   EVAL      VATCNT = VABCNT
038600170418     C                   EVAL      VATTRC = 'S'
038700170418     C                   ADD       1             §CTROKVT
038800170418     C                   WRITE     EDIVAT00
038900170418     C                   ENDIF
039000160321     C* ......VATNOT_IJ
039100161014     C                   MOVEL     *blanks       wEMAIL           70
039200170418     C                   EVAL      wEMAIL = %trim(%subst(vindta:787:60))
039300161014     C                   EVAL      VATNOT = %subst(wEMAIL:1:35)
039400161014     C                   IF        VATNOT <> *blanks
039500161014     C                   EVAL      VATCMR = VABCMR
039600161014     C                   EVAL      VATCNT = VABCNT
039700161014     C                   EVAL      VATTRC = 'I'
039800161014     C                   ADD       1             §CTROKVT
039900161014     C                   WRITE     EDIVAT00
040000161014     C                   ENDIF
040100161014     C                   EVAL      VATNOT = %subst(wEMAIL:36:35)
040200161014     C                   IF        VATNOT <> *blanks
040300161014     C                   EVAL      VATCMR = VABCMR
040400161014     C                   EVAL      VATCNT = VABCNT
040500161014     C                   EVAL      VATTRC = 'J'
040600161014     C                   ADD       1             §CTROKVT
040700161014     C                   WRITE     EDIVAT00
040800161014     C                   ENDIF
040900140922     C* ......VATNOT_P
041000150611     C***                if        *in53
041100140922     C*
041200150611     C* Solo se valorizzato RMA
041300150611     C***                if        wRMA_1 <> *blanks
041400150611     C***                eval      pIn_MaskPDF =
041500150611     C***                                  %trim(%subst(vindta:1292:100))
041600150611     C***                call      'TIS7P2SER'
041700150611     C***                parm      'W'           pIn_Opz           1
041800150611     C***                parm                    tivlrds
041900150611     C***                parm                    EDIVABDS
042000150611     C***                parm      '10'          pIn_CdIdAz        2
042100150611     C***                parm                    pIn_MaskPDF      50
042200150611     C***                parm      *blanks       pOut_Esito        1
042300150611     C***                endif
042400150611     C***                endif
042500140117     C*
042600161014     C* ......VABPKB
042700170418     C                   EVAL      PiStr=%trim(%subst(vindta:251:15))
042800160617     C                   EXSR      CHKNUM
042900160617     C                   IF        PiNum=*on
043000161014     C                   Z-ADD     PiVal         VABPKB
043100160617     C                   ELSE
043200161014     C                   Z-ADD     *zeros        VABPKB
043300160617     C                   EVAL      vinmsg = %trimr(vinmsg)
043400160617     C                             + ' ' + 'VABPKB'
043500160617     C                   ENDIF
043600160617     C* ......VABNCL
043700170418     C                   EVAL      PiStr=%trim(%subst(vindta:231:5))
043800161014     C                   EXSR      CHKNUM
043900161014     C                   IF        PiInt=*on
044000161014     C                   Z-ADD     PiVal         VABNCL
044100161014     C                   ELSE
044200161014     C                   Z-ADD     *zeros        VABNCL
044300161014     C                   EVAL      vinmsg = %trimr(vinmsg)
044400161014     C                             + ' ' + 'VABNCL'
044500161014     C                   ENDIF
044600170418     C* ......VABVLB
044700170418     C                   EVAL      PiStr=%trim(%subst(vindta:879:10))
044800170418     C                   EXSR      CHKNUM
044900170418     C                   IF        PiNum=*on
045000170418     C                   EVAL      PiVal = Pival/1000000                        * da cm3 a m3
045100170418     C                   Z-ADD     PiVal         VABVLB
045200170418     C                   ELSE
045300170418     C                   Z-ADD     *zeros        VABVLB
045400170418     C                   EVAL      vinmsg = %trimr(vinmsg)
045500170418     C                             + ' ' + 'VABVLB'
045600170418     C                   ENDIF
045700161014     C* ......VABDCR
045800170418     C***                IF        %trim(%subst(vindta:517:8)) <> *zeros  AND
045900170418     C***                          %trim(%subst(vindta:517:8)) <> *blanks
046000170418     C***                EVAL      PiStr=%trim(%subst(vindta:517:8))
046100170418     C***                EXSR      CHKNUM
046200170418     C***                IF        PiInt=*on
046300170418     C***                Z-ADD     PiVal         VABDCR
046400170418     C***                ELSE
046500170418     C***                Z-ADD     *zeros        VABDCR
046600170418     C***                EVAL      vinmsg = %trimr(vinmsg)
046700170418     C***                          + ' ' + 'VABDCR'
046800170418     C***                ENDIF
046900170418     C***                ENDIF
047000161014     C* ......VABHCR
047100170418     C***                IF        %trim(%subst(vindta:542:4)) <> *zeros  AND
047200170418     C***                          %trim(%subst(vindta:542:4)) <> *blanks
047300170418     C***                EVAL      PiStr=%trim(%subst(vindta:542:4))
047400170418     C***                EXSR      CHKNUM
047500170418     C***                IF        PiInt=*on
047600170418     C***                Z-ADD     PiVal         VABHCR
047700170418     C***                ELSE
047800170418     C***                Z-ADD     *zeros        VABHCR
047900170418     C***                EVAL      vinmsg = %trimr(vinmsg)
048000170418     C***                          + ' ' + 'VABHCR'
048100170418     C***                ENDIF
048200170418     C***                ENDIF
048300161014     C* ......VABCAS
048400170418     C***                IF        %trim(%subst(vindta:413:13)) <> *zeros  AND
048500170418     C***                          %trim(%subst(vindta:413:13)) <> *blanks
048600170418     C***                EVAL      VABVCA=%trim(%subst(vindta:426:3))
048700170418     C***                EVAL      PiStr=%trim(%subst(vindta:413:13))
048800170418     C***                EVAL      FlgCAS = '1'
048900170418     C***                EXSR      CHKNUM
049000170418     C***                IF        PiNum=*on
049100170418     C***                EVAL      PiVal = PiVal/10000                          * gestisco 4 dec.
049200170418     C***                Z-ADD     PiVal         VABCAS
049300170418     C***                ELSE
049400170418     C***                Z-ADD     *zeros        VABCAS
049500170418     C***                EVAL      vinmsg = %trimr(vinmsg)
049600170418     C***                          + ' ' + 'VABCAS'
049700170418     C***                ENDIF
049800170418     C***                ENDIF
049900161014     C* ......VABVMD
050000170418     C***                IF        %trim(%subst(vindta:486:13)) <> *zeros  AND
050100170418     C***                          %trim(%subst(vindta:486:13)) <> *blanks
050200170418     C***                EVAL      VABVAD=%trim(%subst(vindta:499:3))
050300170418     C***                EVAL      PiStr=%trim(%subst(vindta:486:13))
050400170418     C***                EXSR      CHKNUM
050500170418     C***                IF        PiNum=*on
050600170418     C***                EVAL      PiVal = PiVal/10000                          * gestisco 4 dec.
050700170418     C***                Z-ADD     PiVal         VABVMD
050800170418     C***                ELSE
050900170418     C***                Z-ADD     *zeros        VABVMD
051000170418     C***                EVAL      vinmsg = %trimr(vinmsg)
051100170418     C***                          + ' ' + 'VABVMD'
051200170418     C***                ENDIF
051300170418     C***                ENDIF
051400161014     C* ......VATNOT_E => sviluppo i "chi sono" DAL/AL
051500170418     C                   Z-ADD     1             wCollo            5 0
051600170418     C                   DOW       wCollo <= VABNCL AND wCollo < 999
051700170418     C                   EVAL      VATNOT=%subst(vindta:872:4)+
051800170418     C                                    %subst(vindta:3:10)+
051900170418     C                                    %editc(wCollo:'X')
052000151119     C                   IF        VATNOT <> *blanks
052100151119     C                   EVAL      VATCMR = VABCMR
052200151119     C                   EVAL      VATCNT = VABCNT
052300151119     C                   EVAL      VATTRC = 'E'
052400151119     C                   ADD       1             §CTROKVT
052500151119     C                   WRITE     EDIVAT00
052600151119     C                   ENDIF
052700170418     C                   ADD       1             wCollo
052800161014     C                   ENDDO
052900140117     C***
053000010202     C*
053100000801     C* Ebbene...
053200000801     C                   ADD       1             §CTRMO
053300010201     C                   IF        errore <> *zeros
053400000801     C                   ADD       1             §CTRNO
053500000801     C                   EVAL      vinflg = '2'
053600000801     C                   ELSE
053700010201     C                   ADD       1             §CTROKVB
053800000801     C                   ENDIF
053900000801     C*
054000000801     C                   ENDSR
054100010202     C*----------------------------------------------------*
054200140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
054300010202     C*----------------------------------------------------*
054400020305     C     PREVAT        BEGSR
054500010202     C*
054600140117     C* Compongo il nome del membro da dare al EDIVATWR
054700010202     C                   eval      parmbr = vlrhdl
054800010202     C                   movel     'M'           parmbr
054900060113     C                   eval      parccm = vlrksc
055000010202     C                   eval      paropz = '1'
055100010202     C* Effettuo la chiamata al CLLE preposto
055200140117     C                   call(e)   'TITVEVTC'
055300010202     C                   parm                    parccm
055400010202     C                   parm                    parmbr
055500010202     C                   parm                    paropz
055600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055700010202     C                   if        %error
055800010202     C                   movel     '1'           chkcall
055900010202     C                   else
056000010202     C                   movel     '0'           chkcall
056100010202     C                   endif
056200010202     C*
056300010202     C                   ENDSR
056400000801     C*----------------------------------------------------*
056500000801     C*  CONTROLLO NUMERICITA' CAMPI
056600000801     C*----------------------------------------------------*
056700000801     C     CHKNUM        BEGSR
056800000801     C*
056900000801     C                   call(e)   'ISNUMERIC'
057000000801     C                   PARM                    PiStr            30
057100170418     C                   PARM      ','           PiDecChr          1
057200000801     C                   PARM      *ZEROS        PiVal            30 9
057300000801     C                   PARM      '0'           PiInt             1
057400000801     C                   PARM      '0'           PiNum             1
057500000801     C                   IF        %error
057600000801     C                   EVAL      PiInt=*off
057700000801     C                   ENDIF
057800000801     C*
057900000801     C                   ENDSR
058000000801     C***
058100000801
058200011113
058300011113     C*----------------------------------------------------*
058400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058500011113     C*----------------------------------------------------*
058600011113     C     CHKIMPDIV     BEGSR
058700011113     C*
058800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058900011113     C                   Z-ADD     *zeros        wrkDec            9 9
059000011113     C*
059100011113     C* Come prima cosa effettuo considerazioni sulla divisa
059200011113     C                   IF        vabIAS > *zeros
059300011113     C                   IF        vabVAS <> 'EUR'
059400011113     C                   EVAL      vabVAS =  'ITL'
059500011113     C                   ENDIF
059600011113     C                   ENDIF
059700011113     C*
059800011113     C                   IF        vabCAS > *zeros
059900011113     C                   IF        vabVCA <> 'EUR'
060000011113     C                   EVAL      vabVCA =  'ITL'
060100011113     C                   ENDIF
060200011113     C                   ENDIF
060300011113     C*
060400011113     C                   IF        vabVMD > *zeros
060500020305     C                   IF        vabVAD <> 'EUR'
060600011113     C                   EVAL      vabVAD =  'ITL'
060700011113     C                   ENDIF
060800011113     C                   ENDIF
060900011113     C*
061000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061100011113     C                   Z-ADD     vabIAS        wrkDec
061200011113     C                   IF        wrkDec > *zeros
061300011113     C                   IF        vabVAS = 'ITL'
061400011113     C                   EVAL      vabIAS = *zeros
061500011113     C                   ENDIF
061600011113     C                   ENDIF
061700011113     C*
061800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061900011113     C                   Z-ADD     vabCAS        wrkDec
062000011113     C                   IF        wrkDec > *zeros
062100011113     C                   IF        vabVCA = 'ITL'
062200011113     C                   EVAL      vabCAS = *zeros
062300011113     C                   ENDIF
062400011113     C                   ENDIF
062500011113     C*
062600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062700011113     C                   Z-ADD     vabVMD        wrkDec
062800011113     C                   IF        wrkDec > *zeros
062900011113     C                   IF        vabVAD = 'ITL'
063000011113     C                   EVAL      vabVMD = *zeros
063100011113     C                   ENDIF
063200011113     C                   ENDIF
063300011113     C*
063400011113     C                   ENDSR
063500011113     C***
063600011113
063700011113
063800000801
063900000801
064000990920      /TITLE Invio dei dati al punto operativo.
064100010202     C     invio         BEGSR
064200990920     C*
064300140117     C* 1° invio EDIVAT
064400010201     C                   reset                   dscmz
064500010201     C                   move      vlrpoi        cmzdst
064600140117     C                   eval      cmzfld = 'EDIVATWR'
064700010201     C                   eval      cmzmbd = vlrhdl
064800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064900021009     C***                if        prmfir = *blanks
065000140117     C                   eval      cmzfla = 'EDIVAT0F'
065100140117     C                   eval      cmzmba = 'EDIVAT0F'
065200021009     C***                else
065300021009     C***                eval      cmzfla = prmfir
065400021009     C***                eval      cmzmba = prmfir
065500021009     C***                endif
065600010201     C                   eval      cmznrr = *zeros
065700020305     C                   move      §ctrokvt      cmznrr
065800021018     C                   eval      cmzlba = vlrfl1
065900010201     C                   call(e)   'TIS711C'
066000010201     C                   parm                    dscmz
066100010201     C                   parm      *blanks       esito
066200010205     C                   if        %error
066300010205     C                             or cmzerr = '1'
066400010205     C                             or esito  = '1'
066500010205     C                   eval      wrkesito = '3'
066600010205     C                   else
066700010201     C*
066800140117     C* 2° invio EDIVAB
066900010201     C                   reset                   dscmz
067000010201     C                   move      vlrpoi        cmzdst
067100010201     C                   eval      cmzfld = vlrfou
067200010201     C                   eval      cmzmbd = vlrhdl
067300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067400021009     C***                if        prmfir = *blanks
067500140117     C                   eval      cmzfla = 'EDIVAB0F'
067600140117     C                   eval      cmzmba = 'EDIVAB0F'
067700021009     C***                else
067800021009     C***                eval      cmzfla = prmfir
067900021009     C***                eval      cmzmba = prmfir
068000021009     C***                endif
068100010201     C                   eval      cmznrr = *zeros
068200010201     C                   move      §ctrokvb      cmznrr
068300021018     C                   eval      cmzlba = vlrfl1
068400010201     C                   call(e)   'TIS711C'
068500010201     C                   parm                    dscmz
068600010201     C                   parm      *blanks       esito
068700010201     C                   if        %error
068800010201     C                             or cmzerr = '1'
068900010201     C                             or esito  = '1'
069000010201     C                   eval      wrkesito = '3'
069100010201     C                   endif
069200010205     C                   endif
069300990920     C*
069400000613     C                   ENDSR
069500000613     C***
069600070411
069700070411     C     *pssr         BEGSR
069800070411     C*
069900070411     C                   if        %open(tivin00r)
070000070411     C                   close     tivin00r
070100070411     C                   endif
070200140117     C                   if        %open(edivabwr)
070300140117     C                   close     edivabwr
070400070411     C                   endif
070500140117     C                   if        %open(edivatwr)
070600140117     C                   close     edivatwr
070700070411     C                   endif
070800070411     C*
070900070411     C* Effettuo la chiamata al CLLE preposto
071000140117     C                   call(e)   'TITVEVTC'
071100070411     C                   parm                    parccm
071200070411     C                   parm                    parmbr
071300070411     C                   parm      '2'           paropz
071400070411     C*
071500070411     C                   eval      wrkesito = '2'
071600070411     C*
071700070411     C                   seton                                        LR
071800070411     C*
071900070411     C                   ENDSR     '*CANCL'
072000070411     C***
072100070411
072200990910
072300000613     C     *inzsr        BEGSR
072400990910     C*
072500990910     C     *entry        plist
072600990920     C                   parm                    tivlrds
072700990921     C                   parm      wrkesito      esito
072800000724     C                   parm                    prmlit
072900000710     C                   parm                    prmfir
073000000613     C*
073100000830     C* CALCOLA LA DATA CORRENTE
073200140117     C                   time                    wn14             14 0
073300140117     C                   movel     wn14          oracor            6 0          *ORA
073400100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
073500100722     C                   eval      datcor = %dec(%date() : *ISO)
073600000830     C*
073700000613     C                   ENDSR
073800000613     C***
