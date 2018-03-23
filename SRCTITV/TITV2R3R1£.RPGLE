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
002600140919     D wRMN            s             25
002700140919     D wNum            ds
002800140919     D  Num                           1    dim(10)
002900140919     D  Num_inz                1     10    inz('0123456789')
003000140919     D wI              s              2  0 INZ(*zeros)
003100140919     D wY              s              2  0 INZ(*zeros)
003200000830
003300041025     D*------------------
003400041025     D* DS REPERIMENTO NUMERATORE
003500041025     D*------------------
003600041025     D trul33ds      e ds                  inz
003700041025     D*------------------
003800041025     D* DS ARCHITETTURA
003900041025     D*------------------
004000041025     D kpjba         e ds                  inz
004100041025     D*------------------
004200990908
004300010201
004400010201
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800000613     C*
004900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005000070719     C*
005100070719     C* Esegue lancio TISI95R solo x chiusura
005200070719     C                   CLEAR                   TISI95DS
005300070719     C                   EVAL      I95TLA = 'C'
005400070719     C                   CALL      'TISI95R'
005500070719     C                   PARM                    TISI95DS
005600000613     C*
005700010202     C* Effettuo la chiamata al CLLE preposto
005800140120     C***                call(e)   'TITVEVTC'
005900140120     C***                parm                    parccm
006000140120     C***                parm                    parmbr
006100140120     C***                parm      '2'           paropz
006200000616     C*
006300010201     C                   seton                                        LR
006400990908
006500000801
006600910830     C*--------------------------------------------------------
006700140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006800910830     C*--------------------------------------------------------
006900040526     C     RWFILE        BEGSR
007000990910     C*
007100990914     C                   if        not %open(tivin00r)
007200990908     C                   open      tivin00r
007300990914     C                   endif
007400140117     C                   if        not %open(edivabwr)
007500140117     C                   open      edivabwr
007600990914     C                   endif
007700140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007800020305     C                   exsr      prevat
007900010201     C*
008000010202     C                   if        chkcall = '0'
008100010202     C*
008200140117     C                   if        not %open(edivatwr)
008300140117     C                   open      edivatwr
008400010201     C                   endif
008500990910     C*
008600010201     C                   clear                   §CTROKVB          5 0
008700020305     C                   clear                   §CTROKVT          5 0
008800000801     C                   clear                   §CTRMO            5 0
008900000801     C                   clear                   §CTRNO            5 0
009000100729     C*
009100100729     C                   z-add     1             wGiro             1 0
009200140117     C*
009300140902     C                   if        *in53
009400140117     C                   call      'TIS7P2SER'
009500140117     C                   parm      'O'           pIn_Opz           1
009600140117     C                   parm                    tivlrds
009700140117     C                   parm                    EDIVABDS
009800140117     C                   parm      *blanks       pIn_CdIdAz        2
009900140117     C                   parm      *blanks       pIn_MaskPDF      50
010000140117     C                   parm      *blanks       pOut_Esito        1
010100140902     C                   endif
010200130708     C*
010300130708     C                   SETOFF                                       31
010400921023     C                   DO        *HIVAL
010500990913     C*
010600990915     C                   READ      tivin00r                               70
010700040910     C                   if        vindta > *blanks
010800000613     C                   add       1             rrnum
010900000801     C*
011000000801     C                   if        *in70 = *off
011100000801     C                             and
011200000801     C                             (vinflg = *blanks
011300000801     C                              or vinflg = '0'
011400000801     C                              or vinflg = '2')
011500000801     C*
011600000801     C                   clear                   vinmsg
011700000801     C                   eval      vinflg = '1'
011800040910     C*
011900040910     C* Eseguo routine d traduzione
012000040910     C                   exsr      impvabvat
012100040802     C*
012200010305     C                   endif
012300000905     C*
012400000905     C                   else
012500000905     C                   eval      vinflg = '1'
012600000905     C                   endif
012700000905     C*
012800000905     C  N70              update    tivin000
012900000905     C*
013000991022     C  N70              ENDdo
013100100722     C*
013200100722     C* Scarico i buffer testata ancora "in canna"
013300140117     C  N31              WRITE     EDIVAB00
013400130708     C                   SETOFF                                       31
013500140117     C*
013600140902     C                   if        *in53
013700140117     C                   call      'TIS7P2SER'
013800140117     C                   parm      'C'           pIn_Opz           1
013900140117     C                   parm                    tivlrds
014000140117     C                   parm                    EDIVABDS
014100140117     C                   parm      *blanks       pIn_CdIdAz        2
014200140117     C                   parm      *blanks       pIn_MaskPDF      50
014300140117     C                   parm      *blanks       pOut_Esito        1
014400140902     C                   endif
014500010202     C*
014600010202     C                   endif
014700990910
014800990910     C* Se non ci sono record con errori ...
014900000710     C                   if        §ctrno = 0
015000990910     C* ... restituisco esito OK.
015100990921     C                   eval      wrkesito = '0'
015200990910     C                   else
015300010201     C                   if        §ctrokvb > 0
015400990921     C                   eval      wrkesito = '1'
015500000710     C                   else
015600000710     C                   eval      wrkesito = '2'
015700990910     C                   endif
015800000710     C                   endif
015900990910     C*
016000990914     C                   if        %open(tivin00r)
016100990908     C                   close     tivin00r
016200990914     C                   endif
016300140117     C                   if        %open(edivabwr)
016400140117     C                   close     edivabwr
016500990914     C                   endif
016600140117     C                   if        %open(edivatwr)
016700140117     C                   close     edivatwr
016800010201     C                   endif
016900990910     C*
017000010201     C                   if        §ctrokvb > 0
017100000724     C                             and vlrpoi <> *zeros
017200010202     C                   exsr      invio
017300990920     C                   endif
017400990920     C*
017500910830     C                   ENDSR
017600000613     C***
017700990920
017800000801     C*----------------------------------------------------*
017900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018000000801     C*----------------------------------------------------*
018100010201     C     INZVAR        BEGSR
018200000801     C*
018300040802     C                   Z-ADD     *zeros        Num5_0            5 0
018400040802     C                   MOVEL     '0'           FlgCAS            1
018500000801     C*
018600000801     C                   ENDSR
018700000801     C*----------------------------------------------------*
018800040910     C*  IMPOSTAZIONE CAMPI COSTANTI
018900000801     C*----------------------------------------------------*
019000000801     C     DEFCAM        BEGSR
019100000801     C*
019200140117     C                   CLEAR                   EDIVAB00
019300140117     C                   CLEAR                   EDIVAT00
019400020619     C* Imposto i valori di default...
019500140117     C                   Z-ADD     2495088       VABCCM
019600140117     C                   Z-ADD     2495088       VATCCM
019700140117     C                   Z-ADD     249           VABLNP
019800140117     C                   Z-ADD     249           VATLNP
019900070531     C                   Z-ADD     000           VABCTR
020000140117     C                   MOVEL     '7T'          VABCTM
020100040823     C                   MOVEL     '1'           VABCBO
020200020619     C* ... e poi verifico se sono stati passati come parametri
020300020619     C                   IF        vlrppt > *blanks
020400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020600020619     C                   EXSR      CHKNUM
020700020619     C                   IF        PiInt=*on
020800020619     C                   Z-ADD     PiVal         VABCCM
020900020619     C                   Z-ADD     PiVal         VATCCM
021000020619     C                   ENDIF
021100040506     C                   ENDIF
021200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021400020619     C                   EXSR      CHKNUM
021500020619     C                   IF        PiInt=*on
021600020619     C                   Z-ADD     PiVal         VABLNP
021700020619     C                   Z-ADD     PiVal         VATLNP
021800040506     C                   ENDIF
021900020619     C                   ENDIF
022000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022200020619     C                   EXSR      CHKNUM
022300020619     C                   IF        PiInt=*on
022400020619     C                   Z-ADD     PiVal         VABCTR
022500040506     C                   ENDIF
022600020619     C                   ENDIF
022700060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
022800060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
022900060202     C                   ENDIF
023000140902     C                   IF        %subst(vlrppt:16:1) = 'S'
023100140902     C                   SETON                                        53
023200140902     C                   ELSE
023300140902     C                   SETOFF                                       53
023400140902     C                   ENDIF
023500020619     C                   ENDIF
023600000801     C*
023700000801     C                   ENDSR
023800000801     C*----------------------------------------------------*
023900140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
024000000801     C*----------------------------------------------------*
024100040910     C     IMPVABVAT     BEGSR
024200040910     C*
024300040910     C* Traduzione relativa ai tipi record del file del cliente
024400040910     C*
024500071210     C*
024600071210     C***
024700140117     C* ...tipo record 'HEADINFO' (info CMR)
024800140117     C                   IF        %trim(%subst(vindta:1:10)) = 'HEADINFO'
024900140903     C                   MOVEL     *blanks       savCMR           35
025000140903     C                   EVAL      savCMR = %trim(%subst(vindta:45:35))
025100130620     C                   ENDIF
025200130620     C***
025300140117     C* ...tipo record 'CONSI001' (info testata)
025400140117     C                   IF        %trim(%subst(vindta:1:10)) = 'CONSI001'
025500100722     C*
025600100722     C* Se nn primo giro => scarico il buffer precedente
025700100722     C                   if        wGiro = 1
025800100722     C                   eval      wGiro = 2
025900100722     C                   else
026000141007     C                   SETOFF                                       57
026100140117     C  N31              WRITE     EDIVAB00
026200130708     C                   SETOFF                                       31
026300100722     C                   endif
026400100722     C*
026500071210     C* Resetto indicatore d anomalia sul singolo record
026600071210     C                   eval      vinflg = '1'
026700071210     C* ......inizializzazioni iniziali e formati record file Bartolini
026800071210     C                   EXSR      INZVAR
026900071210     C                   EXSR      DEFCAM
027000071210     C*
027100071210     C                   Z-ADD     *zeros        errore            1 0
027200071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
027300071210     C                   MOVEL     datcor        VABAAS
027400071210     C                   MOVEL     datcor        VATAAS
027500071210     C                   MOVE      datcor        VABMGS
027600071210     C                   MOVE(P)   vlrpoi        VABFGS
027700071210     C                   MOVE(P)   vlrpoi        VATFGS
027800140903     C*
027900140903     C* Dai "fissi"
028000140903     C                   EVAL      VABCMR = savCMR
028100140903     C                   EVAL      VABDCM = datcor
028200140903     C                   EVAL      VABDTS = datcor
028300140903     C                   EVAL      VABHMS = oracor
028400140903     C                   EVAL      VABCNT = 1
028500140903     C*
028600071210     C* ......VABNSP/VATNSP
028700071210     C* NSP => Stacco un numeratore da AZNUM
028800071210     C                   clear                   TRUL33DS
028900071210     C                   eval      I33OPE = *zeros
029000071210     C                   eval      I33CNU = 302
029100071210     C                   eval      I33NUM = 1
029200071210     C                   movel     TRUL33DS      KPJBU
029300071210     C                   call      'TRUL33R'
029400071210     C                   parm                    KPJBA
029500071210     C                   movel     KPJBU         TRUL33DS
029600071210     C                   if        O33ERR = *zeros
029700071210     C                   z-add     O33NRF        VABNSP
029800071210     C                   z-add     O33NRF        VATNSP
029900071210     C                   else
030000071210     C                   Z-ADD     1             errore
030100071210     C                   EVAL      vinmsg = %trimr(vinmsg)
030200071210     C                             + ' ' + 'VABNSP VATNSP'
030300071210     C                   endif
030400140117     C* ......VABRMA
030500140910     C                   EVAL      VABRMA=%trim(%subst(vindta:20:15))
030600140117     C* ......VABDCR
030700140117     C                   EVAL      PiStr=%trim(%subst(vindta:40:8))
030800140117     C                   EXSR      CHKNUM
030900140117     C                   IF        PiInt=*on
031000140910     C                   Z-ADD     PiVal         VABDCR
031100140117     C                   ELSE
031200140117     C                   ADD       1             errore
031300140117     C                   EVAL      vinmsg = %trimr(vinmsg)
031400140910     C                             + ' ' + 'VABDCR'
031500140117     C                   ENDIF
031600140117     C* ......VABCAS
031700140117     C                   IF        %subst(vindta:56:15) <> *all'0'
031800140117     C                   EVAL      FlgCAS = '1'
031900140117     C                   EVAL      PiStr=%trim(%subst(vindta:56:15))
032000140117     C                   EXSR      CHKNUM
032100140117     C                   IF        PiNum=*on
032200140117     C                   EVAL      VABCAS = PiVal / 100
032300140117     C                   ELSE
032400140117     C                   ADD       1             errore
032500140117     C                   EVAL      vinmsg = %trimr(vinmsg)
032600140117     C                             + ' ' + 'VABCAS'
032700140117     C                   ENDIF
032800140117     C                   ENDIF
032900140117     C*
033000140117     C* Considerazioni sul contenuto di campi precedentemente valorizzati
033100140117     C                   IF        FlgCAS <> '0'
033200140117     C                   IF        VABCBO = '1'
033300140117     C                   EVAL      VABCBO = '4'
033400140117     C                   ELSE
033500140117     C                   EVAL      VABCBO = '6'
033600140117     C                   ENDIF
033700140117     C                   ENDIF
033800140117     C*
033900140117     C* Eseguo routine finale x considerazioni specifiche su importi/divise
034000140117     C                   EXSR      CHKIMPDIV
034100140117     C*
034200140117     C                   ENDIF
034300140929     C***
034400140929     C* ...tipo record 'CONSI002' - 'DEL' (info testata)
034500140929     C                   IF        %trim(%subst(vindta:1:10)) = 'CONSI002' AND
034600141007     C                             %trim(%subst(vindta:11:3)) = 'DEL'      AND
034700141007     C                             not *IN57
034800141007     C                   SETON                                        57
034900140929     C                   MOVEL     *blanks       wNOTE            70
035000141007     C                   EVAL      wNOTE =%trim(%subst(vindta:14:70))  +
035100140929     C                                    %trim(%subst(vindta:84:70))  +
035200140929     C                                    %trim(%subst(vindta:154:70))
035300140929     C* ......VABNOT / VABNT2
035400140929     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
035500140929     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:35))
035600140929     C                   ENDIF
035700140117     C***
035800140117     C* ...tipo record 'CONSI004.1' (info testata)
035900140117     C                   IF        %trim(%subst(vindta:1:10)) = 'CONSI004.1'
036000140117     C* ......VABNOT
036100140929     C***                EVAL      VABNOT=%trim(%subst(vindta:69:25))
036200140117     C* ......VABRMN
036300140919     C* Acquisisco il VABRMN da posizione 69 per 25 bytes MA eliminando tutti i caratteri NON
036400140919     C* numerici
036500140919     C                   EVAL      wY = 0
036600140924     C                   EVAL      wRMN=%trim(%subst(vindta:69:25))
036700140919     C     1             DO        25            wI
036800140919     C* se il carattere è numerico lo salvo
036900140919     C                   IF        %lookup(%subst(wRMN : wI : 1) : num) > 0
037000140919     C                   EVAL      wY += 1
037100140919     C                   EVAL      %subst(PiStr : wY : 1) =
037200140919     C                             %subst(wRMN : wI : 1)
037300140919     C                   ENDIF
037400140919     C                   ENDDO
037500140919
037600140919     C***                EVAL      PiStr=%trim(%subst(vindta:69:6))
037700140117     C                   EXSR      CHKNUM
037800140117     C                   IF        PiInt=*on
037900140117     C                   Z-ADD     PiVal         VABRMN
038000140117     C                   ELSE
038100140117     C                   ADD       1             errore
038200140117     C                   EVAL      vinmsg = %trimr(vinmsg)
038300140117     C                             + ' ' + 'VABRMN'
038400140117     C                   ENDIF
038500140117     C* ......VATNOT_P
038600140902     C                   if        *in53
038700140117     C*
038800140121     C* Solo se valorizzato RMN
038900140121     C                   if        VABRMN > *zeros
039000140117     C                   eval      pIn_MaskPDF = '&0RMN*.pdf'
039100140117     C                   call      'TIS7P2SER'
039200140117     C                   parm      'W'           pIn_Opz           1
039300140117     C                   parm                    tivlrds
039400140117     C                   parm                    EDIVABDS
039500140117     C                   parm      '10'          pIn_CdIdAz        2
039600140117     C                   parm                    pIn_MaskPDF      50
039700140117     C                   parm      *blanks       pOut_Esito        1
039800140121     C                   endif
039900140902     C                   endif
040000140117     C* ......VABNCL
040100140117     C                   EVAL      PiStr=%trim(%subst(vindta:96:4))
040200140117     C                   EXSR      CHKNUM
040300140117     C                   IF        PiInt=*on
040400140117     C                   Z-ADD     PiVal         VABNCL
040500140117     C                   ELSE
040600140117     C                   ADD       1             errore
040700140117     C                   EVAL      vinmsg = %trimr(vinmsg)
040800140117     C                             + ' ' + 'VABNCL'
040900140117     C                   ENDIF
041000140117     C* ......VABPKB
041100140117     C                   EVAL      PiStr=%trim(%subst(vindta:100:7))
041200140117     C                   EXSR      CHKNUM
041300140117     C                   IF        PiNum=*on
041400140117     C                   ADD       PiVal         VABPKB
041500140117     C                   ELSE
041600140117     C                   ADD       1             errore
041700140117     C                   EVAL      vinmsg = %trimr(vinmsg)
041800140117     C                             + ' ' + 'VABPKB'
041900140117     C                   ENDIF
042000140117     C*
042100140117     C                   ENDIF
042200140117     C***
042300140117     C* ...tipo record 'CONSI005' (info testata)
042400140117     C                   IF        %trim(%subst(vindta:1:10)) = 'CONSI005'
042500140117     C* Solo sub-types 'CN' o 'DP'
042600140117     C                   IF        %trim(%subst(vindta:11:3)) = 'CN' OR
042700140117     C                             %trim(%subst(vindta:11:3)) = 'DP'
042800060202     C* ......VABRSD
042900140117     C                   EVAL      VABRSD=%trim(%subst(vindta:14:35))
043000130620     C* ......VABRD2
043100140117     C                   EVAL      VABRD2=%trim(%subst(vindta:84:35))
043200100714     C* ......VABIND
043300140117     C                   EVAL      VABIND=%trim(%subst(vindta:49:35))
043400130620     C* ......VABCAD
043500140117     C                   EVAL      VABCAD=%trim(%subst(vindta:154:9))
043600060202     C* ......VABLOD
043700140117     C                   EVAL      VABLOD=%trim(%subst(vindta:119:35))
043800130620     C* ......VABNZD
043900140117     C                   EVAL      VABNZD=%trim(%subst(vindta:163:3))
044000130620     C                   IF        VABNZD='IT'
044100130620     C                   EVAL      VABNZD=*blanks
044200130620     C                   ENDIF
044300140918     C* ......VABPRD
044400140918     C                   EVAL      VABPRD=*blanks
044500060202     C* ......VABPRD
044600070719     C* Reperisco la provincia dal CAP e dalla località
044700070719     C                   IF        VABPRD  = *blanks AND
044800130620     C                             VABLOD <> *blanks AND
044900130620     C                             VABCAD <> *blanks AND
045000130620     C                             VABNZD  = *blanks
045100070719     C                   CLEAR                   TISI95DS
045200070719     C                   EVAL      I95TCN = '3'
045300070719     C                   Z-ADD     datcor        I95DAT
045400070719     C                   EVAL      I95CAP = VABCAD
045500070719     C                   EVAL      I95LOC = VABLOD
045600070719     C                   CALL      'TISI95R'
045700070719     C                   PARM                    TISI95DS
045800070719     C                   EVAL      VABPRD = O95PRV
045900070719     C                   ENDIF
046000140117     C*
046100140117     C                   ENDIF
046200140117     C*
046300140117     C                   ENDIF
046400140117     C***
046500140117     C* ...tipo record 'COLLI001' (info dettagli)
046600140117     C                   IF        %trim(%subst(vindta:1:10)) = 'COLLI001'
046700110425     C* Resetto indicatore d anomalia sul singolo record
046800110425     C                   eval      vinflg = '1'
046900130620     C* ......VATNOT_E
047000140909     C                   EVAL      VATNOT = %trim(%subst(vindta:69:35))
047100140909     C***                EVAL      PiStr=%trim(%subst(vindta:69:35))
047200140909     C***                EXSR      CHKNUM
047300140909     C***                IF        PiInt=*on
047400140909     C***                Z-ADD     PiVal         wNUMNOT_E        35 0
047500140909     C***                EVAL      VATNOT=%trim(%editc(wNUMNOT_E:'4'))
047600140117     C* x ogni tipo record 'COLLI001' devo scaricare il buffer del file EDIVAT
047700100722     C                   IF        VATNOT <> *blanks
047800140117     C                   EVAL      VATCMR = VABCMR
047900140117     C                   EVAL      VATCNT = VABCNT
048000140117     C                   EVAL      VATTRC = 'E'
048100100722     C                   ADD       1             §CTROKVT
048200140117     C                   WRITE     EDIVAT00
048300100722     C                   ENDIF
048400140909     C***                ENDIF
048500100722     C*
048600100722     C                   ENDIF
048700010202     C*
048800000801     C* Ebbene...
048900000801     C                   ADD       1             §CTRMO
049000010201     C                   IF        errore <> *zeros
049100000801     C                   ADD       1             §CTRNO
049200000801     C                   EVAL      vinflg = '2'
049300000801     C                   ELSE
049400010201     C                   ADD       1             §CTROKVB
049500000801     C                   ENDIF
049600000801     C*
049700000801     C                   ENDSR
049800010202     C*----------------------------------------------------*
049900140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
050000010202     C*----------------------------------------------------*
050100020305     C     PREVAT        BEGSR
050200010202     C*
050300140117     C* Compongo il nome del membro da dare al EDIVATWR
050400010202     C                   eval      parmbr = vlrhdl
050500010202     C                   movel     'M'           parmbr
050600060113     C                   eval      parccm = vlrksc
050700010202     C                   eval      paropz = '1'
050800010202     C* Effettuo la chiamata al CLLE preposto
050900140117     C                   call(e)   'TITVEVTC'
051000010202     C                   parm                    parccm
051100010202     C                   parm                    parmbr
051200010202     C                   parm                    paropz
051300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051400010202     C                   if        %error
051500010202     C                   movel     '1'           chkcall
051600010202     C                   else
051700010202     C                   movel     '0'           chkcall
051800010202     C                   endif
051900010202     C*
052000010202     C                   ENDSR
052100000801     C*----------------------------------------------------*
052200000801     C*  CONTROLLO NUMERICITA' CAMPI
052300000801     C*----------------------------------------------------*
052400000801     C     CHKNUM        BEGSR
052500000801     C*
052600000801     C                   call(e)   'ISNUMERIC'
052700000801     C                   PARM                    PiStr            30
052800040714     C                   PARM      ','           PiDecChr          1
052900000801     C                   PARM      *ZEROS        PiVal            30 9
053000000801     C                   PARM      '0'           PiInt             1
053100000801     C                   PARM      '0'           PiNum             1
053200000801     C                   IF        %error
053300000801     C                   EVAL      PiInt=*off
053400000801     C                   ENDIF
053500000801     C*
053600000801     C                   ENDSR
053700000801     C***
053800000801
053900011113
054000011113     C*----------------------------------------------------*
054100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054200011113     C*----------------------------------------------------*
054300011113     C     CHKIMPDIV     BEGSR
054400011113     C*
054500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054600011113     C                   Z-ADD     *zeros        wrkDec            9 9
054700011113     C*
054800011113     C* Come prima cosa effettuo considerazioni sulla divisa
054900011113     C                   IF        vabIAS > *zeros
055000011113     C                   IF        vabVAS <> 'EUR'
055100011113     C                   EVAL      vabVAS =  'ITL'
055200011113     C                   ENDIF
055300011113     C                   ENDIF
055400011113     C*
055500011113     C                   IF        vabCAS > *zeros
055600011113     C                   IF        vabVCA <> 'EUR'
055700011113     C                   EVAL      vabVCA =  'ITL'
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C                   IF        vabVMD > *zeros
056200020305     C                   IF        vabVAD <> 'EUR'
056300011113     C                   EVAL      vabVAD =  'ITL'
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056800011113     C                   Z-ADD     vabIAS        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVAS = 'ITL'
057100011113     C                   EVAL      vabIAS = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057600011113     C                   Z-ADD     vabCAS        wrkDec
057700011113     C                   IF        wrkDec > *zeros
057800011113     C                   IF        vabVCA = 'ITL'
057900011113     C                   EVAL      vabCAS = *zeros
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058400011113     C                   Z-ADD     vabVMD        wrkDec
058500011113     C                   IF        wrkDec > *zeros
058600011113     C                   IF        vabVAD = 'ITL'
058700011113     C                   EVAL      vabVMD = *zeros
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C                   ENDSR
059200011113     C***
059300011113
059400011113
059500000801
059600000801
059700990920      /TITLE Invio dei dati al punto operativo.
059800010202     C     invio         BEGSR
059900990920     C*
060000140117     C* 1° invio EDIVAT
060100010201     C                   reset                   dscmz
060200010201     C                   move      vlrpoi        cmzdst
060300140117     C                   eval      cmzfld = 'EDIVATWR'
060400010201     C                   eval      cmzmbd = vlrhdl
060500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060600021009     C***                if        prmfir = *blanks
060700140117     C                   eval      cmzfla = 'EDIVAT0F'
060800140117     C                   eval      cmzmba = 'EDIVAT0F'
060900021009     C***                else
061000021009     C***                eval      cmzfla = prmfir
061100021009     C***                eval      cmzmba = prmfir
061200021009     C***                endif
061300010201     C                   eval      cmznrr = *zeros
061400020305     C                   move      §ctrokvt      cmznrr
061500021018     C                   eval      cmzlba = vlrfl1
061600010201     C                   call(e)   'TIS711C'
061700010201     C                   parm                    dscmz
061800010201     C                   parm      *blanks       esito
061900010205     C                   if        %error
062000010205     C                             or cmzerr = '1'
062100010205     C                             or esito  = '1'
062200010205     C                   eval      wrkesito = '3'
062300010205     C                   else
062400010201     C*
062500140117     C* 2° invio EDIVAB
062600010201     C                   reset                   dscmz
062700010201     C                   move      vlrpoi        cmzdst
062800010201     C                   eval      cmzfld = vlrfou
062900010201     C                   eval      cmzmbd = vlrhdl
063000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063100021009     C***                if        prmfir = *blanks
063200140117     C                   eval      cmzfla = 'EDIVAB0F'
063300140117     C                   eval      cmzmba = 'EDIVAB0F'
063400021009     C***                else
063500021009     C***                eval      cmzfla = prmfir
063600021009     C***                eval      cmzmba = prmfir
063700021009     C***                endif
063800010201     C                   eval      cmznrr = *zeros
063900010201     C                   move      §ctrokvb      cmznrr
064000021018     C                   eval      cmzlba = vlrfl1
064100010201     C                   call(e)   'TIS711C'
064200010201     C                   parm                    dscmz
064300010201     C                   parm      *blanks       esito
064400010201     C                   if        %error
064500010201     C                             or cmzerr = '1'
064600010201     C                             or esito  = '1'
064700010201     C                   eval      wrkesito = '3'
064800010201     C                   endif
064900010205     C                   endif
065000990920     C*
065100000613     C                   ENDSR
065200000613     C***
065300070411
065400070411     C     *pssr         BEGSR
065500070411     C*
065600070411     C                   if        %open(tivin00r)
065700070411     C                   close     tivin00r
065800070411     C                   endif
065900140117     C                   if        %open(edivabwr)
066000140117     C                   close     edivabwr
066100070411     C                   endif
066200140117     C                   if        %open(edivatwr)
066300140117     C                   close     edivatwr
066400070411     C                   endif
066500070411     C*
066600070411     C* Effettuo la chiamata al CLLE preposto
066700140117     C                   call(e)   'TITVEVTC'
066800070411     C                   parm                    parccm
066900070411     C                   parm                    parmbr
067000070411     C                   parm      '2'           paropz
067100070411     C*
067200070411     C                   eval      wrkesito = '2'
067300070411     C*
067400070411     C                   seton                                        LR
067500070411     C*
067600070411     C                   ENDSR     '*CANCL'
067700070411     C***
067800070411
067900990910
068000000613     C     *inzsr        BEGSR
068100990910     C*
068200990910     C     *entry        plist
068300990920     C                   parm                    tivlrds
068400990921     C                   parm      wrkesito      esito
068500000724     C                   parm                    prmlit
068600000710     C                   parm                    prmfir
068700000613     C*
068800000830     C* CALCOLA LA DATA CORRENTE
068900140117     C                   time                    wn14             14 0
069000140117     C                   movel     wn14          oracor            6 0          *ORA
069100100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
069200100722     C                   eval      datcor = %dec(%date() : *ISO)
069300000830     C*
069400000613     C                   ENDSR
069500000613     C***
