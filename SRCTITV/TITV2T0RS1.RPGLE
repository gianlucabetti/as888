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
002600140922     D depspe          s             17    INZ(*blanks)
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
005300140922     C                   call(e)   'TITVEVTC'
005400140922     C                   parm                    parccm
005500140922     C                   parm                    parmbr
005600140922     C                   parm      '2'           paropz
005700000616     C*
005800010201     C                   seton                                        LR
005900990908
006000000801
006100910830     C*--------------------------------------------------------
006200140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006300910830     C*--------------------------------------------------------
006400040526     C     RWFILE        BEGSR
006500990910     C*
006600990914     C                   if        not %open(tivin00r)
006700990908     C                   open      tivin00r
006800990914     C                   endif
006900140117     C                   if        not %open(edivabwr)
007000140117     C                   open      edivabwr
007100990914     C                   endif
007200140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007300020305     C                   exsr      prevat
007400010201     C*
007500010202     C                   if        chkcall = '0'
007600010202     C*
007700140117     C                   if        not %open(edivatwr)
007800140117     C                   open      edivatwr
007900010201     C                   endif
008000990910     C*
008100010201     C                   clear                   §CTROKVB          5 0
008200020305     C                   clear                   §CTROKVT          5 0
008300000801     C                   clear                   §CTRMO            5 0
008400000801     C                   clear                   §CTRNO            5 0
008500100729     C*
008600100729     C                   z-add     1             wGiro             1 0
008700140117     C*
008800140902     C                   if        *in53
008900140117     C                   call      'TIS7P2SER'
009000140117     C                   parm      'O'           pIn_Opz           1
009100140117     C                   parm                    tivlrds
009200140117     C                   parm                    EDIVABDS
009300140117     C                   parm      *blanks       pIn_CdIdAz        2
009400140117     C                   parm      *blanks       pIn_MaskPDF      50
009500140117     C                   parm      *blanks       pOut_Esito        1
009600140902     C                   endif
009700130708     C*
009800130708     C                   SETOFF                                       31
009900921023     C                   DO        *HIVAL
010000990913     C*
010100990915     C                   READ      tivin00r                               70
010200040910     C                   if        vindta > *blanks
010300000613     C                   add       1             rrnum
010400000801     C*
010500000801     C                   if        *in70 = *off
010600000801     C                             and
010700000801     C                             (vinflg = *blanks
010800000801     C                              or vinflg = '0'
010900000801     C                              or vinflg = '2')
011000000801     C*
011100000801     C                   clear                   vinmsg
011200000801     C                   eval      vinflg = '1'
011300040910     C*
011400040910     C* Eseguo routine d traduzione
011500040910     C                   exsr      impvabvat
011600040802     C*
011700010305     C                   endif
011800000905     C*
011900000905     C                   else
012000000905     C                   eval      vinflg = '1'
012100000905     C                   endif
012200000905     C*
012300000905     C  N70              update    tivin000
012400000905     C*
012500991022     C  N70              ENDdo
012600100722     C*
012700100722     C* Scarico i buffer testata ancora "in canna"
012800140922     C*
012900140922     C  N31              exsr      CHKWRI
013000140117     C  N31              WRITE     EDIVAB00
013100130708     C                   SETOFF                                       31
013200140117     C*
013300140902     C                   if        *in53
013400140117     C                   call      'TIS7P2SER'
013500140117     C                   parm      'C'           pIn_Opz           1
013600140117     C                   parm                    tivlrds
013700140117     C                   parm                    EDIVABDS
013800140117     C                   parm      *blanks       pIn_CdIdAz        2
013900140117     C                   parm      *blanks       pIn_MaskPDF      50
014000140117     C                   parm      *blanks       pOut_Esito        1
014100140902     C                   endif
014200010202     C*
014300010202     C                   endif
014400990910
014500990910     C* Se non ci sono record con errori ...
014600000710     C                   if        §ctrno = 0
014700990910     C* ... restituisco esito OK.
014800990921     C                   eval      wrkesito = '0'
014900990910     C                   else
015000010201     C                   if        §ctrokvb > 0
015100990921     C                   eval      wrkesito = '1'
015200000710     C                   else
015300000710     C                   eval      wrkesito = '2'
015400990910     C                   endif
015500000710     C                   endif
015600990910     C*
015700990914     C                   if        %open(tivin00r)
015800990908     C                   close     tivin00r
015900990914     C                   endif
016000140117     C                   if        %open(edivabwr)
016100140117     C                   close     edivabwr
016200990914     C                   endif
016300140117     C                   if        %open(edivatwr)
016400140117     C                   close     edivatwr
016500010201     C                   endif
016600990910     C*
016700010201     C                   if        §ctrokvb > 0
016800000724     C                             and vlrpoi <> *zeros
016900010202     C                   exsr      invio
017000990920     C                   endif
017100990920     C*
017200910830     C                   ENDSR
017300000613     C***
017400140922
017500140922     C*----------------------------------------------------*
017600140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
017700140922     C*----------------------------------------------------*
017800140922     C     CHKWRI        BEGSR
017900140922     C*
018000140922     C                   if        wRMN_2 > *zeros
018100140922     C                   eval      VABRMN = wRMN_2
018200140922     C                   eval      VABRMA = wRMA_2
018300140922     C                   else
018400140922     C                   eval      VABRMN = wRMN_1
018500140922     C                   eval      VABRMA = wRMA_1
018600140922     C                   endif
018700140922     C*
018800140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
018900140922     C                   IF        FlgCAS <> '0'
019000140922     C                   IF        VABCBO = '1'
019100140922     C                   EVAL      VABCBO = '4'
019200140922     C                   ELSE
019300140922     C                   EVAL      VABCBO = '6'
019400140922     C                   ENDIF
019500140922     C                   ENDIF
019600140922     C*
019700140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
019800140922     C                   EXSR      CHKIMPDIV
019900140922     C*
020000140922     C                   ENDSR
020100990920
020200000801     C*----------------------------------------------------*
020300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020400000801     C*----------------------------------------------------*
020500010201     C     INZVAR        BEGSR
020600000801     C*
020700040802     C                   Z-ADD     *zeros        Num5_0            5 0
020800040802     C                   MOVEL     '0'           FlgCAS            1
020900000801     C*
021000000801     C                   ENDSR
021100000801     C*----------------------------------------------------*
021200040910     C*  IMPOSTAZIONE CAMPI COSTANTI
021300000801     C*----------------------------------------------------*
021400000801     C     DEFCAM        BEGSR
021500000801     C*
021600140117     C                   CLEAR                   EDIVAB00
021700140117     C                   CLEAR                   EDIVAT00
021800020619     C* Imposto i valori di default...
021900140922     C                   Z-ADD     1662533       VABCCM
022000140922     C                   Z-ADD     1662533       VATCCM
022100140922     C                   Z-ADD     166           VABLNP
022200140922     C                   Z-ADD     166           VATLNP
022300140922     C                   Z-ADD     006           VABCTR
022400140117     C                   MOVEL     '7T'          VABCTM
022500040823     C                   MOVEL     '1'           VABCBO
022600020619     C* ... e poi verifico se sono stati passati come parametri
022700020619     C                   IF        vlrppt > *blanks
022800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023000020619     C                   EXSR      CHKNUM
023100020619     C                   IF        PiInt=*on
023200020619     C                   Z-ADD     PiVal         VABCCM
023300020619     C                   Z-ADD     PiVal         VATCCM
023400020619     C                   ENDIF
023500040506     C                   ENDIF
023600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023800020619     C                   EXSR      CHKNUM
023900020619     C                   IF        PiInt=*on
024000020619     C                   Z-ADD     PiVal         VABLNP
024100020619     C                   Z-ADD     PiVal         VATLNP
024200040506     C                   ENDIF
024300020619     C                   ENDIF
024400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024600020619     C                   EXSR      CHKNUM
024700020619     C                   IF        PiInt=*on
024800020619     C                   Z-ADD     PiVal         VABCTR
024900040506     C                   ENDIF
025000020619     C                   ENDIF
025100060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
025200060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
025300060202     C                   ENDIF
025400140902     C                   IF        %subst(vlrppt:16:1) = 'S'
025500140902     C                   SETON                                        53
025600140902     C                   ELSE
025700140902     C                   SETOFF                                       53
025800140902     C                   ENDIF
025900020619     C                   ENDIF
026000000801     C*
026100000801     C                   ENDSR
026200000801     C*----------------------------------------------------*
026300140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
026400000801     C*----------------------------------------------------*
026500040910     C     IMPVABVAT     BEGSR
026600040910     C*
026700040910     C* Traduzione relativa ai tipi record del file del cliente
026800040910     C*
026900071210     C*
027000071210     C***
027100140922     C* ...tipo record 'BGM' (info CMR)
027200140922     C                   IF        %trim(%subst(vindta:1:3)) = 'BGM'
027300140903     C                   MOVEL     *blanks       savCMR           35
027400140922     C                   EVAL      savCMR = %trim(%subst(vindta:17:15)) + ' '+
027500140922     C                                      %char(datcor) + ' ' + %char(oracor)
027600130620     C                   ENDIF
027700140922     C***
027800140922     C* ...tipo record 'CNI' (rottuta di codice spedizione)
027900140922     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI' and
028000140922     C                             (wRMA_2 <> %subst(vindta:797:17) or
028100140922     C                              wGiro = 1)
028200100722     C*
028300100722     C* Se nn primo giro => scarico il buffer precedente
028400100722     C                   if        wGiro = 1
028500100722     C                   eval      wGiro = 2
028600100722     C                   else
028700140922     C*
028800140922     C  N31              exsr      CHKWRI
028900140117     C  N31              WRITE     EDIVAB00
029000130708     C                   SETOFF                                       31
029100100722     C                   endif
029200100722     C*
029300071210     C* Resetto indicatore d anomalia sul singolo record
029400071210     C                   eval      vinflg = '1'
029500071210     C* ......inizializzazioni iniziali e formati record file Bartolini
029600071210     C                   EXSR      INZVAR
029700071210     C                   EXSR      DEFCAM
029800071210     C*
029900071210     C                   Z-ADD     *zeros        errore            1 0
030000071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
030100071210     C                   MOVEL     datcor        VABAAS
030200071210     C                   MOVEL     datcor        VATAAS
030300071210     C                   MOVE      datcor        VABMGS
030400071210     C                   MOVE(P)   vlrpoi        VABFGS
030500071210     C                   MOVE(P)   vlrpoi        VATFGS
030600140903     C*
030700140903     C* Dai "fissi"
030800140903     C                   EVAL      VABCMR = savCMR
030900140903     C                   EVAL      VABDCM = datcor
031000140903     C                   EVAL      VABDTS = datcor
031100140903     C                   EVAL      VABHMS = oracor
031200140903     C                   EVAL      VABCNT = 1
031300140903     C*
031400071210     C* ......VABNSP/VATNSP
031500071210     C* NSP => Stacco un numeratore da AZNUM
031600071210     C                   clear                   TRUL33DS
031700071210     C                   eval      I33OPE = *zeros
031800071210     C                   eval      I33CNU = 302
031900071210     C                   eval      I33NUM = 1
032000071210     C                   movel     TRUL33DS      KPJBU
032100071210     C                   call      'TRUL33R'
032200071210     C                   parm                    KPJBA
032300071210     C                   movel     KPJBU         TRUL33DS
032400071210     C                   if        O33ERR = *zeros
032500071210     C                   z-add     O33NRF        VABNSP
032600071210     C                   z-add     O33NRF        VATNSP
032700071210     C                   else
032800071210     C                   Z-ADD     1             errore
032900071210     C                   EVAL      vinmsg = %trimr(vinmsg)
033000071210     C                             + ' ' + 'VABNSP VATNSP'
033100071210     C                   endif
033200140922     C*
033300140922     C                   ENDIF
033400140922     C***
033500140922     C* ...tipo record 'CNI' (info testata)
033600140922     C                   IF        %trim(%subst(vindta:1:3)) = 'CNI'
033700140922     C                   z-add     *zeros        wRMN_1           17 0
033800140922     C                   z-add     *zeros        wRMN_2           17 0
033900140922     C                   movel     *blanks       wRMA_1           17
034000140922     C                   movel     *blanks       wRMA_2           17
034100140922     C* ......VABRMN_1
034200140922     C                   EVAL      PiStr=%trim(%subst(vindta:9:35))
034300140922     C                   EXSR      CHKNUM
034400140922     C                   IF        PiInt=*on
034500140922     C                   Z-ADD     PiVal         wRMN_1
034600140922     C                   ELSE
034700140922     C                   ADD       1             errore
034800140922     C                   EVAL      vinmsg = %trimr(vinmsg)
034900140922     C                             + ' ' + 'VABRMN'
035000140922     C                   ENDIF
035100140922     C* ......VABRMA_1
035200140922     C                   EVAL      wRMA_1=%trim(%subst(vindta:710:17))
035300140922     C* ......VABNCL
035400140922     C                   EVAL      PiStr=%trim(%subst(vindta:56:5))
035500140922     C                   EXSR      CHKNUM
035600140922     C                   IF        PiInt=*on
035700140922     C                   ADD       PiVal         VABNCL
035800140922     C                   ELSE
035900140922     C                   ADD       1             errore
036000140922     C                   EVAL      vinmsg = %trimr(vinmsg)
036100140922     C                             + ' ' + 'VABNCL'
036200140922     C                   ENDIF
036300140922     C* ......VABPKB
036400140922     C                   EVAL      PiStr=%trim(%subst(vindta:66:12))
036500140922     C                   EXSR      CHKNUM
036600140922     C                   IF        PiNum=*on
036700140922     C                   EVAL      PiVal = PiVAl / 100
036800140922     C                   ADD       PiVal         VABPKB
036900140922     C                   ELSE
037000140922     C                   ADD       1             errore
037100140922     C                   EVAL      vinmsg = %trimr(vinmsg)
037200140922     C                             + ' ' + 'VABPKB'
037300140922     C                   ENDIF
037400140922     C* ......VABVLB
037500140922     C                   EVAL      PiStr=%trim(%subst(vindta:78:12))
037600140922     C                   EXSR      CHKNUM
037700140922     C                   IF        PiNum=*on
037800140922     C                   EVAL      PiVal = PiVAl / 100
037900140922     C                   ADD       PiVal         VABVLB
038000140922     C                   ELSE
038100140922     C                   ADD       1             errore
038200140922     C                   EVAL      vinmsg = %trimr(vinmsg)
038300140922     C                             + ' ' + 'VABVLB'
038400140922     C                   ENDIF
038500140117     C* ......VABCAS
038600140922     C                   IF        %subst(vindta:145:12) <> *all'0'
038700140117     C                   EVAL      FlgCAS = '1'
038800140922     C                   EVAL      VABVCA = 'EUR'
038900140922     C                   EVAL      PiStr=%trim(%subst(vindta:145:12))
039000140117     C                   EXSR      CHKNUM
039100140117     C                   IF        PiNum=*on
039200140922     C                   EVAL      PiVal = PiVAl / 100
039300140922     C                   ADD       PiVal         VABCAS
039400140117     C                   ELSE
039500140117     C                   ADD       1             errore
039600140117     C                   EVAL      vinmsg = %trimr(vinmsg)
039700140117     C                             + ' ' + 'VABCAS'
039800140117     C                   ENDIF
039900140117     C                   ENDIF
040000140922     C* ......VABIAS
040100140922     C                   IF        %subst(vindta:157:12) <> *all'0'
040200140922     C                   EVAL      VABVAS = 'EUR'
040300140922     C                   EVAL      PiStr=%trim(%subst(vindta:157:12))
040400140922     C                   EXSR      CHKNUM
040500140922     C                   IF        PiNum=*on
040600140922     C                   EVAL      PiVal = PiVAl / 100
040700140922     C                   ADD       PiVal         VABIAS
040800140922     C                   ELSE
040900140922     C                   ADD       1             errore
041000140922     C                   EVAL      vinmsg = %trimr(vinmsg)
041100140922     C                             + ' ' + 'VABIAS'
041200140922     C                   ENDIF
041300140922     C                   ENDIF
041400140922     C* ......VABVMD
041500140922     C                   IF        %subst(vindta:169:12) <> *all'0'
041600140922     C                   EVAL      VABVAD = 'EUR'
041700140922     C                   EVAL      PiStr=%trim(%subst(vindta:169:12))
041800140922     C                   EXSR      CHKNUM
041900140922     C                   IF        PiNum=*on
042000140922     C                   EVAL      PiVal = PiVAl / 100
042100140922     C                   ADD       PiVal         VABVMD
042200140922     C                   ELSE
042300140922     C                   ADD       1             errore
042400140922     C                   EVAL      vinmsg = %trimr(vinmsg)
042500140922     C                             + ' ' + 'VABVMD'
042600140922     C                   ENDIF
042700140922     C                   ENDIF
042800140922     C* ......VABRMN_2
042900140922     C                   IF        %subst(vindta:826:17) <> *blanks
043000140922     C                   EVAL      PiStr=%trim(%subst(vindta:826:17))
043100140922     C                   EXSR      CHKNUM
043200140922     C                   IF        PiInt=*on
043300140922     C                   Z-ADD     PiVal         wRMN_2
043400140922     C                   ELSE
043500140922     C                   ADD       1             errore
043600140922     C                   EVAL      vinmsg = %trimr(vinmsg)
043700140922     C                             + ' ' + 'VABRMN'
043800140922     C                   ENDIF
043900140922     C                   ENDIF
044000140922     C* ......VABRMA_2
044100140922     C                   EVAL      wRMA_2=%trim(%subst(vindta:797:17))
044200140922     C                   if        FlgCAS = '1'
044300140922     C                   EVAL      wRMA_2 = *blanks
044400140922     C                   endif
044500140922     C* ......VATNOT_P
044600140922     C                   if        *in53
044700140922     C*
044800140922     C* Solo se valorizzato RMN
044900140922     C                   if        wRMA_1 <> *blanks
045000140922     C                   eval      pIn_MaskPDF = 'RT_' + %trim(wRMA_1) + '_'  +
045100140922     C                                           %trim(%subst(vindta:727:12)) +
045200140922     C                                           '.pdf'
045300140922     C                   call      'TIS7P2SER'
045400140922     C                   parm      'W'           pIn_Opz           1
045500140922     C                   parm                    tivlrds
045600140922     C                   parm                    EDIVABDS
045700140922     C                   parm      '10'          pIn_CdIdAz        2
045800140922     C                   parm                    pIn_MaskPDF      50
045900140922     C                   parm      *blanks       pOut_Esito        1
046000140922     C                   endif
046100140922     C                   endif
046200140117     C*
046300140117     C                   ENDIF
046400140117     C***
046500140922     C* ...tipo record 'NA2/ST' (info testata - destinatario)
046600140922     C                   IF        %trim(%subst(vindta:1:3)) = 'NA2'
046700140922     C* Solo sub-types 'ST'
046800140922     C                   IF        %trim(%subst(vindta:4:2)) = 'ST'
046900060202     C* ......VABRSD
047000140922     C                   EVAL      VABRSD=%trim(%subst(vindta:42:35))
047100130620     C* ......VABRD2
047200140922     C                   EVAL      VABRD2=%trim(%subst(vindta:77:35))
047300100714     C* ......VABIND
047400140922     C                   EVAL      VABIND=%trim(%subst(vindta:112:35))
047500130620     C* ......VABCAD
047600140922     C                   EVAL      VABCAD=%trim(%subst(vindta:226:17))
047700060202     C* ......VABLOD
047800140922     C                   EVAL      VABLOD=%trim(%subst(vindta:182:35))
047900140922     C* ......VABPRD
048000140922     C                   EVAL      VABPRD=%trim(%subst(vindta:217:9))
048100140922     C* ......VABNZD
048200140922     C                   EVAL      VABNZD=%trim(%subst(vindta:243:3))
048300130620     C                   IF        VABNZD='IT'
048400130620     C                   EVAL      VABNZD=*blanks
048500130620     C                   ENDIF
048600060202     C* ......VABPRD
048700070719     C* Reperisco la provincia dal CAP e dalla località
048800070719     C                   IF        VABPRD  = *blanks AND
048900130620     C                             VABLOD <> *blanks AND
049000130620     C                             VABCAD <> *blanks AND
049100130620     C                             VABNZD  = *blanks
049200070719     C                   CLEAR                   TISI95DS
049300070719     C                   EVAL      I95TCN = '3'
049400070719     C                   Z-ADD     datcor        I95DAT
049500070719     C                   EVAL      I95CAP = VABCAD
049600070719     C                   EVAL      I95LOC = VABLOD
049700070719     C                   CALL      'TISI95R'
049800070719     C                   PARM                    TISI95DS
049900070719     C                   EVAL      VABPRD = O95PRV
050000070719     C                   ENDIF
050100140922     C* ......VATNOT_A
050200140922     C                   EVAL      VATNOT=%trim(%subst(vindta:371:35))
050300140922     C                   IF        VATNOT <> *blanks
050400140922     C                   EVAL      VATCMR = VABCMR
050500140922     C                   EVAL      VATCNT = VABCNT
050600140922     C                   EVAL      VATTRC = 'A'
050700140922     C                   ADD       1             §CTROKVT
050800140922     C                   WRITE     EDIVAT00
050900140922     C                   ENDIF
051000140922     C* ......VATNOT_B
051100140922     C                   EVAL      VATNOT=%trim(%subst(vindta:406:20))
051200140922     C                   IF        VATNOT <> *blanks
051300140922     C                   EVAL      VATCMR = VABCMR
051400140922     C                   EVAL      VATCNT = VABCNT
051500140922     C                   EVAL      VATTRC = 'B'
051600140922     C                   ADD       1             §CTROKVT
051700140922     C                   WRITE     EDIVAT00
051800140922     C                   ENDIF
051900140117     C*
052000140117     C                   ENDIF
052100140117     C*
052200140117     C                   ENDIF
052300140922     C***
052400140922     C* ...tipo record 'GID' (dettaglio segnacolli)
052500140922     C                   IF        %trim(%subst(vindta:1:3)) = 'GID'
052600140922     C* ......VATNOT_E
052700140922     C                   EVAL      VATNOT=%trim(%subst(vindta:118:35))
052800140922     C                   IF        VATNOT <> *blanks
052900140922     C                   EVAL      VATCMR = VABCMR
053000140922     C                   EVAL      VATCNT = VABCNT
053100140922     C                   EVAL      VATTRC = 'E'
053200140922     C                   ADD       1             §CTROKVT
053300140922     C                   WRITE     EDIVAT00
053400140922     C                   ENDIF
053500140922     C*
053600140922     C                   ENDIF
053700010202     C*
053800000801     C* Ebbene...
053900000801     C                   ADD       1             §CTRMO
054000010201     C                   IF        errore <> *zeros
054100000801     C                   ADD       1             §CTRNO
054200000801     C                   EVAL      vinflg = '2'
054300000801     C                   ELSE
054400010201     C                   ADD       1             §CTROKVB
054500000801     C                   ENDIF
054600000801     C*
054700000801     C                   ENDSR
054800010202     C*----------------------------------------------------*
054900140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
055000010202     C*----------------------------------------------------*
055100020305     C     PREVAT        BEGSR
055200010202     C*
055300140117     C* Compongo il nome del membro da dare al EDIVATWR
055400010202     C                   eval      parmbr = vlrhdl
055500010202     C                   movel     'M'           parmbr
055600060113     C                   eval      parccm = vlrksc
055700010202     C                   eval      paropz = '1'
055800010202     C* Effettuo la chiamata al CLLE preposto
055900140117     C                   call(e)   'TITVEVTC'
056000010202     C                   parm                    parccm
056100010202     C                   parm                    parmbr
056200010202     C                   parm                    paropz
056300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056400010202     C                   if        %error
056500010202     C                   movel     '1'           chkcall
056600010202     C                   else
056700010202     C                   movel     '0'           chkcall
056800010202     C                   endif
056900010202     C*
057000010202     C                   ENDSR
057100000801     C*----------------------------------------------------*
057200000801     C*  CONTROLLO NUMERICITA' CAMPI
057300000801     C*----------------------------------------------------*
057400000801     C     CHKNUM        BEGSR
057500000801     C*
057600000801     C                   call(e)   'ISNUMERIC'
057700000801     C                   PARM                    PiStr            30
057800040714     C                   PARM      ','           PiDecChr          1
057900000801     C                   PARM      *ZEROS        PiVal            30 9
058000000801     C                   PARM      '0'           PiInt             1
058100000801     C                   PARM      '0'           PiNum             1
058200000801     C                   IF        %error
058300000801     C                   EVAL      PiInt=*off
058400000801     C                   ENDIF
058500000801     C*
058600000801     C                   ENDSR
058700000801     C***
058800000801
058900011113
059000011113     C*----------------------------------------------------*
059100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059200011113     C*----------------------------------------------------*
059300011113     C     CHKIMPDIV     BEGSR
059400011113     C*
059500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059600011113     C                   Z-ADD     *zeros        wrkDec            9 9
059700011113     C*
059800011113     C* Come prima cosa effettuo considerazioni sulla divisa
059900011113     C                   IF        vabIAS > *zeros
060000011113     C                   IF        vabVAS <> 'EUR'
060100011113     C                   EVAL      vabVAS =  'ITL'
060200011113     C                   ENDIF
060300011113     C                   ENDIF
060400011113     C*
060500011113     C                   IF        vabCAS > *zeros
060600011113     C                   IF        vabVCA <> 'EUR'
060700011113     C                   EVAL      vabVCA =  'ITL'
060800011113     C                   ENDIF
060900011113     C                   ENDIF
061000011113     C*
061100011113     C                   IF        vabVMD > *zeros
061200020305     C                   IF        vabVAD <> 'EUR'
061300011113     C                   EVAL      vabVAD =  'ITL'
061400011113     C                   ENDIF
061500011113     C                   ENDIF
061600011113     C*
061700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061800011113     C                   Z-ADD     vabIAS        wrkDec
061900011113     C                   IF        wrkDec > *zeros
062000011113     C                   IF        vabVAS = 'ITL'
062100011113     C                   EVAL      vabIAS = *zeros
062200011113     C                   ENDIF
062300011113     C                   ENDIF
062400011113     C*
062500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062600011113     C                   Z-ADD     vabCAS        wrkDec
062700011113     C                   IF        wrkDec > *zeros
062800011113     C                   IF        vabVCA = 'ITL'
062900011113     C                   EVAL      vabCAS = *zeros
063000011113     C                   ENDIF
063100011113     C                   ENDIF
063200011113     C*
063300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063400011113     C                   Z-ADD     vabVMD        wrkDec
063500011113     C                   IF        wrkDec > *zeros
063600011113     C                   IF        vabVAD = 'ITL'
063700011113     C                   EVAL      vabVMD = *zeros
063800011113     C                   ENDIF
063900011113     C                   ENDIF
064000011113     C*
064100011113     C                   ENDSR
064200011113     C***
064300011113
064400011113
064500000801
064600000801
064700990920      /TITLE Invio dei dati al punto operativo.
064800010202     C     invio         BEGSR
064900990920     C*
065000140117     C* 1° invio EDIVAT
065100010201     C                   reset                   dscmz
065200010201     C                   move      vlrpoi        cmzdst
065300140117     C                   eval      cmzfld = 'EDIVATWR'
065400010201     C                   eval      cmzmbd = vlrhdl
065500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065600021009     C***                if        prmfir = *blanks
065700140117     C                   eval      cmzfla = 'EDIVAT0F'
065800140117     C                   eval      cmzmba = 'EDIVAT0F'
065900021009     C***                else
066000021009     C***                eval      cmzfla = prmfir
066100021009     C***                eval      cmzmba = prmfir
066200021009     C***                endif
066300010201     C                   eval      cmznrr = *zeros
066400020305     C                   move      §ctrokvt      cmznrr
066500021018     C                   eval      cmzlba = vlrfl1
066600010201     C                   call(e)   'TIS711C'
066700010201     C                   parm                    dscmz
066800010201     C                   parm      *blanks       esito
066900010205     C                   if        %error
067000010205     C                             or cmzerr = '1'
067100010205     C                             or esito  = '1'
067200010205     C                   eval      wrkesito = '3'
067300010205     C                   else
067400010201     C*
067500140117     C* 2° invio EDIVAB
067600010201     C                   reset                   dscmz
067700010201     C                   move      vlrpoi        cmzdst
067800010201     C                   eval      cmzfld = vlrfou
067900010201     C                   eval      cmzmbd = vlrhdl
068000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068100021009     C***                if        prmfir = *blanks
068200140117     C                   eval      cmzfla = 'EDIVAB0F'
068300140117     C                   eval      cmzmba = 'EDIVAB0F'
068400021009     C***                else
068500021009     C***                eval      cmzfla = prmfir
068600021009     C***                eval      cmzmba = prmfir
068700021009     C***                endif
068800010201     C                   eval      cmznrr = *zeros
068900010201     C                   move      §ctrokvb      cmznrr
069000021018     C                   eval      cmzlba = vlrfl1
069100010201     C                   call(e)   'TIS711C'
069200010201     C                   parm                    dscmz
069300010201     C                   parm      *blanks       esito
069400010201     C                   if        %error
069500010201     C                             or cmzerr = '1'
069600010201     C                             or esito  = '1'
069700010201     C                   eval      wrkesito = '3'
069800010201     C                   endif
069900010205     C                   endif
070000990920     C*
070100000613     C                   ENDSR
070200000613     C***
070300070411
070400070411     C     *pssr         BEGSR
070500070411     C*
070600070411     C                   if        %open(tivin00r)
070700070411     C                   close     tivin00r
070800070411     C                   endif
070900140117     C                   if        %open(edivabwr)
071000140117     C                   close     edivabwr
071100070411     C                   endif
071200140117     C                   if        %open(edivatwr)
071300140117     C                   close     edivatwr
071400070411     C                   endif
071500070411     C*
071600070411     C* Effettuo la chiamata al CLLE preposto
071700140117     C                   call(e)   'TITVEVTC'
071800070411     C                   parm                    parccm
071900070411     C                   parm                    parmbr
072000070411     C                   parm      '2'           paropz
072100070411     C*
072200070411     C                   eval      wrkesito = '2'
072300070411     C*
072400070411     C                   seton                                        LR
072500070411     C*
072600070411     C                   ENDSR     '*CANCL'
072700070411     C***
072800070411
072900990910
073000000613     C     *inzsr        BEGSR
073100990910     C*
073200990910     C     *entry        plist
073300990920     C                   parm                    tivlrds
073400990921     C                   parm      wrkesito      esito
073500000724     C                   parm                    prmlit
073600000710     C                   parm                    prmfir
073700000613     C*
073800000830     C* CALCOLA LA DATA CORRENTE
073900140117     C                   time                    wn14             14 0
074000140117     C                   movel     wn14          oracor            6 0          *ORA
074100100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
074200100722     C                   eval      datcor = %dec(%date() : *ISO)
074300000830     C*
074400000613     C                   ENDSR
074500000613     C***
