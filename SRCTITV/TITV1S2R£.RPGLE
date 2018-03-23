000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200091006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
001500060725     D tisi95ds      e ds
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
002600000830     D*------------------
002700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800000830     D*------------------
002900000830     D WLBDA8          DS                  INZ
003000000830     D  G08DAT                 1      8  0
003100000830     D  G08INV                 9     16  0
003200000830     D  G08ERR                17     17
003300000830     D  G08TGI                18     22  0
003400000830     D*
003500990908
003600060307     D*------------------
003700060307     D* DS REPERIMENTO NUMERATORE
003800060307     D*------------------
003900060529     D trul33ds      e ds                  inz
004000060307     D*------------------
004100060307     D* DS ARCHITETTURA
004200060307     D*------------------
004300060307     D kpjba         e ds                  inz
004400060307     D*------------------
004500010201
004600091006
004700091006     D*------------------
004800091006     D* LINKING A DEFINIZIONI ESTERNE
004900091006     D*------------------
005000091006     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005100091006     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005200091006
005300010201
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000613     C*
005800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005900000613     C*
006000010202     C* Effettuo la chiamata al CLLE preposto
006100040506     C                   call(e)   'TITVVTC'
006200010202     C                   parm                    parccm
006300010202     C                   parm                    parmbr
006400010202     C                   parm      '2'           paropz
006500050201     C*
006600050201     C* Effettuo lancio TISI95 solo x chiusura
006700050201     C                   CLEAR                   TISI95DS
006800050201     C                   EVAL      I95TLA = 'C'
006900050201     C                   CALL      'TISI95R'
007000050201     C                   PARM                    TISI95DS
007100000616     C*
007200000801     C
007300010201     C                   seton                                        LR
007400990908
007500000801
007600910830     C*--------------------------------------------------------
007700040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007800910830     C*--------------------------------------------------------
007900040526     C     RWFILE        BEGSR
008000990910     C*
008100990914     C                   if        not %open(tivin00r)
008200990908     C                   open      tivin00r
008300990914     C                   endif
008400021113     C                   if        not %open(fivabwwr)
008500021113     C                   open      fivabwwr
008600990914     C                   endif
008700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008800020305     C                   exsr      prevat
008900010201     C*
009000010202     C                   if        chkcall = '0'
009100010202     C*
009200021113     C                   if        not %open(fivatwwr)
009300021113     C                   open      fivatwwr
009400010201     C                   endif
009500990910     C*
009600010201     C                   clear                   §CTROKVB          5 0
009700020305     C                   clear                   §CTROKVT          5 0
009800000801     C                   clear                   §CTRMO            5 0
009900000801     C                   clear                   §CTRNO            5 0
010000990910     C*
010100921023     C                   DO        *HIVAL
010200990913     C*
010300990915     C                   READ      tivin00r                               70
010400050627     C                   if        vindta > *blanks
010500000613     C                   add       1             rrnum
010600000801     C*
010700000801     C                   if        *in70 = *off
010800000801     C                             and
010900000801     C                             (vinflg = *blanks
011000000801     C                              or vinflg = '0'
011100000801     C                              or vinflg = '2')
011200000801     C*
011300000801     C                   clear                   vinmsg
011400000801     C                   eval      vinflg = '1'
011500010305     C*
011600050628     C                   clear                   fivab000
011700060725     C                   clear                   fivat000
011800050628     C                   exsr      impvab                                       => carico VAB
011900060725     C                   exsr      wrivab                                       => scarico VAB
012000000905     C*
012100000905     C                   else
012200000905     C                   eval      vinflg = '1'
012300050628     C                   endif
012400000905     C                   endif
012500000905     C*
012600000905     C  N70              update    tivin000
012700000905     C*
012800991022     C  N70              ENDdo
012900010202     C*
013000010202     C                   endif
013100990910
013200990910     C* Se non ci sono record con errori ...
013300000710     C                   if        §ctrno = 0
013400990910     C* ... restituisco esito OK.
013500990921     C                   eval      wrkesito = '0'
013600990910     C                   else
013700010201     C                   if        §ctrokvb > 0
013800990921     C                   eval      wrkesito = '1'
013900000710     C                   else
014000000710     C                   eval      wrkesito = '2'
014100990910     C                   endif
014200000710     C                   endif
014300990910     C*
014400990914     C                   if        %open(tivin00r)
014500990908     C                   close     tivin00r
014600990914     C                   endif
014700021113     C                   if        %open(fivabwwr)
014800021113     C                   close     fivabwwr
014900990914     C                   endif
015000021113     C                   if        %open(fivatwwr)
015100021113     C                   close     fivatwwr
015200010201     C                   endif
015300990910     C*
015400010201     C                   if        §ctrokvb > 0
015500000724     C                             and vlrpoi <> *zeros
015600010202     C                   exsr      invio
015700990920     C                   endif
015800990920     C*
015900910830     C                   ENDSR
016000000613     C***
016100010305
016200010305     C*----------------------------------------------------*
016300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
016400010305     C*----------------------------------------------------*
016500020305     C     WRIVAB        BEGSR
016600010305     C*
016700060225     C* Quindi scarico il buffer del file d testata
016800021113     C                   write     fivab000                                     => scarico il VAB
016900010305     C*
017000010305     C                   ENDSR
017100091119
017200091119     C*----------------------------------------------------*
017300091119     C*  SCARICAMENTO BUFFER RECORDS VAT
017400091119     C*----------------------------------------------------*
017500091119     C     WRIVAT        BEGSR
017600091119     C*
017700091119     C* Quindi scarico il buffer del file d dettagli
017800091119     C                   write     fivat000                                     => scarico il VAT
017900091119     C*
018000091119     C                   ENDSR
018100990920
018200000801     C*----------------------------------------------------*
018300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018400000801     C*----------------------------------------------------*
018500010201     C     INZVAR        BEGSR
018600000801     C*
018700040802     C                   Z-ADD     *zeros        Num5_0            5 0
018800040802     C                   MOVEL     '0'           FlgCAS            1
018900000801     C*
019000000801     C                   ENDSR
019100000801     C*----------------------------------------------------*
019200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019300000801     C*----------------------------------------------------*
019400000801     C     DEFCAM        BEGSR
019500000801     C*
019600020619     C* Imposto i valori di default...
019700091007     C                   Z-ADD     2493867       VABCCM
019800091007     C                   Z-ADD     2493867       VATCCM
019900091007     C                   Z-ADD     005           VABLNP
020000091007     C                   Z-ADD     005           VATLNP
020100040714     C                   Z-ADD     000           VABCTR
020200060725     C                   MOVEL     '1'           VABCBO
020300091127     C                   MOVEL     '7Q'          VABCTM
020400020619     C* ... e poi verifico se sono stati passati come parametri
020500020619     C                   IF        vlrppt > *blanks
020600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020800020619     C                   EXSR      CHKNUM
020900020619     C                   IF        PiInt=*on
021000020619     C                   Z-ADD     PiVal         VABCCM
021100020619     C                   Z-ADD     PiVal         VATCCM
021200020619     C                   ENDIF
021300040506     C                   ENDIF
021400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABLNP
021900020619     C                   Z-ADD     PiVal         VATLNP
022000040506     C                   ENDIF
022100020619     C                   ENDIF
022200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022400020619     C                   EXSR      CHKNUM
022500020619     C                   IF        PiInt=*on
022600020619     C                   Z-ADD     PiVal         VABCTR
022700040506     C                   ENDIF
022800020619     C                   ENDIF
022900020619     C                   ENDIF
023000000801     C*
023100000801     C                   ENDSR
023200000801     C*----------------------------------------------------*
023300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023400000801     C*----------------------------------------------------*
023500040823     C     IMPVAB        BEGSR
023600040823     C*
023700020305     C                   EXSR      INZVAR
023800020305     C                   EXSR      DEFCAM
023900010305     C*
024000000801     C                   Z-ADD     *zeros        errore            1 0
024100000830     C                   MOVEL     datcor        VABAAS
024200020305     C                   MOVEL     datcor        VATAAS
024300040526     C                   MOVE      datcor        VABMGS
024400050628     C*
024500091007     C                   EVAL      VABRSD=%trim(%subst(vindta:9:35))
024600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
024800020117     C* ==
024900110912     C                   EVAL      VABRD2=%trim(%subst(vindta:390:35))
025000091007     C                   EVAL      VABIND=%trim(%subst(vindta:79:35))
025100091007     C                   EVAL      VABLOD=%trim(%subst(vindta:123:35))
025200091007     C                   EVAL      VABPRD=%trim(%subst(vindta:158:2))
025300091007     C                   EVAL      VABRMA=%trim(%subst(vindta:221:15))
025400091007     C                   EVAL      VABNOT=%trim(%subst(vindta:236:35))
025500091007     C                   EVAL      VABNT2=%trim(%subst(vindta:236+35:35))
025600091007     C* FFD
025700091006     C                   SELECT
025800091007     C                   WHEN      %subst(vindta:313:1) = '1'
025900091007     C                   EVAL      VABFFD = 'S'
026000091006     C                   ENDSL
026100130715     C* GVA
026200130715     C                   SELECT
026300130715     C                   WHEN      %subst(vindta:425:1) = 'U'
026400130715     C                   EVAL      VABGVA = 'U'
026500130715     C                   ENDSL
026600091006     C* CCM
026700091007     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
026800091006     C                   EXSR      CHKNUM
026900091006     C                   IF        PiInt=*on
027000091006     C                   Z-ADD     PiVal         VABCCM
027100091119     C                   Z-ADD     PiVal         VATCCM
027200091006     C                   ELSE
027300091006     C                   ADD       1             errore
027400091006     C                   EVAL      vinmsg = %trimr(vinmsg)
027500091119     C                             + ' ' + 'VABCCM VATCCM'
027600091006     C                   ENDIF
027700060529     C* RMN
027800091007     C                   EVAL      PiStr=%trim(%subst(vindta:206:15))
027900060529     C                   EXSR      CHKNUM
028000060529     C                   IF        PiInt=*on
028100060529     C                   Z-ADD     PiVal         VABRMN
028200060529     C                   ELSE
028300060529     C                   ADD       1             errore
028400060529     C                   EVAL      vinmsg = %trimr(vinmsg)
028500060529     C                             + ' ' + 'VABRMN'
028600060529     C                   ENDIF
028700091007     C*
028800060529     C* NSP => Stacco un numeratore da AZNUM
028900060529     C                   clear                   TRUL33DS
029000060529     C                   eval      I33OPE = *zeros
029100060529     C                   eval      I33CNU = 302
029200060529     C                   eval      I33NUM = 1
029300060529     C                   movel     TRUL33DS      KPJBU
029400060529     C                   call      'TRUL33R'
029500060529     C                   parm                    KPJBA
029600060529     C                   movel     KPJBU         TRUL33DS
029700060529     C                   if        O33ERR = *zeros
029800060529     C                   z-add     O33NRF        VABNSP
029900060529     C                   z-add     O33NRF        VATNSP
030000060529     C                   else
030100060529     C                   Z-ADD     1             errore
030200060529     C                   EVAL      vinmsg = %trimr(vinmsg)
030300060529     C                             + ' ' + 'VABNSP VATNSP'
030400060529     C                   endif
030500060529     C* CAD
030600091007     C                   EVAL      PiStr=%trim(%subst(vindta:114:9))
030700010201     C                   EXSR      CHKNUM
030800010201     C                   IF        PiInt=*on
030900010201     C                   Z-ADD     PiVal         Num5_0
031000040506     C                   MOVEL(P)  Num5_0        VABCAD
031100010201     C                   ELSE
031200040506     C                   ADD       1             errore
031300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031400040506     C                             + ' ' + 'VABCAD'
031500010201     C                   ENDIF
031600091006     C*
031700040506     C* Reperisco la provincia dal CAP e dalla località
031800040526     C                   IF        VABCAD <> *blanks AND
031900040526     C                             VABPRD  = *blanks
032000040506     C                   CLEAR                   TISI95DS
032100040506     C                   EVAL      I95TCN = '3'
032200040506     C                   Z-ADD     datcor        I95DAT
032300040506     C                   EVAL      I95CAP = VABCAD
032400040506     C                   EVAL      I95LOC = VABLOD
032500050627     C                   EVAL      I95NAR = VABNZD
032600040506     C                   CALL      'TISI95R'
032700040506     C                   PARM                    TISI95DS
032800040506     C                   EVAL      VABPRD = O95PRV
032900040506     C                   ENDIF
033000091006     C* NCL
033100091007     C                   EVAL      PiStr=%trim(%subst(vindta:173:5))
033200060725     C                   EXSR      CHKNUM
033300060725     C                   IF        PiInt=*on
033400071214     C                   ADD       PiVal         VABNCL
033500060725     C                   ELSE
033600060725     C                   ADD       1             errore
033700060725     C                   EVAL      vinmsg = %trimr(vinmsg)
033800060725     C                             + ' ' + 'VABNCL'
033900060725     C                   ENDIF
034000040506     C* PKB
034100091007     C                   EVAL      PiStr=%trim(%subst(vindta:178:8))
034200010201     C                   EXSR      CHKNUM
034300010201     C                   IF        PiNum=*on
034400091007     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
034500060725     C                   Z-ADD(H)  PiVal         VABPKB
034600010201     C                   ELSE
034700010201     C                   ADD       1             errore
034800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034900010201     C                             + ' ' + 'VABPKB'
035000010201     C                   ENDIF
035100091006     C* VLB
035200091007     C                   EVAL      PiStr=%trim(%subst(vindta:186:7))
035300091006     C                   EXSR      CHKNUM
035400091006     C                   IF        PiNum=*on
035500091007     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
035600091006     C                   Z-ADD(H)  PiVal         VABVLB
035700091006     C                   ELSE
035800091006     C                   ADD       1             errore
035900091006     C                   EVAL      vinmsg = %trimr(vinmsg)
036000091006     C                             + ' ' + 'VABVLB'
036100091006     C                   ENDIF
036200060725     C* CAS
036300091007     C                   IF        %subst(vindta:193:13)<>*all'0' AND
036400091007     C                             %subst(vindta:193:13)<>*blanks
036500071207     C                   EVAL      FlgCAS='1'
036600091006     C                   EVAL      VABVCA='EUR'
036700091007     C                   EVAL      PiStr=%trim(%subst(vindta:193:13))
036800060307     C                   EXSR      CHKNUM
036900060307     C                   IF        PiNum=*on
037000091126     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
037100091126     C                   Z-ADD(H)  PiVal         VABCAS
037200060307     C                   ELSE
037300060307     C                   ADD       1             errore
037400060307     C                   EVAL      vinmsg = %trimr(vinmsg)
037500060307     C                             + ' ' + 'VABCAS'
037600060307     C                   ENDIF
037700060725     C                   ENDIF
037800091006     C* IAS
037900091007     C                   IF        %subst(vindta:160:13)<>*all'0' AND
038000091007     C                             %subst(vindta:160:13)<>*blanks
038100091006     C                   EVAL      VABVAS='EUR'
038200091007     C                   EVAL      PiStr=%trim(%subst(vindta:160:13))
038300091006     C                   EXSR      CHKNUM
038400091006     C                   IF        PiNum=*on
038500091214     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
038600091006     C                   Z-ADD     PiVal         VABIAS
038700091006     C                   ELSE
038800091006     C                   ADD       1             errore
038900091006     C                   EVAL      vinmsg = %trimr(vinmsg)
039000091006     C                             + ' ' + 'VABIAS'
039100091006     C                   ENDIF
039200091006     C                   ENDIF
039300091007     C* DCR
039400091007     C                   IF        %subst(vindta:314:8)<>*blanks
039500091007     C                   EVAL      PiStr=%subst(vindta:318:4)+
039600091007     C                                   %subst(vindta:316:2)+
039700091007     C                                   %subst(vindta:314:2)
039800091007     C                   EXSR      CHKNUM
039900091007     C                   IF        PiInt=*on
040000091007     C                   ADD       PiVal         VABDCR
040100091007     C                   ELSE
040200091007     C                   ADD       1             errore
040300091007     C                   EVAL      vinmsg = %trimr(vinmsg)
040400091007     C                             + ' ' + 'VABDCR'
040500091007     C                   ENDIF
040600091007     C                   ENDIF
040700060529     C*
040800091006     C* Gestione estensioni "VAT"
040900091006     C                   exsr      exeVATA
041000091007     C                   exsr      exeVATB
041100091127     C                   exsr      exeVATE
041200010205     C*
041300010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041400040802     C                   IF        FlgCAS <> '0'
041500071207     C                   IF        VABCBO = '1'
041600071207     C                   EVAL      VABCBO = '4'
041700071207     C                   ENDIF
041800071207     C                   IF        VABCBO = '2'
041900071207     C                   EVAL      VABCBO = '6'
042000071207     C                   ENDIF
042100071207     C                   ENDIF
042200020305     C*
042300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042400011113     C                   EXSR      CHKIMPDIV
042500010202     C*
042600000801     C* Ebbene...
042700000801     C                   ADD       1             §CTRMO
042800010201     C                   IF        errore <> *zeros
042900000801     C                   ADD       1             §CTRNO
043000000801     C                   EVAL      vinflg = '2'
043100000801     C                   ELSE
043200010201     C                   ADD       1             §CTROKVB
043300000801     C                   ENDIF
043400000801     C*
043500000801     C                   ENDSR
043600050628     C*----------------------------------------------------*
043700060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
043800050628     C*----------------------------------------------------*
043900071207     C     EXEVATE       BEGSR
044000071207     C*
044100091119     C                   EVAL      VATTRC = 'E'
044200091127     C*
044300121220     C                   z-add     1             wColli            5 0
044400091127     C                   z-add     *zeros        wLenRMN           2 0
044500091127     C*
044600091214     C                   movel     *blanks       wVATNOT          35
044700091214     C                   eval      wVATNOT =  %trim(%subst(vindta:206:15))
044800091216     C                   eval      wLenRMN =  %len(%trim(wVATNOT))
044900091127     C*
045000091127     C                   if        wLenRMN >= 6
045100091127     C                   dow       wColli <= VABNCL
045200091127     C                   move(p)   wColli        wColliAlfa        3
045300091214     C                   eval      VATNOT=%subst(%trim(wVATNOT):wLenRMN-6+1:6)+
045400091214     C                                    wColliAlfa
045500091119     C                   exsr      wrivat                                       => scarico VAB
045600071207     C                   add       1             §CTROKVT
045700091127     C*
045800091127     C                   add       1             wColli
045900091127     C                   enddo
046000091127     C                   endif
046100050628     C*
046200050628     C                   ENDSR
046300091006     C*----------------------------------------------------*
046400091006     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
046500091006     C*----------------------------------------------------*
046600091006     C     EXEVATA       BEGSR
046700091006     C*
046800110912     C***                EVAL      VATTRC = 'A'
046900110912     C***                EVAL      VATNOT = %subst(vindta:340:50)
047000110912     C***                IF        VATNOT <> *blanks
047100110912     C***                exsr      wrivat                                       => scarico VAB
047200110912     C***                add       1             §CTROKVT
047300110912     C***                ENDIF
047400110912     C                   EVAL      VATTRC = 'A'
047500110912     C                   EVAL      VATNOT = %subst(vindta:44:35)
047600110912     C                   IF        VATNOT <> *blanks
047700110912     C                   exsr      wrivat                                       => scarico VAB
047800110912     C                   add       1             §CTROKVT
047900110912     C                   ENDIF
048000091006     C*
048100091006     C                   ENDSR
048200091007     C*----------------------------------------------------*
048300091007     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
048400091007     C*----------------------------------------------------*
048500091007     C     EXEVATB       BEGSR
048600091007     C*
048700091007     C                   EVAL      VATTRC = 'B'
048800091007     C                   EVAL      VATNOT = %subst(vindta:330:10)
048900091007     C                   IF        VATNOT <> *blanks
049000091119     C                   exsr      wrivat                                       => scarico VAB
049100091007     C                   add       1             §CTROKVT
049200091007     C                   ENDIF
049300091007     C*
049400091007     C                   ENDSR
049500010202     C*----------------------------------------------------*
049600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049700010202     C*----------------------------------------------------*
049800020305     C     PREVAT        BEGSR
049900010202     C*
050000021113     C* Compongo il nome del membro da dare al FIVATWWR
050100010202     C                   eval      parmbr = vlrhdl
050200010202     C                   movel     'M'           parmbr
050300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
050400010202     C                   eval      paropz = '1'
050500010202     C* Effettuo la chiamata al CLLE preposto
050600040506     C                   call(e)   'TITVVTC'
050700010202     C                   parm                    parccm
050800010202     C                   parm                    parmbr
050900010202     C                   parm                    paropz
051000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051100010202     C                   if        %error
051200010202     C                   movel     '1'           chkcall
051300010202     C                   else
051400010202     C                   movel     '0'           chkcall
051500010202     C                   endif
051600010202     C*
051700010202     C                   ENDSR
051800000801     C*----------------------------------------------------*
051900000801     C*  CONTROLLO NUMERICITA' CAMPI
052000000801     C*----------------------------------------------------*
052100091006     C     CHKNUM        BEGSR
052200091006     C*
052300091006     C                   IF        PiDecChr = *blanks
052400091006     C                   EVAL      PiDecChr = ','
052500091006     C                   ENDIF
052600091006     C*
052700091006     C                   callp(e)  UBISNUM_Check(PiStr
052800091006     C                                          :PiDecChr
052900091006     C                                          :PiVal
053000091006     C                                          :PiNum
053100091006     C                                          :PiInt)
053200091006     C*
053300091006     C                   IF        %error
053400091006     C                   EVAL      PiInt=*off
053500091006     C                   ENDIF
053600000801     C*
053700000801     C                   ENDSR
053800000801     C***
053900000801
054000011113
054100011113     C*----------------------------------------------------*
054200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054300011113     C*----------------------------------------------------*
054400011113     C     CHKIMPDIV     BEGSR
054500011113     C*
054600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054700011113     C                   Z-ADD     *zeros        wrkDec            9 9
054800011113     C*
054900011113     C* Come prima cosa effettuo considerazioni sulla divisa
055000011113     C                   IF        vabIAS > *zeros
055100011113     C                   IF        vabVAS <> 'EUR'
055200011113     C                   EVAL      vabVAS =  'ITL'
055300011113     C                   ENDIF
055400011113     C                   ENDIF
055500011113     C*
055600011113     C                   IF        vabCAS > *zeros
055700011113     C                   IF        vabVCA <> 'EUR'
055800011113     C                   EVAL      vabVCA =  'ITL'
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C                   IF        vabVMD > *zeros
056300020305     C                   IF        vabVAD <> 'EUR'
056400011113     C                   EVAL      vabVAD =  'ITL'
056500011113     C                   ENDIF
056600011113     C                   ENDIF
056700011113     C*
056800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056900011113     C                   Z-ADD     vabIAS        wrkDec
057000011113     C                   IF        wrkDec > *zeros
057100011113     C                   IF        vabVAS = 'ITL'
057200011113     C                   EVAL      vabIAS = *zeros
057300011113     C                   ENDIF
057400011113     C                   ENDIF
057500011113     C*
057600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057700011113     C                   Z-ADD     vabCAS        wrkDec
057800011113     C                   IF        wrkDec > *zeros
057900011113     C                   IF        vabVCA = 'ITL'
058000011113     C                   EVAL      vabCAS = *zeros
058100011113     C                   ENDIF
058200011113     C                   ENDIF
058300011113     C*
058400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058500011113     C                   Z-ADD     vabVMD        wrkDec
058600011113     C                   IF        wrkDec > *zeros
058700011113     C                   IF        vabVAD = 'ITL'
058800011113     C                   EVAL      vabVMD = *zeros
058900011113     C                   ENDIF
059000011113     C                   ENDIF
059100011113     C*
059200011113     C                   ENDSR
059300011113     C***
059400011113
059500011113
059600000801
059700000801
059800990920      /TITLE Invio dei dati al punto operativo.
059900010202     C     invio         BEGSR
060000990920     C*
060100021113     C* 1° invio FIVAT
060200010201     C                   reset                   dscmz
060300091119     C                   move      vlrpoi        cmzdst
060400021113     C                   eval      cmzfld = 'FIVATWWR'
060500010201     C                   eval      cmzmbd = vlrhdl
060600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060700021009     C***                if        prmfir = *blanks
060800021113     C                   eval      cmzfla = 'FIVAT00F'
060900021113     C                   eval      cmzmba = 'FIVAT00F'
061000021009     C***                else
061100021009     C***                eval      cmzfla = prmfir
061200021009     C***                eval      cmzmba = prmfir
061300021009     C***                endif
061400010201     C                   eval      cmznrr = *zeros
061500020305     C                   move      §ctrokvt      cmznrr
061600021018     C                   eval      cmzlba = vlrfl1
061700010201     C                   call(e)   'TIS711C'
061800010201     C                   parm                    dscmz
061900010201     C                   parm      *blanks       esito
062000010205     C                   if        %error
062100010205     C                             or cmzerr = '1'
062200010205     C                             or esito  = '1'
062300010205     C                   eval      wrkesito = '3'
062400010205     C                   else
062500010201     C*
062600021113     C* 2° invio FIVAB
062700010201     C                   reset                   dscmz
062800091119     C                   move      vlrpoi        cmzdst
062900010201     C                   eval      cmzfld = vlrfou
063000010201     C                   eval      cmzmbd = vlrhdl
063100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063200021009     C***                if        prmfir = *blanks
063300021113     C                   eval      cmzfla = 'FIVAB00F'
063400021113     C                   eval      cmzmba = 'FIVAB00F'
063500021009     C***                else
063600021009     C***                eval      cmzfla = prmfir
063700021009     C***                eval      cmzmba = prmfir
063800021009     C***                endif
063900010201     C                   eval      cmznrr = *zeros
064000010201     C                   move      §ctrokvb      cmznrr
064100021018     C                   eval      cmzlba = vlrfl1
064200010201     C                   call(e)   'TIS711C'
064300010201     C                   parm                    dscmz
064400010201     C                   parm      *blanks       esito
064500010201     C                   if        %error
064600010201     C                             or cmzerr = '1'
064700010201     C                             or esito  = '1'
064800010201     C                   eval      wrkesito = '3'
064900010201     C                   endif
065000010205     C                   endif
065100990920     C*
065200000613     C                   ENDSR
065300000613     C***
065400070411
065500070411     C     *pssr         BEGSR
065600070411     C*
065700070411     C                   if        %open(tivin00r)
065800070411     C                   close     tivin00r
065900070411     C                   endif
066000070411     C                   if        %open(fivabwwr)
066100070411     C                   close     fivabwwr
066200070411     C                   endif
066300070411     C                   if        %open(fivatwwr)
066400070411     C                   close     fivatwwr
066500070411     C                   endif
066600070411     C*
066700070411     C* Effettuo la chiamata al CLLE preposto
066800070411     C                   call(e)   'TITVVTC'
066900070411     C                   parm                    parccm
067000070411     C                   parm                    parmbr
067100070411     C                   parm      '2'           paropz
067200070411     C*
067300070411     C                   eval      wrkesito = '2'
067400070411     C*
067500070411     C                   seton                                        LR
067600070411     C*
067700070411     C                   ENDSR     '*CANCL'
067800070411     C***
067900070411
068000990910
068100000613     C     *inzsr        BEGSR
068200990910     C*
068300990910     C     *entry        plist
068400990920     C                   parm                    tivlrds
068500990921     C                   parm      wrkesito      esito
068600000724     C                   parm                    prmlit
068700000710     C                   parm                    prmfir
068800000613     C*
068900000830     C* CALCOLA LA DATA CORRENTE
069000000830     C                   time                    wn14             14 0
069100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069200000830     C                   z-add     wn8           g08dat
069300000830     C                   z-add     *zeros        g08inv
069400000830     C                   movel     '0'           g08err
069500000830     C                   call      'XSRDA8'
069600000830     C                   parm                    wlbda8
069700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069800000830     C*
069900000613     C                   ENDSR
070000000613     C***
