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
023900170704     C*
024000170704     C                   if        vlrpoi = 999
024100170704     C                   eval      VABFGS = VABLNP
024200170704     C                   eval      VATFGS = VATLNP
024300170704     C                   else
024400170704     C                   eval      VABFGS = vlrpoi
024500170704     C                   eval      VATFGS = vlrpoi
024600170704     C                   endif
024700010305     C*
024800000801     C                   Z-ADD     *zeros        errore            1 0
024900000830     C                   MOVEL     datcor        VABAAS
025000020305     C                   MOVEL     datcor        VATAAS
025100040526     C                   MOVE      datcor        VABMGS
025200050628     C*
025300091007     C                   EVAL      VABRSD=%trim(%subst(vindta:9:35))
025400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025600020117     C* ==
025700110912     C                   EVAL      VABRD2=%trim(%subst(vindta:390:35))
025800091007     C                   EVAL      VABIND=%trim(%subst(vindta:79:35))
025900091007     C                   EVAL      VABLOD=%trim(%subst(vindta:123:35))
026000091007     C                   EVAL      VABPRD=%trim(%subst(vindta:158:2))
026100091007     C                   EVAL      VABRMA=%trim(%subst(vindta:221:15))
026200091007     C                   EVAL      VABNOT=%trim(%subst(vindta:236:35))
026300091007     C                   EVAL      VABNT2=%trim(%subst(vindta:236+35:35))
026400091007     C* FFD
026500091006     C                   SELECT
026600091007     C                   WHEN      %subst(vindta:313:1) = '1'
026700091007     C                   EVAL      VABFFD = 'S'
026800091006     C                   ENDSL
026900130715     C* GVA
027000130715     C                   SELECT
027100130715     C                   WHEN      %subst(vindta:425:1) = 'U'
027200130715     C                   EVAL      VABGVA = 'U'
027300130715     C                   ENDSL
027400091006     C* CCM
027500091007     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
027600091006     C                   EXSR      CHKNUM
027700091006     C                   IF        PiInt=*on
027800091006     C                   Z-ADD     PiVal         VABCCM
027900091119     C                   Z-ADD     PiVal         VATCCM
028000091006     C                   ELSE
028100091006     C                   ADD       1             errore
028200091006     C                   EVAL      vinmsg = %trimr(vinmsg)
028300091119     C                             + ' ' + 'VABCCM VATCCM'
028400091006     C                   ENDIF
028500060529     C* RMN
028600091007     C                   EVAL      PiStr=%trim(%subst(vindta:206:15))
028700060529     C                   EXSR      CHKNUM
028800060529     C                   IF        PiInt=*on
028900060529     C                   Z-ADD     PiVal         VABRMN
029000060529     C                   ELSE
029100060529     C                   ADD       1             errore
029200060529     C                   EVAL      vinmsg = %trimr(vinmsg)
029300060529     C                             + ' ' + 'VABRMN'
029400060529     C                   ENDIF
029500091007     C*
029600060529     C* NSP => Stacco un numeratore da AZNUM
029700060529     C                   clear                   TRUL33DS
029800060529     C                   eval      I33OPE = *zeros
029900060529     C                   eval      I33CNU = 302
030000060529     C                   eval      I33NUM = 1
030100060529     C                   movel     TRUL33DS      KPJBU
030200060529     C                   call      'TRUL33R'
030300060529     C                   parm                    KPJBA
030400060529     C                   movel     KPJBU         TRUL33DS
030500060529     C                   if        O33ERR = *zeros
030600060529     C                   z-add     O33NRF        VABNSP
030700060529     C                   z-add     O33NRF        VATNSP
030800060529     C                   else
030900060529     C                   Z-ADD     1             errore
031000060529     C                   EVAL      vinmsg = %trimr(vinmsg)
031100060529     C                             + ' ' + 'VABNSP VATNSP'
031200060529     C                   endif
031300060529     C* CAD
031400091007     C                   EVAL      PiStr=%trim(%subst(vindta:114:9))
031500010201     C                   EXSR      CHKNUM
031600010201     C                   IF        PiInt=*on
031700010201     C                   Z-ADD     PiVal         Num5_0
031800040506     C                   MOVEL(P)  Num5_0        VABCAD
031900010201     C                   ELSE
032000040506     C                   ADD       1             errore
032100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032200040506     C                             + ' ' + 'VABCAD'
032300010201     C                   ENDIF
032400091006     C*
032500040506     C* Reperisco la provincia dal CAP e dalla località
032600040526     C                   IF        VABCAD <> *blanks AND
032700040526     C                             VABPRD  = *blanks
032800040506     C                   CLEAR                   TISI95DS
032900040506     C                   EVAL      I95TCN = '3'
033000040506     C                   Z-ADD     datcor        I95DAT
033100040506     C                   EVAL      I95CAP = VABCAD
033200040506     C                   EVAL      I95LOC = VABLOD
033300050627     C                   EVAL      I95NAR = VABNZD
033400040506     C                   CALL      'TISI95R'
033500040506     C                   PARM                    TISI95DS
033600040506     C                   EVAL      VABPRD = O95PRV
033700040506     C                   ENDIF
033800091006     C* NCL
033900091007     C                   EVAL      PiStr=%trim(%subst(vindta:173:5))
034000060725     C                   EXSR      CHKNUM
034100060725     C                   IF        PiInt=*on
034200071214     C                   ADD       PiVal         VABNCL
034300060725     C                   ELSE
034400060725     C                   ADD       1             errore
034500060725     C                   EVAL      vinmsg = %trimr(vinmsg)
034600060725     C                             + ' ' + 'VABNCL'
034700060725     C                   ENDIF
034800040506     C* PKB
034900091007     C                   EVAL      PiStr=%trim(%subst(vindta:178:8))
035000010201     C                   EXSR      CHKNUM
035100010201     C                   IF        PiNum=*on
035200091007     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
035300060725     C                   Z-ADD(H)  PiVal         VABPKB
035400010201     C                   ELSE
035500010201     C                   ADD       1             errore
035600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035700010201     C                             + ' ' + 'VABPKB'
035800010201     C                   ENDIF
035900091006     C* VLB
036000091007     C                   EVAL      PiStr=%trim(%subst(vindta:186:7))
036100091006     C                   EXSR      CHKNUM
036200091006     C                   IF        PiNum=*on
036300091007     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
036400091006     C                   Z-ADD(H)  PiVal         VABVLB
036500091006     C                   ELSE
036600091006     C                   ADD       1             errore
036700091006     C                   EVAL      vinmsg = %trimr(vinmsg)
036800091006     C                             + ' ' + 'VABVLB'
036900091006     C                   ENDIF
037000060725     C* CAS
037100091007     C                   IF        %subst(vindta:193:13)<>*all'0' AND
037200091007     C                             %subst(vindta:193:13)<>*blanks
037300071207     C                   EVAL      FlgCAS='1'
037400091006     C                   EVAL      VABVCA='EUR'
037500091007     C                   EVAL      PiStr=%trim(%subst(vindta:193:13))
037600060307     C                   EXSR      CHKNUM
037700060307     C                   IF        PiNum=*on
037800091126     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
037900091126     C                   Z-ADD(H)  PiVal         VABCAS
038000060307     C                   ELSE
038100060307     C                   ADD       1             errore
038200060307     C                   EVAL      vinmsg = %trimr(vinmsg)
038300060307     C                             + ' ' + 'VABCAS'
038400060307     C                   ENDIF
038500060725     C                   ENDIF
038600091006     C* IAS
038700091007     C                   IF        %subst(vindta:160:13)<>*all'0' AND
038800091007     C                             %subst(vindta:160:13)<>*blanks
038900091006     C                   EVAL      VABVAS='EUR'
039000091007     C                   EVAL      PiStr=%trim(%subst(vindta:160:13))
039100091006     C                   EXSR      CHKNUM
039200091006     C                   IF        PiNum=*on
039300091214     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
039400091006     C                   Z-ADD     PiVal         VABIAS
039500091006     C                   ELSE
039600091006     C                   ADD       1             errore
039700091006     C                   EVAL      vinmsg = %trimr(vinmsg)
039800091006     C                             + ' ' + 'VABIAS'
039900091006     C                   ENDIF
040000091006     C                   ENDIF
040100091007     C* DCR
040200091007     C                   IF        %subst(vindta:314:8)<>*blanks
040300091007     C                   EVAL      PiStr=%subst(vindta:318:4)+
040400091007     C                                   %subst(vindta:316:2)+
040500091007     C                                   %subst(vindta:314:2)
040600091007     C                   EXSR      CHKNUM
040700091007     C                   IF        PiInt=*on
040800091007     C                   ADD       PiVal         VABDCR
040900091007     C                   ELSE
041000091007     C                   ADD       1             errore
041100091007     C                   EVAL      vinmsg = %trimr(vinmsg)
041200091007     C                             + ' ' + 'VABDCR'
041300091007     C                   ENDIF
041400091007     C                   ENDIF
041500060529     C*
041600091006     C* Gestione estensioni "VAT"
041700091006     C                   exsr      exeVATA
041800091007     C                   exsr      exeVATB
041900091127     C                   exsr      exeVATE
042000010205     C*
042100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042200040802     C                   IF        FlgCAS <> '0'
042300071207     C                   IF        VABCBO = '1'
042400071207     C                   EVAL      VABCBO = '4'
042500071207     C                   ENDIF
042600071207     C                   IF        VABCBO = '2'
042700071207     C                   EVAL      VABCBO = '6'
042800071207     C                   ENDIF
042900071207     C                   ENDIF
043000020305     C*
043100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043200011113     C                   EXSR      CHKIMPDIV
043300010202     C*
043400000801     C* Ebbene...
043500000801     C                   ADD       1             §CTRMO
043600010201     C                   IF        errore <> *zeros
043700000801     C                   ADD       1             §CTRNO
043800000801     C                   EVAL      vinflg = '2'
043900000801     C                   ELSE
044000010201     C                   ADD       1             §CTROKVB
044100000801     C                   ENDIF
044200000801     C*
044300000801     C                   ENDSR
044400050628     C*----------------------------------------------------*
044500060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044600050628     C*----------------------------------------------------*
044700071207     C     EXEVATE       BEGSR
044800071207     C*
044900091119     C                   EVAL      VATTRC = 'E'
045000091127     C*
045100121220     C                   z-add     1             wColli            5 0
045200091127     C                   z-add     *zeros        wLenRMN           2 0
045300091127     C*
045400091214     C                   movel     *blanks       wVATNOT          35
045500091214     C                   eval      wVATNOT =  %trim(%subst(vindta:206:15))
045600091216     C                   eval      wLenRMN =  %len(%trim(wVATNOT))
045700091127     C*
045800091127     C                   if        wLenRMN >= 6
045900091127     C                   dow       wColli <= VABNCL
046000091127     C                   move(p)   wColli        wColliAlfa        3
046100091214     C                   eval      VATNOT=%subst(%trim(wVATNOT):wLenRMN-6+1:6)+
046200091214     C                                    wColliAlfa
046300091119     C                   exsr      wrivat                                       => scarico VAB
046400071207     C                   add       1             §CTROKVT
046500091127     C*
046600091127     C                   add       1             wColli
046700091127     C                   enddo
046800091127     C                   endif
046900050628     C*
047000050628     C                   ENDSR
047100091006     C*----------------------------------------------------*
047200091006     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
047300091006     C*----------------------------------------------------*
047400091006     C     EXEVATA       BEGSR
047500091006     C*
047600110912     C***                EVAL      VATTRC = 'A'
047700110912     C***                EVAL      VATNOT = %subst(vindta:340:50)
047800110912     C***                IF        VATNOT <> *blanks
047900110912     C***                exsr      wrivat                                       => scarico VAB
048000110912     C***                add       1             §CTROKVT
048100110912     C***                ENDIF
048200110912     C                   EVAL      VATTRC = 'A'
048300110912     C                   EVAL      VATNOT = %subst(vindta:44:35)
048400110912     C                   IF        VATNOT <> *blanks
048500110912     C                   exsr      wrivat                                       => scarico VAB
048600110912     C                   add       1             §CTROKVT
048700110912     C                   ENDIF
048800091006     C*
048900091006     C                   ENDSR
049000091007     C*----------------------------------------------------*
049100091007     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
049200091007     C*----------------------------------------------------*
049300091007     C     EXEVATB       BEGSR
049400091007     C*
049500091007     C                   EVAL      VATTRC = 'B'
049600091007     C                   EVAL      VATNOT = %subst(vindta:330:10)
049700091007     C                   IF        VATNOT <> *blanks
049800091119     C                   exsr      wrivat                                       => scarico VAB
049900091007     C                   add       1             §CTROKVT
050000091007     C                   ENDIF
050100091007     C*
050200091007     C                   ENDSR
050300010202     C*----------------------------------------------------*
050400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050500010202     C*----------------------------------------------------*
050600020305     C     PREVAT        BEGSR
050700010202     C*
050800021113     C* Compongo il nome del membro da dare al FIVATWWR
050900010202     C                   eval      parmbr = vlrhdl
051000010202     C                   movel     'M'           parmbr
051100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
051200010202     C                   eval      paropz = '1'
051300010202     C* Effettuo la chiamata al CLLE preposto
051400040506     C                   call(e)   'TITVVTC'
051500010202     C                   parm                    parccm
051600010202     C                   parm                    parmbr
051700010202     C                   parm                    paropz
051800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051900010202     C                   if        %error
052000010202     C                   movel     '1'           chkcall
052100010202     C                   else
052200010202     C                   movel     '0'           chkcall
052300010202     C                   endif
052400010202     C*
052500010202     C                   ENDSR
052600000801     C*----------------------------------------------------*
052700000801     C*  CONTROLLO NUMERICITA' CAMPI
052800000801     C*----------------------------------------------------*
052900091006     C     CHKNUM        BEGSR
053000091006     C*
053100091006     C                   IF        PiDecChr = *blanks
053200091006     C                   EVAL      PiDecChr = ','
053300091006     C                   ENDIF
053400091006     C*
053500091006     C                   callp(e)  UBISNUM_Check(PiStr
053600091006     C                                          :PiDecChr
053700091006     C                                          :PiVal
053800091006     C                                          :PiNum
053900091006     C                                          :PiInt)
054000091006     C*
054100091006     C                   IF        %error
054200091006     C                   EVAL      PiInt=*off
054300091006     C                   ENDIF
054400000801     C*
054500000801     C                   ENDSR
054600000801     C***
054700000801
054800011113
054900011113     C*----------------------------------------------------*
055000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055100011113     C*----------------------------------------------------*
055200011113     C     CHKIMPDIV     BEGSR
055300011113     C*
055400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055500011113     C                   Z-ADD     *zeros        wrkDec            9 9
055600011113     C*
055700011113     C* Come prima cosa effettuo considerazioni sulla divisa
055800011113     C                   IF        vabIAS > *zeros
055900011113     C                   IF        vabVAS <> 'EUR'
056000011113     C                   EVAL      vabVAS =  'ITL'
056100011113     C                   ENDIF
056200011113     C                   ENDIF
056300011113     C*
056400011113     C                   IF        vabCAS > *zeros
056500011113     C                   IF        vabVCA <> 'EUR'
056600011113     C                   EVAL      vabVCA =  'ITL'
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C                   IF        vabVMD > *zeros
057100020305     C                   IF        vabVAD <> 'EUR'
057200011113     C                   EVAL      vabVAD =  'ITL'
057300011113     C                   ENDIF
057400011113     C                   ENDIF
057500011113     C*
057600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057700011113     C                   Z-ADD     vabIAS        wrkDec
057800011113     C                   IF        wrkDec > *zeros
057900011113     C                   IF        vabVAS = 'ITL'
058000011113     C                   EVAL      vabIAS = *zeros
058100011113     C                   ENDIF
058200011113     C                   ENDIF
058300011113     C*
058400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058500011113     C                   Z-ADD     vabCAS        wrkDec
058600011113     C                   IF        wrkDec > *zeros
058700011113     C                   IF        vabVCA = 'ITL'
058800011113     C                   EVAL      vabCAS = *zeros
058900011113     C                   ENDIF
059000011113     C                   ENDIF
059100011113     C*
059200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059300011113     C                   Z-ADD     vabVMD        wrkDec
059400011113     C                   IF        wrkDec > *zeros
059500011113     C                   IF        vabVAD = 'ITL'
059600011113     C                   EVAL      vabVMD = *zeros
059700011113     C                   ENDIF
059800011113     C                   ENDIF
059900011113     C*
060000011113     C                   ENDSR
060100011113     C***
060200011113
060300011113
060400000801
060500000801
060600990920      /TITLE Invio dei dati al punto operativo.
060700010202     C     invio         BEGSR
060800990920     C*
060900021113     C* 1° invio FIVAT
061000010201     C                   reset                   dscmz
061100091119     C                   move      vlrpoi        cmzdst
061200021113     C                   eval      cmzfld = 'FIVATWWR'
061300010201     C                   eval      cmzmbd = vlrhdl
061400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061500021009     C***                if        prmfir = *blanks
061600021113     C                   eval      cmzfla = 'FIVAT00F'
061700021113     C                   eval      cmzmba = 'FIVAT00F'
061800021009     C***                else
061900021009     C***                eval      cmzfla = prmfir
062000021009     C***                eval      cmzmba = prmfir
062100021009     C***                endif
062200010201     C                   eval      cmznrr = *zeros
062300020305     C                   move      §ctrokvt      cmznrr
062400021018     C                   eval      cmzlba = vlrfl1
062500010201     C                   call(e)   'TIS711C'
062600010201     C                   parm                    dscmz
062700010201     C                   parm      *blanks       esito
062800010205     C                   if        %error
062900010205     C                             or cmzerr = '1'
063000010205     C                             or esito  = '1'
063100010205     C                   eval      wrkesito = '3'
063200010205     C                   else
063300010201     C*
063400021113     C* 2° invio FIVAB
063500010201     C                   reset                   dscmz
063600091119     C                   move      vlrpoi        cmzdst
063700010201     C                   eval      cmzfld = vlrfou
063800010201     C                   eval      cmzmbd = vlrhdl
063900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064000021009     C***                if        prmfir = *blanks
064100021113     C                   eval      cmzfla = 'FIVAB00F'
064200021113     C                   eval      cmzmba = 'FIVAB00F'
064300021009     C***                else
064400021009     C***                eval      cmzfla = prmfir
064500021009     C***                eval      cmzmba = prmfir
064600021009     C***                endif
064700010201     C                   eval      cmznrr = *zeros
064800010201     C                   move      §ctrokvb      cmznrr
064900021018     C                   eval      cmzlba = vlrfl1
065000010201     C                   call(e)   'TIS711C'
065100010201     C                   parm                    dscmz
065200010201     C                   parm      *blanks       esito
065300010201     C                   if        %error
065400010201     C                             or cmzerr = '1'
065500010201     C                             or esito  = '1'
065600010201     C                   eval      wrkesito = '3'
065700010201     C                   endif
065800010205     C                   endif
065900990920     C*
066000000613     C                   ENDSR
066100000613     C***
066200070411
066300070411     C     *pssr         BEGSR
066400070411     C*
066500070411     C                   if        %open(tivin00r)
066600070411     C                   close     tivin00r
066700070411     C                   endif
066800070411     C                   if        %open(fivabwwr)
066900070411     C                   close     fivabwwr
067000070411     C                   endif
067100070411     C                   if        %open(fivatwwr)
067200070411     C                   close     fivatwwr
067300070411     C                   endif
067400070411     C*
067500070411     C* Effettuo la chiamata al CLLE preposto
067600070411     C                   call(e)   'TITVVTC'
067700070411     C                   parm                    parccm
067800070411     C                   parm                    parmbr
067900070411     C                   parm      '2'           paropz
068000070411     C*
068100070411     C                   eval      wrkesito = '2'
068200070411     C*
068300070411     C                   seton                                        LR
068400070411     C*
068500070411     C                   ENDSR     '*CANCL'
068600070411     C***
068700070411
068800990910
068900000613     C     *inzsr        BEGSR
069000990910     C*
069100990910     C     *entry        plist
069200990920     C                   parm                    tivlrds
069300990921     C                   parm      wrkesito      esito
069400000724     C                   parm                    prmlit
069500000710     C                   parm                    prmfir
069600000613     C*
069700000830     C* CALCOLA LA DATA CORRENTE
069800000830     C                   time                    wn14             14 0
069900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
070000000830     C                   z-add     wn8           g08dat
070100000830     C                   z-add     *zeros        g08inv
070200000830     C                   movel     '0'           g08err
070300000830     C                   call      'XSRDA8'
070400000830     C                   parm                    wlbda8
070500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
070600000830     C*
070700000613     C                   ENDSR
070800000613     C***
