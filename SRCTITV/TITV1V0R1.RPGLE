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
001500060307     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500070402     D curSped         s             25    INZ(*blanks)
002600070402     D depSped         s             25    INZ(*blanks)
002700100118
002800100118     D*------------------
002900100118     D* DS REPERIMENTO NUMERATORE
003000100118     D*------------------
003100100118     D trul33ds      e ds                  inz
003200100118     D*------------------
003300100118     D* DS ARCHITETTURA
003400100118     D*------------------
003500100118     D kpjba         e ds                  inz
003600100118
003700010201
003800010201
003900090127     C*
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300000613     C*
004400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004500000613     C*
004600010202     C* Effettuo la chiamata al CLLE preposto
004700040506     C                   call(e)   'TITVVTC'
004800010202     C                   parm                    parccm
004900010202     C                   parm                    parmbr
005000010202     C                   parm      '2'           paropz
005100050201     C*
005200050201     C* Effettuo lancio TISI95 solo x chiusura
005300050201     C                   CLEAR                   TISI95DS
005400050201     C                   EVAL      I95TLA = 'C'
005500050201     C                   CALL      'TISI95R'
005600050201     C                   PARM                    TISI95DS
005700000616     C*
005800000801     C
005900010201     C                   seton                                        LR
006000990908
006100000801
006200910830     C*--------------------------------------------------------
006300040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006400910830     C*--------------------------------------------------------
006500040526     C     RWFILE        BEGSR
006600990910     C*
006700990914     C                   if        not %open(tivin00r)
006800990908     C                   open      tivin00r
006900990914     C                   endif
007000021113     C                   if        not %open(fivabwwr)
007100021113     C                   open      fivabwwr
007200990914     C                   endif
007300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007400020305     C                   exsr      prevat
007500010201     C*
007600010202     C                   if        chkcall = '0'
007700010202     C*
007800021113     C                   if        not %open(fivatwwr)
007900021113     C                   open      fivatwwr
008000010201     C                   endif
008100990910     C*
008200010201     C                   clear                   §CTROKVB          5 0
008300020305     C                   clear                   §CTROKVT          5 0
008400000801     C                   clear                   §CTRMO            5 0
008500000801     C                   clear                   §CTRNO            5 0
008600990910     C*
008700921023     C                   DO        *HIVAL
008800990913     C*
008900990915     C                   READ      tivin00r                               70
009000050627     C                   if        vindta > *blanks
009100000613     C                   add       1             rrnum
009200000801     C*
009300000801     C                   if        *in70 = *off
009400000801     C                             and
009500000801     C                             (vinflg = *blanks
009600000801     C                              or vinflg = '0'
009700000801     C                              or vinflg = '2')
009800000801     C*
009900000801     C                   clear                   vinmsg
010000000801     C                   eval      vinflg = '1'
010100070402     C*
010200070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
010300070402     C                   eval      curSped = %subst(vindta:46:25)
010400070402     C                   if        curSped <> depSped
010500070402     C                   clear                   fivab000
010600070402     C                   clear                   fivat000
010700050628     C*
010800060315     C                   exsr      impvab                                       => carico  VAB
010900090127     C*
011000060315     C                   exsr      wrivab                                       => scarico VAB
011100070402     C*
011200070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
011300100219     C***                exsr      exevata
011400070402     C*
011500070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
011600070402     C                   exsr      exevatb
011700070402     C*
011800070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
011900070402     C                   exsr      exevate
012000121017     C*
012100121017     C* Scrivo anche e-mail x avviso spedizione - tipo record "I"
012200121017     C                   exsr      exevatI
012300121017     C*
012400121017     C* Scrivo anche e-mail x avviso spedizione - tipo record "J" => dati ulteriori
012500121017     C                   exsr      exevatJ
012600160205     C*
012700160205     C* Scrivo anche SMS x avviso spedizione - tipo record "S"
012800160205     C                   exsr      exevatS
012900070402     C*
013000070402     C* Salvo il raggruppamento spedizione cliente corrente
013100070402     C                   eval      depSped = curSped
013200070402     C*
013300070402     C                   else
013400070402     C*
013500070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013600070402     C                   exsr      exevate
013700070402     C                   endif
013800000905     C*
013900000905     C                   else
014000000905     C                   eval      vinflg = '1'
014100050628     C                   endif
014200000905     C                   endif
014300000905     C*
014400000905     C  N70              update    tivin000
014500000905     C*
014600991022     C  N70              ENDdo
014700010202     C*
014800010202     C                   endif
014900990910
015000990910     C* Se non ci sono record con errori ...
015100000710     C                   if        §ctrno = 0
015200990910     C* ... restituisco esito OK.
015300990921     C                   eval      wrkesito = '0'
015400990910     C                   else
015500010201     C                   if        §ctrokvb > 0
015600990921     C                   eval      wrkesito = '1'
015700000710     C                   else
015800000710     C                   eval      wrkesito = '2'
015900990910     C                   endif
016000000710     C                   endif
016100990910     C*
016200990914     C                   if        %open(tivin00r)
016300990908     C                   close     tivin00r
016400990914     C                   endif
016500021113     C                   if        %open(fivabwwr)
016600021113     C                   close     fivabwwr
016700990914     C                   endif
016800021113     C                   if        %open(fivatwwr)
016900021113     C                   close     fivatwwr
017000010201     C                   endif
017100990910     C*
017200010201     C                   if        §ctrokvb > 0
017300000724     C                             and vlrpoi <> *zeros
017400010202     C                   exsr      invio
017500990920     C                   endif
017600990920     C*
017700910830     C                   ENDSR
017800000613     C***
017900010305
018000010305     C*----------------------------------------------------*
018100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
018200010305     C*----------------------------------------------------*
018300090127     C     WRIVAB        BEGSR
018400090127     C*
018500090127     C* Quindi scarico il buffer del file d testata
018600090127     C                   write     fivab000                                     => scarico il VAB
018700010305     C*
018800010305     C                   ENDSR
018900990920
019000000801     C*----------------------------------------------------*
019100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019200000801     C*----------------------------------------------------*
019300010201     C     INZVAR        BEGSR
019400000801     C*
019500040802     C                   Z-ADD     *zeros        Num5_0            5 0
019600040802     C                   MOVEL     '0'           FlgCAS            1
019700000801     C*
019800000801     C                   ENDSR
019900000801     C*----------------------------------------------------*
020000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020100000801     C*----------------------------------------------------*
020200000801     C     DEFCAM        BEGSR
020300070718     C*
020400070718     C* Imposto i valori di default...
020500100219     C                   Z-ADD     0262145       VABCCM
020600100219     C                   Z-ADD     0262145       VATCCM
020700100219     C                   Z-ADD     026           VABLNP
020800100219     C                   Z-ADD     026           VATLNP
020900091119     C                   Z-ADD     000           VABCTR
021000070718     C                   MOVEL     '1'           VABCBO
021100070718     C                   MOVEL     '7Q'          VABCTM
021200020619     C* ... e poi verifico se sono stati passati come parametri
021300020619     C                   IF        vlrppt > *blanks
021400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABCCM
021900020619     C                   Z-ADD     PiVal         VATCCM
022000020619     C                   ENDIF
022100040506     C                   ENDIF
022200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022400020619     C                   EXSR      CHKNUM
022500020619     C                   IF        PiInt=*on
022600020619     C                   Z-ADD     PiVal         VABLNP
022700020619     C                   Z-ADD     PiVal         VATLNP
022800040506     C                   ENDIF
022900020619     C                   ENDIF
023000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023200020619     C                   EXSR      CHKNUM
023300020619     C                   IF        PiInt=*on
023400020619     C                   Z-ADD     PiVal         VABCTR
023500040506     C                   ENDIF
023600020619     C                   ENDIF
023700020619     C                   ENDIF
023800000801     C*
023900000801     C                   ENDSR
024000000801     C*----------------------------------------------------*
024100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024200000801     C*----------------------------------------------------*
024300040823     C     IMPVAB        BEGSR
024400040823     C*
024500020305     C                   EXSR      INZVAR
024600020305     C                   EXSR      DEFCAM
024700010305     C*
024800000801     C                   Z-ADD     *zeros        errore            1 0
024900000830     C                   MOVEL     datcor        VABAAS
025000020305     C                   MOVEL     datcor        VATAAS
025100040526     C                   MOVE      datcor        VABMGS
025200040823     C                   MOVE(P)   vlrpoi        VABFGS
025300040823     C                   MOVE(P)   vlrpoi        VATFGS
025400050628     C*
025500070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
025600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025800020117     C* ==
025900070719     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
026000090427     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
026100070102     C                                    %trim(%subst(vindta:341:30))+' '+
026200090427     C                                    %trim(%subst(vindta:371:5)))
026300070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
026400070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
026500090511     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
026600120320     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
026700160209     C                   IF        %subst(vindta:660:2) <> *blanks
026800160205     C                   EVAL      VABCBO=%trim(%subst(vindta:660:2))
026900160209     C                   ENDIF
027000100311     C* GC1/GC2
027100100311     C                   SELECT
027200100311     C* Lunedì
027300100311     C                   WHEN      %scan('Lun:M':%subst(vindta:514:30)) > 0
027400100311     C                   IF        VABGC1 = *blanks
027500100311     C                   EVAL      VABGC1 = '1P'
027600100311     C                   ELSE
027700100311     C                   EVAL      VABGC2 = '1P'
027800100311     C                   ENDIF
027900100311     C                   WHEN      %scan('Lun:P':%subst(vindta:514:30)) > 0
028000100311     C                   IF        VABGC1 = *blanks
028100100311     C                   EVAL      VABGC1 = '1M'
028200100311     C                   ELSE
028300100311     C                   EVAL      VABGC2 = '1M'
028400100311     C                   ENDIF
028500100311     C                   WHEN      %scan('Lun:.':%subst(vindta:514:30)) > 0
028600100311     C                   IF        VABGC1 = *blanks
028700100311     C                   EVAL      VABGC1 = '1 '
028800100311     C                   ELSE
028900100311     C                   EVAL      VABGC2 = '1 '
029000100311     C                   ENDIF
029100100311     C* Martedì
029200100311     C                   WHEN      %scan('Mar:M':%subst(vindta:514:30)) > 0
029300100311     C                   IF        VABGC1 = *blanks
029400100311     C                   EVAL      VABGC1 = '2P'
029500100311     C                   ELSE
029600100311     C                   EVAL      VABGC2 = '2P'
029700100311     C                   ENDIF
029800100311     C                   WHEN      %scan('Mar:P':%subst(vindta:514:30)) > 0
029900100311     C                   IF        VABGC1 = *blanks
030000100311     C                   EVAL      VABGC1 = '2M'
030100100311     C                   ELSE
030200100311     C                   EVAL      VABGC2 = '2M'
030300100311     C                   ENDIF
030400100311     C                   WHEN      %scan('Mar:.':%subst(vindta:514:30)) > 0
030500100311     C                   IF        VABGC1 = *blanks
030600100311     C                   EVAL      VABGC1 = '2 '
030700100311     C                   ELSE
030800100311     C                   EVAL      VABGC2 = '2 '
030900100311     C                   ENDIF
031000100311     C* Mercoledì
031100100311     C                   WHEN      %scan('Mer:M':%subst(vindta:514:30)) > 0
031200100311     C                   IF        VABGC1 = *blanks
031300100311     C                   EVAL      VABGC1 = '3P'
031400100311     C                   ELSE
031500100311     C                   EVAL      VABGC2 = '3P'
031600100311     C                   ENDIF
031700100311     C                   WHEN      %scan('Mer:P':%subst(vindta:514:30)) > 0
031800100311     C                   IF        VABGC1 = *blanks
031900100311     C                   EVAL      VABGC1 = '3M'
032000100311     C                   ELSE
032100100311     C                   EVAL      VABGC2 = '3M'
032200100311     C                   ENDIF
032300100311     C                   WHEN      %scan('Mer:.':%subst(vindta:514:30)) > 0
032400100311     C                   IF        VABGC1 = *blanks
032500100311     C                   EVAL      VABGC1 = '3 '
032600100311     C                   ELSE
032700100311     C                   EVAL      VABGC2 = '3 '
032800100311     C                   ENDIF
032900100311     C* Giovedì
033000100311     C                   WHEN      %scan('Gio:M':%subst(vindta:514:30)) > 0
033100100311     C                   IF        VABGC1 = *blanks
033200100311     C                   EVAL      VABGC1 = '4P'
033300100311     C                   ELSE
033400100311     C                   EVAL      VABGC2 = '4P'
033500100311     C                   ENDIF
033600100311     C                   WHEN      %scan('Gio:P':%subst(vindta:514:30)) > 0
033700100311     C                   IF        VABGC1 = *blanks
033800100311     C                   EVAL      VABGC1 = '4M'
033900100311     C                   ELSE
034000100311     C                   EVAL      VABGC2 = '4M'
034100100311     C                   ENDIF
034200100311     C                   WHEN      %scan('Gio:.':%subst(vindta:514:30)) > 0
034300100311     C                   IF        VABGC1 = *blanks
034400100311     C                   EVAL      VABGC1 = '4 '
034500100311     C                   ELSE
034600100311     C                   EVAL      VABGC2 = '4 '
034700100311     C                   ENDIF
034800100311     C* Venerdì
034900100311     C                   WHEN      %scan('Ven:M':%subst(vindta:514:30)) > 0
035000100311     C                   IF        VABGC1 = *blanks
035100100311     C                   EVAL      VABGC1 = '5P'
035200100311     C                   ELSE
035300100311     C                   EVAL      VABGC2 = '5P'
035400100311     C                   ENDIF
035500100311     C                   WHEN      %scan('Ven:P':%subst(vindta:514:30)) > 0
035600100311     C                   IF        VABGC1 = *blanks
035700100311     C                   EVAL      VABGC1 = '5M'
035800100311     C                   ELSE
035900100311     C                   EVAL      VABGC2 = '5M'
036000100311     C                   ENDIF
036100100311     C                   WHEN      %scan('Ven:.':%subst(vindta:514:30)) > 0
036200100311     C                   IF        VABGC1 = *blanks
036300100311     C                   EVAL      VABGC1 = '5 '
036400100311     C                   ELSE
036500100311     C                   EVAL      VABGC2 = '5 '
036600100311     C                   ENDIF
036700100311     C* Sabato
036800100311     C                   WHEN      %scan('Sab:M':%subst(vindta:514:30)) > 0
036900100311     C                   IF        VABGC1 = *blanks
037000100311     C                   EVAL      VABGC1 = '6P'
037100100311     C                   ELSE
037200100311     C                   EVAL      VABGC2 = '6P'
037300100311     C                   ENDIF
037400100311     C                   WHEN      %scan('Sab:P':%subst(vindta:514:30)) > 0
037500100311     C                   IF        VABGC1 = *blanks
037600100311     C                   EVAL      VABGC1 = '6M'
037700100311     C                   ELSE
037800100311     C                   EVAL      VABGC2 = '6M'
037900100311     C                   ENDIF
038000100311     C                   WHEN      %scan('Sab:.':%subst(vindta:514:30)) > 0
038100100311     C                   IF        VABGC1 = *blanks
038200100311     C                   EVAL      VABGC1 = '6 '
038300100311     C                   ELSE
038400100311     C                   EVAL      VABGC2 = '6 '
038500100311     C                   ENDIF
038600100311     C*
038700100311     C                   ENDSL
038800100311     C*
038900070102     C                   IF        %subst(vindta:43:1) = 'N'
039000160205     C***                EVAL      VABCBO='1'
039100070102     C                   ENDIF
039200070102     C                   IF        %subst(vindta:43:1) = 'P'
039300160205     C***                EVAL      VABCBO='2'
039400070102     C                   ENDIF
039500121126     C                   IF        %subst(vindta:43:1) = '1'
039600160205     C***                EVAL      VABCBO='1'
039700121126     C                   EVAL      VABFFD='S'
039800121126     C                   ENDIF
039900121126     C                   IF        %subst(vindta:43:1) = '2'
040000160205     C***                EVAL      VABCBO='2'
040100121126     C                   EVAL      VABFFD='S'
040200121126     C                   ENDIF
040300070102     C                   IF        %subst(vindta:511:3) = 'ABM'
040400070102     C                   EVAL      VABTIC='BM'
040500070102     C                   ENDIF
040600070102     C                   IF        %subst(vindta:511:3) = 'ABS'
040700070102     C                   EVAL      VABTIC='B'
040800070102     C                   ENDIF
040900070102     C                   IF        %subst(vindta:511:3) = 'CON'
041000070102     C                   EVAL      VABTIC=*blanks
041100070102     C                   ENDIF
041200070102     C                   IF        %subst(vindta:511:3) = 'ACM'
041300070102     C                   EVAL      VABTIC='CM'
041400070102     C                   ENDIF
041500050628     C* CAP
041600070719     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
041700010201     C                   EXSR      CHKNUM
041800010201     C                   IF        PiInt=*on
041900010201     C                   Z-ADD     PiVal         Num5_0
042000040506     C                   MOVEL(P)  Num5_0        VABCAD
042100010201     C                   ELSE
042200040506     C                   ADD       1             errore
042300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
042400040506     C                             + ' ' + 'VABCAD'
042500010201     C                   ENDIF
042600040506     C* Reperisco la provincia dal CAP e dalla località
042700040526     C                   IF        VABCAD <> *blanks AND
042800040526     C                             VABPRD  = *blanks
042900040506     C                   CLEAR                   TISI95DS
043000040506     C                   EVAL      I95TCN = '3'
043100040506     C                   Z-ADD     datcor        I95DAT
043200040506     C                   EVAL      I95CAP = VABCAD
043300040506     C                   EVAL      I95LOC = VABLOD
043400050627     C                   EVAL      I95NAR = VABNZD
043500040506     C                   CALL      'TISI95R'
043600040506     C                   PARM                    TISI95DS
043700040506     C                   EVAL      VABPRD = O95PRV
043800040506     C                   ENDIF
043900060307     C* NCL
044000070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
044100060307     C                   EXSR      CHKNUM
044200060307     C                   IF        PiInt=*on
044300060307     C                   ADD       PiVal         VABNCL
044400060307     C                   ELSE
044500060307     C                   ADD       1             errore
044600060307     C                   EVAL      vinmsg = %trimr(vinmsg)
044700060307     C                             + ' ' + 'VABNCL'
044800060307     C                   ENDIF
044900040506     C* PKB
045000070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
045100010201     C                   EXSR      CHKNUM
045200010201     C                   IF        PiNum=*on
045300070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
045400060316     C                   Z-ADD     PiVal         VABPKB
045500100219     C                   IF        VABPKB < 1
045600100219     C                   EVAL      VABPKB = VABPKB + 0,1
045700100219     C                   ENDIF
045800010201     C                   ELSE
045900010201     C                   ADD       1             errore
046000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
046100010201     C                             + ' ' + 'VABPKB'
046200010201     C                   ENDIF
046300100118     C* NSP => Stacco un numeratore da AZNUM
046400100118     C                   clear                   TRUL33DS
046500100118     C                   eval      I33OPE = *zeros
046600100118     C                   eval      I33CNU = 302
046700100118     C                   eval      I33NUM = 1
046800100118     C                   movel     TRUL33DS      KPJBU
046900100118     C                   call      'TRUL33R'
047000100118     C                   parm                    KPJBA
047100100118     C                   movel     KPJBU         TRUL33DS
047200100118     C                   if        O33ERR = *zeros
047300100118     C                   z-add     O33NRF        VABNSP
047400100118     C                   z-add     O33NRF        VATNSP
047500100118     C                   else
047600100118     C                   ADD       1             errore
047700100118     C                   EVAL      vinmsg = %trimr(vinmsg)
047800100118     C                             + ' ' + 'VABNSP VATNSP'
047900100118     C                   endif
048000090511     C* RMN
048100091210     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
048200100118     C                   EVAL      PiStr=%trim(%subst(vindta:47:24))
048300090511     C                   EXSR      CHKNUM
048400090511     C                   IF        PiInt=*on
048500090511     C                   Z-ADD     PiVal         VABRMN
048600090511     C                   ELSE
048700090511     C                   ADD       1             errore
048800090511     C                   EVAL      vinmsg = %trimr(vinmsg)
048900090511     C                             + ' ' + 'VABRMN'
049000090511     C                   ENDIF
049100060307     C* CAS
049200090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
049300090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
049400090608     C                             %trim(%subst(vindta:502:9))<>*zeros
049500070102     C                   EVAL      VABVCA = 'EUR'
049600060307     C                   EVAL      FlgCAS = '1'
049700070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
049800060307     C                   EXSR      CHKNUM
049900060307     C                   IF        PiNum=*on
050000060316     C                   Z-ADD     PiVal         VABCAS
050100060307     C                   ELSE
050200060307     C                   ADD       1             errore
050300060307     C                   EVAL      vinmsg = %trimr(vinmsg)
050400060307     C                             + ' ' + 'VABCAS'
050500060307     C                   ENDIF
050600070102     C                   ENDIF
050700060316     C* IAS
050800070719     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
050900070102     C                   EVAL      VABVAS = 'EUR'
051000070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
051100060316     C                   EXSR      CHKNUM
051200060316     C                   IF        PiNum=*on
051300060316     C                   Z-ADD     PiVal         VABIAS
051400060316     C                   ELSE
051500060316     C                   ADD       1             errore
051600060316     C                   EVAL      vinmsg = %trimr(vinmsg)
051700060316     C                             + ' ' + 'VABIAS'
051800070102     C                   ENDIF
051900060316     C                   ENDIF
052000010205     C*
052100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
052200040802     C                   IF        FlgCAS <> '0'
052300070102     C                   IF        VABCBO = '1'
052400010205     C                   EVAL      VABCBO = '4'
052500160211     C                   ENDIF
052600160211     C                   IF        VABCBO = '2'
052700070102     C                   EVAL      VABCBO = '6'
052800070102     C                   ENDIF
052900010205     C                   ENDIF
053000020305     C*
053100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
053200011113     C                   EXSR      CHKIMPDIV
053300010202     C*
053400000801     C* Ebbene...
053500000801     C                   ADD       1             §CTRMO
053600010201     C                   IF        errore <> *zeros
053700000801     C                   ADD       1             §CTRNO
053800000801     C                   EVAL      vinflg = '2'
053900000801     C                   ELSE
054000010201     C                   ADD       1             §CTROKVB
054100000801     C                   ENDIF
054200000801     C*
054300000801     C                   ENDSR
054400070102     C*----------------------------------------------------*
054500070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
054600070102     C*----------------------------------------------------*
054700070102     C     EXEVATA       BEGSR
054800070102     C*
054900070102     C                   EXSR      INZVAR
055000070102     C                   EXSR      DEFCAM
055100070102     C*
055200070102     C                   EVAL      VATTRC='A'
055300090127     C                   EVAL      VATNOT = %trim(%subst(vindta:581:30))
055400070102     C                   exsr      wrivat                                       => scarico VAT
055500070102     C*
055600070102     C                   ENDSR
055700060307     C*----------------------------------------------------*
055800060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
055900060307     C*----------------------------------------------------*
056000060307     C     EXEVATB       BEGSR
056100060307     C*
056200060307     C                   EXSR      INZVAR
056300060307     C                   EXSR      DEFCAM
056400060307     C*
056500060307     C                   EVAL      VATTRC='B'
056600070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
056700060307     C                   exsr      wrivat                                       => scarico VAT
056800060307     C*
056900060307     C                   ENDSR
057000070402     C*----------------------------------------------------*
057100070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
057200070402     C*----------------------------------------------------*
057300070402     C     EXEVATE       BEGSR
057400070402     C*
057500070402     C                   EXSR      INZVAR
057600070402     C                   EXSR      DEFCAM
057700070402     C*
057800070402     C                   EVAL      VATTRC='E'
057900070402     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
058000070402     C                   exsr      wrivat                                       => scarico VAT
058100070402     C*
058200070402     C                   ENDSR
058300121017     C*----------------------------------------------------*
058400121017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
058500121017     C*----------------------------------------------------*
058600121017     C     EXEVATI       BEGSR
058700121017     C*
058800121017     C                   EXSR      INZVAR
058900121017     C                   EXSR      DEFCAM
059000121017     C*
059100121017     C                   EVAL      VATTRC='I'
059200160205     C                   EVAL      VATNOT = %trim(%subst(vindta:662:35))
059300121017     C                   exsr      wrivat                                       => scarico VAT
059400121017     C*
059500121017     C                   ENDSR
059600121017     C*----------------------------------------------------*
059700121017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
059800121017     C*----------------------------------------------------*
059900121017     C     EXEVATJ       BEGSR
060000121017     C*
060100121017     C                   EXSR      INZVAR
060200121017     C                   EXSR      DEFCAM
060300121017     C*
060400121017     C                   EVAL      VATTRC='J'
060500160205     C                   EVAL      VATNOT = %trim(%subst(vindta:662+35:15))
060600121017     C                   exsr      wrivat                                       => scarico VAT
060700121017     C*
060800121017     C                   ENDSR
060900160205     C*----------------------------------------------------*
061000160205     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "S"
061100160205     C*----------------------------------------------------*
061200160205     C     EXEVATS       BEGSR
061300160205     C*
061400160205     C                   EXSR      INZVAR
061500160205     C                   EXSR      DEFCAM
061600160205     C*
061700160205     C                   EVAL      VATTRC='S'
061800160205     C                   EVAL      VATNOT = %trim(%subst(vindta:712:15))
061900160205     C                   exsr      wrivat                                       => scarico VAT
062000160205     C*
062100160205     C                   ENDSR
062200010201     C*----------------------------------------------------*
062300040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
062400010201     C*----------------------------------------------------*
062500020305     C     WRIVAT        BEGSR
062600050628     C*
062700060223     C* Scrivo solo se valorizzato qualcosa
062800060223     C                   IF        VATNOT <> *blanks
062900040802     C                   WRITE     FIVAT000
063000060223     C                   ENDIF
063100010201     C*
063200010201     C                   ENDSR
063300010202     C*----------------------------------------------------*
063400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
063500010202     C*----------------------------------------------------*
063600020305     C     PREVAT        BEGSR
063700010202     C*
063800021113     C* Compongo il nome del membro da dare al FIVATWWR
063900010202     C                   eval      parmbr = vlrhdl
064000010202     C                   movel     'M'           parmbr
064100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
064200010202     C                   eval      paropz = '1'
064300010202     C* Effettuo la chiamata al CLLE preposto
064400040506     C                   call(e)   'TITVVTC'
064500010202     C                   parm                    parccm
064600010202     C                   parm                    parmbr
064700010202     C                   parm                    paropz
064800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
064900010202     C                   if        %error
065000010202     C                   movel     '1'           chkcall
065100010202     C                   else
065200010202     C                   movel     '0'           chkcall
065300010202     C                   endif
065400010202     C*
065500010202     C                   ENDSR
065600000801     C*----------------------------------------------------*
065700000801     C*  CONTROLLO NUMERICITA' CAMPI
065800000801     C*----------------------------------------------------*
065900000801     C     CHKNUM        BEGSR
066000000801     C*
066100000801     C                   call(e)   'ISNUMERIC'
066200000801     C                   PARM                    PiStr            30
066300070102     C                   PARM      '.'           PiDecChr          1
066400000801     C                   PARM      *ZEROS        PiVal            30 9
066500000801     C                   PARM      '0'           PiInt             1
066600000801     C                   PARM      '0'           PiNum             1
066700000801     C                   IF        %error
066800000801     C                   EVAL      PiInt=*off
066900000801     C                   ENDIF
067000000801     C*
067100000801     C                   ENDSR
067200000801     C***
067300000801
067400011113
067500011113     C*----------------------------------------------------*
067600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
067700011113     C*----------------------------------------------------*
067800011113     C     CHKIMPDIV     BEGSR
067900011113     C*
068000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
068100011113     C                   Z-ADD     *zeros        wrkDec            9 9
068200011113     C*
068300011113     C* Come prima cosa effettuo considerazioni sulla divisa
068400011113     C                   IF        vabIAS > *zeros
068500011113     C                   IF        vabVAS <> 'EUR'
068600011113     C                   EVAL      vabVAS =  'ITL'
068700011113     C                   ENDIF
068800011113     C                   ENDIF
068900011113     C*
069000011113     C                   IF        vabCAS > *zeros
069100011113     C                   IF        vabVCA <> 'EUR'
069200011113     C                   EVAL      vabVCA =  'ITL'
069300011113     C                   ENDIF
069400011113     C                   ENDIF
069500011113     C*
069600011113     C                   IF        vabVMD > *zeros
069700020305     C                   IF        vabVAD <> 'EUR'
069800011113     C                   EVAL      vabVAD =  'ITL'
069900011113     C                   ENDIF
070000011113     C                   ENDIF
070100011113     C*
070200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
070300011113     C                   Z-ADD     vabIAS        wrkDec
070400011113     C                   IF        wrkDec > *zeros
070500011113     C                   IF        vabVAS = 'ITL'
070600011113     C                   EVAL      vabIAS = *zeros
070700011113     C                   ENDIF
070800011113     C                   ENDIF
070900011113     C*
071000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
071100011113     C                   Z-ADD     vabCAS        wrkDec
071200011113     C                   IF        wrkDec > *zeros
071300011113     C                   IF        vabVCA = 'ITL'
071400011113     C                   EVAL      vabCAS = *zeros
071500011113     C                   ENDIF
071600011113     C                   ENDIF
071700011113     C*
071800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
071900011113     C                   Z-ADD     vabVMD        wrkDec
072000011113     C                   IF        wrkDec > *zeros
072100011113     C                   IF        vabVAD = 'ITL'
072200011113     C                   EVAL      vabVMD = *zeros
072300011113     C                   ENDIF
072400011113     C                   ENDIF
072500011113     C*
072600011113     C                   ENDSR
072700011113     C***
072800011113
072900011113
073000000801
073100000801
073200990920      /TITLE Invio dei dati al punto operativo.
073300010202     C     invio         BEGSR
073400990920     C*
073500021113     C* 1° invio FIVAT
073600010201     C                   reset                   dscmz
073700010201     C                   move      vlrpoi        cmzdst
073800021113     C                   eval      cmzfld = 'FIVATWWR'
073900010201     C                   eval      cmzmbd = vlrhdl
074000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074100021009     C***                if        prmfir = *blanks
074200021113     C                   eval      cmzfla = 'FIVAT00F'
074300021113     C                   eval      cmzmba = 'FIVAT00F'
074400021009     C***                else
074500021009     C***                eval      cmzfla = prmfir
074600021009     C***                eval      cmzmba = prmfir
074700021009     C***                endif
074800010201     C                   eval      cmznrr = *zeros
074900020305     C                   move      §ctrokvt      cmznrr
075000021018     C                   eval      cmzlba = vlrfl1
075100010201     C                   call(e)   'TIS711C'
075200010201     C                   parm                    dscmz
075300010201     C                   parm      *blanks       esito
075400010205     C                   if        %error
075500010205     C                             or cmzerr = '1'
075600010205     C                             or esito  = '1'
075700010205     C                   eval      wrkesito = '3'
075800010205     C                   else
075900010201     C*
076000021113     C* 2° invio FIVAB
076100010201     C                   reset                   dscmz
076200010201     C                   move      vlrpoi        cmzdst
076300010201     C                   eval      cmzfld = vlrfou
076400010201     C                   eval      cmzmbd = vlrhdl
076500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076600021009     C***                if        prmfir = *blanks
076700021113     C                   eval      cmzfla = 'FIVAB00F'
076800021113     C                   eval      cmzmba = 'FIVAB00F'
076900021009     C***                else
077000021009     C***                eval      cmzfla = prmfir
077100021009     C***                eval      cmzmba = prmfir
077200021009     C***                endif
077300010201     C                   eval      cmznrr = *zeros
077400010201     C                   move      §ctrokvb      cmznrr
077500021018     C                   eval      cmzlba = vlrfl1
077600010201     C                   call(e)   'TIS711C'
077700010201     C                   parm                    dscmz
077800010201     C                   parm      *blanks       esito
077900010201     C                   if        %error
078000010201     C                             or cmzerr = '1'
078100010201     C                             or esito  = '1'
078200010201     C                   eval      wrkesito = '3'
078300010201     C                   endif
078400010205     C                   endif
078500990920     C*
078600000613     C                   ENDSR
078700000613     C***
078800990910
078900000613     C     *inzsr        BEGSR
079000990910     C*
079100990910     C     *entry        plist
079200990920     C                   parm                    tivlrds
079300990921     C                   parm      wrkesito      esito
079400000724     C                   parm                    prmlit
079500000710     C                   parm                    prmfir
079600000613     C*
079700000830     C* CALCOLA LA DATA CORRENTE
079800100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079900100118     C                   eval      datcor = %dec(%date() : *ISO)
080000000830     C*
080100000613     C                   ENDSR
080200000613     C***
