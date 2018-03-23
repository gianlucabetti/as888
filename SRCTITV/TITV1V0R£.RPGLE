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
012600070402     C*
012700070402     C* Salvo il raggruppamento spedizione cliente corrente
012800070402     C                   eval      depSped = curSped
012900070402     C*
013000070402     C                   else
013100070402     C*
013200070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
013300070402     C                   exsr      exevate
013400070402     C                   endif
013500000905     C*
013600000905     C                   else
013700000905     C                   eval      vinflg = '1'
013800050628     C                   endif
013900000905     C                   endif
014000000905     C*
014100000905     C  N70              update    tivin000
014200000905     C*
014300991022     C  N70              ENDdo
014400010202     C*
014500010202     C                   endif
014600990910
014700990910     C* Se non ci sono record con errori ...
014800000710     C                   if        §ctrno = 0
014900990910     C* ... restituisco esito OK.
015000990921     C                   eval      wrkesito = '0'
015100990910     C                   else
015200010201     C                   if        §ctrokvb > 0
015300990921     C                   eval      wrkesito = '1'
015400000710     C                   else
015500000710     C                   eval      wrkesito = '2'
015600990910     C                   endif
015700000710     C                   endif
015800990910     C*
015900990914     C                   if        %open(tivin00r)
016000990908     C                   close     tivin00r
016100990914     C                   endif
016200021113     C                   if        %open(fivabwwr)
016300021113     C                   close     fivabwwr
016400990914     C                   endif
016500021113     C                   if        %open(fivatwwr)
016600021113     C                   close     fivatwwr
016700010201     C                   endif
016800990910     C*
016900010201     C                   if        §ctrokvb > 0
017000000724     C                             and vlrpoi <> *zeros
017100010202     C                   exsr      invio
017200990920     C                   endif
017300990920     C*
017400910830     C                   ENDSR
017500000613     C***
017600010305
017700010305     C*----------------------------------------------------*
017800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017900010305     C*----------------------------------------------------*
018000090127     C     WRIVAB        BEGSR
018100090127     C*
018200090127     C* Quindi scarico il buffer del file d testata
018300090127     C                   write     fivab000                                     => scarico il VAB
018400010305     C*
018500010305     C                   ENDSR
018600990920
018700000801     C*----------------------------------------------------*
018800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018900000801     C*----------------------------------------------------*
019000010201     C     INZVAR        BEGSR
019100000801     C*
019200040802     C                   Z-ADD     *zeros        Num5_0            5 0
019300040802     C                   MOVEL     '0'           FlgCAS            1
019400000801     C*
019500000801     C                   ENDSR
019600000801     C*----------------------------------------------------*
019700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019800000801     C*----------------------------------------------------*
019900000801     C     DEFCAM        BEGSR
020000070718     C*
020100070718     C* Imposto i valori di default...
020200100219     C                   Z-ADD     0262145       VABCCM
020300100219     C                   Z-ADD     0262145       VATCCM
020400100219     C                   Z-ADD     026           VABLNP
020500100219     C                   Z-ADD     026           VATLNP
020600091119     C                   Z-ADD     000           VABCTR
020700070718     C                   MOVEL     '1'           VABCBO
020800070718     C                   MOVEL     '7Q'          VABCTM
020900020619     C* ... e poi verifico se sono stati passati come parametri
021000020619     C                   IF        vlrppt > *blanks
021100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021300020619     C                   EXSR      CHKNUM
021400020619     C                   IF        PiInt=*on
021500020619     C                   Z-ADD     PiVal         VABCCM
021600020619     C                   Z-ADD     PiVal         VATCCM
021700020619     C                   ENDIF
021800040506     C                   ENDIF
021900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022100020619     C                   EXSR      CHKNUM
022200020619     C                   IF        PiInt=*on
022300020619     C                   Z-ADD     PiVal         VABLNP
022400020619     C                   Z-ADD     PiVal         VATLNP
022500040506     C                   ENDIF
022600020619     C                   ENDIF
022700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
022800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
022900020619     C                   EXSR      CHKNUM
023000020619     C                   IF        PiInt=*on
023100020619     C                   Z-ADD     PiVal         VABCTR
023200040506     C                   ENDIF
023300020619     C                   ENDIF
023400020619     C                   ENDIF
023500000801     C*
023600000801     C                   ENDSR
023700000801     C*----------------------------------------------------*
023800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023900000801     C*----------------------------------------------------*
024000040823     C     IMPVAB        BEGSR
024100040823     C*
024200020305     C                   EXSR      INZVAR
024300020305     C                   EXSR      DEFCAM
024400010305     C*
024500000801     C                   Z-ADD     *zeros        errore            1 0
024600000830     C                   MOVEL     datcor        VABAAS
024700020305     C                   MOVEL     datcor        VATAAS
024800040526     C                   MOVE      datcor        VABMGS
024900040823     C                   MOVE(P)   vlrpoi        VABFGS
025000040823     C                   MOVE(P)   vlrpoi        VATFGS
025100050628     C*
025200070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
025300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025500020117     C* ==
025600070719     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
025700090427     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
025800070102     C                                    %trim(%subst(vindta:341:30))+' '+
025900090427     C                                    %trim(%subst(vindta:371:5)))
026000070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
026100070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
026200090511     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
026300120320     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
026400100311     C* GC1/GC2
026500100311     C                   SELECT
026600100311     C* Lunedì
026700100311     C                   WHEN      %scan('Lun:M':%subst(vindta:514:30)) > 0
026800100311     C                   IF        VABGC1 = *blanks
026900100311     C                   EVAL      VABGC1 = '1P'
027000100311     C                   ELSE
027100100311     C                   EVAL      VABGC2 = '1P'
027200100311     C                   ENDIF
027300100311     C                   WHEN      %scan('Lun:P':%subst(vindta:514:30)) > 0
027400100311     C                   IF        VABGC1 = *blanks
027500100311     C                   EVAL      VABGC1 = '1M'
027600100311     C                   ELSE
027700100311     C                   EVAL      VABGC2 = '1M'
027800100311     C                   ENDIF
027900100311     C                   WHEN      %scan('Lun:.':%subst(vindta:514:30)) > 0
028000100311     C                   IF        VABGC1 = *blanks
028100100311     C                   EVAL      VABGC1 = '1 '
028200100311     C                   ELSE
028300100311     C                   EVAL      VABGC2 = '1 '
028400100311     C                   ENDIF
028500100311     C* Martedì
028600100311     C                   WHEN      %scan('Mar:M':%subst(vindta:514:30)) > 0
028700100311     C                   IF        VABGC1 = *blanks
028800100311     C                   EVAL      VABGC1 = '2P'
028900100311     C                   ELSE
029000100311     C                   EVAL      VABGC2 = '2P'
029100100311     C                   ENDIF
029200100311     C                   WHEN      %scan('Mar:P':%subst(vindta:514:30)) > 0
029300100311     C                   IF        VABGC1 = *blanks
029400100311     C                   EVAL      VABGC1 = '2M'
029500100311     C                   ELSE
029600100311     C                   EVAL      VABGC2 = '2M'
029700100311     C                   ENDIF
029800100311     C                   WHEN      %scan('Mar:.':%subst(vindta:514:30)) > 0
029900100311     C                   IF        VABGC1 = *blanks
030000100311     C                   EVAL      VABGC1 = '2 '
030100100311     C                   ELSE
030200100311     C                   EVAL      VABGC2 = '2 '
030300100311     C                   ENDIF
030400100311     C* Mercoledì
030500100311     C                   WHEN      %scan('Mer:M':%subst(vindta:514:30)) > 0
030600100311     C                   IF        VABGC1 = *blanks
030700100311     C                   EVAL      VABGC1 = '3P'
030800100311     C                   ELSE
030900100311     C                   EVAL      VABGC2 = '3P'
031000100311     C                   ENDIF
031100100311     C                   WHEN      %scan('Mer:P':%subst(vindta:514:30)) > 0
031200100311     C                   IF        VABGC1 = *blanks
031300100311     C                   EVAL      VABGC1 = '3M'
031400100311     C                   ELSE
031500100311     C                   EVAL      VABGC2 = '3M'
031600100311     C                   ENDIF
031700100311     C                   WHEN      %scan('Mer:.':%subst(vindta:514:30)) > 0
031800100311     C                   IF        VABGC1 = *blanks
031900100311     C                   EVAL      VABGC1 = '3 '
032000100311     C                   ELSE
032100100311     C                   EVAL      VABGC2 = '3 '
032200100311     C                   ENDIF
032300100311     C* Giovedì
032400100311     C                   WHEN      %scan('Gio:M':%subst(vindta:514:30)) > 0
032500100311     C                   IF        VABGC1 = *blanks
032600100311     C                   EVAL      VABGC1 = '4P'
032700100311     C                   ELSE
032800100311     C                   EVAL      VABGC2 = '4P'
032900100311     C                   ENDIF
033000100311     C                   WHEN      %scan('Gio:P':%subst(vindta:514:30)) > 0
033100100311     C                   IF        VABGC1 = *blanks
033200100311     C                   EVAL      VABGC1 = '4M'
033300100311     C                   ELSE
033400100311     C                   EVAL      VABGC2 = '4M'
033500100311     C                   ENDIF
033600100311     C                   WHEN      %scan('Gio:.':%subst(vindta:514:30)) > 0
033700100311     C                   IF        VABGC1 = *blanks
033800100311     C                   EVAL      VABGC1 = '4 '
033900100311     C                   ELSE
034000100311     C                   EVAL      VABGC2 = '4 '
034100100311     C                   ENDIF
034200100311     C* Venerdì
034300100311     C                   WHEN      %scan('Ven:M':%subst(vindta:514:30)) > 0
034400100311     C                   IF        VABGC1 = *blanks
034500100311     C                   EVAL      VABGC1 = '5P'
034600100311     C                   ELSE
034700100311     C                   EVAL      VABGC2 = '5P'
034800100311     C                   ENDIF
034900100311     C                   WHEN      %scan('Ven:P':%subst(vindta:514:30)) > 0
035000100311     C                   IF        VABGC1 = *blanks
035100100311     C                   EVAL      VABGC1 = '5M'
035200100311     C                   ELSE
035300100311     C                   EVAL      VABGC2 = '5M'
035400100311     C                   ENDIF
035500100311     C                   WHEN      %scan('Ven:.':%subst(vindta:514:30)) > 0
035600100311     C                   IF        VABGC1 = *blanks
035700100311     C                   EVAL      VABGC1 = '5 '
035800100311     C                   ELSE
035900100311     C                   EVAL      VABGC2 = '5 '
036000100311     C                   ENDIF
036100100311     C* Sabato
036200100311     C                   WHEN      %scan('Sab:M':%subst(vindta:514:30)) > 0
036300100311     C                   IF        VABGC1 = *blanks
036400100311     C                   EVAL      VABGC1 = '6P'
036500100311     C                   ELSE
036600100311     C                   EVAL      VABGC2 = '6P'
036700100311     C                   ENDIF
036800100311     C                   WHEN      %scan('Sab:P':%subst(vindta:514:30)) > 0
036900100311     C                   IF        VABGC1 = *blanks
037000100311     C                   EVAL      VABGC1 = '6M'
037100100311     C                   ELSE
037200100311     C                   EVAL      VABGC2 = '6M'
037300100311     C                   ENDIF
037400100311     C                   WHEN      %scan('Sab:.':%subst(vindta:514:30)) > 0
037500100311     C                   IF        VABGC1 = *blanks
037600100311     C                   EVAL      VABGC1 = '6 '
037700100311     C                   ELSE
037800100311     C                   EVAL      VABGC2 = '6 '
037900100311     C                   ENDIF
038000100311     C*
038100100311     C                   ENDSL
038200100311     C*
038300070102     C                   IF        %subst(vindta:43:1) = 'N'
038400070102     C                   EVAL      VABCBO='1'
038500070102     C                   ENDIF
038600070102     C                   IF        %subst(vindta:43:1) = 'P'
038700070102     C                   EVAL      VABCBO='2'
038800070102     C                   ENDIF
038900121126     C                   IF        %subst(vindta:43:1) = '1'
039000121126     C                   EVAL      VABCBO='1'
039100121126     C                   EVAL      VABFFD='S'
039200121126     C                   ENDIF
039300121126     C                   IF        %subst(vindta:43:1) = '2'
039400121126     C                   EVAL      VABCBO='2'
039500121126     C                   EVAL      VABFFD='S'
039600121126     C                   ENDIF
039700070102     C                   IF        %subst(vindta:511:3) = 'ABM'
039800070102     C                   EVAL      VABTIC='BM'
039900070102     C                   ENDIF
040000070102     C                   IF        %subst(vindta:511:3) = 'ABS'
040100070102     C                   EVAL      VABTIC='B'
040200070102     C                   ENDIF
040300070102     C                   IF        %subst(vindta:511:3) = 'CON'
040400070102     C                   EVAL      VABTIC=*blanks
040500070102     C                   ENDIF
040600070102     C                   IF        %subst(vindta:511:3) = 'ACM'
040700070102     C                   EVAL      VABTIC='CM'
040800070102     C                   ENDIF
040900050628     C* CAP
041000070719     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
041100010201     C                   EXSR      CHKNUM
041200010201     C                   IF        PiInt=*on
041300010201     C                   Z-ADD     PiVal         Num5_0
041400040506     C                   MOVEL(P)  Num5_0        VABCAD
041500010201     C                   ELSE
041600040506     C                   ADD       1             errore
041700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
041800040506     C                             + ' ' + 'VABCAD'
041900010201     C                   ENDIF
042000040506     C* Reperisco la provincia dal CAP e dalla località
042100040526     C                   IF        VABCAD <> *blanks AND
042200040526     C                             VABPRD  = *blanks
042300040506     C                   CLEAR                   TISI95DS
042400040506     C                   EVAL      I95TCN = '3'
042500040506     C                   Z-ADD     datcor        I95DAT
042600040506     C                   EVAL      I95CAP = VABCAD
042700040506     C                   EVAL      I95LOC = VABLOD
042800050627     C                   EVAL      I95NAR = VABNZD
042900040506     C                   CALL      'TISI95R'
043000040506     C                   PARM                    TISI95DS
043100040506     C                   EVAL      VABPRD = O95PRV
043200040506     C                   ENDIF
043300060307     C* NCL
043400070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
043500060307     C                   EXSR      CHKNUM
043600060307     C                   IF        PiInt=*on
043700060307     C                   ADD       PiVal         VABNCL
043800060307     C                   ELSE
043900060307     C                   ADD       1             errore
044000060307     C                   EVAL      vinmsg = %trimr(vinmsg)
044100060307     C                             + ' ' + 'VABNCL'
044200060307     C                   ENDIF
044300040506     C* PKB
044400070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
044500010201     C                   EXSR      CHKNUM
044600010201     C                   IF        PiNum=*on
044700070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
044800060316     C                   Z-ADD     PiVal         VABPKB
044900100219     C                   IF        VABPKB < 1
045000100219     C                   EVAL      VABPKB = VABPKB + 0,1
045100100219     C                   ENDIF
045200010201     C                   ELSE
045300010201     C                   ADD       1             errore
045400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
045500010201     C                             + ' ' + 'VABPKB'
045600010201     C                   ENDIF
045700100118     C* NSP => Stacco un numeratore da AZNUM
045800100118     C                   clear                   TRUL33DS
045900100118     C                   eval      I33OPE = *zeros
046000100118     C                   eval      I33CNU = 302
046100100118     C                   eval      I33NUM = 1
046200100118     C                   movel     TRUL33DS      KPJBU
046300100118     C                   call      'TRUL33R'
046400100118     C                   parm                    KPJBA
046500100118     C                   movel     KPJBU         TRUL33DS
046600100118     C                   if        O33ERR = *zeros
046700100118     C                   z-add     O33NRF        VABNSP
046800100118     C                   z-add     O33NRF        VATNSP
046900100118     C                   else
047000100118     C                   ADD       1             errore
047100100118     C                   EVAL      vinmsg = %trimr(vinmsg)
047200100118     C                             + ' ' + 'VABNSP VATNSP'
047300100118     C                   endif
047400090511     C* RMN
047500091210     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
047600100118     C                   EVAL      PiStr=%trim(%subst(vindta:47:24))
047700090511     C                   EXSR      CHKNUM
047800090511     C                   IF        PiInt=*on
047900090511     C                   Z-ADD     PiVal         VABRMN
048000090511     C                   ELSE
048100090511     C                   ADD       1             errore
048200090511     C                   EVAL      vinmsg = %trimr(vinmsg)
048300090511     C                             + ' ' + 'VABRMN'
048400090511     C                   ENDIF
048500060307     C* CAS
048600090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
048700090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
048800090608     C                             %trim(%subst(vindta:502:9))<>*zeros
048900070102     C                   EVAL      VABVCA = 'EUR'
049000060307     C                   EVAL      FlgCAS = '1'
049100070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
049200060307     C                   EXSR      CHKNUM
049300060307     C                   IF        PiNum=*on
049400060316     C                   Z-ADD     PiVal         VABCAS
049500060307     C                   ELSE
049600060307     C                   ADD       1             errore
049700060307     C                   EVAL      vinmsg = %trimr(vinmsg)
049800060307     C                             + ' ' + 'VABCAS'
049900060307     C                   ENDIF
050000070102     C                   ENDIF
050100060316     C* IAS
050200070719     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
050300070102     C                   EVAL      VABVAS = 'EUR'
050400070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
050500060316     C                   EXSR      CHKNUM
050600060316     C                   IF        PiNum=*on
050700060316     C                   Z-ADD     PiVal         VABIAS
050800060316     C                   ELSE
050900060316     C                   ADD       1             errore
051000060316     C                   EVAL      vinmsg = %trimr(vinmsg)
051100060316     C                             + ' ' + 'VABIAS'
051200070102     C                   ENDIF
051300060316     C                   ENDIF
051400010205     C*
051500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
051600040802     C                   IF        FlgCAS <> '0'
051700070102     C                   IF        VABCBO = '1'
051800010205     C                   EVAL      VABCBO = '4'
051900010205     C                   ELSE
052000070102     C                   EVAL      VABCBO = '6'
052100070102     C                   ENDIF
052200010205     C                   ENDIF
052300020305     C*
052400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052500011113     C                   EXSR      CHKIMPDIV
052600010202     C*
052700000801     C* Ebbene...
052800000801     C                   ADD       1             §CTRMO
052900010201     C                   IF        errore <> *zeros
053000000801     C                   ADD       1             §CTRNO
053100000801     C                   EVAL      vinflg = '2'
053200000801     C                   ELSE
053300010201     C                   ADD       1             §CTROKVB
053400000801     C                   ENDIF
053500000801     C*
053600000801     C                   ENDSR
053700070102     C*----------------------------------------------------*
053800070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
053900070102     C*----------------------------------------------------*
054000070102     C     EXEVATA       BEGSR
054100070102     C*
054200070102     C                   EXSR      INZVAR
054300070102     C                   EXSR      DEFCAM
054400070102     C*
054500070102     C                   EVAL      VATTRC='A'
054600090127     C                   EVAL      VATNOT = %trim(%subst(vindta:581:30))
054700070102     C                   exsr      wrivat                                       => scarico VAT
054800070102     C*
054900070102     C                   ENDSR
055000060307     C*----------------------------------------------------*
055100060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
055200060307     C*----------------------------------------------------*
055300060307     C     EXEVATB       BEGSR
055400060307     C*
055500060307     C                   EXSR      INZVAR
055600060307     C                   EXSR      DEFCAM
055700060307     C*
055800060307     C                   EVAL      VATTRC='B'
055900070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
056000060307     C                   exsr      wrivat                                       => scarico VAT
056100060307     C*
056200060307     C                   ENDSR
056300070402     C*----------------------------------------------------*
056400070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
056500070402     C*----------------------------------------------------*
056600070402     C     EXEVATE       BEGSR
056700070402     C*
056800070402     C                   EXSR      INZVAR
056900070402     C                   EXSR      DEFCAM
057000070402     C*
057100070402     C                   EVAL      VATTRC='E'
057200070402     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
057300070402     C                   exsr      wrivat                                       => scarico VAT
057400070402     C*
057500070402     C                   ENDSR
057600121017     C*----------------------------------------------------*
057700121017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
057800121017     C*----------------------------------------------------*
057900121017     C     EXEVATI       BEGSR
058000121017     C*
058100121017     C                   EXSR      INZVAR
058200121017     C                   EXSR      DEFCAM
058300121017     C*
058400121017     C                   EVAL      VATTRC='I'
058500121017     C                   EVAL      VATNOT = %trim(%subst(vindta:663:35))
058600121017     C                   exsr      wrivat                                       => scarico VAT
058700121017     C*
058800121017     C                   ENDSR
058900121017     C*----------------------------------------------------*
059000121017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "J"
059100121017     C*----------------------------------------------------*
059200121017     C     EXEVATJ       BEGSR
059300121017     C*
059400121017     C                   EXSR      INZVAR
059500121017     C                   EXSR      DEFCAM
059600121017     C*
059700121017     C                   EVAL      VATTRC='J'
059800121017     C                   EVAL      VATNOT = %trim(%subst(vindta:663+35:20))
059900121017     C                   exsr      wrivat                                       => scarico VAT
060000121017     C*
060100121017     C                   ENDSR
060200010201     C*----------------------------------------------------*
060300040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
060400010201     C*----------------------------------------------------*
060500020305     C     WRIVAT        BEGSR
060600050628     C*
060700060223     C* Scrivo solo se valorizzato qualcosa
060800060223     C                   IF        VATNOT <> *blanks
060900040802     C                   WRITE     FIVAT000
061000060223     C                   ENDIF
061100010201     C*
061200010201     C                   ENDSR
061300010202     C*----------------------------------------------------*
061400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
061500010202     C*----------------------------------------------------*
061600020305     C     PREVAT        BEGSR
061700010202     C*
061800021113     C* Compongo il nome del membro da dare al FIVATWWR
061900010202     C                   eval      parmbr = vlrhdl
062000010202     C                   movel     'M'           parmbr
062100050627     C                   eval      parccm = %subst(vlrKSC:2:7)
062200010202     C                   eval      paropz = '1'
062300010202     C* Effettuo la chiamata al CLLE preposto
062400040506     C                   call(e)   'TITVVTC'
062500010202     C                   parm                    parccm
062600010202     C                   parm                    parmbr
062700010202     C                   parm                    paropz
062800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062900010202     C                   if        %error
063000010202     C                   movel     '1'           chkcall
063100010202     C                   else
063200010202     C                   movel     '0'           chkcall
063300010202     C                   endif
063400010202     C*
063500010202     C                   ENDSR
063600000801     C*----------------------------------------------------*
063700000801     C*  CONTROLLO NUMERICITA' CAMPI
063800000801     C*----------------------------------------------------*
063900000801     C     CHKNUM        BEGSR
064000000801     C*
064100000801     C                   call(e)   'ISNUMERIC'
064200000801     C                   PARM                    PiStr            30
064300070102     C                   PARM      '.'           PiDecChr          1
064400000801     C                   PARM      *ZEROS        PiVal            30 9
064500000801     C                   PARM      '0'           PiInt             1
064600000801     C                   PARM      '0'           PiNum             1
064700000801     C                   IF        %error
064800000801     C                   EVAL      PiInt=*off
064900000801     C                   ENDIF
065000000801     C*
065100000801     C                   ENDSR
065200000801     C***
065300000801
065400011113
065500011113     C*----------------------------------------------------*
065600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065700011113     C*----------------------------------------------------*
065800011113     C     CHKIMPDIV     BEGSR
065900011113     C*
066000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066100011113     C                   Z-ADD     *zeros        wrkDec            9 9
066200011113     C*
066300011113     C* Come prima cosa effettuo considerazioni sulla divisa
066400011113     C                   IF        vabIAS > *zeros
066500011113     C                   IF        vabVAS <> 'EUR'
066600011113     C                   EVAL      vabVAS =  'ITL'
066700011113     C                   ENDIF
066800011113     C                   ENDIF
066900011113     C*
067000011113     C                   IF        vabCAS > *zeros
067100011113     C                   IF        vabVCA <> 'EUR'
067200011113     C                   EVAL      vabVCA =  'ITL'
067300011113     C                   ENDIF
067400011113     C                   ENDIF
067500011113     C*
067600011113     C                   IF        vabVMD > *zeros
067700020305     C                   IF        vabVAD <> 'EUR'
067800011113     C                   EVAL      vabVAD =  'ITL'
067900011113     C                   ENDIF
068000011113     C                   ENDIF
068100011113     C*
068200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068300011113     C                   Z-ADD     vabIAS        wrkDec
068400011113     C                   IF        wrkDec > *zeros
068500011113     C                   IF        vabVAS = 'ITL'
068600011113     C                   EVAL      vabIAS = *zeros
068700011113     C                   ENDIF
068800011113     C                   ENDIF
068900011113     C*
069000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069100011113     C                   Z-ADD     vabCAS        wrkDec
069200011113     C                   IF        wrkDec > *zeros
069300011113     C                   IF        vabVCA = 'ITL'
069400011113     C                   EVAL      vabCAS = *zeros
069500011113     C                   ENDIF
069600011113     C                   ENDIF
069700011113     C*
069800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
069900011113     C                   Z-ADD     vabVMD        wrkDec
070000011113     C                   IF        wrkDec > *zeros
070100011113     C                   IF        vabVAD = 'ITL'
070200011113     C                   EVAL      vabVMD = *zeros
070300011113     C                   ENDIF
070400011113     C                   ENDIF
070500011113     C*
070600011113     C                   ENDSR
070700011113     C***
070800011113
070900011113
071000000801
071100000801
071200990920      /TITLE Invio dei dati al punto operativo.
071300010202     C     invio         BEGSR
071400990920     C*
071500021113     C* 1° invio FIVAT
071600010201     C                   reset                   dscmz
071700010201     C                   move      vlrpoi        cmzdst
071800021113     C                   eval      cmzfld = 'FIVATWWR'
071900010201     C                   eval      cmzmbd = vlrhdl
072000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072100021009     C***                if        prmfir = *blanks
072200021113     C                   eval      cmzfla = 'FIVAT00F'
072300021113     C                   eval      cmzmba = 'FIVAT00F'
072400021009     C***                else
072500021009     C***                eval      cmzfla = prmfir
072600021009     C***                eval      cmzmba = prmfir
072700021009     C***                endif
072800010201     C                   eval      cmznrr = *zeros
072900020305     C                   move      §ctrokvt      cmznrr
073000021018     C                   eval      cmzlba = vlrfl1
073100010201     C                   call(e)   'TIS711C'
073200010201     C                   parm                    dscmz
073300010201     C                   parm      *blanks       esito
073400010205     C                   if        %error
073500010205     C                             or cmzerr = '1'
073600010205     C                             or esito  = '1'
073700010205     C                   eval      wrkesito = '3'
073800010205     C                   else
073900010201     C*
074000021113     C* 2° invio FIVAB
074100010201     C                   reset                   dscmz
074200010201     C                   move      vlrpoi        cmzdst
074300010201     C                   eval      cmzfld = vlrfou
074400010201     C                   eval      cmzmbd = vlrhdl
074500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074600021009     C***                if        prmfir = *blanks
074700021113     C                   eval      cmzfla = 'FIVAB00F'
074800021113     C                   eval      cmzmba = 'FIVAB00F'
074900021009     C***                else
075000021009     C***                eval      cmzfla = prmfir
075100021009     C***                eval      cmzmba = prmfir
075200021009     C***                endif
075300010201     C                   eval      cmznrr = *zeros
075400010201     C                   move      §ctrokvb      cmznrr
075500021018     C                   eval      cmzlba = vlrfl1
075600010201     C                   call(e)   'TIS711C'
075700010201     C                   parm                    dscmz
075800010201     C                   parm      *blanks       esito
075900010201     C                   if        %error
076000010201     C                             or cmzerr = '1'
076100010201     C                             or esito  = '1'
076200010201     C                   eval      wrkesito = '3'
076300010201     C                   endif
076400010205     C                   endif
076500990920     C*
076600000613     C                   ENDSR
076700000613     C***
076800990910
076900000613     C     *inzsr        BEGSR
077000990910     C*
077100990910     C     *entry        plist
077200990920     C                   parm                    tivlrds
077300990921     C                   parm      wrkesito      esito
077400000724     C                   parm                    prmlit
077500000710     C                   parm                    prmfir
077600000613     C*
077700000830     C* CALCOLA LA DATA CORRENTE
077800100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077900100118     C                   eval      datcor = %dec(%date() : *ISO)
078000000830     C*
078100000613     C                   ENDSR
078200000613     C***
