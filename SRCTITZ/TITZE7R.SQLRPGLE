000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200150130      *
000300150130      *Particolarità:
000400150130      *preso ad esempio un traduttore GLS3 ma era qualcosa di ancora diverso
000500150130      *
000600081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000700990908
000800020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000900100412     FFIVABwwr  O    E             DISK    usropn
001000021113     FFIVATwwr  O    E             DISK    usropn
001100990908
001200000801     D*----------------------------------------------------
001300000801     D* DICHIARAZIOINE VARIABILI DI WRK
001400000801     D*----------------------------------------------------
001500990920     D dscmz         e ds                  inz
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900070730     D tisi95ds      e ds
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400000613     D rrnum           s              6  0 INZ(*zeros)
002500150130     D depspe          s             16    INZ(*blanks)
002600150130     D curspe          s             16    INZ(*blanks)
002700010202     D parccm          s              8    INZ(*blanks)
002800010202     D parmbr          s             10    INZ(*blanks)
002900010202     D paropz          s              1    INZ(*blanks)
003000010202     D chkcall         s              1    INZ(*blanks)
003100080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003200091223     D Num5_0          s              5  0
003300130911     D IdColloIn       s             15  0
003400020725
003500020725     D*------------------
003600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003700020725     D*------------------
003800070530     D INPUT_DS        DS                  INZ
003900100729     D  VINDTA                     2048
004000100729     D  VINFLG                        1
004100150130     D  VINSPE                       16
004200100729     D  VINRRN                        8  0
004300020725     D*
004400081113
004500091223     D*------------------
004600091223     D* DS REPERIMENTO NUMERATORE
004700091223     D*------------------
004800100309     D trul33ds      e ds                  inz
004900091223     D*------------------
005000091223     D* DS ARCHITETTURA
005100091223     D*------------------
005200091223     D kpjba         e ds                  inz
005300091223
005400101119
005500101119     D*-------------------
005600101119     D* COSTANTI
005700101119     D*-------------------
005800101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
005900101119     D                                     èéù')
006000101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
006100101119     D                                     EEU')
006200081113
006300081113     D*------------------
006400081113     D* LINKING A DEFINIZIONI ESTERNE
006500081113     D*------------------
006600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006700090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006800081113
006900990908
007000010201
007100010201
007200080117     C*
007300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007400080117     C
007500080117     C/EXEC SQL
007600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007700080117     C/END-EXEC
007800080117     C
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200000613     C*
008300100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008400070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008500000613     C*
008600010202     C* Effettuo la chiamata al CLLE preposto
008700100525     C  N51              call(e)   'TITVVTC'
008800100525     C                   parm                    parccm
008900100525     C                   parm                    parmbr
009000100525     C                   parm      '2'           paropz
009100100525     C   51              call(e)   'TITVVBC'
009200100525     C                   parm                    parccm
009300100525     C                   parm                    parmbr
009400100525     C                   parm      '2'           paropz
009500070730     C*
009600070730     C* Effettuo lancio TISI95 solo x chiusura
009700070730     C                   CLEAR                   TISI95DS
009800070730     C                   EVAL      I95TLA = 'C'
009900070730     C                   CALL      'TISI95R'
010000070730     C                   PARM                    TISI95DS
010100000616     C*
010200000801     C
010300010201     C                   seton                                        LR
010400000801
010500910830     C*--------------------------------------------------------
010600070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010700910830     C*--------------------------------------------------------
010800070530     C     RWFILE        BEGSR
010900990910     C*
011000990914     C                   if        not %open(tivin00r)
011100990908     C                   open      tivin00r
011200990914     C                   endif
011300070530     C*
011400100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
011500100525     C                   exsr      prevasx
011600010201     C*
011700010202     C                   if        chkcall = '0'
011800010202     C*
011900100525     C                   if        not %open(fivabwwr)
012000100525     C                   open      fivabwwr
012100100525     C                   endif
012200100525     C*
012300021113     C                   if        not %open(fivatwwr)
012400021113     C                   open      fivatwwr
012500010201     C                   endif
012600080117     C*
012700080117     C                   EXSR      INZVAR
012800080117     C                   EXSR      DEFCAM
012900990910     C*
013000010201     C                   clear                   §CTROKVB          5 0
013100020305     C                   clear                   §CTROKVT          5 0
013200000801     C                   clear                   §CTRMO            5 0
013300000801     C                   clear                   §CTRNO            5 0
013400990910     C*
013500020725     C/EXEC SQL
013600020725     C+ declare C1 cursor for select
013700110201     C+ vindta, vinflg, substr(vindta, 108, 10) as sped, rrn(tivin00r)
013800060223     C+ from tivin00r
013900110201     C+ order by sped, substr(vindta, 108, 10) desc
014000020725     C+ for read only
014100020725     C/END-EXEC
014200020725     C
014300020725     C/EXEC SQL
014400020725     C+ open C1
014500020725     C/END-EXEC
014600020725     C
014700020725     C/EXEC SQL
014800070530     C+ Fetch C1 into :INPUT_DS
014900020725     C/END-EXEC
015000020725     C*
015100020725     C                   dow       sqlcod = *zeros
015200990913     C*
015300100310     C                   if        vindta > *blanks
015400000613     C                   add       1             rrnum
015500000801     C*
015600020725     C                   if        vinflg = *blanks
015700020725     C                             or vinflg = '0'
015800020725     C                             or vinflg = '2'
015900000801     C*
016000020725     C                   clear                   x_vinmsg
016100020725     C                   eval      x_vinflg = '1'
016200101119     C*
016300101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
016400101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
016500110201     C*
016600110201     C* Skippo il record finale 'EOF'
016700110201     C                   IF        %trim(vindta) = 'EOF'
016800110201     C                   eval      x_vinflg = '1'
016900110201     C                   ELSE
017000010305     C*
017100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
017200150130     C                   EVAL      curspe=%trim(%subst(vindta:257:16))
017300010305     C*
017400100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017500080923     C                   if        *in50 = *off
017600101119     C                   exsr      inzvar
017700101119     C                   exsr      defcam
017800071003     C                   exsr      impvab
017900100525     C                   exsr      wrivab
018000100127     C                   exsr      wrivat_a                                     => carico VAT
018100071003     C                   exsr      wrivat_b                                     => carico VAT
018200140725     C                   exsr      wrivat_i                                     => carico VAT
018300140725     C                   exsr      wrivat_s                                     => carico VAT
018400100525     C   51              exsr      wrivat_e                                     => carico VAT
018500071003     C                   else
018600080923     C*
018700071009     C                   if        wDISK = 'D'
018800091223     C                   exsr      repNSP
018900071009     C                   exsr      impvab
019000100412     C                   exsr      wrivab
019100100127     C                   exsr      wrivat_a                                     => carico VAT
019200071009     C                   exsr      wrivat_b                                     => carico VAT
019300140725     C                   exsr      wrivat_i                                     => carico VAT
019400140725     C                   exsr      wrivat_s                                     => carico VAT
019500071009     C                   exsr      wrivat_e                                     => carico VAT
019600071009     C                   else
019700071009     C*
019800010305     C                   if        depspe = *blanks                             => 1° giro
019900010305     C                   eval      depspe = curspe                              => memorizz. spediz
020000080117     C                   clear                   tivinds
020100091223     C                   exsr      repNSP
020200020305     C                   exsr      impvab
020300100127     C                   exsr      wrivat_a                                     => carico VAT
020400071003     C                   exsr      wrivat_b                                     => carico VAT
020500140725     C                   exsr      wrivat_i                                     => carico VAT
020600140725     C                   exsr      wrivat_s                                     => carico VAT
020700071003     C   50              exsr      wrivat_e                                     => carico VAT
020800010305     C                   else
020900020725     C                   if        depspe <> curspe                             => rottura di spediz
021000010305     C                   eval      depspe = curspe                              => memorizz. spediz
021100100412     C                   exsr      wrivab
021200080117     C                   clear                   tivinds
021300091223     C                   exsr      repNSP
021400020305     C                   exsr      impvab
021500100127     C                   exsr      wrivat_a                                     => carico VAT
021600071003     C                   exsr      wrivat_b                                     => carico VAT
021700140725     C                   exsr      wrivat_i                                     => carico VAT
021800140725     C                   exsr      wrivat_s                                     => carico VAT
021900071003     C   50              exsr      wrivat_e                                     => carico VAT
022000020305     C                   else                                                   => x stessa spediz
022100100401     C*
022200100401     C* Se nn richiesta esclusione spedizioni "duplicate"
022300100928     C                   select
022400100928     C                   when      wDUPREC = ' '
022500020305     C                   exsr      impvab
022600100127     C                   exsr      wrivat_a                                     => carico VAT
022700071003     C                   exsr      wrivat_b                                     => carico VAT
022800140725     C                   exsr      wrivat_i                                     => carico VAT
022900140725     C                   exsr      wrivat_s                                     => carico VAT
023000071003     C   50              exsr      wrivat_e                                     => carico VAT
023100100928     C                   when      wDUPREC = 'C'
023200100928     C   50              exsr      wrivat_e                                     => carico VAT
023300100928     C                   endsl
023400010305     C                   endif
023500010305     C                   endif
023600010305     C                   endif
023700071003     C                   endif
023800110201     C*
023900110201     C                   endif
024000071009     C                   endif
024100000905     C*
024200000905     C                   else
024300020725     C                   eval      x_vinflg = '1'
024400000905     C                   endif
024500000905     C*
024600020725     C     VINRRN        chain     tivin000
024700020725     C                   if        %found(tivin00r)
024800020725     C                   eval      y_vinflg = x_vinflg
024900020725     C                   eval      y_vinmsg = x_vinmsg
025000020725     C                   update    tivin000
025100020725     C                   endif
025200020725     C*
025300020725     C/EXEC SQL
025400070530     C+ Fetch C1 into :INPUT_DS
025500020725     C/END-EXEC
025600020725     C*
025700020725     C                   enddo
025800020725     C*
025900020725     C/EXEC SQL
026000020725     C+ close C1
026100020725     C/END-EXEC
026200000905     C*
026300020305     C* Scarico i VAB rimasti "in sospeso"
026400071009     C                   if        wDISK = 'C'
026500100412     C                   exsr      wrivab
026600071009     C                   endif
026700010202     C*
026800010202     C                   endif
026900990910
027000990910     C* Se non ci sono record con errori ...
027100000710     C                   if        §ctrno = 0
027200990910     C* ... restituisco esito OK.
027300990921     C                   eval      wrkesito = '0'
027400990910     C                   else
027500100525     C                   if        §ctrokvb > 0 OR
027600100525     C                             §ctrokvt > 0
027700990921     C                   eval      wrkesito = '1'
027800000710     C                   else
027900000710     C                   eval      wrkesito = '2'
028000990910     C                   endif
028100000710     C                   endif
028200990910     C*
028300990914     C                   if        %open(tivin00r)
028400990908     C                   close     tivin00r
028500990914     C                   endif
028600021113     C                   if        %open(fivabwwr)
028700021113     C                   close     fivabwwr
028800990914     C                   endif
028900021113     C                   if        %open(fivatwwr)
029000021113     C                   close     fivatwwr
029100010201     C                   endif
029200990910     C*
029300100525     C                   if        §ctrokvb > 0 OR
029400100525     C                             §ctrokvt > 0
029500000724     C                             and vlrpoi <> *zeros
029600010202     C                   exsr      invio
029700990920     C                   endif
029800990920     C*
029900910830     C                   ENDSR
030000000613     C***
030100100412
030200100412
030300010305
030400010305     C*----------------------------------------------------*
030500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030600010305     C*----------------------------------------------------*
030700020305     C     WRIVAB        BEGSR
030800100412     C*
030900100412     C* Gestisco l'eventuale rottura x numero spedizione
031000100412     C                   if        VABRMA = *blanks
031100100412     C                   movel(P)  VABRMN        VABRMA
031200100412     C                   endif
031300100412     C*
031400100525     C                   if        *in51 = *off and *in31 = *off
031500100525     C                   write     fivab000                                     => scarico il VAB
031600100525     C                   endif
031700010305     C*
031800010305     C                   ENDSR
031900990920
032000000801     C*----------------------------------------------------*
032100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032200000801     C*----------------------------------------------------*
032300010201     C     INZVAR        BEGSR
032400000801     C*
032500010201     C                   Z-ADD     *zeros        Num5_0
032600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032700020725     C                   MOVEL     '0'           FlgCAS            1
032800000801     C*
032900000801     C                   ENDSR
033000000801     C*----------------------------------------------------*
033100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033200000801     C*----------------------------------------------------*
033300020904     C     DEFCAM        BEGSR
033400080923     C*
033500100928     C                   SETOFF                                       505160
033600000801     C*
033700021113     C                   CLEAR                   FIVAB000
033800021113     C                   CLEAR                   FIVAT000
033900070730     C* Imposto i valori di default...
034000150130     C                   EVAL      VABCCM = 0288384
034100150130     C                   EVAL      VATCCM = 0288384
034200150130     C                   EVAL      VABLNP = 028
034300150130     C                   EVAL      VATLNP = 028
034400150130     C                   EVAL      VABCTR = 001
034500100525     C                   EVAL      VABCBO = '1'
034600070222     C* ... e poi verifico se sono stati passati come parametri
034700070222     C                   IF        vlrppt > *blanks
034800100525     C*
034900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035100070222     C                   EXSR      CHKNUM
035200070222     C                   IF        PiInt=*on
035300070222     C                   Z-ADD     PiVal         VABCCM
035400070222     C                   Z-ADD     PiVal         VATCCM
035500070222     C                   ENDIF
035600070222     C                   ENDIF
035700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035900070222     C                   EXSR      CHKNUM
036000070222     C                   IF        PiInt=*on
036100070222     C                   Z-ADD     PiVal         VABLNP
036200070222     C                   Z-ADD     PiVal         VATLNP
036300070222     C                   ENDIF
036400070222     C                   ENDIF
036500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036700070222     C                   EXSR      CHKNUM
036800070222     C                   IF        PiInt=*on
036900070222     C                   Z-ADD     PiVal         VABCTR
037000070222     C                   ENDIF
037100070928     C                   ENDIF
037200071009     C                   MOVEL     *blanks       wDISK             1
037300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037400071009     C                   IF        wDISK <> *blanks
037500070928     C                   SETON                                        50
037600070222     C                   ENDIF
037700100525     C                   IF        wDISK  = 'T'
037800100525     C                   SETOFF                                       50
037900100525     C                   SETON                                        51
038000100525     C                   ENDIF
038100100401     C                   MOVEL     *blanks       wDUPREC           1
038200100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
038300100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
038400100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
038500100525     C                   ENDIF
038600100525     C*
038700070222     C                   ENDIF
038800071009     C*
038900071009     C   50              EVAL      VABCTM = '7Q'
039000000801     C*
039100000801     C                   ENDSR
039200091223     C*----------------------------------------------------*
039300091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
039400091223     C*----------------------------------------------------*
039500091223     C     REPNSP        BEGSR
039600091223     C*
039700091223     C                   EXSR      INZVAR
039800091223     C                   EXSR      DEFCAM
039900100928     C*
040000100928     C                   SETON                                        60
040100091223     C*
040200091223     C* NSP => Stacco un numeratore da AZNUM
040300091223     C                   clear                   TRUL33DS
040400091223     C                   eval      I33OPE = *zeros
040500091223     C                   eval      I33CNU = 302
040600091223     C                   eval      I33NUM = 1
040700091223     C                   movel     TRUL33DS      KPJBU
040800091223     C                   call      'TRUL33R'
040900091223     C                   parm                    KPJBA
041000091223     C                   movel     KPJBU         TRUL33DS
041100091223     C                   if        O33ERR = *zeros
041200091223     C                   z-add     O33NRF        VABNSP
041300091223     C                   z-add     O33NRF        VATNSP
041400091223     C                   else
041500091223     C                   SETON                                        31
041600091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041700091223     C                             + ' ' + 'VABNSP VATNSP'
041800091223     C                   endif
041900091223     C*
042000091223     C                   ENDSR
042100000801     C*----------------------------------------------------*
042200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
042300000801     C*----------------------------------------------------*
042400100730     C     IMPVAB        BEGSR
042500070530     C*
042600070530     C                   SETOFF                                       3132
042700070928     C*
042800070928     C                   MOVE(P)   vlrpoi        VABFGS
042900070928     C                   MOVE(P)   vlrpoi        VATFGS
043000070928     C*
043100070928     C                   MOVEL     datcor        VABAAS
043200070928     C                   MOVEL     datcor        VATAAS
043300070928     C                   MOVE      datcor        VABMGS
043400100729     C*
043500100729     C* Reperimento campi ALFA
043600110201     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
043700110201     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
043800110201     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
043900110201     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
044000110201     C                   EVAL      VABCAD=%trim(%subst(vindta:101:5))
044100110127     C*
044200150130     C                   IF        %scan('FERMO DEPOSITO' : VABIND : 1) > 0
044300150130     C                   EVAL      VABFFD = 'S'
044400150130     C                   ENDIF
044500150130     C*
044600150130     C                   SELECT
044700150130     C                   WHEN      %subst(vindta:207:1) = 'F'
044800150130     C                   EVAL      VABCBO = '1'
044900150130     C                   WHEN      %subst(vindta:207:1) = 'A'
045000150130     C                   EVAL      VABCBO = '2'
045100150130     C                   ENDSL
045200110127     C*
045300140725     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
045400100729     C*
045500100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
045600100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045700100729     C     '@':'A'       XLATE     VABRSD        VABRSD
045800100729     C* ==
045900100729     C*
046000100729     C* Reperimento campi NUMERICI
046100100729     C                   MOVEL     DATCOR        VABAAS
046200100729     C                   MOVE      DATCOR        VABMGS
046300110127     C* NSP / RMN
046400101119     C                   IF        *IN51 = *off
046500150130     C                   EVAL      PiStr=%trim(%subst(vindta:257:16))
046600100730     C                   EXSR      CHKNUM
046700100730     C                   IF        PiInt=*on
046800100730     C                   Z-ADD     PiVal         VABRMN
046900100928     C  N60              Z-ADD     PiVal         VABNSP
047000100928     C  N60              Z-ADD     PiVal         VATNSP
047100100730     C                   ELSE
047200100730     C                   SETON                                        32
047300100730     C                   Z-ADD     1             VABRMN
047400100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047500101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
047600100730     C                   ENDIF
047700101119     C*
047800101119     C                   ELSE
047900101119     C*
048000150130     C                   EVAL      PiStr=%trim(%subst(vindta:257:16))
048100101119     C                   EXSR      CHKNUM
048200101119     C                   IF        PiInt=*on
048300101119     C  N60              Z-ADD     PiVal         VATNSP
048400101119     C                   ELSE
048500101119     C                   SETON                                        32
048600101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048700101119     C                             + ' ' + 'VATNSP'
048800101119     C                   ENDIF
048900101119     C                   ENDIF
049000110201     C* NCL
049100110201     C                   z-add     *zeros        wCOLLI            5 0
049200110201     C                   z-add     *zeros        wBANCALI          5 0
049300110201     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
049400100928     C                   EXSR      CHKNUM
049500100928     C                   IF        PiInt=*on
049600110201     C                   Z-ADD     PiVal         wCOLLI
049700100928     C                   ELSE
049800101119     C  N51              SETON                                        32
049900101119     C  N51              Z-ADD     *zeros        VABNCL
050000101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050100100928     C                             + ' ' + 'VABNCL'
050200100928     C                   ENDIF
050300110201     C*
050400110201     C                   EVAL      VABNCL = wCOLLI + wBANCALI
050500100729     C* PKB
050600110201     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
050700100729     C                   EXSR      CHKNUM
050800100729     C                   IF        PiNum=*on
050900110201     C                   Z-ADD     PiVal         VABPKB
051000100729     C                   ELSE
051100101119     C  N51              SETON                                        32
051200101119     C  N51              Z-ADD     *zeros        VABPKB
051300101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
051400100729     C                             + ' ' + 'VABPKB'
051500100729     C                   ENDIF
051600100729     C* CAS
051700140725     C                   IF        %subst(vindta:137:10)<> *blanks      AND
051800140725     C                             %subst(vindta:137:10)<> *zeros       AND
051900140725     C                             %subst(vindta:137:10)<> '0000000,00' AND
052000140725     C                             %trim(%subst(vindta:137:10))<> '0,00'
052100100729     C                   EVAL      FlgCAS = '1'
052200110127     C                   EVAL      VABVCA = 'EUR'
052300140929     C***                EVAL      VABTIC=%trim(%subst(vindta:147:40))
052400110201     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
052500100729     C                   EXSR      CHKNUM
052600100729     C                   IF        PiNum=*on
052700100928     C                   Z-ADD(H)  PiVal         VABCAS
052800100729     C                   ELSE
052900101119     C  N51              SETON                                        32
053000101119     C  N51              Z-ADD     *zeros        VABCAS
053100101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053200100729     C                             + ' ' + 'VABCAS'
053300100729     C                   ENDIF
053400101119     C                   ELSE
053500101119     C                   EVAL      FlgCAS = '0'
053600100729     C                   ENDIF
053700110201     C* IAS
053800140725     C                   IF        %subst(vindta:223:11)<> *blanks       AND
053900140725     C                             %subst(vindta:223:11)<> *zeros        AND
054000140725     C                             %subst(vindta:223:11)<> '00000000,00' AND
054100140725     C                             %trim(%subst(vindta:223:11))<> '0,00'
054200110201     C                   EVAL      VABVAS = 'EUR'
054300110201     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
054400110201     C                   EXSR      CHKNUM
054500110201     C                   IF        PiNum=*on
054600110201     C                   Z-ADD(H)  PiVal         VABIAS
054700110201     C                   ELSE
054800110201     C  N51              SETON                                        32
054900110201     C  N51              Z-ADD     *zeros        VABIAS
055000110201     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
055100110201     C                             + ' ' + 'VABIAS'
055200110201     C                   ENDIF
055300110201     C                   ENDIF
055400100729     C*
055500100729     C* Se provincia nn valorizzata la reperisco
055600100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
055700100729     C                   IF        VABNZD  = *blanks AND
055800100729     C                             VABPRD  = *blanks AND
055900100729     C                             VABCAD <> *blanks
056000100729     C                   CLEAR                   TISI95DS
056100100729     C                   EVAL      I95TCN = '3'
056200100729     C                   Z-ADD     datcor        I95DAT
056300100729     C                   EVAL      I95NAR = VABNZD
056400100729     C                   EVAL      I95CAP = VABCAD
056500100729     C                   EVAL      I95LOC = VABLOD
056600100729     C                   CALL      'TISI95R'
056700100729     C                   PARM                    TISI95DS
056800100729     C                   EVAL      VABPRD = O95PRV
056900100729     C                   ENDIF
057000100729     C*
057100100729     C* Considerazioni finali su CBO/CAS
057200100729     C                   IF        FlgCAS = '1'
057300100729     C                   IF        VABCBO = '1'
057400100729     C                   EVAL      VABCBO = '4'
057500100729     C                   ENDIF
057600100729     C                   IF        VABCBO = '2'
057700100729     C                   EVAL      VABCBO = '6'
057800100729     C                   ENDIF
057900100729     C                   ENDIF
058000010202     C*
058100000801     C* Ebbene...
058200000801     C                   ADD       1             §CTRMO
058300070530     C                   IF        *in31 <> *zeros OR
058400070530     C                             *in32 <> *zeros
058500000801     C                   ADD       1             §CTRNO
058600020725     C                   EVAL      x_vinflg = '2'
058700000801     C                   ELSE
058800010201     C                   ADD       1             §CTROKVB
058900000801     C                   ENDIF
059000000801     C*
059100000801     C                   ENDSR
059200100127     C*----------------------------------------------------*
059300100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
059400100127     C*----------------------------------------------------*
059500100127     C     WRIVAT_A      BEGSR
059600100127     C*
059700150130     C**Valorizzo l buffer di scrittura del FIVAT
059800150130     C**- TIPO RECORD "A"
059900150130     C***                eval      VATTRC = 'A'
060000150130     C***                eval      VATNOT = %trim(%subst(vindta:1008:10))
060100150130     C***                exsr      wrivat
060200100127     C*
060300100127     C                   ENDSR
060400010201     C*----------------------------------------------------*
060500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
060600010201     C*----------------------------------------------------*
060700071003     C     WRIVAT_B      BEGSR
060800010201     C*
060900021113     C* Valorizzo l buffer di scrittura del FIVAT
061000070928     C* - TIPO RECORD "B"
061100101119     C***                eval      VATTRC = 'B'
061200101119     C***                eval      VATNOT = %trim(%subst(vindta:726:12))
061300101119     C***                exsr      wrivat
061400010201     C*
061500010201     C                   ENDSR
061600140725     C*----------------------------------------------------*
061700140725     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"
061800140725     C*----------------------------------------------------*
061900140725     C     WRIVAT_I      BEGSR
062000140725     C*
062100140725     C* Valorizzo l buffer di scrittura del FIVAT
062200140725     C* - TIPO RECORD "I" / "J"
062300140929     C                   movel     *blanks       wTRC             70
062400140929     C                   eval      wTRC   = %trim(%subst(vindta:927:70))
062500140929     C                   EVAL      wTRC = %xlate(',':';':wTRC)
062600140929     C                   eval      VATTRC = 'I'
062700140929     C                   eval      VATNOT = %subst(wTRC:01:35)
062800140929     C                   exsr      wrivat
062900140929     C                   eval      VATTRC = 'J'
063000140929     C                   eval      VATNOT = %subst(wTRC:36:35)
063100140929     C                   exsr      wrivat
063200140725     C*
063300140725     C                   ENDSR
063400140725     C*----------------------------------------------------*
063500140725     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
063600140725     C*----------------------------------------------------*
063700140725     C     WRIVAT_S      BEGSR
063800140725     C*
063900150130     C**Valorizzo l buffer di scrittura del FIVAT
064000150130     C**- TIPO RECORD "S"
064100150130     C***                eval      VATTRC = 'S'
064200150130     C***                eval      VATNOT = %trim(%subst(vindta:1018:10))
064300150130     C***                exsr      wrivat
064400140725     C*
064500140725     C                   ENDSR
064600071003     C*----------------------------------------------------*
064700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
064800071003     C*----------------------------------------------------*
064900071003     C     WRIVAT_E      BEGSR
065000071003     C*
065100071003     C* Valorizzo l buffer di scrittura del FIVAT
065200071003     C* - TIPO RECORD "E"
065300100729     C*
065400130911     C* sviluppo i CHI SONO partendo dall'ID collo iniziale per quanti sono i colli
065500130911     C*
065600150130     C                   eval      VATTRC = 'E'
065700150130     C                   eval      VATNOT = %trim(%subst(vindta:257:16))
065800150130     C                   exsr      wrivat
065900071003     C*
066000071003     C                   ENDSR
066100100127     C*----------------------------------------------------*
066200100127     C*  SCARICO BUFFER FIVAT
066300100127     C*----------------------------------------------------*
066400100127     C     WRIVAT        BEGSR
066500100127     C*
066600100127     C                   if        vatNOT <> *blanks
066700100127     C                   ADD       1             §CTROKVT
066800100127     C                   write     FIVAT000
066900100127     C                   endif
067000100127     C*
067100100127     C                   ENDSR
067200020904
067300020904
067400010202     C*----------------------------------------------------*
067500100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
067600010202     C*----------------------------------------------------*
067700100525     C     PREVASX       BEGSR
067800100525     C*
067900021113     C* Compongo il nome del membro da dare al FIVATWWR
068000010202     C                   eval      parmbr = vlrhdl
068100010202     C                   movel     'M'           parmbr
068200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
068300010202     C                   eval      paropz = '1'
068400100525     C*
068500010202     C* Effettuo la chiamata al CLLE preposto
068600100525     C  N51              call(e)   'TITVVTC'
068700010202     C                   parm                    parccm
068800010202     C                   parm                    parmbr
068900010202     C                   parm                    paropz
069000100525     C   51              call(e)   'TITVVBC'
069100100525     C                   parm                    parccm
069200100525     C                   parm                    parmbr
069300100525     C                   parm                    paropz
069400100525     C*
069500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
069600010202     C                   if        %error
069700010202     C                   movel     '1'           chkcall
069800010202     C                   else
069900010202     C                   movel     '0'           chkcall
070000010202     C                   endif
070100010202     C*
070200010202     C                   ENDSR
070300000801     C*----------------------------------------------------*
070400000801     C*  CONTROLLO NUMERICITA' CAMPI
070500000801     C*----------------------------------------------------*
070600000801     C     CHKNUM        BEGSR
070700081113     C*
070800081113     C                   IF        PiDecChr = *blanks
070900110201     C                   EVAL      PiDecChr = ','
071000081113     C                   ENDIF
071100091223     C*
071200081113     C                   callp(e)  UBISNUM_Check(PiStr
071300081113     C                                          :PiDecChr
071400081113     C                                          :PiVal
071500081113     C                                          :PiNum
071600081113     C                                          :PiInt)
071700081113     C*
071800000801     C                   IF        %error
071900000801     C                   EVAL      PiInt=*off
072000000801     C                   ENDIF
072100000801     C*
072200000801     C                   ENDSR
072300000801     C***
072400011113
072500011113
072600000801
072700000801
072800990920      /TITLE Invio dei dati al punto operativo.
072900010202     C     invio         BEGSR
073000990920     C*
073100021113     C* 1° invio FIVAT
073200010201     C                   reset                   dscmz
073300010201     C                   move      vlrpoi        cmzdst
073400021113     C                   eval      cmzfld = 'FIVATWWR'
073500010201     C                   eval      cmzmbd = vlrhdl
073600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073700021009     C***                if        prmfir = *blanks
073800021113     C                   eval      cmzfla = 'FIVAT00F'
073900021113     C                   eval      cmzmba = 'FIVAT00F'
074000021009     C***                else
074100021009     C***                eval      cmzfla = prmfir
074200021009     C***                eval      cmzmba = prmfir
074300021009     C***                endif
074400010201     C                   eval      cmznrr = *zeros
074500020305     C                   move      §ctrokvt      cmznrr
074600021018     C                   eval      cmzlba = vlrfl1
074700010201     C                   call(e)   'TIS711C'
074800010201     C                   parm                    dscmz
074900010201     C                   parm      *blanks       esito
075000010205     C                   if        %error
075100010205     C                             or cmzerr = '1'
075200010205     C                             or esito  = '1'
075300010205     C                   eval      wrkesito = '3'
075400010205     C                   else
075500010201     C*
075600021113     C* 2° invio FIVAB
075700100525     C                   if        *in51 = *off
075800010201     C                   reset                   dscmz
075900010201     C                   move      vlrpoi        cmzdst
076000010201     C                   eval      cmzfld = vlrfou
076100010201     C                   eval      cmzmbd = vlrhdl
076200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076300021009     C***                if        prmfir = *blanks
076400021113     C                   eval      cmzfla = 'FIVAB00F'
076500021113     C                   eval      cmzmba = 'FIVAB00F'
076600021009     C***                else
076700021009     C***                eval      cmzfla = prmfir
076800021009     C***                eval      cmzmba = prmfir
076900021009     C***                endif
077000010201     C                   eval      cmznrr = *zeros
077100010201     C                   move      §ctrokvb      cmznrr
077200021018     C                   eval      cmzlba = vlrfl1
077300010201     C                   call(e)   'TIS711C'
077400010201     C                   parm                    dscmz
077500010201     C                   parm      *blanks       esito
077600010201     C                   if        %error
077700010201     C                             or cmzerr = '1'
077800010201     C                             or esito  = '1'
077900010201     C                   eval      wrkesito = '3'
078000010201     C                   endif
078100100525     C                   endif
078200010205     C                   endif
078300990920     C*
078400000613     C                   ENDSR
078500000613     C***
078600070411
078700070411     C     *pssr         BEGSR
078800070411     C*
078900070411     C                   if        %open(tivin00r)
079000070411     C                   close     tivin00r
079100070411     C                   endif
079200070411     C                   if        %open(fivabwwr)
079300070411     C                   close     fivabwwr
079400070411     C                   endif
079500070411     C                   if        %open(fivatwwr)
079600070411     C                   close     fivatwwr
079700070411     C                   endif
079800070411     C*
079900070411     C* Effettuo la chiamata al CLLE preposto
080000100525     C  N51              call(e)   'TITVVTC'
080100100525     C                   parm                    parccm
080200100525     C                   parm                    parmbr
080300100525     C                   parm      '2'           paropz
080400100525     C   51              call(e)   'TITVVBC'
080500100525     C                   parm                    parccm
080600100525     C                   parm                    parmbr
080700100525     C                   parm      '2'           paropz
080800070411     C*
080900070411     C                   eval      wrkesito = '2'
081000070411     C*
081100070411     C                   seton                                        LR
081200070411     C*
081300070411     C                   ENDSR     '*CANCL'
081400070411     C***
081500070411
081600990910
081700000613     C     *inzsr        BEGSR
081800990910     C*
081900990910     C     *entry        plist
082000990920     C                   parm                    tivlrds
082100990921     C                   parm      wrkesito      esito
082200000724     C                   parm                    prmlit
082300000710     C                   parm                    prmfir
082400000613     C*
082500000830     C* CALCOLA LA DATA CORRENTE
082600091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
082700091223     C                   eval      datcor = %dec(%date() : *ISO)
082800000830     C*
082900000613     C                   ENDSR
083000000613     C***
