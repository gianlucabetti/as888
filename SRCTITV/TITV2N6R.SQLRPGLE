000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130121      *
000300130121      * PARTICOLARITA':
000400130121      * 12/10/29 a differenza dei traduttori di copia (TITV1F3R - TITV1F4R) la data consegna
000500130121      * deve essere sempre presa e non solo se il tipo consegna è = '1' ma rimane la gestione TC1
000600130121      * 13/01/21
000700130121      * Nei 2 campi VABPKB e VABCAS viene utilizzata la virgola
000800130121      * VABCBO è impostato = 4 su tutti e 3 i record. Dovrebbe essere impostato = 4 solo in presenza
000900130121      * di contrassegno (1° record) e = 1 per tutti gli altri record senza contrassegno.
001000130121      * INDIRIZZO E-MAIL VATNOT_I_J = da posizione 574 fino al ritorno a capo (il campo non riescono
001100130121      * a renderlo a posizioni fisse, al massimo sarà di 70 caratteri).
001200130121      *
001300121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001400990908
001500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001600021113     FFIVABwwr  O    E             DISK    usropn
001700021113     FFIVATwwr  O    E             DISK    usropn
001800990908
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600070730     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100000613     D rrnum           s              6  0 INZ(*zeros)
003200130121     D depspe          s              7    INZ(*blanks)
003300130121     D curspe          s              7    INZ(*blanks)
003400010202     D parccm          s              8    INZ(*blanks)
003500010202     D parmbr          s             10    INZ(*blanks)
003600010202     D paropz          s              1    INZ(*blanks)
003700010202     D chkcall         s              1    INZ(*blanks)
003800080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003900070530     D Num5_0          s              5  0
004000000830
004100020725
004200020725     D*------------------
004300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004400020725     D*------------------
004500070530     D INPUT_DS        DS                  INZ
004600101007     D  VINDTA                     2048
004700101007     D  VINFLG                        1
004800130121     D  VINSPE                        7
004900101007     D  VINRRN                        8  0
005000020725     D*
005100080923     D*------------------
005200080923     D* DS REPERIMENTO NUMERATORE
005300080923     D*------------------
005400080923     D trul33ds      e ds                  inz
005500080923     D*------------------
005600080923     D* DS ARCHITETTURA
005700080923     D*------------------
005800080923     D kpjba         e ds                  inz
005900080923     D*------------------
006000081113
006100081113     D*------------------
006200081113     D* LINKING A DEFINIZIONI ESTERNE
006300081113     D*------------------
006400100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006500081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006600081113
006700130121     D w70             s             70
006800990908
006900010201
007000010201
007100080117     C*
007200080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007300080117     C
007400080117     C/EXEC SQL
007500080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007600080117     C/END-EXEC
007700080117     C
007800000913     C                   reset                   rrnum
007900990921     C                   reset                   esito
008000990921     C                   reset                   wrkesito
008100000613     C*
008200070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008300000613     C*
008400010202     C* Effettuo la chiamata al CLLE preposto
008500050128     C                   call(e)   'TITVVTC'
008600010202     C                   parm                    parccm
008700010202     C                   parm                    parmbr
008800010202     C                   parm      '2'           paropz
008900070730     C*
009000070730     C* Effettuo lancio TISI95 solo x chiusura
009100070730     C                   CLEAR                   TISI95DS
009200070730     C                   EVAL      I95TLA = 'C'
009300070730     C                   CALL      'TISI95R'
009400070730     C                   PARM                    TISI95DS
009500000616     C*
009600000801     C
009700010201     C                   seton                                        LR
009800000801
009900910830     C*--------------------------------------------------------
010000070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010100910830     C*--------------------------------------------------------
010200070530     C     RWFILE        BEGSR
010300990910     C*
010400990914     C                   if        not %open(tivin00r)
010500990908     C                   open      tivin00r
010600990914     C                   endif
010700021113     C                   if        not %open(fivabwwr)
010800021113     C                   open      fivabwwr
010900990914     C                   endif
011000070530     C*
011100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011200020305     C                   exsr      prevat
011300010201     C*
011400010202     C                   if        chkcall = '0'
011500010202     C*
011600021113     C                   if        not %open(fivatwwr)
011700021113     C                   open      fivatwwr
011800010201     C                   endif
011900080117     C*
012000080117     C                   EXSR      INZVAR
012100080117     C                   EXSR      DEFCAM
012200990910     C*
012300010201     C                   clear                   §CTROKVB          5 0
012400020305     C                   clear                   §CTROKVT          5 0
012500000801     C                   clear                   §CTRMO            5 0
012600000801     C                   clear                   §CTRNO            5 0
012700990910     C*
012800020725     C/EXEC SQL
012900020725     C+ declare C1 cursor for select
013000130121     C+ vindta, vinflg, substr(vindta, 83, 7) as sped, rrn(tivin00r)
013100060223     C+ from tivin00r
013200060223     C+ order by sped
013300020725     C+ for read only
013400020725     C/END-EXEC
013500020725     C
013600020725     C/EXEC SQL
013700020725     C+ open C1
013800020725     C/END-EXEC
013900020725     C
014000020725     C/EXEC SQL
014100070530     C+ Fetch C1 into :INPUT_DS
014200020725     C/END-EXEC
014300020725     C*
014400020725     C                   dow       sqlcod = *zeros
014500990913     C*
014600020725     C                   if        vindta > *blanks
014700000613     C                   add       1             rrnum
014800000801     C*
014900020725     C                   if        vinflg = *blanks
015000020725     C                             or vinflg = '0'
015100020725     C                             or vinflg = '2'
015200000801     C*
015300020725     C                   clear                   x_vinmsg
015400020725     C                   eval      x_vinflg = '1'
015500010305     C*
015600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015700130121     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
015800020305     C                   MOVEL(p)  PiStr         curspe
015900010305     C*
016000071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016100080923     C                   if        *in50 = *off
016200071003     C                   exsr      impvab
016300071003     C                   exsr      wrivab
016400130121     C                   exsr      wrivat_ij                                    => carico VAT
016500071003     C                   else
016600080923     C*
016700071009     C                   if        wDISK = 'D'
016800071009     C                   exsr      impvab
016900071009     C                   exsr      wrivab
017000130121     C                   exsr      wrivat_ij                                    => carico VAT
017100130121     C***                exsr      wrivat_e                                     => carico VAT
017200071009     C                   else
017300071009     C*
017400010305     C                   if        depspe = *blanks                             => 1° giro
017500010305     C                   eval      depspe = curspe                              => memorizz. spediz
017600080117     C                   clear                   tivinds
017700020305     C                   exsr      impvab
017800130121     C                   exsr      wrivat_ij                                    => carico VAT
017900130121     C***50              exsr      wrivat_e                                     => carico VAT
018000010305     C                   else
018100020725     C                   if        depspe <> curspe                             => rottura di spediz
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300070928     C                   exsr      wrivab
018400080117     C                   clear                   tivinds
018500020305     C                   exsr      impvab
018600130121     C                   exsr      wrivat_ij                                    => carico VAT
018700130121     C***50              exsr      wrivat_e                                     => carico VAT
018800020305     C                   else                                                   => x stessa spediz
018900090204     C***                exsr      impvab
019000130121     C                   exsr      wrivat_ij                                    => carico VAT
019100130121     C***50              exsr      wrivat_e                                     => carico VAT
019200010305     C                   endif
019300010305     C                   endif
019400010305     C                   endif
019500071003     C                   endif
019600071009     C                   endif
019700000905     C*
019800000905     C                   else
019900020725     C                   eval      x_vinflg = '1'
020000000905     C                   endif
020100000905     C*
020200020725     C     VINRRN        chain     tivin000
020300020725     C                   if        %found(tivin00r)
020400020725     C                   eval      y_vinflg = x_vinflg
020500020725     C                   eval      y_vinmsg = x_vinmsg
020600020725     C                   update    tivin000
020700020725     C                   endif
020800020725     C*
020900020725     C/EXEC SQL
021000070530     C+ Fetch C1 into :INPUT_DS
021100020725     C/END-EXEC
021200020725     C*
021300020725     C                   enddo
021400020725     C*
021500020725     C/EXEC SQL
021600020725     C+ close C1
021700020725     C/END-EXEC
021800000905     C*
021900020305     C* Scarico i VAB rimasti "in sospeso"
022000071009     C                   if        wDISK = 'C'
022100071009     C                   exsr      wrivab
022200071009     C                   endif
022300010202     C*
022400010202     C                   endif
022500990910
022600990910     C* Se non ci sono record con errori ...
022700000710     C                   if        §ctrno = 0
022800990910     C* ... restituisco esito OK.
022900990921     C                   eval      wrkesito = '0'
023000990910     C                   else
023100010201     C                   if        §ctrokvb > 0
023200990921     C                   eval      wrkesito = '1'
023300000710     C                   else
023400000710     C                   eval      wrkesito = '2'
023500990910     C                   endif
023600000710     C                   endif
023700990910     C*
023800990914     C                   if        %open(tivin00r)
023900990908     C                   close     tivin00r
024000990914     C                   endif
024100021113     C                   if        %open(fivabwwr)
024200021113     C                   close     fivabwwr
024300990914     C                   endif
024400021113     C                   if        %open(fivatwwr)
024500021113     C                   close     fivatwwr
024600010201     C                   endif
024700990910     C*
024800010201     C                   if        §ctrokvb > 0
024900000724     C                             and vlrpoi <> *zeros
025000010202     C                   exsr      invio
025100990920     C                   endif
025200990920     C*
025300910830     C                   ENDSR
025400000613     C***
025500010305
025600010305     C*----------------------------------------------------*
025700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025800010305     C*----------------------------------------------------*
025900020305     C     WRIVAB        BEGSR
026000070730     C*
026100071003     C* Considerazioni finali
026200071003     C                   if        VABRMA = *blanks
026300071003     C                   movel(P)  VABRMN        VABRMA
026400071003     C                   endif
026500071003     C*
026600021113     C                   write     fivab000                                     => scarico il VAB
026700010305     C*
026800010305     C                   ENDSR
026900990920
027000000801     C*----------------------------------------------------*
027100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027200000801     C*----------------------------------------------------*
027300010201     C     INZVAR        BEGSR
027400000801     C*
027500010201     C                   Z-ADD     *zeros        Num5_0
027600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027700020725     C                   MOVEL     '0'           FlgCAS            1
027800000801     C*
027900000801     C                   ENDSR
028000000801     C*----------------------------------------------------*
028100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028200000801     C*----------------------------------------------------*
028300020904     C     DEFCAM        BEGSR
028400080923     C*
028500080923     C                   SETOFF                                       5051
028600000801     C*
028700021113     C                   CLEAR                   FIVAB000
028800021113     C                   CLEAR                   FIVAT000
028900070730     C* Imposto i valori di default...
029000130121     C                   EVAL      VABCCM = 0934812
029100130121     C                   EVAL      VATCCM = 0934812
029200130121     C                   EVAL      VABLNP = 093
029300130121     C                   EVAL      VATLNP = 093
029400130121     C                   EVAL      VABCTR = 100
029500130121     C                   EVAL      VABCTM = '7Q'
029600130121     C                   EVAL      VABCBO = '1'
029700070222     C* ... e poi verifico se sono stati passati come parametri
029800070222     C                   IF        vlrppt > *blanks
029900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030100070222     C                   EXSR      CHKNUM
030200070222     C                   IF        PiInt=*on
030300070222     C                   Z-ADD     PiVal         VABCCM
030400070222     C                   Z-ADD     PiVal         VATCCM
030500070222     C                   ENDIF
030600070222     C                   ENDIF
030700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030900070222     C                   EXSR      CHKNUM
031000070222     C                   IF        PiInt=*on
031100070222     C                   Z-ADD     PiVal         VABLNP
031200070222     C                   Z-ADD     PiVal         VATLNP
031300070222     C                   ENDIF
031400070222     C                   ENDIF
031500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031700070222     C                   EXSR      CHKNUM
031800070222     C                   IF        PiInt=*on
031900070222     C                   Z-ADD     PiVal         VABCTR
032000070222     C                   ENDIF
032100070928     C                   ENDIF
032200071009     C                   MOVEL     *blanks       wDISK             1
032300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032400071009     C                   IF        wDISK <> *blanks
032500070928     C                   SETON                                        50
032600070222     C                   ENDIF
032700080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032800080923     C                   SETON                                        51
032900080923     C                   ENDIF
033000070222     C                   ENDIF
033100071009     C*
033200071009     C   50              EVAL      VABCTM = '7Q'
033300000801     C*
033400000801     C                   ENDSR
033500000801     C*----------------------------------------------------*
033600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033700000801     C*----------------------------------------------------*
033800010201     C     IMPVAB        BEGSR
033900070530     C*
034000070530     C                   SETOFF                                       3132
034100070928     C*
034200070928     C                   EXSR      INZVAR
034300070928     C                   EXSR      DEFCAM
034400070928     C*
034500070928     C                   MOVE(P)   vlrpoi        VABFGS
034600070928     C                   MOVE(P)   vlrpoi        VATFGS
034700070928     C*
034800070928     C                   MOVEL     datcor        VABAAS
034900070928     C                   MOVEL     datcor        VATAAS
035000070928     C                   MOVE      datcor        VABMGS
035100130121     C*
035200130121     C* Reperimento campi ALFA
035300130121     C                   EVAL      VABRSD=%trim(%subst(vindta:224:30))
035400130121     C                   EVAL      VABIND=%trim(%subst(vindta:254:30))
035500130121     C                   EVAL      VABLOD=%trim(%subst(vindta:284:30))
035600130121     C                   EVAL      VABCAD=%trim(%subst(vindta:319:8))
035700130121     C                   EVAL      VABPRD=%trim(%subst(vindta:314:5))
035800130121     C                   EVAL      VABRMA=%trim(%subst(vindta:11:20))
035900130121     C                   EVAL      VABNOT=%trim(%subst(vindta:491:35))
036000130121     C*
036100130121     C* Considerazioni sulla ragione sociale del destinatario da indicare
036200130121     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036300130121     C     '@':'A'       XLATE     VABRSD        VABRSD
036400130121     C* ==
036500130121     C*
036600130121     C* GC1
036700130121     C                   IF        %subst(vindta:478:3) = 'LUN'
036800130121     C                   EVAL      VABGC1 = '1'
036900130121     C                   ENDIF
037000130121     C                   IF        %subst(vindta:478:3) = 'MAR'
037100130121     C                   EVAL      VABGC1 = '2'
037200130121     C                   ENDIF
037300130121     C                   IF        %subst(vindta:478:3) = 'MER'
037400130121     C                   EVAL      VABGC1 = '3'
037500130121     C                   ENDIF
037600130121     C                   IF        %subst(vindta:478:3) = 'GIO'
037700130121     C                   EVAL      VABGC1 = '4'
037800130121     C                   ENDIF
037900130121     C                   IF        %subst(vindta:478:3) = 'VEN'
038000130121     C                   EVAL      VABGC1 = '5'
038100130121     C                   ENDIF
038200130121     C                   IF        %subst(vindta:478:3) = 'SAB'
038300130121     C                   EVAL      VABGC1 = '6'
038400130121     C                   ENDIF
038500130121     C                   IF        %subst(vindta:478:3) = 'DOM'
038600130121     C                   EVAL      VABGC1 = '7'
038700130121     C                   ENDIF
038800130121     C* GC1
038900130121     C                   IF        %subst(vindta:481:1) = '1'
039000130121     C                   EVAL      %subst(VABGC1:2:1) = 'M'
039100130121     C                   ENDIF
039200130121     C                   IF        %subst(vindta:481:1) = '2'
039300130121     C                   EVAL      %subst(VABGC1:2:1) = 'P'
039400130121     C                   ENDIF
039500130121     C* CBO
039600130121     C                   IF        %subst(vindta:2:1) = '1'
039700130121     C                   EVAL      VABCBO = '1'
039800130121     C                   ENDIF
039900130121     C                   IF        %subst(vindta:2:1) = '6'
040000130121     C                   EVAL      VABCBO = '2'
040100130121     C                   ENDIF
040200130121     C*
040300130121     C* Reperimento campi NUMERICI
040400130121     C                   MOVEL     DATCOR        VABAAS
040500130121     C                   MOVE      DATCOR        VABMGS
040600130121     C* NSP
040700130121     C                   EVAL      PiStr=%trim(%subst(vindta:83:7))
040800130121     C                   EXSR      CHKNUM
040900130121     C                   IF        PiInt=*on
041000130121     C                   Z-ADD     PiVal         VABNSP
041100130121     C                   ELSE
041200130121     C                   SETON                                        31
041300130121     C                   Z-ADD     *zeros        VABNSP
041400130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041500130121     C                             + ' ' + 'VABNSP'
041600130121     C                   ENDIF
041700130121     C* RMN
041800130121     C                   EVAL      PiStr=%trim(%subst(vindta:38:13))
041900130121     C                   EXSR      CHKNUM
042000130121     C                   IF        PiInt=*on
042100130121     C                   Z-ADD     PiVal         VABRMN
042200130121     C                   ELSE
042300130121     C                   SETON                                        31
042400130121     C                   Z-ADD     1             VABRMN
042500130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042600130121     C                             + ' ' + 'VABRMN'
042700130121     C                   ENDIF
042800130121     C* CAD
042900130121     C***                EVAL      PiStr=%trim(%subst(vindta:319:8))
043000130121     C***                EXSR      CHKNUM
043100130121     C***                IF        PiInt=*on
043200130121     C***                Z-ADD     PiVal         Num5_0
043300130121     C***                MOVEL(p)  Num5_0        VABCAD
043400130121     C***                ELSE
043500130121     C***                SETON                                        32
043600130121     C***                EVAL      VABCAD = *zeros
043700130121     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
043800130121     C***                          + ' ' + 'VABCAD'
043900130121     C***                ENDIF
044000130121     C* Reperisco la provincia dal CAP e dalla località
044100130121     C                   IF        VABCAD <> *blanks AND
044200130121     C                             VABNZD  = *blanks AND
044300130121     C                             VABPRD  = *blanks
044400130121     C                   CLEAR                   TISI95DS
044500130121     C                   EVAL      I95TCN = '3'
044600130121     C                   Z-ADD     datcor        I95DAT
044700130121     C                   EVAL      I95CAP = VABCAD
044800130121     C                   EVAL      I95LOC = VABLOD
044900130121     C                   EVAL      I95NAR = VABNZD
045000130121     C                   CALL      'TISI95R'
045100130121     C                   PARM                    TISI95DS
045200130121     C                   EVAL      VABPRD = O95PRV
045300130121     C                   ENDIF
045400130121     C* DCR/TC1
045500130121     C***                IF        %subst(vindta:490:1) = '1'
045600130121     C* DCR
045700130121     C                   EVAL      PiStr=%trim(%subst(vindta:482:8))
045800130121     C                   EXSR      CHKNUM
045900130121     C                   IF        PiInt=*on
046000130121     C                   Z-ADD     PiVal         VABDCR
046100130121     C                   ELSE
046200130121     C                   SETON                                        32
046300130121     C                   Z-ADD     *zeros        VABDCR
046400130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046500130121     C                             + ' ' + 'VABDCR'
046600130121     C                   ENDIF
046700130121     C***                ELSE
046800130121     C* TC1
046900130121     C                   IF        %subst(vindta:490:1) = '2'
047000130121     C                   EVAL      VABTC1 = 'A'
047100130121     C                   ENDIF
047200130121     C***                ENDIF
047300130121     C* NCL
047400130121     C                   IF        %subst(vindta:352:5)<>'00000'
047500130121     C                   EVAL      PiStr=%trim(%subst(vindta:352:5))
047600130121     C                   EXSR      CHKNUM
047700130121     C                   IF        PiInt=*on
047800130121     C                   Z-ADD     PiVal         VABNCL
047900130121     C                   ELSE
048000130121     C                   SETON                                        32
048100130121     C                   Z-ADD     *zeros        VABNCL
048200130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048300130121     C                             + ' ' + 'VABNCL'
048400130121     C                   ENDIF
048500130121     C                   ELSE
048600130121     C                   EVAL      PiStr=%trim(%subst(vindta:364:5))
048700130121     C                   EXSR      CHKNUM
048800130121     C                   IF        PiInt=*on
048900130121     C                   Z-ADD     PiVal         VABNCL
049000130121     C                   ELSE
049100130121     C                   SETON                                        32
049200130121     C                   Z-ADD     *zeros        VABNCL
049300130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049400130121     C                             + ' ' + 'VABNCL'
049500130121     C                   ENDIF
049600130121     C                   ENDIF
049700130121     C* PKB
049800130121     C                   EVAL      PiStr=%trim(%subst(vindta:357:7))
049900130121     C                   EXSR      CHKNUM
050000130121     C                   IF        PiNum=*on
050100130121     C                   EVAL      PiVal=PiVal
050200130121     C                   Z-ADD(H)  PiVal         VABPKB
050300130121     C                   ELSE
050400130121     C                   SETON                                        32
050500130121     C                   Z-ADD     *zeros        VABPKB
050600130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050700130121     C                             + ' ' + 'VABPKB'
050800130121     C                   ENDIF
050900130121     C* CAS
051000130121     C                   IF        %subst(vindta:391:10)<>'0000000000' and
051100130121     C                             %subst(vindta:391:10)<>*blank       and
051200130121     C                             %subst(vindta:391:10)<>'0000000,00'
051300130121     C                   EVAL      FlgCAS = '1'
051400130121     C                   EVAL      VABVCA = 'EUR'
051500130121     C                   EVAL      PiStr=%trim(%subst(vindta:391:10))
051600130121     C                   EXSR      CHKNUM
051700130121     C                   IF        PiNum=*on
051800130121     C                   EVAL      PiVal=PiVal
051900130121     C                   Z-ADD(H)  PiVal         VABCAS
052000130121     C                   ELSE
052100130121     C                   SETON                                        32
052200130121     C                   Z-ADD     *zeros        VABCAS
052300130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052400130121     C                             + ' ' + 'VABCAS'
052500130121     C                   ENDIF
052600130121     C                   ENDIF
052700130121     C* IAS
052800130121     C                   IF        %subst(vindta:558:13)<>'0000000000000'
052900130121     C                   EVAL      VABVAS = 'EUR'
053000130121     C                   EVAL      PiStr=%trim(%subst(vindta:558:13))
053100130121     C                   EXSR      CHKNUM
053200130121     C                   IF        PiNum=*on
053300130121     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
053400130121     C                   Z-ADD(H)  PiVal         VABIAS
053500130121     C                   ELSE
053600130121     C                   SETON                                        32
053700130121     C                   Z-ADD     *zeros        VABIAS
053800130121     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053900130121     C                             + ' ' + 'VABIAS'
054000130121     C                   ENDIF
054100130121     C                   ENDIF
054200070730     C*
054300070730     C* Considerazioni finali su CBO/CAS
054400070730     C                   IF        FlgCAS = '1'
054500070730     C                   IF        VABCBO = '1'
054600070730     C                   EVAL      VABCBO = '4'
054700070730     C                   ENDIF
054800070730     C                   IF        VABCBO = '2'
054900070730     C                   EVAL      VABCBO = '6'
055000070730     C                   ENDIF
055100070730     C                   ENDIF
055200020305     C*
055300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055400011113     C                   EXSR      CHKIMPDIV
055500010202     C*
055600000801     C* Ebbene...
055700000801     C                   ADD       1             §CTRMO
055800070530     C                   IF        *in31 <> *zeros OR
055900070530     C                             *in32 <> *zeros
056000000801     C                   ADD       1             §CTRNO
056100020725     C                   EVAL      x_vinflg = '2'
056200000801     C                   ELSE
056300010201     C                   ADD       1             §CTROKVB
056400000801     C                   ENDIF
056500000801     C*
056600000801     C                   ENDSR
056700010201     C*----------------------------------------------------*
056800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
056900010201     C*----------------------------------------------------*
057000130121     C     WRIVAT_IJ     BEGSR
057100090204     C*
057200090204     C                   eval      vatAAS = vabAAS
057300090204     C                   eval      vatLNP = vabLNP
057400090204     C                   eval      vatNRS = vabNRS
057500090204     C                   eval      vatNSP = vabNSP
057600010201     C*
057700101007     C* Valorizzo il buffer di scrittura del FIVAT
057800130121     C                   eval      w70 = %trim(%subst(vindta:574:70))
057900130121     C* - TIPO RECORD "I"
058000130121     C                   if        %subst(w70:1:35) <> *blanks
058100130121     C                   eval      VATTRC = 'I'
058200130121     C                   eval      VATNOT = %trim(%subst(w70:1:35))
058300021113     C                   write     FIVAT000
058400020725     C                   endif
058500130121     C* - TIPO RECORD "J"
058600130121     C                   if        %subst(w70:36:35) <> *blanks
058700130121     C                   eval      VATTRC = 'J'
058800130121     C                   eval      VATNOT = %trim(%subst(w70:36:35))
058900130121     C                   write     FIVAT000
059000130121     C                   endif
059100010201     C*
059200010201     C                   ENDSR
059300071003     C*----------------------------------------------------*
059400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059500071003     C*----------------------------------------------------*
059600071003     C     WRIVAT_E      BEGSR
059700090204     C*
059800090204     C                   eval      vatAAS = vabAAS
059900090204     C                   eval      vatLNP = vabLNP
060000090204     C                   eval      vatNRS = vabNRS
060100090204     C                   eval      vatNSP = vabNSP
060200071003     C*
060300101007     C* Valorizzo il buffer di scrittura del FIVAT
060400071003     C* - TIPO RECORD "E"
060500071003     C                   if        %subst(vindta:331:26) <> *blanks
060600071003     C                   eval      VATTRC = 'E'
060700130121     C***                eval      VATNOT = %trim(%subst(vindta:331:26))
060800071003     C                   write     FIVAT000
060900071003     C                   endif
061000071003     C*
061100071003     C                   ENDSR
061200020904
061300020904
061400010202     C*----------------------------------------------------*
061500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
061600010202     C*----------------------------------------------------*
061700020305     C     PREVAT        BEGSR
061800010202     C*
061900021113     C* Compongo il nome del membro da dare al FIVATWWR
062000010202     C                   eval      parmbr = vlrhdl
062100010202     C                   movel     'M'           parmbr
062200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
062300010202     C                   eval      paropz = '1'
062400010202     C* Effettuo la chiamata al CLLE preposto
062500050128     C                   call(e)   'TITVVTC'
062600010202     C                   parm                    parccm
062700010202     C                   parm                    parmbr
062800010202     C                   parm                    paropz
062900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
063000010202     C                   if        %error
063100010202     C                   movel     '1'           chkcall
063200010202     C                   else
063300010202     C                   movel     '0'           chkcall
063400010202     C                   endif
063500010202     C*
063600010202     C                   ENDSR
063700000801     C*----------------------------------------------------*
063800000801     C*  CONTROLLO NUMERICITA' CAMPI
063900000801     C*----------------------------------------------------*
064000000801     C     CHKNUM        BEGSR
064100081113     C*
064200081113     C                   IF        PiDecChr = *blanks
064300121214     C                   EVAL      PiDecChr = ','
064400081113     C                   ENDIF
064500081113     C*
064600081113     C                   callp(e)  UBISNUM_Check(PiStr
064700081113     C                                          :PiDecChr
064800081113     C                                          :PiVal
064900081113     C                                          :PiNum
065000081113     C                                          :PiInt)
065100081113     C*
065200000801     C                   IF        %error
065300000801     C                   EVAL      PiInt=*off
065400000801     C                   ENDIF
065500000801     C*
065600000801     C                   ENDSR
065700000801     C***
065800000801
065900011113
066000011113     C*----------------------------------------------------*
066100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066200011113     C*----------------------------------------------------*
066300011113     C     CHKIMPDIV     BEGSR
066400011113     C*
066500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066600011113     C                   Z-ADD     *zeros        wrkDec            9 9
066700011113     C*
066800011113     C* Come prima cosa effettuo considerazioni sulla divisa
066900011113     C                   IF        vabIAS > *zeros
067000011113     C                   IF        vabVAS <> 'EUR'
067100011113     C                   EVAL      vabVAS =  'ITL'
067200011113     C                   ENDIF
067300011113     C                   ENDIF
067400011113     C*
067500011113     C                   IF        vabCAS > *zeros
067600011113     C                   IF        vabVCA <> 'EUR'
067700011113     C                   EVAL      vabVCA =  'ITL'
067800011113     C                   ENDIF
067900011113     C                   ENDIF
068000011113     C*
068100011113     C                   IF        vabVMD > *zeros
068200020305     C                   IF        vabVAD <> 'EUR'
068300011113     C                   EVAL      vabVAD =  'ITL'
068400011113     C                   ENDIF
068500011113     C                   ENDIF
068600011113     C*
068700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068800011113     C                   Z-ADD     vabIAS        wrkDec
068900011113     C                   IF        wrkDec > *zeros
069000011113     C                   IF        vabVAS = 'ITL'
069100011113     C                   EVAL      vabIAS = *zeros
069200011113     C                   ENDIF
069300011113     C                   ENDIF
069400011113     C*
069500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069600011113     C                   Z-ADD     vabCAS        wrkDec
069700011113     C                   IF        wrkDec > *zeros
069800011113     C                   IF        vabVCA = 'ITL'
069900011113     C                   EVAL      vabCAS = *zeros
070000011113     C                   ENDIF
070100011113     C                   ENDIF
070200011113     C*
070300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070400011113     C                   Z-ADD     vabVMD        wrkDec
070500011113     C                   IF        wrkDec > *zeros
070600011113     C                   IF        vabVAD = 'ITL'
070700011113     C                   EVAL      vabVMD = *zeros
070800011113     C                   ENDIF
070900011113     C                   ENDIF
071000011113     C*
071100011113     C                   ENDSR
071200011113     C***
071300011113
071400011113
071500000801
071600000801
071700990920      /TITLE Invio dei dati al punto operativo.
071800010202     C     invio         BEGSR
071900990920     C*
072000021113     C* 1° invio FIVAT
072100010201     C                   reset                   dscmz
072200010201     C                   move      vlrpoi        cmzdst
072300021113     C                   eval      cmzfld = 'FIVATWWR'
072400010201     C                   eval      cmzmbd = vlrhdl
072500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072600021009     C***                if        prmfir = *blanks
072700021113     C                   eval      cmzfla = 'FIVAT00F'
072800021113     C                   eval      cmzmba = 'FIVAT00F'
072900021009     C***                else
073000021009     C***                eval      cmzfla = prmfir
073100021009     C***                eval      cmzmba = prmfir
073200021009     C***                endif
073300010201     C                   eval      cmznrr = *zeros
073400020305     C                   move      §ctrokvt      cmznrr
073500021018     C                   eval      cmzlba = vlrfl1
073600010201     C                   call(e)   'TIS711C'
073700010201     C                   parm                    dscmz
073800010201     C                   parm      *blanks       esito
073900010205     C                   if        %error
074000010205     C                             or cmzerr = '1'
074100010205     C                             or esito  = '1'
074200010205     C                   eval      wrkesito = '3'
074300010205     C                   else
074400010201     C*
074500021113     C* 2° invio FIVAB
074600010201     C                   reset                   dscmz
074700010201     C                   move      vlrpoi        cmzdst
074800010201     C                   eval      cmzfld = vlrfou
074900010201     C                   eval      cmzmbd = vlrhdl
075000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075100021009     C***                if        prmfir = *blanks
075200021113     C                   eval      cmzfla = 'FIVAB00F'
075300021113     C                   eval      cmzmba = 'FIVAB00F'
075400021009     C***                else
075500021009     C***                eval      cmzfla = prmfir
075600021009     C***                eval      cmzmba = prmfir
075700021009     C***                endif
075800010201     C                   eval      cmznrr = *zeros
075900010201     C                   move      §ctrokvb      cmznrr
076000021018     C                   eval      cmzlba = vlrfl1
076100010201     C                   call(e)   'TIS711C'
076200010201     C                   parm                    dscmz
076300010201     C                   parm      *blanks       esito
076400010201     C                   if        %error
076500010201     C                             or cmzerr = '1'
076600010201     C                             or esito  = '1'
076700010201     C                   eval      wrkesito = '3'
076800010201     C                   endif
076900010205     C                   endif
077000990920     C*
077100000613     C                   ENDSR
077200000613     C***
077300070411
077400070411     C     *pssr         BEGSR
077500070411     C*
077600070411     C                   if        %open(tivin00r)
077700070411     C                   close     tivin00r
077800070411     C                   endif
077900070411     C                   if        %open(fivabwwr)
078000070411     C                   close     fivabwwr
078100070411     C                   endif
078200070411     C                   if        %open(fivatwwr)
078300070411     C                   close     fivatwwr
078400070411     C                   endif
078500070411     C*
078600070411     C* Effettuo la chiamata al CLLE preposto
078700070411     C                   call(e)   'TITVVTC'
078800070411     C                   parm                    parccm
078900070411     C                   parm                    parmbr
079000070411     C                   parm      '2'           paropz
079100070411     C*
079200070411     C                   eval      wrkesito = '2'
079300070411     C*
079400070411     C                   seton                                        LR
079500070411     C*
079600070411     C                   ENDSR     '*CANCL'
079700070411     C***
079800070411
079900990910
080000000613     C     *inzsr        BEGSR
080100990910     C*
080200990910     C     *entry        plist
080300990920     C                   parm                    tivlrds
080400990921     C                   parm      wrkesito      esito
080500000724     C                   parm                    prmlit
080600000710     C                   parm                    prmfir
080700000613     C*
080800000830     C* CALCOLA LA DATA CORRENTE
080900100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
081000100324     C                   eval      datcor = %dec(%date() : *ISO)
081100000830     C*
081200000613     C                   ENDSR
081300000613     C***
