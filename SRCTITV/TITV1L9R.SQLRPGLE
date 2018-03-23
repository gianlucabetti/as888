000100090114      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400120320     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500090114     FFIVABwwr  O    E             DISK    usropn
000600090114     FFIVATwwr  O    E             DISK    usropn
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
002500120320     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002600090114     D curSped         s             20    INZ(*blanks)
002700090114     D depSped         s             20    INZ(*blanks)
002800120320
002900120320     D*------------------
003000120320     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003100120320     D*------------------
003200120320     D INPUT_DS        DS                  INZ
003300120320     D  VINDTA                     2048
003400120320     D  VINFLG                        1
003500120320     D  VINSPE                       20
003600120320     D  VINRRN                        8  0
003700120320     D*
003800120320
003900090114
004000090114
004100090114     D*------------------
004200090114     D* DS REPERIMENTO NUMERATORE
004300090114     D*------------------
004400090114     D trul33ds      e ds                  inz
004500090114     D*------------------
004600090114     D* DS ARCHITETTURA
004700090114     D*------------------
004800090114     D kpjba         e ds                  inz
004900090114
005000081217
005100081217     D*------------------
005200081217     D* LINKING A DEFINIZIONI ESTERNE
005300081217     D*------------------
005400081217     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005500081217     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005600081217
005700081217
005800010201
005900081217
006000081217     C*
006100081217     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006200081217     C
006300081217     C/EXEC SQL
006400081217     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006500081217     C/END-EXEC
006600081217     C
006700000913     C                   reset                   rrnum
006800990921     C                   reset                   esito
006900990921     C                   reset                   wrkesito
007000000613     C*
007100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
007200000613     C*
007300010202     C* Effettuo la chiamata al CLLE preposto
007400090114     C                   call(e)   'TITVVTC'
007500010202     C                   parm                    parccm
007600010202     C                   parm                    parmbr
007700010202     C                   parm      '2'           paropz
007800050201     C*
007900050201     C* Effettuo lancio TISI95 solo x chiusura
008000050201     C                   CLEAR                   TISI95DS
008100050201     C                   EVAL      I95TLA = 'C'
008200050201     C                   CALL      'TISI95R'
008300050201     C                   PARM                    TISI95DS
008400000616     C*
008500000801     C
008600010201     C                   seton                                        LR
008700990908
008800000801
008900910830     C*--------------------------------------------------------
009000090114     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009100910830     C*--------------------------------------------------------
009200040526     C     RWFILE        BEGSR
009300990910     C*
009400990914     C                   if        not %open(tivin00r)
009500990908     C                   open      tivin00r
009600990914     C                   endif
009700090114     C                   if        not %open(fivabwwr)
009800090114     C                   open      fivabwwr
009900990914     C                   endif
010000070103     C*
010100090114     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010200020305     C                   exsr      prevat
010300010201     C*
010400010202     C                   if        chkcall = '0'
010500010202     C*
010600090114     C                   if        not %open(fivatwwr)
010700090114     C                   open      fivatwwr
010800010201     C                   endif
010900990910     C*
011000010201     C                   clear                   §CTROKVB          5 0
011100020305     C                   clear                   §CTROKVT          5 0
011200000801     C                   clear                   §CTRMO            5 0
011300000801     C                   clear                   §CTRNO            5 0
011400120320     C*
011500120320     C/EXEC SQL
011600120320     C+ declare C1 cursor for select
011700120320     C+ vindta, vinflg, substr(vindta, 1, 20) as sped, rrn(tivin00r)
011800120320     C+ from tivin00r
011900120320     C+ order by sped
012000120320     C+ for read only
012100120320     C/END-EXEC
012200120320     C
012300120320     C/EXEC SQL
012400120320     C+ open C1
012500120320     C/END-EXEC
012600120320     C
012700120320     C/EXEC SQL
012800120320     C+ Fetch C1 into :INPUT_DS
012900120320     C/END-EXEC
013000120320     C*
013100120320     C                   dow       sqlcod = *zeros
013200050627     C                   if        vindta > *blanks
013300000613     C                   add       1             rrnum
013400000801     C*
013500120320     C                   if        vinflg = *blanks
013600120320     C                             or vinflg = '0'
013700120320     C                             or vinflg = '2'
013800000801     C*
013900120320     C                   clear                   x_vinmsg
014000120320     C                   eval      x_vinflg = '1'
014100070103     C*
014200070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
014300090114     C                   eval      curSped = %subst(vindta:1:20)
014400070213     C                   if        curSped <> depSped
014500070213     C* Se prima bolla => importo bolla corrente
014600070213     C                   if        §CTRMO = *zeros
014700090114     C                   exsr      repNSP
014800070213     C                   exsr      impvab                                       => carico  VAB
014900070213     C                   else
015000070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
015100070213     C                   exsr      wrivab                                       => scarico VAB
015200090114     C                   exsr      repNSP
015300070213     C                   exsr      impvab                                       => carico  VAB
015400070213     C                   endif
015500071121     C                   exsr      exevate                                      => write VAT-E
015600070103     C* Salvo il raggruppamento spedizione cliente corrente
015700070103     C                   eval      depSped = curSped
015800070213     C*
015900070213     C* Se collo successivo x stessa bolla
016000070213     C                   else
016100070213     C                   exsr      impvab                                       => carico  VAB
016200070213     C                   exsr      exevate                                      => write VAT-E
016300070103     C                   endif
016400000905     C*
016500000905     C                   else
016600120320     C                   eval      x_vinflg = '1'
016700050628     C                   endif
016800000905     C                   endif
016900120320     C*
017000120320     C     VINRRN        chain     tivin000
017100120320     C                   if        %found(tivin00r)
017200120320     C                   eval      y_vinflg = x_vinflg
017300120320     C                   eval      y_vinmsg = x_vinmsg
017400120320     C                   update    tivin000
017500120320     C                   endif
017600120320     C*
017700120320     C/EXEC SQL
017800120320     C+ Fetch C1 into :INPUT_DS
017900120320     C/END-EXEC
018000120320     C*
018100120320     C                   enddo
018200120320     C*
018300120320     C/EXEC SQL
018400120320     C+ close C1
018500120320     C/END-EXEC
018600070213     C*
018700070213     C* Scarico testata bolla rimasta in sospesa
018800070213     C                   exsr      wrivab                                       => scarico VAB
018900010202     C*
019000010202     C                   endif
019100990910
019200990910     C* Se non ci sono record con errori ...
019300000710     C                   if        §ctrno = 0
019400990910     C* ... restituisco esito OK.
019500990921     C                   eval      wrkesito = '0'
019600990910     C                   else
019700010201     C                   if        §ctrokvb > 0
019800990921     C                   eval      wrkesito = '1'
019900000710     C                   else
020000000710     C                   eval      wrkesito = '2'
020100990910     C                   endif
020200000710     C                   endif
020300990910     C*
020400990914     C                   if        %open(tivin00r)
020500990908     C                   close     tivin00r
020600990914     C                   endif
020700090114     C                   if        %open(fivabwwr)
020800090114     C                   close     fivabwwr
020900990914     C                   endif
021000090114     C                   if        %open(fivatwwr)
021100090114     C                   close     fivatwwr
021200010201     C                   endif
021300990910     C*
021400010201     C                   if        §ctrokvb > 0
021500000724     C                             and vlrpoi <> *zeros
021600010202     C                   exsr      invio
021700990920     C                   endif
021800990920     C*
021900910830     C                   ENDSR
022000000613     C***
022100010305
022200010305     C*----------------------------------------------------*
022300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022400010305     C*----------------------------------------------------*
022500020305     C     WRIVAB        BEGSR
022600010305     C*
022700060225     C* Quindi scarico il buffer del file d testata
022800090114     C                   write     fivab000                                     => scarico il VAB
022900010305     C*
023000010305     C                   ENDSR
023100990920
023200000801     C*----------------------------------------------------*
023300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023400000801     C*----------------------------------------------------*
023500010201     C     INZVAR        BEGSR
023600000801     C*
023700090114     C                   CLEAR                   FIVAB000
023800090114     C                   CLEAR                   FIVAT000
023900070213     C*
024000040802     C                   Z-ADD     *zeros        Num5_0            5 0
024100040802     C                   MOVEL     '0'           FlgCAS            1
024200000801     C*
024300000801     C                   ENDSR
024400000801     C*----------------------------------------------------*
024500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024600000801     C*----------------------------------------------------*
024700000801     C     DEFCAM        BEGSR
024800000801     C*
024900020619     C* Imposto i valori di default...
025000090114     C                   Z-ADD     0895020       VABCCM
025100090114     C                   Z-ADD     0895020       VATCCM
025200090114     C                   Z-ADD     089           VABLNP
025300090114     C                   Z-ADD     089           VATLNP
025400081217     C                   Z-ADD     000           VABCTR
025500090114     C                   MOVEL     'D'           VABTSP
025600090114     C                   EVAL      VABNAS = 'ABBIGLIAMENTO'
025700070103     C                   MOVEL     '7Q'          VABCTM
025800040823     C                   MOVEL     '1'           VABCBO
025900020619     C* ... e poi verifico se sono stati passati come parametri
026000020619     C                   IF        vlrppt > *blanks
026100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
026200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026300020619     C                   EXSR      CHKNUM
026400020619     C                   IF        PiInt=*on
026500020619     C                   Z-ADD     PiVal         VABCCM
026600020619     C                   Z-ADD     PiVal         VATCCM
026700020619     C                   ENDIF
026800040506     C                   ENDIF
026900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
027000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027100020619     C                   EXSR      CHKNUM
027200020619     C                   IF        PiInt=*on
027300020619     C                   Z-ADD     PiVal         VABLNP
027400020619     C                   Z-ADD     PiVal         VATLNP
027500040506     C                   ENDIF
027600020619     C                   ENDIF
027700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027900020619     C                   EXSR      CHKNUM
028000020619     C                   IF        PiInt=*on
028100020619     C                   Z-ADD     PiVal         VABCTR
028200040506     C                   ENDIF
028300020619     C                   ENDIF
028400020619     C                   ENDIF
028500000801     C*
028600000801     C                   ENDSR
028700090114     C*----------------------------------------------------*
028800090114     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
028900090114     C*----------------------------------------------------*
029000090114     C     REPNSP        BEGSR
029100090114     C*
029200090114     C                   EXSR      INZVAR
029300090114     C                   EXSR      DEFCAM
029400090114     C*
029500090114     C* NSP => Stacco un numeratore da AZNUM
029600090114     C                   clear                   TRUL33DS
029700090114     C                   eval      I33OPE = *zeros
029800090114     C                   eval      I33CNU = 302
029900090114     C                   eval      I33NUM = 1
030000090114     C                   movel     TRUL33DS      KPJBU
030100090114     C                   call      'TRUL33R'
030200090114     C                   parm                    KPJBA
030300090114     C                   movel     KPJBU         TRUL33DS
030400090114     C                   if        O33ERR = *zeros
030500090114     C                   z-add     O33NRF        VABNSP
030600090114     C                   z-add     O33NRF        VATNSP
030700090114     C                   else
030800090114     C                   SETON                                        31
030900120320     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
031000090114     C                             + ' ' + 'VABNSP VATNSP'
031100090114     C                   endif
031200090114     C*
031300090114     C                   ENDSR
031400000801     C*----------------------------------------------------*
031500090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031600000801     C*----------------------------------------------------*
031700040823     C     IMPVAB        BEGSR
031800010305     C*
031900000801     C                   Z-ADD     *zeros        errore            1 0
032000000830     C                   MOVEL     datcor        VABAAS
032100020305     C                   MOVEL     datcor        VATAAS
032200040526     C                   MOVE      datcor        VABMGS
032300040823     C                   MOVE(P)   vlrpoi        VABFGS
032400040823     C                   MOVE(P)   vlrpoi        VATFGS
032500080409     C*
032600080409     C* Reperimento campi ALFA
032700080409     C*
032800080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
032900090114     C                   EVAL      VABRSD=%trim(%subst(vindta:29:35))
033000080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033100080409     C     '@':'A'       XLATE     VABRSD        VABRSD
033200080409     C* ==
033300090114     C                   EVAL      VABRD2=%trim(%subst(vindta:64:35))
033400090114     C                   EVAL      VABIND=%trim(%subst(vindta:129:35))
033500090114     C                   EVAL      VABLOD=%trim(%subst(vindta:229:35))
033600090114     C                   EVAL      VABPRD=%trim(%subst(vindta:299:2))
033700090114     C                   EVAL      VABRMA=%trim(%subst(vindta:1:15))
033800090114     C                   EVAL      VABNOT=%trim(%subst(vindta:381:17))
033900090114     C                   EVAL      VABNT2=%trim(%subst(vindta:164:35))
034000080409     C*
034100080409     C* Reperimento campi NUMERICI
034200081217     C* RMN
034300090114     C                   EVAL      PiStr=%trim(%subst(vindta:16:5))
034400081217     C                   EXSR      CHKNUM
034500081217     C                   IF        PiInt=*on
034600081217     C                   Z-ADD     PiVal         VABRMN
034700081217     C                   ELSE
034800081217     C                   ADD       1             errore
034900081217     C                   Z-ADD     1             VABRMN
035000120320     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035100081217     C                             + ' ' + 'VABRMN'
035200081217     C                   ENDIF
035300080409     C* CAD
035400090114     C                   EVAL      PiStr=%trim(%subst(vindta:290:9))
035500090114     C                   EXSR      CHKNUM
035600090114     C                   IF        PiInt=*on
035700090114     C                   Z-ADD     PiVal         Num5_0
035800090114     C                   MOVEL(p)  Num5_0        VABCAD
035900090114     C                   ELSE
036000090114     C                   ADD       1             errore
036100090114     C                   EVAL      VABCAD = *zeros
036200120320     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036300090114     C                             + ' ' + 'VABCAD'
036400090114     C                   ENDIF
036500081217     C* NCL
036600090114     C                   EVAL      PiStr=%trim(%subst(vindta:351:5))
036700081217     C                   EXSR      CHKNUM
036800081217     C                   IF        PiInt=*on
036900081217     C                   Z-ADD     PiVal         VABNCL
037000081217     C                   ELSE
037100120320     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037200081217     C                             + ' ' + 'VABNCL'
037300081217     C                   ENDIF
037400081217     C* PKB
037500081217     C*
037600081217     C/EXEC SQL
037700120320     C+ declare C2 cursor for
037800090114     C+ select sum(decimal(substr(vindta, 373, 8))/1000) as peso
037900081217     C+ from tivin00r
038000090114     C+ where substr(vindta, 1, 20) = :curSped
038100081217     C+ for read only
038200081217     C/END-EXEC
038300081217     C
038400081217     C/EXEC SQL
038500120320     C+ open C2
038600081217     C/END-EXEC
038700081217     C                   IF        sqlcod <= *zeros
038800081217     C
038900081217     C/EXEC SQL
039000120320     C+ Fetch C2 into :VABPKB
039100081217     C/END-EXEC
039200081217     C
039300081217     C/EXEC SQL
039400120320     C+ close C2
039500081217     C/END-EXEC
039600081217     C*
039700081217     C                   ELSE
039800081217     C                   ADD       1             errore
039900120320     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040000081217     C                             + ' ' + 'VABPKB'
040100081217     C                   ENDIF
040200090114     C* VLB
040300090114     C*
040400090114     C/EXEC SQL
040500120320     C+ declare C3 cursor for
040600090114     C+ select sum(decimal(substr(vindta, 365, 8))/100000) as volume
040700090114     C+ from tivin00r
040800090114     C+ where substr(vindta, 1, 20) = :curSped
040900090114     C+ for read only
041000090114     C/END-EXEC
041100090114     C
041200090114     C/EXEC SQL
041300120320     C+ open C3
041400090114     C/END-EXEC
041500090114     C                   IF        sqlcod <= *zeros
041600090114     C
041700090114     C/EXEC SQL
041800120320     C+ Fetch C3 into :VABVLB
041900090114     C/END-EXEC
042000090114     C
042100090114     C/EXEC SQL
042200120320     C+ close C3
042300090114     C/END-EXEC
042400090114     C*
042500090114     C                   ELSE
042600090114     C                   ADD       1             errore
042700120320     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042800090114     C                             + ' ' + 'VABVLB'
042900090114     C                   ENDIF
043000071121     C*
043100080415     C* Carico l'estensioni A e B del FNVAT
043200080512     C                   exsr      exevata
043300080512     C                   exsr      exevatb
043400010205     C*
043500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
043600040802     C                   IF        FlgCAS <> '0'
043700010205     C                   EVAL      VABCBO = '4'
043800010205     C                   ENDIF
043900020305     C*
044000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044100011113     C                   EXSR      CHKIMPDIV
044200010202     C*
044300000801     C* Ebbene...
044400000801     C                   ADD       1             §CTRMO
044500010201     C                   IF        errore <> *zeros
044600000801     C                   ADD       1             §CTRNO
044700120320     C                   EVAL      vinflg = '2'
044800000801     C                   ELSE
044900010201     C                   ADD       1             §CTROKVB
045000000801     C                   ENDIF
045100000801     C*
045200000801     C                   ENDSR
045300070102     C*----------------------------------------------------*
045400090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045500070102     C*----------------------------------------------------*
045600070103     C     EXEVATE       BEGSR
045700070102     C*
045800070103     C                   EVAL      VATTRC='E'
045900120411     C                   EVAL      VATNOT=%trim(%subst(vindta:13:2))+
046000120411     C                                    %trim(%subst(vindta:10:1))+
046100120411     C                                    %trim(%subst(vindta:358:7))
046200120411     C***                EVAL      VATNOT=%trim(%subst(vindta:356:9))
046300120411     C***                IF        %subst(vindta:10:1) = '5'
046400120411     C***                EVAL      VATNOT=%subst(vindta:13:2)+'5'+
046500120411     C***                                 %trim(%subst(vindta:358:7))
046600120411     C***                ENDIF
046700120411     C***                IF        %subst(vindta:10:1) = '4'
046800120411     C***                EVAL      VATNOT=%subst(vindta:13:2)+'4'+
046900120411     C***                                 %trim(%subst(vindta:358:7))
047000110120     C*******            EVAL      VATNOT=%subst(vindta:13:2)+'40'+
047100110120     C*******                             %trim(%subst(vindta:359:6))
047200120411     C***                ENDIF
047300080512     C*
047400080512     C                   exsr      wrivat                                       => scarico VAT
047500070102     C*
047600070102     C                   ENDSR
047700080415     C*----------------------------------------------------*
047800090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
047900080415     C*----------------------------------------------------*
048000080415     C     EXEVATA       BEGSR
048100080415     C*
048200080415     C*
048300080415     C***                exsr      wrivat                                       => scarico VAT
048400080415     C*
048500080415     C                   ENDSR
048600071121     C*----------------------------------------------------*
048700090114     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
048800071121     C*----------------------------------------------------*
048900071121     C     EXEVATB       BEGSR
049000071121     C*
049100071121     C*
049200080409     C***                exsr      wrivat                                       => scarico VAT
049300071121     C*
049400071121     C                   ENDSR
049500010201     C*----------------------------------------------------*
049600090114     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
049700010201     C*----------------------------------------------------*
049800020305     C     WRIVAT        BEGSR
049900050628     C*
050000060223     C* Scrivo solo se valorizzato qualcosa
050100060223     C                   IF        VATNOT <> *blanks
050200090114     C                   WRITE     FIVAT000
050300060223     C                   ENDIF
050400010201     C*
050500010201     C                   ENDSR
050600010202     C*----------------------------------------------------*
050700090114     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050800010202     C*----------------------------------------------------*
050900020305     C     PREVAT        BEGSR
051000010202     C*
051100090114     C* Compongo il nome del membro da dare al FIVATWWR
051200010202     C                   eval      parmbr = vlrhdl
051300010202     C                   movel     'M'           parmbr
051400050627     C                   eval      parccm = %subst(vlrKSC:2:7)
051500010202     C                   eval      paropz = '1'
051600010202     C* Effettuo la chiamata al CLLE preposto
051700090114     C                   call(e)   'TITVVTC'
051800010202     C                   parm                    parccm
051900010202     C                   parm                    parmbr
052000010202     C                   parm                    paropz
052100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052200010202     C                   if        %error
052300010202     C                   movel     '1'           chkcall
052400010202     C                   else
052500010202     C                   movel     '0'           chkcall
052600010202     C                   endif
052700010202     C*
052800010202     C                   ENDSR
052900000801     C*----------------------------------------------------*
053000000801     C*  CONTROLLO NUMERICITA' CAMPI
053100000801     C*----------------------------------------------------*
053200000801     C     CHKNUM        BEGSR
053300081217     C*
053400081217     C                   IF        PiDecChr = *blanks
053500081217     C                   EVAL      PiDecChr = '.'
053600081217     C                   ENDIF
053700081217     C*
053800081217     C                   callp(e)  UBISNUM_Check(PiStr
053900081217     C                                          :PiDecChr
054000081217     C                                          :PiVal
054100081217     C                                          :PiNum
054200081217     C                                          :PiInt)
054300081217     C*
054400081217     C                   IF        %error
054500081217     C                   EVAL      PiInt=*off
054600081217     C                   ENDIF
054700000801     C*
054800000801     C                   ENDSR
054900000801     C***
055000000801
055100011113
055200011113     C*----------------------------------------------------*
055300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055400011113     C*----------------------------------------------------*
055500011113     C     CHKIMPDIV     BEGSR
055600011113     C*
055700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055800011113     C                   Z-ADD     *zeros        wrkDec            9 9
055900011113     C*
056000011113     C* Come prima cosa effettuo considerazioni sulla divisa
056100011113     C                   IF        vabIAS > *zeros
056200011113     C                   IF        vabVAS <> 'EUR'
056300011113     C                   EVAL      vabVAS =  'ITL'
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C                   IF        vabCAS > *zeros
056800011113     C                   IF        vabVCA <> 'EUR'
056900011113     C                   EVAL      vabVCA =  'ITL'
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C                   IF        vabVMD > *zeros
057400020305     C                   IF        vabVAD <> 'EUR'
057500011113     C                   EVAL      vabVAD =  'ITL'
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058000011113     C                   Z-ADD     vabIAS        wrkDec
058100011113     C                   IF        wrkDec > *zeros
058200011113     C                   IF        vabVAS = 'ITL'
058300011113     C                   EVAL      vabIAS = *zeros
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058800011113     C                   Z-ADD     vabCAS        wrkDec
058900011113     C                   IF        wrkDec > *zeros
059000011113     C                   IF        vabVCA = 'ITL'
059100011113     C                   EVAL      vabCAS = *zeros
059200011113     C                   ENDIF
059300011113     C                   ENDIF
059400011113     C*
059500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059600011113     C                   Z-ADD     vabVMD        wrkDec
059700011113     C                   IF        wrkDec > *zeros
059800011113     C                   IF        vabVAD = 'ITL'
059900011113     C                   EVAL      vabVMD = *zeros
060000011113     C                   ENDIF
060100011113     C                   ENDIF
060200011113     C*
060300011113     C                   ENDSR
060400011113     C***
060500011113
060600011113
060700000801
060800000801
060900990920      /TITLE Invio dei dati al punto operativo.
061000010202     C     invio         BEGSR
061100990920     C*
061200090114     C* 1° invio FIVAT
061300010201     C                   reset                   dscmz
061400010201     C                   move      vlrpoi        cmzdst
061500090114     C                   eval      cmzfld = 'FIVATWWR'
061600010201     C                   eval      cmzmbd = vlrhdl
061700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061800021009     C***                if        prmfir = *blanks
061900090114     C                   eval      cmzfla = 'FIVAT00F'
062000090114     C                   eval      cmzmba = 'FIVAT00F'
062100021009     C***                else
062200021009     C***                eval      cmzfla = prmfir
062300021009     C***                eval      cmzmba = prmfir
062400021009     C***                endif
062500010201     C                   eval      cmznrr = *zeros
062600020305     C                   move      §ctrokvt      cmznrr
062700021018     C                   eval      cmzlba = vlrfl1
062800010201     C                   call(e)   'TIS711C'
062900010201     C                   parm                    dscmz
063000010201     C                   parm      *blanks       esito
063100010205     C                   if        %error
063200010205     C                             or cmzerr = '1'
063300010205     C                             or esito  = '1'
063400010205     C                   eval      wrkesito = '3'
063500010205     C                   else
063600010201     C*
063700090114     C* 2° invio FIVAB
063800010201     C                   reset                   dscmz
063900010201     C                   move      vlrpoi        cmzdst
064000010201     C                   eval      cmzfld = vlrfou
064100010201     C                   eval      cmzmbd = vlrhdl
064200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064300021009     C***                if        prmfir = *blanks
064400090114     C                   eval      cmzfla = 'FIVAB00F'
064500090114     C                   eval      cmzmba = 'FIVAB00F'
064600021009     C***                else
064700021009     C***                eval      cmzfla = prmfir
064800021009     C***                eval      cmzmba = prmfir
064900021009     C***                endif
065000010201     C                   eval      cmznrr = *zeros
065100010201     C                   move      §ctrokvb      cmznrr
065200021018     C                   eval      cmzlba = vlrfl1
065300010201     C                   call(e)   'TIS711C'
065400010201     C                   parm                    dscmz
065500010201     C                   parm      *blanks       esito
065600010201     C                   if        %error
065700010201     C                             or cmzerr = '1'
065800010201     C                             or esito  = '1'
065900010201     C                   eval      wrkesito = '3'
066000010201     C                   endif
066100010205     C                   endif
066200990920     C*
066300000613     C                   ENDSR
066400000613     C***
066500070411
066600070411     C     *pssr         BEGSR
066700070411     C*
066800070411     C                   if        %open(tivin00r)
066900070411     C                   close     tivin00r
067000070411     C                   endif
067100090114     C                   if        %open(fivabwwr)
067200090114     C                   close     fivabwwr
067300070411     C                   endif
067400090114     C                   if        %open(fivatwwr)
067500090114     C                   close     fivatwwr
067600070411     C                   endif
067700070411     C*
067800070411     C* Effettuo la chiamata al CLLE preposto
067900090114     C                   call(e)   'TITVVTC'
068000070411     C                   parm                    parccm
068100070411     C                   parm                    parmbr
068200070411     C                   parm      '2'           paropz
068300070411     C*
068400070411     C                   eval      wrkesito = '2'
068500070411     C*
068600070411     C                   seton                                        LR
068700070411     C*
068800070411     C                   ENDSR     '*CANCL'
068900070411     C***
069000070411
069100990910
069200000613     C     *inzsr        BEGSR
069300990910     C*
069400990910     C     *entry        plist
069500990920     C                   parm                    tivlrds
069600990921     C                   parm      wrkesito      esito
069700000724     C                   parm                    prmlit
069800000710     C                   parm                    prmfir
069900000613     C*
070000000830     C* CALCOLA LA DATA CORRENTE
070100120320     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
070200120320     C                   eval      datcor = %dec(%date() : *ISO)
070300000830     C*
070400000613     C                   ENDSR
070500000613     C***
