000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800111003     Ftitv2i3p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000111003     Ftitv2i3ps O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200990908
001300000512     D*------------
001400000512     D* COMANDI
001500000512     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000801     D*----------------------------------------------------
001800000801     D* DICHIARAZIOINE VARIABILI DI WRK
001900000801     D*----------------------------------------------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300990920     D tivlrds       e ds                  extname(tivlr00f)
002400060307     D tisi95ds      e ds
002500990910     D esito           s              1
002600000724     D prmlit          s             10
002700000710     D prmfir          s             10
002800990921     D wrkesito        s                   like(esito)
002900990915     D wrkdata         s               d
003000990915     D wrkora          s               t
003100000613     D rrnum           s              6  0 INZ(*zeros)
003200000621     D recko           s            150    INZ(*blanks)
003300011113     D depcmd          s            150    INZ(*blanks)
003400010202     D parccm          s              8    INZ(*blanks)
003500010202     D parmbr          s             10    INZ(*blanks)
003600010202     D paropz          s              1    INZ(*blanks)
003700010202     D chkcall         s              1    INZ(*blanks)
003800070402     D curSped         s             25    INZ(*blanks)
003900070402     D depSped         s             25    INZ(*blanks)
004000100118
004100100118     D*------------------
004200100118     D* DS REPERIMENTO NUMERATORE
004300100118     D*------------------
004400100118     D trul33ds      e ds                  inz
004500100118     D*------------------
004600100118     D* DS ARCHITETTURA
004700100118     D*------------------
004800100118     D kpjba         e ds                  inz
004900100118
005000010201
005100010201
005200090127     C*
005300990915     C                   time                    wrkdata
005400990915     C                   time                    wrkora
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800000724     C*
005900000724     C* SE OCCORRE SPEDIRE IN FILIALE
006000000724     C                   if        vlrpoi <> *zeros
006100000724     C*
006200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006400000724     C     vlrpoi        chain     azorg01l
006500000724     C                   if        %found
006600000616     C                   movel(p)  CMD(1)        depcmd
006700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006800000616     C*
006900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007000011113     C                   Z-ADD     150           LENGH            15 5
007100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007200000616     C                   PARM                    depcmd
007300000616     C                   PARM                    LENGH
007400000724     C*
007500000724     C                   endif
007600000724     C                   endif
007700000616     C*
007800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900000616     C                   movel(p)  CMD(2)        depcmd
008000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008100000616     C*
008200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000616     C                   PARM                    depcmd
008600000616     C                   PARM                    LENGH
008700000616     C*
008800111003     C                   if        not %open(titv2i3ps)
008900111003     C                   open      titv2i3ps
009000000616     C                   except    testdett
009100000613     C                   endif
009200000613     C*
009300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009500000613     C*
009600010202     C* Effettuo la chiamata al CLLE preposto
009700040506     C                   call(e)   'TITVVTC'
009800010202     C                   parm                    parccm
009900010202     C                   parm                    parmbr
010000010202     C                   parm      '2'           paropz
010100050201     C*
010200050201     C* Effettuo lancio TISI95 solo x chiusura
010300050201     C                   CLEAR                   TISI95DS
010400050201     C                   EVAL      I95TLA = 'C'
010500050201     C                   CALL      'TISI95R'
010600050201     C                   PARM                    TISI95DS
010700010202     C*
010800111003     C                   if        %open(titv2i3ps)
010900000616     C                   except    findett
011000111003     C                   close     titv2i3ps
011100000613     C                   endif
011200000616     C*
011300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000616     C                   PARM                    CMD(3)
011700000616     C                   PARM                    LENGH
011800000616     C*
011900000801     C
012000010201     C                   seton                                        LR
012100000613
012200000613
012300000613     C*--------------------------------------------------------
012400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012500000613     C*--------------------------------------------------------
012600000621     C     STPR          BEGSR
012700000613     C*
012800111003     C                   if        not %open(titv2i3p)
012900111003     C                   open      titv2i3p
013000990915     C                   endif
013100990915     C*
013200990915     C                   except    riepilogo
013300990915     C*
013400111003     C                   if        %open(titv2i3p)
013500111003     C                   close     titv2i3p
013600990914     C                   endif
013700990910     C*
013800000613     C                   ENDSR
013900000613     C***
014000990908
014100000801
014200910830     C*--------------------------------------------------------
014300040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014400910830     C*--------------------------------------------------------
014500040526     C     RWFILE        BEGSR
014600990910     C*
014700990914     C                   if        not %open(tivin00r)
014800990908     C                   open      tivin00r
014900990914     C                   endif
015000021113     C                   if        not %open(fivabwwr)
015100021113     C                   open      fivabwwr
015200990914     C                   endif
015300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015400020305     C                   exsr      prevat
015500010201     C*
015600010202     C                   if        chkcall = '0'
015700010202     C*
015800021113     C                   if        not %open(fivatwwr)
015900021113     C                   open      fivatwwr
016000010201     C                   endif
016100990910     C*
016200010201     C                   clear                   §CTROKVB          5 0
016300020305     C                   clear                   §CTROKVT          5 0
016400000801     C                   clear                   §CTRMO            5 0
016500000801     C                   clear                   §CTRNO            5 0
016600990910     C*
016700921023     C                   DO        *HIVAL
016800990913     C*
016900990915     C                   READ      tivin00r                               70
017000050627     C                   if        vindta > *blanks
017100000613     C                   add       1             rrnum
017200000801     C*
017300000801     C                   if        *in70 = *off
017400000801     C                             and
017500000801     C                             (vinflg = *blanks
017600000801     C                              or vinflg = '0'
017700000801     C                              or vinflg = '2')
017800000801     C*
017900000801     C                   clear                   vinmsg
018000000801     C                   eval      vinflg = '1'
018100070402     C*
018200070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
018300070402     C                   eval      curSped = %subst(vindta:46:25)
018400070402     C                   if        curSped <> depSped
018500070402     C                   clear                   fivab000
018600070402     C                   clear                   fivat000
018700050628     C*
018800060315     C                   exsr      impvab                                       => carico  VAB
018900090127     C*
019000060315     C                   exsr      wrivab                                       => scarico VAB
019100070402     C*
019200070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
019300070402     C                   exsr      exevata
019400070402     C*
019500070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
019600070402     C                   exsr      exevatb
019700070402     C*
019800070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
019900070402     C                   exsr      exevate
020000070402     C*
020100070402     C* Salvo il raggruppamento spedizione cliente corrente
020200070402     C                   eval      depSped = curSped
020300070402     C*
020400070402     C                   else
020500070402     C*
020600070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
020700070402     C                   exsr      exevate
020800070402     C                   endif
020900000905     C*
021000000905     C                   else
021100000905     C                   eval      vinflg = '1'
021200050628     C                   endif
021300000905     C                   endif
021400000905     C*
021500000905     C  N70              update    tivin000
021600000905     C*
021700991022     C  N70              ENDdo
021800010202     C*
021900010202     C                   endif
022000990910
022100990910     C* Se non ci sono record con errori ...
022200000710     C                   if        §ctrno = 0
022300990910     C* ... restituisco esito OK.
022400990921     C                   eval      wrkesito = '0'
022500990910     C                   else
022600010201     C                   if        §ctrokvb > 0
022700990921     C                   eval      wrkesito = '1'
022800000710     C                   else
022900000710     C                   eval      wrkesito = '2'
023000990910     C                   endif
023100000710     C                   endif
023200990910     C*
023300990914     C                   if        %open(tivin00r)
023400990908     C                   close     tivin00r
023500990914     C                   endif
023600021113     C                   if        %open(fivabwwr)
023700021113     C                   close     fivabwwr
023800990914     C                   endif
023900021113     C                   if        %open(fivatwwr)
024000021113     C                   close     fivatwwr
024100010201     C                   endif
024200990910     C*
024300010201     C                   if        §ctrokvb > 0
024400000724     C                             and vlrpoi <> *zeros
024500010202     C                   exsr      invio
024600990920     C                   endif
024700990920     C*
024800910830     C                   ENDSR
024900000613     C***
025000010305
025100010305     C*----------------------------------------------------*
025200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025300010305     C*----------------------------------------------------*
025400090127     C     WRIVAB        BEGSR
025500090127     C*
025600090127     C* Quindi scarico il buffer del file d testata
025700090127     C                   write     fivab000                                     => scarico il VAB
025800010305     C*
025900010305     C                   ENDSR
026000990920
026100000801     C*----------------------------------------------------*
026200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026300000801     C*----------------------------------------------------*
026400010201     C     INZVAR        BEGSR
026500000801     C*
026600040802     C                   Z-ADD     *zeros        Num5_0            5 0
026700040802     C                   MOVEL     '0'           FlgCAS            1
026800000801     C*
026900000801     C                   ENDSR
027000000801     C*----------------------------------------------------*
027100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027200000801     C*----------------------------------------------------*
027300000801     C     DEFCAM        BEGSR
027400070718     C*
027500070718     C* Imposto i valori di default...
027600111003     C                   Z-ADD     0434461       VABCCM
027700111003     C                   Z-ADD     0434461       VATCCM
027800111003     C                   Z-ADD     043           VABLNP
027900111003     C                   Z-ADD     043           VATLNP
028000111003     C                   Z-ADD     000           VABCTR
028100070718     C                   MOVEL     '1'           VABCBO
028200070718     C                   MOVEL     '7Q'          VABCTM
028300020619     C* ... e poi verifico se sono stati passati come parametri
028400020619     C                   IF        vlrppt > *blanks
028500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028700020619     C                   EXSR      CHKNUM
028800020619     C                   IF        PiInt=*on
028900020619     C                   Z-ADD     PiVal         VABCCM
029000020619     C                   Z-ADD     PiVal         VATCCM
029100020619     C                   ENDIF
029200040506     C                   ENDIF
029300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029500020619     C                   EXSR      CHKNUM
029600020619     C                   IF        PiInt=*on
029700020619     C                   Z-ADD     PiVal         VABLNP
029800020619     C                   Z-ADD     PiVal         VATLNP
029900040506     C                   ENDIF
030000020619     C                   ENDIF
030100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
030200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030300020619     C                   EXSR      CHKNUM
030400020619     C                   IF        PiInt=*on
030500020619     C                   Z-ADD     PiVal         VABCTR
030600040506     C                   ENDIF
030700020619     C                   ENDIF
030800020619     C                   ENDIF
030900000801     C*
031000000801     C                   ENDSR
031100000801     C*----------------------------------------------------*
031200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031300000801     C*----------------------------------------------------*
031400040823     C     IMPVAB        BEGSR
031500040823     C*
031600020305     C                   EXSR      INZVAR
031700020305     C                   EXSR      DEFCAM
031800010305     C*
031900000801     C                   Z-ADD     *zeros        errore            1 0
032000000830     C                   MOVEL     datcor        VABAAS
032100020305     C                   MOVEL     datcor        VATAAS
032200040526     C                   MOVE      datcor        VABMGS
032300040823     C                   MOVE(P)   vlrpoi        VABFGS
032400040823     C                   MOVE(P)   vlrpoi        VATFGS
032500050628     C*
032600070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
032700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032900020117     C* ==
033000070719     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
033100090427     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
033200070102     C                                    %trim(%subst(vindta:341:30))+' '+
033300090427     C                                    %trim(%subst(vindta:371:5)))
033400070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
033500070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
033600111003     C***                EVAL      VABRMA=%trim(%subst(vindta:46:10))
033700111003     C                   EVAL      VABRMA=%trim(%subst(vindta:46:15))
033800070102     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
033900070102     C                   IF        %subst(vindta:43:1) = 'N'
034000070102     C                   EVAL      VABCBO='1'
034100070102     C                   ENDIF
034200070102     C                   IF        %subst(vindta:43:1) = 'P'
034300070102     C                   EVAL      VABCBO='2'
034400070102     C                   ENDIF
034500070102     C                   IF        %subst(vindta:511:3) = 'ABM'
034600070102     C                   EVAL      VABTIC='BM'
034700070102     C                   ENDIF
034800070102     C                   IF        %subst(vindta:511:3) = 'ABS'
034900070102     C                   EVAL      VABTIC='B'
035000070102     C                   ENDIF
035100070102     C                   IF        %subst(vindta:511:3) = 'CON'
035200070102     C                   EVAL      VABTIC=*blanks
035300070102     C                   ENDIF
035400070102     C                   IF        %subst(vindta:511:3) = 'ACM'
035500070102     C                   EVAL      VABTIC='CM'
035600070102     C                   ENDIF
035700100622     C                   IF        %subst(vindta:511:3) = 'ACS'
035800100622     C                   EVAL      VABTIC='CB'
035900100622     C                   ENDIF
036000100622     C                   IF        %subst(vindta:511:3) = 'VAR'
036100100622     C                   EVAL      VABTIC=*blanks
036200100622     C                   ENDIF
036300050628     C* CAP
036400070719     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
036500010201     C                   EXSR      CHKNUM
036600010201     C                   IF        PiInt=*on
036700010201     C                   Z-ADD     PiVal         Num5_0
036800040506     C                   MOVEL(P)  Num5_0        VABCAD
036900010201     C                   ELSE
037000040506     C                   ADD       1             errore
037100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037200040506     C                             + ' ' + 'VABCAD'
037300010201     C                   ENDIF
037400040506     C* Reperisco la provincia dal CAP e dalla località
037500040526     C                   IF        VABCAD <> *blanks AND
037600040526     C                             VABPRD  = *blanks
037700040506     C                   CLEAR                   TISI95DS
037800040506     C                   EVAL      I95TCN = '3'
037900040506     C                   Z-ADD     datcor        I95DAT
038000040506     C                   EVAL      I95CAP = VABCAD
038100040506     C                   EVAL      I95LOC = VABLOD
038200050627     C                   EVAL      I95NAR = VABNZD
038300040506     C                   CALL      'TISI95R'
038400040506     C                   PARM                    TISI95DS
038500040506     C                   EVAL      VABPRD = O95PRV
038600040506     C                   ENDIF
038700060307     C* NCL
038800070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
038900060307     C                   EXSR      CHKNUM
039000060307     C                   IF        PiInt=*on
039100060307     C                   ADD       PiVal         VABNCL
039200060307     C                   ELSE
039300060307     C                   ADD       1             errore
039400060307     C                   EVAL      vinmsg = %trimr(vinmsg)
039500060307     C                             + ' ' + 'VABNCL'
039600060307     C                   ENDIF
039700040506     C* PKB
039800070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
039900010201     C                   EXSR      CHKNUM
040000010201     C                   IF        PiNum=*on
040100070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
040200060316     C                   Z-ADD     PiVal         VABPKB
040300010201     C                   ELSE
040400010201     C                   ADD       1             errore
040500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040600010201     C                             + ' ' + 'VABPKB'
040700010201     C                   ENDIF
040800100118     C* NSP => Stacco un numeratore da AZNUM
040900100118     C                   clear                   TRUL33DS
041000100118     C                   eval      I33OPE = *zeros
041100100118     C                   eval      I33CNU = 302
041200100118     C                   eval      I33NUM = 1
041300100118     C                   movel     TRUL33DS      KPJBU
041400100118     C                   call      'TRUL33R'
041500100118     C                   parm                    KPJBA
041600100118     C                   movel     KPJBU         TRUL33DS
041700100118     C                   if        O33ERR = *zeros
041800100118     C                   z-add     O33NRF        VABNSP
041900100118     C                   z-add     O33NRF        VATNSP
042000100118     C                   else
042100100118     C                   ADD       1             errore
042200100118     C                   EVAL      vinmsg = %trimr(vinmsg)
042300100118     C                             + ' ' + 'VABNSP VATNSP'
042400100118     C                   endif
042500090511     C* RMN
042600100913     C***                EVAL      VABRMN = VABNSP
042700100913     C                   EVAL      PiStr=%trim(%subst(vindta:46+10:15))
042800091210     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
042900100726     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
043000100913     C                   EXSR      CHKNUM
043100100913     C                   IF        PiInt=*on
043200100913     C                   Z-ADD     PiVal         VABRMN
043300100913     C                   ELSE
043400100913     C                   ADD       1             errore
043500100913     C                   EVAL      vinmsg = %trimr(vinmsg)
043600100913     C                             + ' ' + 'VABRMN'
043700100913     C                   ENDIF
043800060307     C* CAS
043900090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
044000090608     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
044100090608     C                             %trim(%subst(vindta:502:9))<>*zeros
044200070102     C                   EVAL      VABVCA = 'EUR'
044300060307     C                   EVAL      FlgCAS = '1'
044400070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
044500060307     C                   EXSR      CHKNUM
044600060307     C                   IF        PiNum=*on
044700060316     C                   Z-ADD     PiVal         VABCAS
044800060307     C                   ELSE
044900060307     C                   ADD       1             errore
045000060307     C                   EVAL      vinmsg = %trimr(vinmsg)
045100060307     C                             + ' ' + 'VABCAS'
045200060307     C                   ENDIF
045300070102     C                   ENDIF
045400060316     C* IAS
045500070719     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
045600070102     C                   EVAL      VABVAS = 'EUR'
045700070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
045800060316     C                   EXSR      CHKNUM
045900060316     C                   IF        PiNum=*on
046000060316     C                   Z-ADD     PiVal         VABIAS
046100060316     C                   ELSE
046200060316     C                   ADD       1             errore
046300060316     C                   EVAL      vinmsg = %trimr(vinmsg)
046400060316     C                             + ' ' + 'VABIAS'
046500070102     C                   ENDIF
046600060316     C                   ENDIF
046700010205     C*
046800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
046900040802     C                   IF        FlgCAS <> '0'
047000070102     C                   IF        VABCBO = '1'
047100010205     C                   EVAL      VABCBO = '4'
047200010205     C                   ELSE
047300070102     C                   EVAL      VABCBO = '6'
047400070102     C                   ENDIF
047500010205     C                   ENDIF
047600020305     C*
047700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047800011113     C                   EXSR      CHKIMPDIV
047900010202     C*
048000000801     C* Ebbene...
048100000801     C                   ADD       1             §CTRMO
048200010201     C                   IF        errore <> *zeros
048300000801     C                   ADD       1             §CTRNO
048400000801     C                   EVAL      recko = vindta
048500000801     C                   EXCEPT    rigadett
048600000801     C                   EVAL      vinflg = '2'
048700000801     C                   ELSE
048800010201     C                   ADD       1             §CTROKVB
048900000801     C                   ENDIF
049000000801     C*
049100000801     C                   ENDSR
049200070102     C*----------------------------------------------------*
049300070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
049400070102     C*----------------------------------------------------*
049500070102     C     EXEVATA       BEGSR
049600070102     C*
049700111003     C***                EXSR      INZVAR
049800111003     C***                EXSR      DEFCAM
049900070102     C*
050000100722     C***                EVAL      VATTRC='A'
050100100722     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
050200100722     C***                exsr      wrivat                                       => scarico VAT
050300070102     C*
050400070102     C                   ENDSR
050500060307     C*----------------------------------------------------*
050600060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
050700060307     C*----------------------------------------------------*
050800060307     C     EXEVATB       BEGSR
050900060307     C*
051000111003     C***                EXSR      INZVAR
051100111003     C***                EXSR      DEFCAM
051200060307     C*
051300111003     C***                EVAL      VATTRC='B'
051400111003     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
051500111003     C***                exsr      wrivat                                       => scarico VAT
051600060307     C*
051700060307     C                   ENDSR
051800070402     C*----------------------------------------------------*
051900070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
052000070402     C*----------------------------------------------------*
052100070402     C     EXEVATE       BEGSR
052200070402     C*
052300070402     C                   EXSR      INZVAR
052400070402     C                   EXSR      DEFCAM
052500070402     C*
052600070402     C                   EVAL      VATTRC='E'
052700070402     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
052800070402     C                   exsr      wrivat                                       => scarico VAT
052900070402     C*
053000070402     C                   ENDSR
053100010201     C*----------------------------------------------------*
053200040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
053300010201     C*----------------------------------------------------*
053400020305     C     WRIVAT        BEGSR
053500050628     C*
053600060223     C* Scrivo solo se valorizzato qualcosa
053700060223     C                   IF        VATNOT <> *blanks
053800040802     C                   WRITE     FIVAT000
053900060223     C                   ENDIF
054000010201     C*
054100010201     C                   ENDSR
054200010202     C*----------------------------------------------------*
054300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054400010202     C*----------------------------------------------------*
054500020305     C     PREVAT        BEGSR
054600010202     C*
054700021113     C* Compongo il nome del membro da dare al FIVATWWR
054800010202     C                   eval      parmbr = vlrhdl
054900010202     C                   movel     'M'           parmbr
055000050627     C                   eval      parccm = %subst(vlrKSC:2:7)
055100010202     C                   eval      paropz = '1'
055200010202     C* Effettuo la chiamata al CLLE preposto
055300040506     C                   call(e)   'TITVVTC'
055400010202     C                   parm                    parccm
055500010202     C                   parm                    parmbr
055600010202     C                   parm                    paropz
055700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055800010202     C                   if        %error
055900010202     C                   movel     '1'           chkcall
056000010202     C                   else
056100010202     C                   movel     '0'           chkcall
056200010202     C                   endif
056300010202     C*
056400010202     C                   ENDSR
056500000801     C*----------------------------------------------------*
056600000801     C*  CONTROLLO NUMERICITA' CAMPI
056700000801     C*----------------------------------------------------*
056800000801     C     CHKNUM        BEGSR
056900000801     C*
057000000801     C                   call(e)   'ISNUMERIC'
057100000801     C                   PARM                    PiStr            30
057200070102     C                   PARM      '.'           PiDecChr          1
057300000801     C                   PARM      *ZEROS        PiVal            30 9
057400000801     C                   PARM      '0'           PiInt             1
057500000801     C                   PARM      '0'           PiNum             1
057600000801     C                   IF        %error
057700000801     C                   EVAL      PiInt=*off
057800000801     C                   ENDIF
057900000801     C*
058000000801     C                   ENDSR
058100000801     C***
058200000801
058300011113
058400011113     C*----------------------------------------------------*
058500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058600011113     C*----------------------------------------------------*
058700011113     C     CHKIMPDIV     BEGSR
058800011113     C*
058900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059000011113     C                   Z-ADD     *zeros        wrkDec            9 9
059100011113     C*
059200011113     C* Come prima cosa effettuo considerazioni sulla divisa
059300011113     C                   IF        vabIAS > *zeros
059400011113     C                   IF        vabVAS <> 'EUR'
059500011113     C                   EVAL      vabVAS =  'ITL'
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C                   IF        vabCAS > *zeros
060000011113     C                   IF        vabVCA <> 'EUR'
060100011113     C                   EVAL      vabVCA =  'ITL'
060200011113     C                   ENDIF
060300011113     C                   ENDIF
060400011113     C*
060500011113     C                   IF        vabVMD > *zeros
060600020305     C                   IF        vabVAD <> 'EUR'
060700011113     C                   EVAL      vabVAD =  'ITL'
060800011113     C                   ENDIF
060900011113     C                   ENDIF
061000011113     C*
061100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061200011113     C                   Z-ADD     vabIAS        wrkDec
061300011113     C                   IF        wrkDec > *zeros
061400011113     C                   IF        vabVAS = 'ITL'
061500011113     C                   EVAL      vabIAS = *zeros
061600011113     C                   ENDIF
061700011113     C                   ENDIF
061800011113     C*
061900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062000011113     C                   Z-ADD     vabCAS        wrkDec
062100011113     C                   IF        wrkDec > *zeros
062200011113     C                   IF        vabVCA = 'ITL'
062300011113     C                   EVAL      vabCAS = *zeros
062400011113     C                   ENDIF
062500011113     C                   ENDIF
062600011113     C*
062700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062800011113     C                   Z-ADD     vabVMD        wrkDec
062900011113     C                   IF        wrkDec > *zeros
063000011113     C                   IF        vabVAD = 'ITL'
063100011113     C                   EVAL      vabVMD = *zeros
063200011113     C                   ENDIF
063300011113     C                   ENDIF
063400011113     C*
063500011113     C                   ENDSR
063600011113     C***
063700011113
063800011113
063900000801
064000000801
064100990920      /TITLE Invio dei dati al punto operativo.
064200010202     C     invio         BEGSR
064300990920     C*
064400021113     C* 1° invio FIVAT
064500010201     C                   reset                   dscmz
064600010201     C                   move      vlrpoi        cmzdst
064700021113     C                   eval      cmzfld = 'FIVATWWR'
064800010201     C                   eval      cmzmbd = vlrhdl
064900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065000021009     C***                if        prmfir = *blanks
065100021113     C                   eval      cmzfla = 'FIVAT00F'
065200021113     C                   eval      cmzmba = 'FIVAT00F'
065300021009     C***                else
065400021009     C***                eval      cmzfla = prmfir
065500021009     C***                eval      cmzmba = prmfir
065600021009     C***                endif
065700010201     C                   eval      cmznrr = *zeros
065800020305     C                   move      §ctrokvt      cmznrr
065900021018     C                   eval      cmzlba = vlrfl1
066000010201     C                   call(e)   'TIS711C'
066100010201     C                   parm                    dscmz
066200010201     C                   parm      *blanks       esito
066300010205     C                   if        %error
066400010205     C                             or cmzerr = '1'
066500010205     C                             or esito  = '1'
066600010205     C                   eval      wrkesito = '3'
066700010205     C                   else
066800010201     C*
066900021113     C* 2° invio FIVAB
067000010201     C                   reset                   dscmz
067100010201     C                   move      vlrpoi        cmzdst
067200010201     C                   eval      cmzfld = vlrfou
067300010201     C                   eval      cmzmbd = vlrhdl
067400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067500021009     C***                if        prmfir = *blanks
067600021113     C                   eval      cmzfla = 'FIVAB00F'
067700021113     C                   eval      cmzmba = 'FIVAB00F'
067800021009     C***                else
067900021009     C***                eval      cmzfla = prmfir
068000021009     C***                eval      cmzmba = prmfir
068100021009     C***                endif
068200010201     C                   eval      cmznrr = *zeros
068300010201     C                   move      §ctrokvb      cmznrr
068400021018     C                   eval      cmzlba = vlrfl1
068500010201     C                   call(e)   'TIS711C'
068600010201     C                   parm                    dscmz
068700010201     C                   parm      *blanks       esito
068800010201     C                   if        %error
068900010201     C                             or cmzerr = '1'
069000010201     C                             or esito  = '1'
069100010201     C                   eval      wrkesito = '3'
069200010201     C                   endif
069300010205     C                   endif
069400990920     C*
069500000613     C                   ENDSR
069600000613     C***
069700990910
069800000613     C     *inzsr        BEGSR
069900990910     C*
070000990910     C     *entry        plist
070100990920     C                   parm                    tivlrds
070200990921     C                   parm      wrkesito      esito
070300000724     C                   parm                    prmlit
070400000710     C                   parm                    prmfir
070500000613     C*
070600000830     C* CALCOLA LA DATA CORRENTE
070700100118     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
070800100118     C                   eval      datcor = %dec(%date() : *ISO)
070900000830     C*
071000000613     C                   ENDSR
071100000613     C***
071200990908
071300100722
071400100722
071500111003     Otitv2i3p  E            riepilogo         2
071600990915     O                                              'Upload via Internet'
071700990915     O                                           +1 'Traduzione TIVIN00R -'
071800021113     O                                           +1 'FIVABWWR/FIVATWWR'
071900010201     O                                           +1 'Report testate bolle'
072000990915     O          E            riepilogo   2
072100990915     O                       wrkdata
072200990915     O                       wrkora              +1
072300990915     O                       procname            +1
072400990915     O          E            riepilogo   2
072500990915     O                                              'Cliente..................:'
072600990915     O                       VABCCM        z     +1
072700990915     O          E            riepilogo   2
072800990920     O                                              'Riferimento Strategi.....:'
072900990920     O                       vlrhdl              +1
073000990915     O          E            riepilogo   2
073100990915     O                                              'Giusti...................:'
073200010201     O                       §CTROKVB      2   +  1
073300990915     O          E            riepilogo   2
073400990915     O                                              'Sbagliati e corretti.....:'
073500971022     O                       §CTRMO        2   +  1
073600990915     O          E            riepilogo   2
073700990915     O                                              'Sbagliati e scartati.....:'
073800971022     O                       §CTRNO        2   +  1
073900000613
074000111003     Otitv2i3ps E            testdett          2
074100000613     O                                              'Upload via Internet'
074200000613     O                                           +1 'Traduzione TIVIN00R -'
074300021113     O                                           +1 'FIVABWWR/FIVATWWR'
074400010201     O                                           +1 'Report testate bolle'
074500000616     O          E            testdett    3
074600000613     O                                           +2 'N° rec'
074700000613     O                                           +3 'Anteprima contenuto'
074800000616     O          E            rigadett    2
074900000613     O                       rrnum               +2
075000000621     O                       recko               +3
075100000616     O          E            findett     2
075200000613     O                       wrkdata
075300000613     O                       wrkora              +1
075400000613     O                       procname            +1
075500000616     O          E            findett     2
075600000613     O                                              'Cliente..................:'
075700000613     O                       VABCCM        z     +1
075800000616     O          E            findett     2
075900000613     O                                              'Riferimento Strategi.....:'
076000000613     O                       vlrhdl              +1
076100000616     O          E            findett     2
076200000613     O                                              'Giusti...................:'
076300010201     O                       §CTROKVB      2   +  1
076400000616     O          E            findett     2
076500000613     O                                              'Sbagliati e corretti.....:'
076600000613     O                       §CTRMO        2   +  1
076700000616     O          E            findett     2
076800000613     O                                              'Sbagliati e scartati.....:'
076900000613     O                       §CTRNO        2   +  1
077000000512** CMD - COMANDI CL
077100111003OVRPRTF FILE(TITV2I3P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
077200111003OVRPRTF FILE(TITV2I3PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
077300111003DLTOVR FILE(TITV2I3P TITV2I3PS) LVL(*)
077400000512
077500000512
