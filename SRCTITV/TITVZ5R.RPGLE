000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800070102     Ftitvw1p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070102     Ftitvw1ps  O    f  198        PRINTER usropn
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
003701070402     D curSped         s             25    INZ(*blanks)
003702070402     D depSped         s             25    INZ(*blanks)
003800000830
003900000830     D*------------------
004000000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004100000830     D*------------------
004200000830     D WLBDA8          DS                  INZ
004300000830     D  G08DAT                 1      8  0
004400000830     D  G08INV                 9     16  0
004500000830     D  G08ERR                17     17
004600000830     D  G08TGI                18     22  0
004700000830     D*
004800010201
004900010201
005000990915     C                   time                    wrkdata
005100990915     C                   time                    wrkora
005200000913     C                   reset                   rrnum
005300990921     C                   reset                   esito
005400990921     C                   reset                   wrkesito
005500000724     C*
005600000724     C* SE OCCORRE SPEDIRE IN FILIALE
005700000724     C                   if        vlrpoi <> *zeros
005800000724     C*
005900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006100000724     C     vlrpoi        chain     azorg01l
006200000724     C                   if        %found
006300000616     C                   movel(p)  CMD(1)        depcmd
006400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006500000616     C*
006600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006700011113     C                   Z-ADD     150           LENGH            15 5
006800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
006900000616     C                   PARM                    depcmd
007000000616     C                   PARM                    LENGH
007100000724     C*
007200000724     C                   endif
007300000724     C                   endif
007400000616     C*
007500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007600000616     C                   movel(p)  CMD(2)        depcmd
007700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
007800000616     C*
007900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008000011113     C                   Z-ADD     150           LENGH            15 5
008100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200000616     C                   PARM                    depcmd
008300000616     C                   PARM                    LENGH
008400000616     C*
008500070102     C                   if        not %open(titvw1ps)
008600070102     C                   open      titvw1ps
008700000616     C                   except    testdett
008800000613     C                   endif
008900000613     C*
009000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009200000613     C*
009300010202     C* Effettuo la chiamata al CLLE preposto
009400040506     C                   call(e)   'TITVVTC'
009500010202     C                   parm                    parccm
009600010202     C                   parm                    parmbr
009700010202     C                   parm      '2'           paropz
009800050201     C*
009900050201     C* Effettuo lancio TISI95 solo x chiusura
010000050201     C                   CLEAR                   TISI95DS
010100050201     C                   EVAL      I95TLA = 'C'
010200050201     C                   CALL      'TISI95R'
010300050201     C                   PARM                    TISI95DS
010400010202     C*
010500070102     C                   if        %open(titvw1ps)
010600000616     C                   except    findett
010700070102     C                   close     titvw1ps
010800000613     C                   endif
010900000616     C*
011000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011100011113     C                   Z-ADD     150           LENGH            15 5
011200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011300000616     C                   PARM                    CMD(3)
011400000616     C                   PARM                    LENGH
011500000616     C*
011600000801     C
011700010201     C                   seton                                        LR
011800000613
011900000613
012000000613     C*--------------------------------------------------------
012100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012200000613     C*--------------------------------------------------------
012300000621     C     STPR          BEGSR
012400000613     C*
012500070102     C                   if        not %open(titvw1p)
012600070102     C                   open      titvw1p
012700990915     C                   endif
012800990915     C*
012900990915     C                   except    riepilogo
013000990915     C*
013100070102     C                   if        %open(titvw1p)
013200070102     C                   close     titvw1p
013300990914     C                   endif
013400990910     C*
013500000613     C                   ENDSR
013600000613     C***
013700990908
013800000801
013900910830     C*--------------------------------------------------------
014000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014100910830     C*--------------------------------------------------------
014200040526     C     RWFILE        BEGSR
014300990910     C*
014400990914     C                   if        not %open(tivin00r)
014500990908     C                   open      tivin00r
014600990914     C                   endif
014700021113     C                   if        not %open(fivabwwr)
014800021113     C                   open      fivabwwr
014900990914     C                   endif
015000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015100020305     C                   exsr      prevat
015200010201     C*
015300010202     C                   if        chkcall = '0'
015400010202     C*
015500021113     C                   if        not %open(fivatwwr)
015600021113     C                   open      fivatwwr
015700010201     C                   endif
015800990910     C*
015900010201     C                   clear                   §CTROKVB          5 0
016000020305     C                   clear                   §CTROKVT          5 0
016100000801     C                   clear                   §CTRMO            5 0
016200000801     C                   clear                   §CTRNO            5 0
016300990910     C*
016400921023     C                   DO        *HIVAL
016500990913     C*
016600990915     C                   READ      tivin00r                               70
016700050627     C                   if        vindta > *blanks
016800000613     C                   add       1             rrnum
016900000801     C*
017000000801     C                   if        *in70 = *off
017100000801     C                             and
017200000801     C                             (vinflg = *blanks
017300000801     C                              or vinflg = '0'
017400000801     C                              or vinflg = '2')
017500000801     C*
017600000801     C                   clear                   vinmsg
017700000801     C                   eval      vinflg = '1'
018001070402     C*
018002070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
018003070402     C                   eval      curSped = %subst(vindta:46:25)
018004070402     C                   if        curSped <> depSped
018005070402     C                   clear                   fivab000
018006070402     C                   clear                   fivat000
018100050628     C*
018200060315     C                   exsr      impvab                                       => carico  VAB
018300060315     C                   exsr      wrivab                                       => scarico VAB
018302070402     C*
018303070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
018304070402     C                   exsr      exevata
018305070402     C*
018306070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
018307070402     C                   exsr      exevatb
018308070402     C*
018309070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
018310070402     C                   exsr      exevate
018311070402     C*
018312070402     C* Salvo il raggruppamento spedizione cliente corrente
018313070402     C                   eval      depSped = curSped
018314070402     C*
018315070402     C                   else
018317070402     C*
018318070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
018319070402     C                   exsr      exevate
018320070402     C                   endif
018400000905     C*
018500000905     C                   else
018600000905     C                   eval      vinflg = '1'
018700050628     C                   endif
018800000905     C                   endif
018900000905     C*
019000000905     C  N70              update    tivin000
019100000905     C*
019200991022     C  N70              ENDdo
019300010202     C*
019400010202     C                   endif
019500990910
019600990910     C* Se non ci sono record con errori ...
019700000710     C                   if        §ctrno = 0
019800990910     C* ... restituisco esito OK.
019900990921     C                   eval      wrkesito = '0'
020000990910     C                   else
020100010201     C                   if        §ctrokvb > 0
020200990921     C                   eval      wrkesito = '1'
020300000710     C                   else
020400000710     C                   eval      wrkesito = '2'
020500990910     C                   endif
020600000710     C                   endif
020700990910     C*
020800990914     C                   if        %open(tivin00r)
020900990908     C                   close     tivin00r
021000990914     C                   endif
021100021113     C                   if        %open(fivabwwr)
021200021113     C                   close     fivabwwr
021300990914     C                   endif
021400021113     C                   if        %open(fivatwwr)
021500021113     C                   close     fivatwwr
021600010201     C                   endif
021700990910     C*
021800010201     C                   if        §ctrokvb > 0
021900000724     C                             and vlrpoi <> *zeros
022000010202     C                   exsr      invio
022100990920     C                   endif
022200990920     C*
022300910830     C                   ENDSR
022400000613     C***
022500010305
022600010305     C*----------------------------------------------------*
022700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022800010305     C*----------------------------------------------------*
022900020305     C     WRIVAB        BEGSR
023000010305     C*
023100060225     C* Quindi scarico il buffer del file d testata
023200021113     C                   write     fivab000                                     => scarico il VAB
023300010305     C*
023400010305     C                   ENDSR
023500990920
023600000801     C*----------------------------------------------------*
023700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023800000801     C*----------------------------------------------------*
023900010201     C     INZVAR        BEGSR
024000000801     C*
024100040802     C                   Z-ADD     *zeros        Num5_0            5 0
024200040802     C                   MOVEL     '0'           FlgCAS            1
024300000801     C*
024400000801     C                   ENDSR
024500000801     C*----------------------------------------------------*
024600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024700000801     C*----------------------------------------------------*
024800000801     C     DEFCAM        BEGSR
024801070718     C*
024802070718     C* Imposto i valori di default...
024803070718     C                   Z-ADD     0250477       VABCCM
024804070718     C                   Z-ADD     0250477       VATCCM
024805070718     C                   Z-ADD     025           VABLNP
024806070718     C                   Z-ADD     025           VATLNP
024807070718     C                   Z-ADD     000           VABCTR
024808070718     C                   MOVEL     '1'           VABCBO
024809070718     C                   MOVEL     '7Q'          VABCTM
025700020619     C* ... e poi verifico se sono stati passati come parametri
025800020619     C                   IF        vlrppt > *blanks
025900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
026000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026100020619     C                   EXSR      CHKNUM
026200020619     C                   IF        PiInt=*on
026300020619     C                   Z-ADD     PiVal         VABCCM
026400020619     C                   Z-ADD     PiVal         VATCCM
026500020619     C                   ENDIF
026600040506     C                   ENDIF
026700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
026800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026900020619     C                   EXSR      CHKNUM
027000020619     C                   IF        PiInt=*on
027100020619     C                   Z-ADD     PiVal         VABLNP
027200020619     C                   Z-ADD     PiVal         VATLNP
027300040506     C                   ENDIF
027400020619     C                   ENDIF
027500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027700020619     C                   EXSR      CHKNUM
027800020619     C                   IF        PiInt=*on
027900020619     C                   Z-ADD     PiVal         VABCTR
028000040506     C                   ENDIF
028100020619     C                   ENDIF
028200020619     C                   ENDIF
028300000801     C*
028400000801     C                   ENDSR
028500000801     C*----------------------------------------------------*
028600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028700000801     C*----------------------------------------------------*
028800040823     C     IMPVAB        BEGSR
028900040823     C*
029000020305     C                   EXSR      INZVAR
029100020305     C                   EXSR      DEFCAM
029200010305     C*
029300000801     C                   Z-ADD     *zeros        errore            1 0
029400000830     C                   MOVEL     datcor        VABAAS
029500020305     C                   MOVEL     datcor        VATAAS
029600040526     C                   MOVE      datcor        VABMGS
029700040823     C                   MOVE(P)   vlrpoi        VABFGS
029800040823     C                   MOVE(P)   vlrpoi        VATFGS
029900050628     C*
030000070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
030100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030300020117     C* ==
030301070719     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
030400070102     C                   EVAL      VABIND=%trim(%subst(vindta:336:5))+' '+
030500070102     C                                    %trim(%subst(vindta:341:30))+' '+
030600070102     C                                    %trim(%subst(vindta:371:5))
030700070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
030800070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
030900070102     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
031000070102     C                   EVAL      VABNAS=%trim(%subst(vindta:581:30))
031100070102     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
031200070102     C                   IF        %subst(vindta:43:1) = 'N'
031300070102     C                   EVAL      VABCBO='1'
031400070102     C                   ENDIF
031500070102     C                   IF        %subst(vindta:43:1) = 'P'
031600070102     C                   EVAL      VABCBO='2'
031700070102     C                   ENDIF
031800070102     C                   IF        %subst(vindta:511:3) = 'ABM'
031900070102     C                   EVAL      VABTIC='BM'
032000070102     C                   ENDIF
032100070102     C                   IF        %subst(vindta:511:3) = 'ABS'
032200070102     C                   EVAL      VABTIC='B'
032300070102     C                   ENDIF
032400070102     C                   IF        %subst(vindta:511:3) = 'CON'
032500070102     C                   EVAL      VABTIC=*blanks
032600070102     C                   ENDIF
032700070102     C                   IF        %subst(vindta:511:3) = 'ACM'
032800070102     C                   EVAL      VABTIC='CM'
032900070102     C                   ENDIF
032901070718     C* CCM
032902070718     C                   EVAL      PiStr=%trim(%subst(vindta:1:12))
032903070718     C                   EXSR      CHKNUM
032904070718     C                   IF        PiInt=*on
032905070718     C                   Z-ADD     PiVal         VABCCM
032906070718     C                   ELSE
032907070718     C                   SETON                                        32
032908070718     C                   EVAL      vinmsg = %trimr(vinmsg)
032909070718     C                             + ' ' + 'VABCCM'
032910070718     C                   ENDIF
033000050628     C* CAP
033100070719     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
033200010201     C                   EXSR      CHKNUM
033300010201     C                   IF        PiInt=*on
033400010201     C                   Z-ADD     PiVal         Num5_0
033500040506     C                   MOVEL(P)  Num5_0        VABCAD
033600010201     C                   ELSE
033700040506     C                   ADD       1             errore
033800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033900040506     C                             + ' ' + 'VABCAD'
034000010201     C                   ENDIF
034100040506     C* Reperisco la provincia dal CAP e dalla località
034200040526     C                   IF        VABCAD <> *blanks AND
034300040526     C                             VABPRD  = *blanks
034400040506     C                   CLEAR                   TISI95DS
034500040506     C                   EVAL      I95TCN = '3'
034600040506     C                   Z-ADD     datcor        I95DAT
034700040506     C                   EVAL      I95CAP = VABCAD
034800040506     C                   EVAL      I95LOC = VABLOD
034900050627     C                   EVAL      I95NAR = VABNZD
035000040506     C                   CALL      'TISI95R'
035100040506     C                   PARM                    TISI95DS
035200040506     C                   EVAL      VABPRD = O95PRV
035300040506     C                   ENDIF
035400060307     C* NCL
035500070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
035600060307     C                   EXSR      CHKNUM
035700060307     C                   IF        PiInt=*on
035800060307     C                   ADD       PiVal         VABNCL
035900060307     C                   ELSE
036000060307     C                   ADD       1             errore
036100060307     C                   EVAL      vinmsg = %trimr(vinmsg)
036200060307     C                             + ' ' + 'VABNCL'
036300060307     C                   ENDIF
036400040506     C* PKB
036500070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
036600010201     C                   EXSR      CHKNUM
036700010201     C                   IF        PiNum=*on
036800070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
036900060316     C                   Z-ADD     PiVal         VABPKB
037000010201     C                   ELSE
037100010201     C                   ADD       1             errore
037200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037300010201     C                             + ' ' + 'VABPKB'
037400010201     C                   ENDIF
037500060316     C* NSP/RMN
037600070719     C                   EVAL      PiStr=%trim(%subst(vindta:13:30))
037700050928     C                   EXSR      CHKNUM
037800050928     C                   IF        PiInt=*on
037900060316     C                   Z-ADD     PiVal         VABNSP
038000060316     C                   Z-ADD     PiVal         VATNSP
038100050928     C                   Z-ADD     PiVal         VABRMN
038200050928     C                   ELSE
038300050928     C                   ADD       1             errore
038400050928     C                   EVAL      vinmsg = %trimr(vinmsg)
038500060316     C                             + ' ' + 'VABNSP VATNSP VABRMN'
038600050928     C                   ENDIF
038700060307     C* CAS
038800070719     C                   IF        %trim(%subst(vindta:502:9)) <> '000000.00'
038900070102     C                   EVAL      VABVCA = 'EUR'
039000060307     C                   EVAL      FlgCAS = '1'
039100070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
039200060307     C                   EXSR      CHKNUM
039300060307     C                   IF        PiNum=*on
039400060316     C                   Z-ADD     PiVal         VABCAS
039500060307     C                   ELSE
039600060307     C                   ADD       1             errore
039700060307     C                   EVAL      vinmsg = %trimr(vinmsg)
039800060307     C                             + ' ' + 'VABCAS'
039900060307     C                   ENDIF
040000070102     C                   ENDIF
040100060316     C* IAS
040200070719     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
040300070102     C                   EVAL      VABVAS = 'EUR'
040400070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
040500060316     C                   EXSR      CHKNUM
040600060316     C                   IF        PiNum=*on
040700060316     C                   Z-ADD     PiVal         VABIAS
040800060316     C                   ELSE
040900060316     C                   ADD       1             errore
041000060316     C                   EVAL      vinmsg = %trimr(vinmsg)
041100060316     C                             + ' ' + 'VABIAS'
041200070102     C                   ENDIF
041300060316     C                   ENDIF
042000010205     C*
042100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042200040802     C                   IF        FlgCAS <> '0'
042300070102     C                   IF        VABCBO = '1'
042400010205     C                   EVAL      VABCBO = '4'
042500010205     C                   ELSE
042600070102     C                   EVAL      VABCBO = '6'
042700070102     C                   ENDIF
042800010205     C                   ENDIF
042900020305     C*
043000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043100011113     C                   EXSR      CHKIMPDIV
043200010202     C*
043300000801     C* Ebbene...
043400000801     C                   ADD       1             §CTRMO
043500010201     C                   IF        errore <> *zeros
043600000801     C                   ADD       1             §CTRNO
043700000801     C                   EVAL      recko = vindta
043800000801     C                   EXCEPT    rigadett
043900000801     C                   EVAL      vinflg = '2'
044000000801     C                   ELSE
044100010201     C                   ADD       1             §CTROKVB
044200000801     C                   ENDIF
044300000801     C*
044400000801     C                   ENDSR
044500070102     C*----------------------------------------------------*
044600070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
044700070102     C*----------------------------------------------------*
044800070102     C     EXEVATA       BEGSR
044900070102     C*
045000070102     C                   EXSR      INZVAR
045100070102     C                   EXSR      DEFCAM
045200070102     C*
045300070102     C                   EVAL      VATTRC='A'
045400070102     C                   EVAL      VATNOT = %trim(%subst(vindta:316:20))
045500070102     C                   exsr      wrivat                                       => scarico VAT
045600070102     C*
045700070102     C                   ENDSR
045800060307     C*----------------------------------------------------*
045900060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
046000060307     C*----------------------------------------------------*
046100060307     C     EXEVATB       BEGSR
046200060307     C*
046300060307     C                   EXSR      INZVAR
046400060307     C                   EXSR      DEFCAM
046500060307     C*
046600060307     C                   EVAL      VATTRC='B'
046700070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
046800060307     C                   exsr      wrivat                                       => scarico VAT
046900060307     C*
047000060307     C                   ENDSR
047001070402     C*----------------------------------------------------*
047002070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
047003070402     C*----------------------------------------------------*
047004070402     C     EXEVATE       BEGSR
047005070402     C*
047006070402     C                   EXSR      INZVAR
047007070402     C                   EXSR      DEFCAM
047008070402     C*
047009070402     C                   EVAL      VATTRC='E'
047010070402     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
047011070402     C                   exsr      wrivat                                       => scarico VAT
047012070402     C*
047013070402     C                   ENDSR
047100010201     C*----------------------------------------------------*
047200040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047300010201     C*----------------------------------------------------*
047400020305     C     WRIVAT        BEGSR
047500050628     C*
047600060223     C* Scrivo solo se valorizzato qualcosa
047700060223     C                   IF        VATNOT <> *blanks
047800040802     C                   WRITE     FIVAT000
047900060223     C                   ENDIF
048000010201     C*
048100010201     C                   ENDSR
048200010202     C*----------------------------------------------------*
048300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048400010202     C*----------------------------------------------------*
048500020305     C     PREVAT        BEGSR
048600010202     C*
048700021113     C* Compongo il nome del membro da dare al FIVATWWR
048800010202     C                   eval      parmbr = vlrhdl
048900010202     C                   movel     'M'           parmbr
049000050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049100010202     C                   eval      paropz = '1'
049200010202     C* Effettuo la chiamata al CLLE preposto
049300040506     C                   call(e)   'TITVVTC'
049400010202     C                   parm                    parccm
049500010202     C                   parm                    parmbr
049600010202     C                   parm                    paropz
049700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049800010202     C                   if        %error
049900010202     C                   movel     '1'           chkcall
050000010202     C                   else
050100010202     C                   movel     '0'           chkcall
050200010202     C                   endif
050300010202     C*
050400010202     C                   ENDSR
050500000801     C*----------------------------------------------------*
050600000801     C*  CONTROLLO NUMERICITA' CAMPI
050700000801     C*----------------------------------------------------*
050800000801     C     CHKNUM        BEGSR
050900000801     C*
051000000801     C                   call(e)   'ISNUMERIC'
051100000801     C                   PARM                    PiStr            30
051200070102     C                   PARM      '.'           PiDecChr          1
051300000801     C                   PARM      *ZEROS        PiVal            30 9
051400000801     C                   PARM      '0'           PiInt             1
051500000801     C                   PARM      '0'           PiNum             1
051600000801     C                   IF        %error
051700000801     C                   EVAL      PiInt=*off
051800000801     C                   ENDIF
051900000801     C*
052000000801     C                   ENDSR
052100000801     C***
052200000801
052300011113
052400011113     C*----------------------------------------------------*
052500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052600011113     C*----------------------------------------------------*
052700011113     C     CHKIMPDIV     BEGSR
052800011113     C*
052900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053000011113     C                   Z-ADD     *zeros        wrkDec            9 9
053100011113     C*
053200011113     C* Come prima cosa effettuo considerazioni sulla divisa
053300011113     C                   IF        vabIAS > *zeros
053400011113     C                   IF        vabVAS <> 'EUR'
053500011113     C                   EVAL      vabVAS =  'ITL'
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C                   IF        vabCAS > *zeros
054000011113     C                   IF        vabVCA <> 'EUR'
054100011113     C                   EVAL      vabVCA =  'ITL'
054200011113     C                   ENDIF
054300011113     C                   ENDIF
054400011113     C*
054500011113     C                   IF        vabVMD > *zeros
054600020305     C                   IF        vabVAD <> 'EUR'
054700011113     C                   EVAL      vabVAD =  'ITL'
054800011113     C                   ENDIF
054900011113     C                   ENDIF
055000011113     C*
055100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055200011113     C                   Z-ADD     vabIAS        wrkDec
055300011113     C                   IF        wrkDec > *zeros
055400011113     C                   IF        vabVAS = 'ITL'
055500011113     C                   EVAL      vabIAS = *zeros
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056000011113     C                   Z-ADD     vabCAS        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVCA = 'ITL'
056300011113     C                   EVAL      vabCAS = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056800011113     C                   Z-ADD     vabVMD        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVAD = 'ITL'
057100011113     C                   EVAL      vabVMD = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C                   ENDSR
057600011113     C***
057700011113
057800011113
057900000801
058000000801
058100990920      /TITLE Invio dei dati al punto operativo.
058200010202     C     invio         BEGSR
058300990920     C*
058400021113     C* 1° invio FIVAT
058500010201     C                   reset                   dscmz
058600010201     C                   move      vlrpoi        cmzdst
058700021113     C                   eval      cmzfld = 'FIVATWWR'
058800010201     C                   eval      cmzmbd = vlrhdl
058900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059000021009     C***                if        prmfir = *blanks
059100021113     C                   eval      cmzfla = 'FIVAT00F'
059200021113     C                   eval      cmzmba = 'FIVAT00F'
059300021009     C***                else
059400021009     C***                eval      cmzfla = prmfir
059500021009     C***                eval      cmzmba = prmfir
059600021009     C***                endif
059700010201     C                   eval      cmznrr = *zeros
059800020305     C                   move      §ctrokvt      cmznrr
059900021018     C                   eval      cmzlba = vlrfl1
060000010201     C                   call(e)   'TIS711C'
060100010201     C                   parm                    dscmz
060200010201     C                   parm      *blanks       esito
060300010205     C                   if        %error
060400010205     C                             or cmzerr = '1'
060500010205     C                             or esito  = '1'
060600010205     C                   eval      wrkesito = '3'
060700010205     C                   else
060800010201     C*
060900021113     C* 2° invio FIVAB
061000010201     C                   reset                   dscmz
061100010201     C                   move      vlrpoi        cmzdst
061200010201     C                   eval      cmzfld = vlrfou
061300010201     C                   eval      cmzmbd = vlrhdl
061400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061500021009     C***                if        prmfir = *blanks
061600021113     C                   eval      cmzfla = 'FIVAB00F'
061700021113     C                   eval      cmzmba = 'FIVAB00F'
061800021009     C***                else
061900021009     C***                eval      cmzfla = prmfir
062000021009     C***                eval      cmzmba = prmfir
062100021009     C***                endif
062200010201     C                   eval      cmznrr = *zeros
062300010201     C                   move      §ctrokvb      cmznrr
062400021018     C                   eval      cmzlba = vlrfl1
062500010201     C                   call(e)   'TIS711C'
062600010201     C                   parm                    dscmz
062700010201     C                   parm      *blanks       esito
062800010201     C                   if        %error
062900010201     C                             or cmzerr = '1'
063000010201     C                             or esito  = '1'
063100010201     C                   eval      wrkesito = '3'
063200010201     C                   endif
063300010205     C                   endif
063400990920     C*
063500000613     C                   ENDSR
063600000613     C***
063700990910
063800000613     C     *inzsr        BEGSR
063900990910     C*
064000990910     C     *entry        plist
064100990920     C                   parm                    tivlrds
064200990921     C                   parm      wrkesito      esito
064300000724     C                   parm                    prmlit
064400000710     C                   parm                    prmfir
064500000613     C*
064600000830     C* CALCOLA LA DATA CORRENTE
064700000830     C                   time                    wn14             14 0
064800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064900000830     C                   z-add     wn8           g08dat
065000000830     C                   z-add     *zeros        g08inv
065100000830     C                   movel     '0'           g08err
065200000830     C                   call      'XSRDA8'
065300000830     C                   parm                    wlbda8
065400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065500000830     C*
065600000613     C                   ENDSR
065700000613     C***
065800990908
065900070102     Otitvw1p   E            riepilogo         2
066000990915     O                                              'Upload via Internet'
066100990915     O                                           +1 'Traduzione TIVIN00R -'
066200021113     O                                           +1 'FIVABWWR/FIVATWWR'
066300010201     O                                           +1 'Report testate bolle'
066400990915     O          E            riepilogo   2
066500990915     O                       wrkdata
066600990915     O                       wrkora              +1
066700990915     O                       procname            +1
066800990915     O          E            riepilogo   2
066900990915     O                                              'Cliente..................:'
067000990915     O                       VABCCM        z     +1
067100990915     O          E            riepilogo   2
067200990920     O                                              'Riferimento Strategi.....:'
067300990920     O                       vlrhdl              +1
067400990915     O          E            riepilogo   2
067500990915     O                                              'Giusti...................:'
067600010201     O                       §CTROKVB      2   +  1
067700990915     O          E            riepilogo   2
067800990915     O                                              'Sbagliati e corretti.....:'
067900971022     O                       §CTRMO        2   +  1
068000990915     O          E            riepilogo   2
068100990915     O                                              'Sbagliati e scartati.....:'
068200971022     O                       §CTRNO        2   +  1
068300000613
068400070102     Otitvw1ps  E            testdett          2
068500000613     O                                              'Upload via Internet'
068600000613     O                                           +1 'Traduzione TIVIN00R -'
068700021113     O                                           +1 'FIVABWWR/FIVATWWR'
068800010201     O                                           +1 'Report testate bolle'
068900000616     O          E            testdett    3
069000000613     O                                           +2 'N° rec'
069100000613     O                                           +3 'Anteprima contenuto'
069200000616     O          E            rigadett    2
069300000613     O                       rrnum               +2
069400000621     O                       recko               +3
069500000616     O          E            findett     2
069600000613     O                       wrkdata
069700000613     O                       wrkora              +1
069800000613     O                       procname            +1
069900000616     O          E            findett     2
070000000613     O                                              'Cliente..................:'
070100000613     O                       VABCCM        z     +1
070200000616     O          E            findett     2
070300000613     O                                              'Riferimento Strategi.....:'
070400000613     O                       vlrhdl              +1
070500000616     O          E            findett     2
070600000613     O                                              'Giusti...................:'
070700010201     O                       §CTROKVB      2   +  1
070800000616     O          E            findett     2
070900000613     O                                              'Sbagliati e corretti.....:'
071000000613     O                       §CTRMO        2   +  1
071100000616     O          E            findett     2
071200000613     O                                              'Sbagliati e scartati.....:'
071300000613     O                       §CTRNO        2   +  1
071400000512** CMD - COMANDI CL
071500070102OVRPRTF FILE(TITVW1P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
071600070102OVRPRTF FILE(TITVW1PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071700070102DLTOVR FILE(TITVW1P TITVW1PS) LVL(*)
071800000512
071900000512
