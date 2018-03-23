000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800090127     Ftitv1m5p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000090127     Ftitv1m5ps O    f  198        PRINTER usropn
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
002500090127     D fivabds       e ds                  extname(fivab00f)
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500010202     D parccm          s              8    INZ(*blanks)
003600010202     D parmbr          s             10    INZ(*blanks)
003700010202     D paropz          s              1    INZ(*blanks)
003800010202     D chkcall         s              1    INZ(*blanks)
003900070402     D curSped         s             25    INZ(*blanks)
004000070402     D depSped         s             25    INZ(*blanks)
004100000830
004200000830     D*------------------
004300000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004400000830     D*------------------
004500000830     D WLBDA8          DS                  INZ
004600000830     D  G08DAT                 1      8  0
004700000830     D  G08INV                 9     16  0
004800000830     D  G08ERR                17     17
004900000830     D  G08TGI                18     22  0
005000000830     D*
005100010201
005200090127
005300090127
005400090127     C*
005500990915     C                   time                    wrkdata
005600990915     C                   time                    wrkora
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000000724     C*
006100000724     C* SE OCCORRE SPEDIRE IN FILIALE
006200000724     C                   if        vlrpoi <> *zeros
006300000724     C*
006400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006600000724     C     vlrpoi        chain     azorg01l
006700000724     C                   if        %found
006800000616     C                   movel(p)  CMD(1)        depcmd
006900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007000000616     C*
007100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007200011113     C                   Z-ADD     150           LENGH            15 5
007300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007400000616     C                   PARM                    depcmd
007500000616     C                   PARM                    LENGH
007600000724     C*
007700000724     C                   endif
007800000724     C                   endif
007900000616     C*
008000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008100000616     C                   movel(p)  CMD(2)        depcmd
008200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008300000616     C*
008400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000616     C                   PARM                    depcmd
008800000616     C                   PARM                    LENGH
008900000616     C*
009000090127     C                   if        not %open(titv1m5ps)
009100090127     C                   open      titv1m5ps
009200000616     C                   except    testdett
009300000613     C                   endif
009400000613     C*
009500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009700000613     C*
009800010202     C* Effettuo la chiamata al CLLE preposto
009900040506     C                   call(e)   'TITVVTC'
010000010202     C                   parm                    parccm
010100010202     C                   parm                    parmbr
010200010202     C                   parm      '2'           paropz
010300050201     C*
010400050201     C* Effettuo lancio TISI95 solo x chiusura
010500050201     C                   CLEAR                   TISI95DS
010600050201     C                   EVAL      I95TLA = 'C'
010700050201     C                   CALL      'TISI95R'
010800050201     C                   PARM                    TISI95DS
010900010202     C*
011000090127     C                   if        %open(titv1m5ps)
011100000616     C                   except    findett
011200090127     C                   close     titv1m5ps
011300000613     C                   endif
011400000616     C*
011500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011600011113     C                   Z-ADD     150           LENGH            15 5
011700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011800000616     C                   PARM                    CMD(3)
011900000616     C                   PARM                    LENGH
012000000616     C*
012100000801     C
012200010201     C                   seton                                        LR
012300000613
012400000613
012500000613     C*--------------------------------------------------------
012600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012700000613     C*--------------------------------------------------------
012800000621     C     STPR          BEGSR
012900000613     C*
013000090127     C                   if        not %open(titv1m5p)
013100090127     C                   open      titv1m5p
013200990915     C                   endif
013300990915     C*
013400990915     C                   except    riepilogo
013500990915     C*
013600090127     C                   if        %open(titv1m5p)
013700090127     C                   close     titv1m5p
013800990914     C                   endif
013900990910     C*
014000000613     C                   ENDSR
014100000613     C***
014200990908
014300000801
014400910830     C*--------------------------------------------------------
014500040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014600910830     C*--------------------------------------------------------
014700040526     C     RWFILE        BEGSR
014800990910     C*
014900990914     C                   if        not %open(tivin00r)
015000990908     C                   open      tivin00r
015100990914     C                   endif
015200021113     C                   if        not %open(fivabwwr)
015300021113     C                   open      fivabwwr
015400990914     C                   endif
015500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015600020305     C                   exsr      prevat
015700010201     C*
015800010202     C                   if        chkcall = '0'
015900010202     C*
016000021113     C                   if        not %open(fivatwwr)
016100021113     C                   open      fivatwwr
016200010201     C                   endif
016300990910     C*
016400010201     C                   clear                   §CTROKVB          5 0
016500020305     C                   clear                   §CTROKVT          5 0
016600000801     C                   clear                   §CTRMO            5 0
016700000801     C                   clear                   §CTRNO            5 0
016800990910     C*
016900921023     C                   DO        *HIVAL
017000990913     C*
017100990915     C                   READ      tivin00r                               70
017200050627     C                   if        vindta > *blanks
017300000613     C                   add       1             rrnum
017400000801     C*
017500000801     C                   if        *in70 = *off
017600000801     C                             and
017700000801     C                             (vinflg = *blanks
017800000801     C                              or vinflg = '0'
017900000801     C                              or vinflg = '2')
018000000801     C*
018100000801     C                   clear                   vinmsg
018200000801     C                   eval      vinflg = '1'
018300100416     C*
018400100416     C* Ignoro i record nn d tipo BOLLA
018500100416     C                   if        %trim(%subst(vindta:457:3)) = 'ANR'      AND
018600100504     C                             %subst(%subst(vindta:13:30):1:2)='37'
018700100416     C                   eval      vinflg = '1'
018800100416     C                   else
018900070402     C*
019000070402     C* Verifico la rottura d codice x raggruppamento spedizione cliente
019100070402     C                   eval      curSped = %subst(vindta:46:25)
019200070402     C                   if        curSped <> depSped
019300070402     C                   clear                   fivab000
019400070402     C                   clear                   fivat000
019500050628     C*
019600060315     C                   exsr      impvab                                       => carico  VAB
019700090127     C*
019800060315     C                   exsr      wrivab                                       => scarico VAB
019900070402     C*
020000070402     C* Scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE CONSEGNA
020100070402     C                   exsr      exevata
020200070402     C*
020300070402     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
020400070402     C                   exsr      exevatb
020500070402     C*
020600070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
020700070402     C                   exsr      exevate
020800070402     C*
020900070402     C* Salvo il raggruppamento spedizione cliente corrente
021000070402     C                   eval      depSped = curSped
021100070402     C*
021200070402     C                   else
021300070402     C*
021400070402     C* Scrivo anche estensione dettaglio bolle - tipo record "E" => "CHI SONO"
021500070402     C                   exsr      exevate
021600070402     C                   endif
021700100416     C*
021800100416     C                   endif
021900000905     C*
022000000905     C                   else
022100000905     C                   eval      vinflg = '1'
022200050628     C                   endif
022300000905     C                   endif
022400000905     C*
022500000905     C  N70              update    tivin000
022600000905     C*
022700991022     C  N70              ENDdo
022800010202     C*
022900010202     C                   endif
023000990910
023100990910     C* Se non ci sono record con errori ...
023200000710     C                   if        §ctrno = 0
023300990910     C* ... restituisco esito OK.
023400990921     C                   eval      wrkesito = '0'
023500990910     C                   else
023600010201     C                   if        §ctrokvb > 0
023700990921     C                   eval      wrkesito = '1'
023800000710     C                   else
023900000710     C                   eval      wrkesito = '2'
024000990910     C                   endif
024100000710     C                   endif
024200990910     C*
024300990914     C                   if        %open(tivin00r)
024400990908     C                   close     tivin00r
024500990914     C                   endif
024600021113     C                   if        %open(fivabwwr)
024700021113     C                   close     fivabwwr
024800990914     C                   endif
024900021113     C                   if        %open(fivatwwr)
025000021113     C                   close     fivatwwr
025100010201     C                   endif
025200990910     C*
025300010201     C                   if        §ctrokvb > 0
025400000724     C                             and vlrpoi <> *zeros
025500010202     C                   exsr      invio
025600990920     C                   endif
025700990920     C*
025800910830     C                   ENDSR
025900000613     C***
026000010305
026100010305     C*----------------------------------------------------*
026200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026300010305     C*----------------------------------------------------*
026400090127     C     WRIVAB        BEGSR
026500090127     C*
026600090127     C* Quindi scarico il buffer del file d testata
026700090127     C                   write     fivab000                                     => scarico il VAB
026800010305     C*
026900010305     C                   ENDSR
027000990920
027100000801     C*----------------------------------------------------*
027200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027300000801     C*----------------------------------------------------*
027400010201     C     INZVAR        BEGSR
027500000801     C*
027600040802     C                   Z-ADD     *zeros        Num5_0            5 0
027700040802     C                   MOVEL     '0'           FlgCAS            1
027800000801     C*
027900000801     C                   ENDSR
028000000801     C*----------------------------------------------------*
028100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028200000801     C*----------------------------------------------------*
028300000801     C     DEFCAM        BEGSR
028400070718     C*
028500070718     C* Imposto i valori di default...
028600090127     C                   Z-ADD     0054868       VABCCM
028700090423     C                   Z-ADD     0054868       VATCCM
028800090127     C                   Z-ADD     005           VABLNP
028900090127     C                   Z-ADD     005           VATLNP
029000070718     C                   Z-ADD     000           VABCTR
029100070718     C                   MOVEL     '1'           VABCBO
029200070718     C                   MOVEL     '7Q'          VABCTM
029300020619     C* ... e poi verifico se sono stati passati come parametri
029400020619     C                   IF        vlrppt > *blanks
029500040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029700020619     C                   EXSR      CHKNUM
029800020619     C                   IF        PiInt=*on
029900020619     C                   Z-ADD     PiVal         VABCCM
030000020619     C                   Z-ADD     PiVal         VATCCM
030100020619     C                   ENDIF
030200040506     C                   ENDIF
030300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030500020619     C                   EXSR      CHKNUM
030600020619     C                   IF        PiInt=*on
030700020619     C                   Z-ADD     PiVal         VABLNP
030800020619     C                   Z-ADD     PiVal         VATLNP
030900040506     C                   ENDIF
031000020619     C                   ENDIF
031100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031300020619     C                   EXSR      CHKNUM
031400020619     C                   IF        PiInt=*on
031500020619     C                   Z-ADD     PiVal         VABCTR
031600040506     C                   ENDIF
031700020619     C                   ENDIF
031800020619     C                   ENDIF
031900000801     C*
032000000801     C                   ENDSR
032100000801     C*----------------------------------------------------*
032200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032300000801     C*----------------------------------------------------*
032400040823     C     IMPVAB        BEGSR
032500040823     C*
032600020305     C                   EXSR      INZVAR
032700020305     C                   EXSR      DEFCAM
032800010305     C*
032900000801     C                   Z-ADD     *zeros        errore            1 0
033000000830     C                   MOVEL     datcor        VABAAS
033100020305     C                   MOVEL     datcor        VATAAS
033200040526     C                   MOVE      datcor        VABMGS
033300040823     C                   MOVE(P)   vlrpoi        VABFGS
033400040823     C                   MOVE(P)   vlrpoi        VATFGS
033500050628     C*
033600070102     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
033700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033900020117     C* ==
034000070719     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
034100090427     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
034200070102     C                                    %trim(%subst(vindta:341:30))+' '+
034300090427     C                                    %trim(%subst(vindta:371:5)))
034400070102     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
034500070102     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
034600090427     C                   EVAL      VABRMA=%trim(%subst(vindta:470:9))
034700070102     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
034800070102     C                   IF        %subst(vindta:43:1) = 'N'
034900070102     C                   EVAL      VABCBO='1'
035000070102     C                   ENDIF
035100070102     C                   IF        %subst(vindta:43:1) = 'P'
035200070102     C                   EVAL      VABCBO='2'
035300070102     C                   ENDIF
035400070102     C                   IF        %subst(vindta:511:3) = 'ABM'
035500070102     C                   EVAL      VABTIC='BM'
035600070102     C                   ENDIF
035700070102     C                   IF        %subst(vindta:511:3) = 'ABS'
035800070102     C                   EVAL      VABTIC='B'
035900070102     C                   ENDIF
036000070102     C                   IF        %subst(vindta:511:3) = 'CON'
036100070102     C                   EVAL      VABTIC=*blanks
036200070102     C                   ENDIF
036300070102     C                   IF        %subst(vindta:511:3) = 'ACM'
036400070102     C                   EVAL      VABTIC='CM'
036500070102     C                   ENDIF
036600100416     C* GMA
036700100416     C                   IF        %subst(vindta:457:3) = 'ANR'
036800100416     C                   EVAL      VABGMA='RC'
036900100416     C                   ENDIF
037000050628     C* CAP
037100070719     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
037200010201     C                   EXSR      CHKNUM
037300010201     C                   IF        PiInt=*on
037400010201     C                   Z-ADD     PiVal         Num5_0
037500040506     C                   MOVEL(P)  Num5_0        VABCAD
037600010201     C                   ELSE
037700040506     C                   ADD       1             errore
037800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037900040506     C                             + ' ' + 'VABCAD'
038000010201     C                   ENDIF
038100040506     C* Reperisco la provincia dal CAP e dalla località
038200040526     C                   IF        VABCAD <> *blanks AND
038300040526     C                             VABPRD  = *blanks
038400040506     C                   CLEAR                   TISI95DS
038500040506     C                   EVAL      I95TCN = '3'
038600040506     C                   Z-ADD     datcor        I95DAT
038700040506     C                   EVAL      I95CAP = VABCAD
038800040506     C                   EVAL      I95LOC = VABLOD
038900050627     C                   EVAL      I95NAR = VABNZD
039000040506     C                   CALL      'TISI95R'
039100040506     C                   PARM                    TISI95DS
039200040506     C                   EVAL      VABPRD = O95PRV
039300040506     C                   ENDIF
039400060307     C* NCL
039500070102     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
039600060307     C                   EXSR      CHKNUM
039700060307     C                   IF        PiInt=*on
039800060307     C                   ADD       PiVal         VABNCL
039900060307     C                   ELSE
040000060307     C                   ADD       1             errore
040100060307     C                   EVAL      vinmsg = %trimr(vinmsg)
040200060307     C                             + ' ' + 'VABNCL'
040300060307     C                   ENDIF
040400040506     C* PKB
040500070102     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
040600010201     C                   EXSR      CHKNUM
040700010201     C                   IF        PiNum=*on
040800070102     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
040900060316     C                   Z-ADD     PiVal         VABPKB
041000010201     C                   ELSE
041100010201     C                   ADD       1             errore
041200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
041300010201     C                             + ' ' + 'VABPKB'
041400010201     C                   ENDIF
041500060316     C* NSP/RMN
041600090127     C                   EVAL      PiStr=%trim(%subst(vindta:46:25))
041700050928     C                   EXSR      CHKNUM
041800050928     C                   IF        PiInt=*on
041900060316     C                   Z-ADD     PiVal         VABNSP
042000060316     C                   Z-ADD     PiVal         VATNSP
042100050928     C                   Z-ADD     PiVal         VABRMN
042200050928     C                   ELSE
042300050928     C                   ADD       1             errore
042400050928     C                   EVAL      vinmsg = %trimr(vinmsg)
042500060316     C                             + ' ' + 'VABNSP VATNSP VABRMN'
042600050928     C                   ENDIF
042700060307     C* CAS
042800090127     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
042900090127     C                             %trim(%subst(vindta:502:9))<>*blanks
043000070102     C                   EVAL      VABVCA = 'EUR'
043100060307     C                   EVAL      FlgCAS = '1'
043200070102     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
043300060307     C                   EXSR      CHKNUM
043400060307     C                   IF        PiNum=*on
043500060316     C                   Z-ADD     PiVal         VABCAS
043600060307     C                   ELSE
043700060307     C                   ADD       1             errore
043800060307     C                   EVAL      vinmsg = %trimr(vinmsg)
043900060307     C                             + ' ' + 'VABCAS'
044000060307     C                   ENDIF
044100070102     C                   ENDIF
044200060316     C* IAS
044300070719     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
044400070102     C                   EVAL      VABVAS = 'EUR'
044500070102     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
044600060316     C                   EXSR      CHKNUM
044700060316     C                   IF        PiNum=*on
044800060316     C                   Z-ADD     PiVal         VABIAS
044900060316     C                   ELSE
045000060316     C                   ADD       1             errore
045100060316     C                   EVAL      vinmsg = %trimr(vinmsg)
045200060316     C                             + ' ' + 'VABIAS'
045300070102     C                   ENDIF
045400060316     C                   ENDIF
045500010205     C*
045600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045700040802     C                   IF        FlgCAS <> '0'
045800070102     C                   IF        VABCBO = '1'
045900010205     C                   EVAL      VABCBO = '4'
046000010205     C                   ELSE
046100070102     C                   EVAL      VABCBO = '6'
046200070102     C                   ENDIF
046300010205     C                   ENDIF
046400020305     C*
046500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046600011113     C                   EXSR      CHKIMPDIV
046700010202     C*
046800000801     C* Ebbene...
046900000801     C                   ADD       1             §CTRMO
047000010201     C                   IF        errore <> *zeros
047100000801     C                   ADD       1             §CTRNO
047200000801     C                   EVAL      recko = vindta
047300000801     C                   EXCEPT    rigadett
047400000801     C                   EVAL      vinflg = '2'
047500000801     C                   ELSE
047600010201     C                   ADD       1             §CTROKVB
047700000801     C                   ENDIF
047800000801     C*
047900000801     C                   ENDSR
048000070102     C*----------------------------------------------------*
048100070102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
048200070102     C*----------------------------------------------------*
048300070102     C     EXEVATA       BEGSR
048400070102     C*
048500070102     C                   EXSR      INZVAR
048600070102     C                   EXSR      DEFCAM
048700070102     C*
048800070102     C                   EVAL      VATTRC='A'
048900090127     C                   EVAL      VATNOT = %trim(%subst(vindta:581:30))
049000070102     C                   exsr      wrivat                                       => scarico VAT
049100070102     C*
049200070102     C                   ENDSR
049300060307     C*----------------------------------------------------*
049400060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
049500060307     C*----------------------------------------------------*
049600060307     C     EXEVATB       BEGSR
049700060307     C*
049800060307     C                   EXSR      INZVAR
049900060307     C                   EXSR      DEFCAM
050000060307     C*
050100060307     C                   EVAL      VATTRC='B'
050200070102     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
050300060307     C                   exsr      wrivat                                       => scarico VAT
050400060307     C*
050500060307     C                   ENDSR
050600070402     C*----------------------------------------------------*
050700070402     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
050800070402     C*----------------------------------------------------*
050900070402     C     EXEVATE       BEGSR
051000070402     C*
051100070402     C                   EXSR      INZVAR
051200070402     C                   EXSR      DEFCAM
051300070402     C*
051400070402     C                   EVAL      VATTRC='E'
051500091111     C                   EVALR     VATNOT = %trim(%subst(vindta:554:25))
051600091111     C*
051700091111     C* Autoincremento i "chi sono" (ovviamente sequenziali) x quanti sono i colli della sped.
051800091111     C                   z-add     *zeros        i                 5 0
051900091111     C                   z-add     *zeros        wCurrWhoIs       25 0
052000091111     C                   z-add     *zeros        wLenWhoIs         2 0
052100091111     C                   eval      wLenWhoIs = %len(%trim(VATNOT))
052200091111     C                   move(p)   VATNOT        wCurrWhoIs
052300091111     C                   eval      wCurrWhoIs = wCurrWhoIs - 1
052400091111     C                   for       i = 1 to vabncl
052500091111     C                   eval      wCurrWhoIs = wCurrWhoIs + 1
052600091111     C                   move(p)   wCurrWhoIs    VATNOT
052700091111     C                   EVAL      VATNOT = %subst(VATNOT:35-wLenWhoIs+1:
052800091111     C                                             wLenWhoIs)
052900070402     C                   exsr      wrivat                                       => scarico VAT
053000091111     C                   endfor
053100070402     C*
053200070402     C                   ENDSR
053300010201     C*----------------------------------------------------*
053400040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
053500010201     C*----------------------------------------------------*
053600020305     C     WRIVAT        BEGSR
053700050628     C*
053800060223     C* Scrivo solo se valorizzato qualcosa
053900060223     C                   IF        VATNOT <> *blanks
054000040802     C                   WRITE     FIVAT000
054100060223     C                   ENDIF
054200010201     C*
054300010201     C                   ENDSR
054400010202     C*----------------------------------------------------*
054500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054600010202     C*----------------------------------------------------*
054700020305     C     PREVAT        BEGSR
054800010202     C*
054900021113     C* Compongo il nome del membro da dare al FIVATWWR
055000010202     C                   eval      parmbr = vlrhdl
055100010202     C                   movel     'M'           parmbr
055200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
055300010202     C                   eval      paropz = '1'
055400010202     C* Effettuo la chiamata al CLLE preposto
055500040506     C                   call(e)   'TITVVTC'
055600010202     C                   parm                    parccm
055700010202     C                   parm                    parmbr
055800010202     C                   parm                    paropz
055900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056000010202     C                   if        %error
056100010202     C                   movel     '1'           chkcall
056200010202     C                   else
056300010202     C                   movel     '0'           chkcall
056400010202     C                   endif
056500010202     C*
056600010202     C                   ENDSR
056700000801     C*----------------------------------------------------*
056800000801     C*  CONTROLLO NUMERICITA' CAMPI
056900000801     C*----------------------------------------------------*
057000000801     C     CHKNUM        BEGSR
057100000801     C*
057200000801     C                   call(e)   'ISNUMERIC'
057300000801     C                   PARM                    PiStr            30
057400070102     C                   PARM      '.'           PiDecChr          1
057500000801     C                   PARM      *ZEROS        PiVal            30 9
057600000801     C                   PARM      '0'           PiInt             1
057700000801     C                   PARM      '0'           PiNum             1
057800000801     C                   IF        %error
057900000801     C                   EVAL      PiInt=*off
058000000801     C                   ENDIF
058100000801     C*
058200000801     C                   ENDSR
058300000801     C***
058400000801
058500011113
058600011113     C*----------------------------------------------------*
058700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058800011113     C*----------------------------------------------------*
058900011113     C     CHKIMPDIV     BEGSR
059000011113     C*
059100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059200011113     C                   Z-ADD     *zeros        wrkDec            9 9
059300011113     C*
059400011113     C* Come prima cosa effettuo considerazioni sulla divisa
059500011113     C                   IF        vabIAS > *zeros
059600011113     C                   IF        vabVAS <> 'EUR'
059700011113     C                   EVAL      vabVAS =  'ITL'
059800011113     C                   ENDIF
059900011113     C                   ENDIF
060000011113     C*
060100011113     C                   IF        vabCAS > *zeros
060200011113     C                   IF        vabVCA <> 'EUR'
060300011113     C                   EVAL      vabVCA =  'ITL'
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C                   IF        vabVMD > *zeros
060800020305     C                   IF        vabVAD <> 'EUR'
060900011113     C                   EVAL      vabVAD =  'ITL'
061000011113     C                   ENDIF
061100011113     C                   ENDIF
061200011113     C*
061300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061400011113     C                   Z-ADD     vabIAS        wrkDec
061500011113     C                   IF        wrkDec > *zeros
061600011113     C                   IF        vabVAS = 'ITL'
061700011113     C                   EVAL      vabIAS = *zeros
061800011113     C                   ENDIF
061900011113     C                   ENDIF
062000011113     C*
062100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062200011113     C                   Z-ADD     vabCAS        wrkDec
062300011113     C                   IF        wrkDec > *zeros
062400011113     C                   IF        vabVCA = 'ITL'
062500011113     C                   EVAL      vabCAS = *zeros
062600011113     C                   ENDIF
062700011113     C                   ENDIF
062800011113     C*
062900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063000011113     C                   Z-ADD     vabVMD        wrkDec
063100011113     C                   IF        wrkDec > *zeros
063200011113     C                   IF        vabVAD = 'ITL'
063300011113     C                   EVAL      vabVMD = *zeros
063400011113     C                   ENDIF
063500011113     C                   ENDIF
063600011113     C*
063700011113     C                   ENDSR
063800011113     C***
063900011113
064000011113
064100000801
064200000801
064300990920      /TITLE Invio dei dati al punto operativo.
064400010202     C     invio         BEGSR
064500990920     C*
064600021113     C* 1° invio FIVAT
064700010201     C                   reset                   dscmz
064800010201     C                   move      vlrpoi        cmzdst
064900021113     C                   eval      cmzfld = 'FIVATWWR'
065000010201     C                   eval      cmzmbd = vlrhdl
065100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065200021009     C***                if        prmfir = *blanks
065300021113     C                   eval      cmzfla = 'FIVAT00F'
065400021113     C                   eval      cmzmba = 'FIVAT00F'
065500021009     C***                else
065600021009     C***                eval      cmzfla = prmfir
065700021009     C***                eval      cmzmba = prmfir
065800021009     C***                endif
065900010201     C                   eval      cmznrr = *zeros
066000020305     C                   move      §ctrokvt      cmznrr
066100021018     C                   eval      cmzlba = vlrfl1
066200010201     C                   call(e)   'TIS711C'
066300010201     C                   parm                    dscmz
066400010201     C                   parm      *blanks       esito
066500010205     C                   if        %error
066600010205     C                             or cmzerr = '1'
066700010205     C                             or esito  = '1'
066800010205     C                   eval      wrkesito = '3'
066900010205     C                   else
067000010201     C*
067100021113     C* 2° invio FIVAB
067200010201     C                   reset                   dscmz
067300010201     C                   move      vlrpoi        cmzdst
067400010201     C                   eval      cmzfld = vlrfou
067500010201     C                   eval      cmzmbd = vlrhdl
067600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067700021009     C***                if        prmfir = *blanks
067800021113     C                   eval      cmzfla = 'FIVAB00F'
067900021113     C                   eval      cmzmba = 'FIVAB00F'
068000021009     C***                else
068100021009     C***                eval      cmzfla = prmfir
068200021009     C***                eval      cmzmba = prmfir
068300021009     C***                endif
068400010201     C                   eval      cmznrr = *zeros
068500010201     C                   move      §ctrokvb      cmznrr
068600021018     C                   eval      cmzlba = vlrfl1
068700010201     C                   call(e)   'TIS711C'
068800010201     C                   parm                    dscmz
068900010201     C                   parm      *blanks       esito
069000010201     C                   if        %error
069100010201     C                             or cmzerr = '1'
069200010201     C                             or esito  = '1'
069300010201     C                   eval      wrkesito = '3'
069400010201     C                   endif
069500010205     C                   endif
069600990920     C*
069700000613     C                   ENDSR
069800000613     C***
069900090127
070000090127
070100990910
070200000613     C     *inzsr        BEGSR
070300990910     C*
070400990910     C     *entry        plist
070500990920     C                   parm                    tivlrds
070600990921     C                   parm      wrkesito      esito
070700000724     C                   parm                    prmlit
070800000710     C                   parm                    prmfir
070900000613     C*
071000000830     C* CALCOLA LA DATA CORRENTE
071100000830     C                   time                    wn14             14 0
071200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
071300000830     C                   z-add     wn8           g08dat
071400000830     C                   z-add     *zeros        g08inv
071500000830     C                   movel     '0'           g08err
071600000830     C                   call      'XSRDA8'
071700000830     C                   parm                    wlbda8
071800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
071900000830     C*
072000000613     C                   ENDSR
072100000613     C***
072200990908
072300090127     Otitv1m5p  E            riepilogo         2
072400990915     O                                              'Upload via Internet'
072500990915     O                                           +1 'Traduzione TIVIN00R -'
072600021113     O                                           +1 'FIVABWWR/FIVATWWR'
072700010201     O                                           +1 'Report testate bolle'
072800990915     O          E            riepilogo   2
072900990915     O                       wrkdata
073000990915     O                       wrkora              +1
073100990915     O                       procname            +1
073200990915     O          E            riepilogo   2
073300990915     O                                              'Cliente..................:'
073400990915     O                       VABCCM        z     +1
073500990915     O          E            riepilogo   2
073600990920     O                                              'Riferimento Strategi.....:'
073700990920     O                       vlrhdl              +1
073800990915     O          E            riepilogo   2
073900990915     O                                              'Giusti...................:'
074000010201     O                       §CTROKVB      2   +  1
074100990915     O          E            riepilogo   2
074200990915     O                                              'Sbagliati e corretti.....:'
074300971022     O                       §CTRMO        2   +  1
074400990915     O          E            riepilogo   2
074500990915     O                                              'Sbagliati e scartati.....:'
074600971022     O                       §CTRNO        2   +  1
074700000613
074800090127     Otitv1m5ps E            testdett          2
074900000613     O                                              'Upload via Internet'
075000000613     O                                           +1 'Traduzione TIVIN00R -'
075100021113     O                                           +1 'FIVABWWR/FIVATWWR'
075200010201     O                                           +1 'Report testate bolle'
075300000616     O          E            testdett    3
075400000613     O                                           +2 'N° rec'
075500000613     O                                           +3 'Anteprima contenuto'
075600000616     O          E            rigadett    2
075700000613     O                       rrnum               +2
075800000621     O                       recko               +3
075900000616     O          E            findett     2
076000000613     O                       wrkdata
076100000613     O                       wrkora              +1
076200000613     O                       procname            +1
076300000616     O          E            findett     2
076400000613     O                                              'Cliente..................:'
076500000613     O                       VABCCM        z     +1
076600000616     O          E            findett     2
076700000613     O                                              'Riferimento Strategi.....:'
076800000613     O                       vlrhdl              +1
076900000616     O          E            findett     2
077000000613     O                                              'Giusti...................:'
077100010201     O                       §CTROKVB      2   +  1
077200000616     O          E            findett     2
077300000613     O                                              'Sbagliati e corretti.....:'
077400000613     O                       §CTRMO        2   +  1
077500000616     O          E            findett     2
077600000613     O                                              'Sbagliati e scartati.....:'
077700000613     O                       §CTRNO        2   +  1
077800000512** CMD - COMANDI CL
077900090127OVRPRTF FILE(TITV1M5P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
078000090127OVRPRTF FILE(TITV1M5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
078100090127DLTOVR FILE(TITV1M5P TITV1M5PS) LVL(*)
078200000512
078300000512
