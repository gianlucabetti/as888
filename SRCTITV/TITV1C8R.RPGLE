000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800071207     Ftitv1c8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100071207     Ftitv1c8ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600060725     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000071207     D wNumColloDA     s             20  0 INZ(*zeros)
004100071207     D wNumColloA      s             20  0 INZ(*zeros)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100000830     D*
005200990908
005300060307     D*------------------
005400060307     D* DS REPERIMENTO NUMERATORE
005500060307     D*------------------
005600060529     D trul33ds      e ds                  inz
005700060307     D*------------------
005800060307     D* DS ARCHITETTURA
005900060307     D*------------------
006000060307     D kpjba         e ds                  inz
006100060307     D*------------------
006200010201
006300010201
006400990915     C                   time                    wrkdata
006500990915     C                   time                    wrkora
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900000724     C*
007000000724     C* SE OCCORRE SPEDIRE IN FILIALE
007100000724     C                   if        vlrpoi <> *zeros
007200000724     C*
007300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007500000724     C     vlrpoi        chain     azorg01l
007600000724     C                   if        %found
007700000616     C                   movel(p)  CMD(1)        depcmd
007800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007900000616     C*
008000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008100011113     C                   Z-ADD     150           LENGH            15 5
008200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000616     C                   PARM                    depcmd
008400000616     C                   PARM                    LENGH
008500000724     C*
008600000724     C                   endif
008700000724     C                   endif
008800000616     C*
008900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009000000616     C                   movel(p)  CMD(2)        depcmd
009100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009200000616     C*
009300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009400011113     C                   Z-ADD     150           LENGH            15 5
009500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009600000616     C                   PARM                    depcmd
009700000616     C                   PARM                    LENGH
009800000616     C*
009900071207     C                   if        not %open(titv1c8ps)
010000071207     C                   open      titv1c8ps
010100000616     C                   except    testdett
010200000613     C                   endif
010300000613     C*
010400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010600000613     C*
010700010202     C* Effettuo la chiamata al CLLE preposto
010800040506     C                   call(e)   'TITVVTC'
010900010202     C                   parm                    parccm
011000010202     C                   parm                    parmbr
011100010202     C                   parm      '2'           paropz
011200050201     C*
011300050201     C* Effettuo lancio TISI95 solo x chiusura
011400050201     C                   CLEAR                   TISI95DS
011500050201     C                   EVAL      I95TLA = 'C'
011600050201     C                   CALL      'TISI95R'
011700050201     C                   PARM                    TISI95DS
011800010202     C*
011900071207     C                   if        %open(titv1c8ps)
012000000616     C                   except    findett
012100071207     C                   close     titv1c8ps
012200000613     C                   endif
012300000616     C*
012400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    CMD(3)
012800000616     C                   PARM                    LENGH
012900000616     C*
013000000801     C
013100010201     C                   seton                                        LR
013200000613
013300000613
013400000613     C*--------------------------------------------------------
013500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013600000613     C*--------------------------------------------------------
013700000621     C     STPR          BEGSR
013800000613     C*
013900071207     C                   if        not %open(titv1c8p)
014000071207     C                   open      titv1c8p
014100990915     C                   endif
014200990915     C*
014300990915     C                   except    riepilogo
014400990915     C*
014500071207     C                   if        %open(titv1c8p)
014600071207     C                   close     titv1c8p
014700990914     C                   endif
014800990910     C*
014900000613     C                   ENDSR
015000000613     C***
015100990908
015200000801
015300910830     C*--------------------------------------------------------
015400040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015500910830     C*--------------------------------------------------------
015600040526     C     RWFILE        BEGSR
015700990910     C*
015800990914     C                   if        not %open(tivin00r)
015900990908     C                   open      tivin00r
016000990914     C                   endif
016100021113     C                   if        not %open(fivabwwr)
016200021113     C                   open      fivabwwr
016300990914     C                   endif
016400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016500020305     C                   exsr      prevat
016600010201     C*
016700010202     C                   if        chkcall = '0'
016800010202     C*
016900021113     C                   if        not %open(fivatwwr)
017000021113     C                   open      fivatwwr
017100010201     C                   endif
017200990910     C*
017300010201     C                   clear                   §CTROKVB          5 0
017400020305     C                   clear                   §CTROKVT          5 0
017500000801     C                   clear                   §CTRMO            5 0
017600000801     C                   clear                   §CTRNO            5 0
017700990910     C*
017800921023     C                   DO        *HIVAL
017900990913     C*
018000990915     C                   READ      tivin00r                               70
018100050627     C                   if        vindta > *blanks
018200000613     C                   add       1             rrnum
018300000801     C*
018400000801     C                   if        *in70 = *off
018500000801     C                             and
018600000801     C                             (vinflg = *blanks
018700000801     C                              or vinflg = '0'
018800000801     C                              or vinflg = '2')
018900000801     C*
019000000801     C                   clear                   vinmsg
019100000801     C                   eval      vinflg = '1'
019200010305     C*
019300050628     C                   clear                   fivab000
019400060725     C                   clear                   fivat000
019500050628     C                   exsr      impvab                                       => carico VAB
019600060725     C                   exsr      wrivab                                       => scarico VAB
019700000905     C*
019800000905     C                   else
019900000905     C                   eval      vinflg = '1'
020000050628     C                   endif
020100000905     C                   endif
020200000905     C*
020300000905     C  N70              update    tivin000
020400000905     C*
020500991022     C  N70              ENDdo
020600010202     C*
020700010202     C                   endif
020800990910
020900990910     C* Se non ci sono record con errori ...
021000000710     C                   if        §ctrno = 0
021100990910     C* ... restituisco esito OK.
021200990921     C                   eval      wrkesito = '0'
021300990910     C                   else
021400010201     C                   if        §ctrokvb > 0
021500990921     C                   eval      wrkesito = '1'
021600000710     C                   else
021700000710     C                   eval      wrkesito = '2'
021800990910     C                   endif
021900000710     C                   endif
022000990910     C*
022100990914     C                   if        %open(tivin00r)
022200990908     C                   close     tivin00r
022300990914     C                   endif
022400021113     C                   if        %open(fivabwwr)
022500021113     C                   close     fivabwwr
022600990914     C                   endif
022700021113     C                   if        %open(fivatwwr)
022800021113     C                   close     fivatwwr
022900010201     C                   endif
023000990910     C*
023100010201     C                   if        §ctrokvb > 0
023200000724     C                             and vlrpoi <> *zeros
023300010202     C                   exsr      invio
023400990920     C                   endif
023500990920     C*
023600910830     C                   ENDSR
023700000613     C***
023800010305
023900010305     C*----------------------------------------------------*
024000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024100010305     C*----------------------------------------------------*
024200020305     C     WRIVAB        BEGSR
024300010305     C*
024400060225     C* Quindi scarico il buffer del file d testata
024500021113     C                   write     fivab000                                     => scarico il VAB
024600010305     C*
024700010305     C                   ENDSR
024800990920
024900000801     C*----------------------------------------------------*
025000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025100000801     C*----------------------------------------------------*
025200010201     C     INZVAR        BEGSR
025300000801     C*
025400040802     C                   Z-ADD     *zeros        Num5_0            5 0
025500040802     C                   MOVEL     '0'           FlgCAS            1
025600000801     C*
025700000801     C                   ENDSR
025800000801     C*----------------------------------------------------*
025900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026000000801     C*----------------------------------------------------*
026100000801     C     DEFCAM        BEGSR
026200000801     C*
026300020619     C* Imposto i valori di default...
026400071207     C                   Z-ADD     1201284       VABCCM
026500071207     C                   Z-ADD     1201284       VATCCM
026600071207     C                   Z-ADD     120           VABLNP
026700071207     C                   Z-ADD     120           VATLNP
026800040714     C                   Z-ADD     000           VABCTR
026900060529     C                   MOVEL     '7Q'          VABCTM
027000060725     C                   MOVEL     '1'           VABCBO
027100020619     C* ... e poi verifico se sono stati passati come parametri
027200020619     C                   IF        vlrppt > *blanks
027300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
027400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027500020619     C                   EXSR      CHKNUM
027600020619     C                   IF        PiInt=*on
027700020619     C                   Z-ADD     PiVal         VABCCM
027800020619     C                   Z-ADD     PiVal         VATCCM
027900020619     C                   ENDIF
028000040506     C                   ENDIF
028100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
028200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028300020619     C                   EXSR      CHKNUM
028400020619     C                   IF        PiInt=*on
028500020619     C                   Z-ADD     PiVal         VABLNP
028600020619     C                   Z-ADD     PiVal         VATLNP
028700040506     C                   ENDIF
028800020619     C                   ENDIF
028900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
029000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029100020619     C                   EXSR      CHKNUM
029200020619     C                   IF        PiInt=*on
029300020619     C                   Z-ADD     PiVal         VABCTR
029400040506     C                   ENDIF
029500020619     C                   ENDIF
029600020619     C                   ENDIF
029700000801     C*
029800000801     C                   ENDSR
029900000801     C*----------------------------------------------------*
030000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030100000801     C*----------------------------------------------------*
030200040823     C     IMPVAB        BEGSR
030300040823     C*
030400020305     C                   EXSR      INZVAR
030500020305     C                   EXSR      DEFCAM
030600010305     C*
030700000801     C                   Z-ADD     *zeros        errore            1 0
030800000830     C                   MOVEL     datcor        VABAAS
030900020305     C                   MOVEL     datcor        VATAAS
031000040526     C                   MOVE      datcor        VABMGS
031100060529     C                   MOVE(P)   vlrpoi        VABFGS
031200060529     C                   MOVE(P)   vlrpoi        VATFGS
031300050628     C*
031400071207     C                   EVAL      VABRSD=%trim(%subst(vindta:130:30))
031500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031700020117     C* ==
031800071207     C                   EVAL      VABIND=%trim(%subst(vindta:160:30))
031900071207     C                   EVAL      VABLOD=%trim(%subst(vindta:190:25))
032000071207     C                   EVAL      VABPRD=%trim(%subst(vindta:215:2))
032100071207     C                   EVAL      VABNAS=%trim(%subst(vindta:316:30))
032200071224     C***                EVAL      VABRMO=%trim(%subst(vindta:110:14))
032300060725     C                   EVAL      VABNOT=%trim(%subst(vindta:322:35))
032400071207     C                   EVAL      VABRMA=%trim(%subst(vindta:249:2))+
032500071207     C                                    %trim(%subst(vindta:19:2))+
032600071207     C                                    %trim(%subst(vindta:252:9))
032700071224     C                   EVAL      VABRMO=VABRMA
032800071207     C                   IF        %trim(%subst(vindta:247:1)) = 'F'
032900060725     C                   EVAL      VABCBO='1'
033000060725     C                   ENDIF
033100071207     C                   IF        %trim(%subst(vindta:247:1)) = 'A'
033200071207     C                   EVAL      VABCBO='2'
033300060725     C                   ENDIF
033400060529     C* RMN
033500071207     C                   EVAL      PiStr=%trim(%subst(vindta:233:6))
033600060529     C                   EXSR      CHKNUM
033700060529     C                   IF        PiInt=*on
033800060529     C                   Z-ADD     PiVal         VABRMN
033900060529     C                   ELSE
034000060529     C                   ADD       1             errore
034100060529     C                   EVAL      vinmsg = %trimr(vinmsg)
034200060529     C                             + ' ' + 'VABRMN'
034300060529     C                   ENDIF
034400060529     C* NSP => Stacco un numeratore da AZNUM
034500060529     C                   clear                   TRUL33DS
034600060529     C                   eval      I33OPE = *zeros
034700060529     C                   eval      I33CNU = 302
034800060529     C                   eval      I33NUM = 1
034900060529     C                   movel     TRUL33DS      KPJBU
035000060529     C                   call      'TRUL33R'
035100060529     C                   parm                    KPJBA
035200060529     C                   movel     KPJBU         TRUL33DS
035300060529     C                   if        O33ERR = *zeros
035400060529     C                   z-add     O33NRF        VABNSP
035500060529     C                   z-add     O33NRF        VATNSP
035600060529     C                   else
035700060529     C                   Z-ADD     1             errore
035800060529     C                   EVAL      vinmsg = %trimr(vinmsg)
035900060529     C                             + ' ' + 'VABNSP VATNSP'
036000060529     C                   endif
036100060529     C* CAD
036200071207     C                   EVAL      PiStr=%trim(%subst(vindta:217:5))
036300010201     C                   EXSR      CHKNUM
036400010201     C                   IF        PiInt=*on
036500010201     C                   Z-ADD     PiVal         Num5_0
036600040506     C                   MOVEL(P)  Num5_0        VABCAD
036700010201     C                   ELSE
036800040506     C                   ADD       1             errore
036900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037000040506     C                             + ' ' + 'VABCAD'
037100010201     C                   ENDIF
037200040506     C* Reperisco la provincia dal CAP e dalla località
037300040526     C                   IF        VABCAD <> *blanks AND
037400040526     C                             VABPRD  = *blanks
037500040506     C                   CLEAR                   TISI95DS
037600040506     C                   EVAL      I95TCN = '3'
037700040506     C                   Z-ADD     datcor        I95DAT
037800040506     C                   EVAL      I95CAP = VABCAD
037900040506     C                   EVAL      I95LOC = VABLOD
038000050627     C                   EVAL      I95NAR = VABNZD
038100040506     C                   CALL      'TISI95R'
038200040506     C                   PARM                    TISI95DS
038300040506     C                   EVAL      VABPRD = O95PRV
038400040506     C                   ENDIF
038500060725     C* NCL
038600071207     C                   EVAL      PiStr=%trim(%subst(vindta:80:5))
038700060725     C                   EXSR      CHKNUM
038800060725     C                   IF        PiInt=*on
038900060725     C                   Z-ADD     PiVal         VABNCL
039000060725     C                   ELSE
039100060725     C                   ADD       1             errore
039200060725     C                   EVAL      vinmsg = %trimr(vinmsg)
039300060725     C                             + ' ' + 'VABNCL'
039400060725     C                   ENDIF
039500040506     C* PKB
039600071207     C                   EVAL      PiStr=%trim(%subst(vindta:71:9))
039700010201     C                   EXSR      CHKNUM
039800010201     C                   IF        PiNum=*on
039900071207     C                   EVAL      PiVal=PiVal / 1000                           * gestisco 3 dec.
040000060725     C                   Z-ADD(H)  PiVal         VABPKB
040100010201     C                   ELSE
040200010201     C                   ADD       1             errore
040300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040400010201     C                             + ' ' + 'VABPKB'
040500010201     C                   ENDIF
040600060725     C* CAS
040700071207     C                   IF        %subst(vindta:95:15)<>*all'0'
040800071207     C                   EVAL      FlgCAS='1'
040900060725     C                   EVAL      VABVCA='EUR'
041000071207     C                   EVAL      PiStr=%trim(%subst(vindta:95:15))
041100060307     C                   EXSR      CHKNUM
041200060307     C                   IF        PiNum=*on
041300071207     C                   EVAL      PiVal=PiVal / 1000                           * gestisco 3 dec.
041400060725     C                   Z-ADD     PiVal         VABCAS
041500060307     C                   ELSE
041600060307     C                   ADD       1             errore
041700060307     C                   EVAL      vinmsg = %trimr(vinmsg)
041800060307     C                             + ' ' + 'VABCAS'
041900060307     C                   ENDIF
042000060725     C                   ENDIF
042100060529     C*
042200060529     C* Gestione Disk C
042300060725     C                   exsr      exeVATE
042400010205     C*
042500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042600040802     C                   IF        FlgCAS <> '0'
042700071207     C                   IF        VABCBO = '1'
042800071207     C                   EVAL      VABCBO = '4'
042900071207     C                   ENDIF
043000071207     C                   IF        VABCBO = '2'
043100071207     C                   EVAL      VABCBO = '6'
043200071207     C                   ENDIF
043300071207     C                   ENDIF
043400020305     C*
043500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043600011113     C                   EXSR      CHKIMPDIV
043700010202     C*
043800000801     C* Ebbene...
043900000801     C                   ADD       1             §CTRMO
044000010201     C                   IF        errore <> *zeros
044100000801     C                   ADD       1             §CTRNO
044200000801     C                   EVAL      recko = vindta
044300000801     C                   EXCEPT    rigadett
044400000801     C                   EVAL      vinflg = '2'
044500000801     C                   ELSE
044600010201     C                   ADD       1             §CTROKVB
044700000801     C                   ENDIF
044800000801     C*
044900000801     C                   ENDSR
045000050628     C*----------------------------------------------------*
045100060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045200050628     C*----------------------------------------------------*
045300071207     C     EXEVATE       BEGSR
045400071207     C*
045500071207     C* Inizializzo progressivo collo su totale colli della spedizione
045600071207     C                   Z-ADD     *zeros        wNumColloDA
045700071207     C                   Z-ADD     *zeros        wNumColloA
045800071207     C* Collo "DA"
045900071207     C                   EVAL      PiStr=%trim(%subst(vindta:264:20))
046000071207     C                   EXSR      CHKNUM
046100071207     C                   IF        PiInt=*on
046200071207     C                   Z-ADD     PiVal         wNumColloDA
046300071207     C                   ELSE
046400071207     C                   ADD       1             errore
046500071207     C                   EVAL      vinmsg = %trimr(vinmsg)
046600071207     C                             + ' ' + 'VATNOT_E'
046700071207     C                   ENDIF
046800071207     C* Collo "A"
046900071207     C                   EVAL      PiStr=%trim(%subst(vindta:284:20))
047000071207     C                   EXSR      CHKNUM
047100071207     C                   IF        PiInt=*on
047200071207     C                   Z-ADD     PiVal         wNumColloA
047300071207     C                   ELSE
047400071207     C                   ADD       1             errore
047500071207     C                   EVAL      vinmsg = %trimr(vinmsg)
047600071207     C                             + ' ' + 'VATNOT_E'
047700071207     C                   ENDIF
047800071207     C*
047900071207     C* Costriuisco <N> barcode a seconda del numero d colli presenti sulla bolla
048000071207     C                   EVAL      VATTRC = 'E'
048100071207     C                   DOW       wNumColloDA <= wNumColloA
048200071207     C                   MOVEL(P)  wNumColloDA   VATNOT
048300071207     C                   EVAL      wNumColloDA = wNumColloDA + 1
048400071207     C                   write     FIVAT000
048500071207     C                   add       1             §CTROKVT
048600071207     C                   ENDDO
048700050628     C*
048800050628     C                   ENDSR
048900010202     C*----------------------------------------------------*
049000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049100010202     C*----------------------------------------------------*
049200020305     C     PREVAT        BEGSR
049300010202     C*
049400021113     C* Compongo il nome del membro da dare al FIVATWWR
049500010202     C                   eval      parmbr = vlrhdl
049600010202     C                   movel     'M'           parmbr
049700050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049800010202     C                   eval      paropz = '1'
049900010202     C* Effettuo la chiamata al CLLE preposto
050000040506     C                   call(e)   'TITVVTC'
050100010202     C                   parm                    parccm
050200010202     C                   parm                    parmbr
050300010202     C                   parm                    paropz
050400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050500010202     C                   if        %error
050600010202     C                   movel     '1'           chkcall
050700010202     C                   else
050800010202     C                   movel     '0'           chkcall
050900010202     C                   endif
051000010202     C*
051100010202     C                   ENDSR
051200000801     C*----------------------------------------------------*
051300000801     C*  CONTROLLO NUMERICITA' CAMPI
051400000801     C*----------------------------------------------------*
051500000801     C     CHKNUM        BEGSR
051600000801     C*
051700000801     C                   call(e)   'ISNUMERIC'
051800000801     C                   PARM                    PiStr            30
051900060725     C                   PARM      '.'           PiDecChr          1
052000000801     C                   PARM      *ZEROS        PiVal            30 9
052100000801     C                   PARM      '0'           PiInt             1
052200000801     C                   PARM      '0'           PiNum             1
052300000801     C                   IF        %error
052400000801     C                   EVAL      PiInt=*off
052500000801     C                   ENDIF
052600000801     C*
052700000801     C                   ENDSR
052800000801     C***
052900000801
053000011113
053100011113     C*----------------------------------------------------*
053200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053300011113     C*----------------------------------------------------*
053400011113     C     CHKIMPDIV     BEGSR
053500011113     C*
053600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053700011113     C                   Z-ADD     *zeros        wrkDec            9 9
053800011113     C*
053900011113     C* Come prima cosa effettuo considerazioni sulla divisa
054000011113     C                   IF        vabIAS > *zeros
054100011113     C                   IF        vabVAS <> 'EUR'
054200011113     C                   EVAL      vabVAS =  'ITL'
054300011113     C                   ENDIF
054400011113     C                   ENDIF
054500011113     C*
054600011113     C                   IF        vabCAS > *zeros
054700011113     C                   IF        vabVCA <> 'EUR'
054800011113     C                   EVAL      vabVCA =  'ITL'
054900011113     C                   ENDIF
055000011113     C                   ENDIF
055100011113     C*
055200011113     C                   IF        vabVMD > *zeros
055300020305     C                   IF        vabVAD <> 'EUR'
055400011113     C                   EVAL      vabVAD =  'ITL'
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055900011113     C                   Z-ADD     vabIAS        wrkDec
056000011113     C                   IF        wrkDec > *zeros
056100011113     C                   IF        vabVAS = 'ITL'
056200011113     C                   EVAL      vabIAS = *zeros
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056700011113     C                   Z-ADD     vabCAS        wrkDec
056800011113     C                   IF        wrkDec > *zeros
056900011113     C                   IF        vabVCA = 'ITL'
057000011113     C                   EVAL      vabCAS = *zeros
057100011113     C                   ENDIF
057200011113     C                   ENDIF
057300011113     C*
057400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057500011113     C                   Z-ADD     vabVMD        wrkDec
057600011113     C                   IF        wrkDec > *zeros
057700011113     C                   IF        vabVAD = 'ITL'
057800011113     C                   EVAL      vabVMD = *zeros
057900011113     C                   ENDIF
058000011113     C                   ENDIF
058100011113     C*
058200011113     C                   ENDSR
058300011113     C***
058400011113
058500011113
058600000801
058700000801
058800990920      /TITLE Invio dei dati al punto operativo.
058900010202     C     invio         BEGSR
059000990920     C*
059100021113     C* 1° invio FIVAT
059200010201     C                   reset                   dscmz
059300010201     C                   move      vlrpoi        cmzdst
059400021113     C                   eval      cmzfld = 'FIVATWWR'
059500010201     C                   eval      cmzmbd = vlrhdl
059600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059700021009     C***                if        prmfir = *blanks
059800021113     C                   eval      cmzfla = 'FIVAT00F'
059900021113     C                   eval      cmzmba = 'FIVAT00F'
060000021009     C***                else
060100021009     C***                eval      cmzfla = prmfir
060200021009     C***                eval      cmzmba = prmfir
060300021009     C***                endif
060400010201     C                   eval      cmznrr = *zeros
060500020305     C                   move      §ctrokvt      cmznrr
060600021018     C                   eval      cmzlba = vlrfl1
060700010201     C                   call(e)   'TIS711C'
060800010201     C                   parm                    dscmz
060900010201     C                   parm      *blanks       esito
061000010205     C                   if        %error
061100010205     C                             or cmzerr = '1'
061200010205     C                             or esito  = '1'
061300010205     C                   eval      wrkesito = '3'
061400010205     C                   else
061500010201     C*
061600021113     C* 2° invio FIVAB
061700010201     C                   reset                   dscmz
061800010201     C                   move      vlrpoi        cmzdst
061900010201     C                   eval      cmzfld = vlrfou
062000010201     C                   eval      cmzmbd = vlrhdl
062100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062200021009     C***                if        prmfir = *blanks
062300021113     C                   eval      cmzfla = 'FIVAB00F'
062400021113     C                   eval      cmzmba = 'FIVAB00F'
062500021009     C***                else
062600021009     C***                eval      cmzfla = prmfir
062700021009     C***                eval      cmzmba = prmfir
062800021009     C***                endif
062900010201     C                   eval      cmznrr = *zeros
063000010201     C                   move      §ctrokvb      cmznrr
063100021018     C                   eval      cmzlba = vlrfl1
063200010201     C                   call(e)   'TIS711C'
063300010201     C                   parm                    dscmz
063400010201     C                   parm      *blanks       esito
063500010201     C                   if        %error
063600010201     C                             or cmzerr = '1'
063700010201     C                             or esito  = '1'
063800010201     C                   eval      wrkesito = '3'
063900010201     C                   endif
064000010205     C                   endif
064100990920     C*
064200000613     C                   ENDSR
064300000613     C***
064400070411
064500070411     C     *pssr         BEGSR
064600070411     C*
064700070411     C                   if        %open(tivin00r)
064800070411     C                   close     tivin00r
064900070411     C                   endif
065000070411     C                   if        %open(fivabwwr)
065100070411     C                   close     fivabwwr
065200070411     C                   endif
065300070411     C                   if        %open(fivatwwr)
065400070411     C                   close     fivatwwr
065500070411     C                   endif
065600070411     C*
065700070411     C* Effettuo la chiamata al CLLE preposto
065800070411     C                   call(e)   'TITVVTC'
065900070411     C                   parm                    parccm
066000070411     C                   parm                    parmbr
066100070411     C                   parm      '2'           paropz
066200070411     C*
066300070411     C                   eval      wrkesito = '2'
066400070411     C*
066500070411     C                   seton                                        LR
066600070411     C*
066700070411     C                   ENDSR     '*CANCL'
066800070411     C***
066900070411
067000990910
067100000613     C     *inzsr        BEGSR
067200990910     C*
067300990910     C     *entry        plist
067400990920     C                   parm                    tivlrds
067500990921     C                   parm      wrkesito      esito
067600000724     C                   parm                    prmlit
067700000710     C                   parm                    prmfir
067800000613     C*
067900000830     C* CALCOLA LA DATA CORRENTE
068000000830     C                   time                    wn14             14 0
068100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068200000830     C                   z-add     wn8           g08dat
068300000830     C                   z-add     *zeros        g08inv
068400000830     C                   movel     '0'           g08err
068500000830     C                   call      'XSRDA8'
068600000830     C                   parm                    wlbda8
068700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068800000830     C*
068900000613     C                   ENDSR
069000000613     C***
069100990908
069200071207     Otitv1c8p  E            riepilogo         2
069300990915     O                                              'Upload via Internet'
069400990915     O                                           +1 'Traduzione TIVIN00R -'
069500021113     O                                           +1 'FIVABWWR/FIVATWWR'
069600010201     O                                           +1 'Report testate bolle'
069700990915     O          E            riepilogo   2
069800990915     O                       wrkdata
069900990915     O                       wrkora              +1
070000990915     O                       procname            +1
070100990915     O          E            riepilogo   2
070200990915     O                                              'Cliente..................:'
070300990915     O                       VABCCM        z     +1
070400990915     O          E            riepilogo   2
070500990920     O                                              'Riferimento Strategi.....:'
070600990920     O                       vlrhdl              +1
070700990915     O          E            riepilogo   2
070800990915     O                                              'Giusti...................:'
070900010201     O                       §CTROKVB      2   +  1
071000990915     O          E            riepilogo   2
071100990915     O                                              'Sbagliati e corretti.....:'
071200971022     O                       §CTRMO        2   +  1
071300990915     O          E            riepilogo   2
071400990915     O                                              'Sbagliati e scartati.....:'
071500971022     O                       §CTRNO        2   +  1
071600000613
071700071207     Otitv1c8ps E            testdett          2
071800000613     O                                              'Upload via Internet'
071900000613     O                                           +1 'Traduzione TIVIN00R -'
072000021113     O                                           +1 'FIVABWWR/FIVATWWR'
072100010201     O                                           +1 'Report testate bolle'
072200000616     O          E            testdett    3
072300000613     O                                           +2 'N° rec'
072400000613     O                                           +3 'Anteprima contenuto'
072500000616     O          E            rigadett    2
072600000613     O                       rrnum               +2
072700000621     O                       recko               +3
072800000616     O          E            findett     2
072900000613     O                       wrkdata
073000000613     O                       wrkora              +1
073100000613     O                       procname            +1
073200000616     O          E            findett     2
073300000613     O                                              'Cliente..................:'
073400000613     O                       VABCCM        z     +1
073500000616     O          E            findett     2
073600000613     O                                              'Riferimento Strategi.....:'
073700000613     O                       vlrhdl              +1
073800000616     O          E            findett     2
073900000613     O                                              'Giusti...................:'
074000010201     O                       §CTROKVB      2   +  1
074100000616     O          E            findett     2
074200000613     O                                              'Sbagliati e corretti.....:'
074300000613     O                       §CTRMO        2   +  1
074400000616     O          E            findett     2
074500000613     O                                              'Sbagliati e scartati.....:'
074600000613     O                       §CTRNO        2   +  1
074700000512** CMD - COMANDI CL
074800071207OVRPRTF FILE(TITV1C8P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074900071207OVRPRTF FILE(TITV1C8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075000071207DLTOVR FILE(TITV1C8P TITV1C8PS) LVL(*)
075100000512
075200000512
