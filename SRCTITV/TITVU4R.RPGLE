000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800060725     Ftitvu4p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060725     Ftitvu4ps  O    f  198        PRINTER usropn
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
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900000830     D*
005000990908
005100060307     D*------------------
005200060307     D* DS REPERIMENTO NUMERATORE
005300060307     D*------------------
005400060529     D trul33ds      e ds                  inz
005500060307     D*------------------
005600060307     D* DS ARCHITETTURA
005700060307     D*------------------
005800060307     D kpjba         e ds                  inz
005900060307     D*------------------
006000010201
006100010201
006200990915     C                   time                    wrkdata
006300990915     C                   time                    wrkora
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700000724     C*
006800000724     C* SE OCCORRE SPEDIRE IN FILIALE
006900000724     C                   if        vlrpoi <> *zeros
007000000724     C*
007100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007300000724     C     vlrpoi        chain     azorg01l
007400000724     C                   if        %found
007500000616     C                   movel(p)  CMD(1)        depcmd
007600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007700000616     C*
007800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007900011113     C                   Z-ADD     150           LENGH            15 5
008000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008100000616     C                   PARM                    depcmd
008200000616     C                   PARM                    LENGH
008300000724     C*
008400000724     C                   endif
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000616     C*
009700060725     C                   if        not %open(titvu4ps)
009800060725     C                   open      titvu4ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100000613     C*
010200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010400000613     C*
010500010202     C* Effettuo la chiamata al CLLE preposto
010600040506     C                   call(e)   'TITVVTC'
010700010202     C                   parm                    parccm
010800010202     C                   parm                    parmbr
010900010202     C                   parm      '2'           paropz
011000050201     C*
011100050201     C* Effettuo lancio TISI95 solo x chiusura
011200050201     C                   CLEAR                   TISI95DS
011300050201     C                   EVAL      I95TLA = 'C'
011400050201     C                   CALL      'TISI95R'
011500050201     C                   PARM                    TISI95DS
011600010202     C*
011700060725     C                   if        %open(titvu4ps)
011800000616     C                   except    findett
011900060725     C                   close     titvu4ps
012000000613     C                   endif
012100000616     C*
012200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    CMD(3)
012600000616     C                   PARM                    LENGH
012700000616     C*
012800000801     C
012900010201     C                   seton                                        LR
013000000613
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700060725     C                   if        not %open(titvu4p)
013800060725     C                   open      titvu4p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300060725     C                   if        %open(titvu4p)
014400060725     C                   close     titvu4p
014500990914     C                   endif
014600990910     C*
014700000613     C                   ENDSR
014800000613     C***
014900990908
015000000801
015100910830     C*--------------------------------------------------------
015200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015300910830     C*--------------------------------------------------------
015400040526     C     RWFILE        BEGSR
015500990910     C*
015600990914     C                   if        not %open(tivin00r)
015700990908     C                   open      tivin00r
015800990914     C                   endif
015900021113     C                   if        not %open(fivabwwr)
016000021113     C                   open      fivabwwr
016100990914     C                   endif
016200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016300020305     C                   exsr      prevat
016400010201     C*
016500010202     C                   if        chkcall = '0'
016600010202     C*
016700021113     C                   if        not %open(fivatwwr)
016800021113     C                   open      fivatwwr
016900010201     C                   endif
017000990910     C*
017100010201     C                   clear                   §CTROKVB          5 0
017200020305     C                   clear                   §CTROKVT          5 0
017300000801     C                   clear                   §CTRMO            5 0
017400000801     C                   clear                   §CTRNO            5 0
017500990910     C*
017600921023     C                   DO        *HIVAL
017700990913     C*
017800990915     C                   READ      tivin00r                               70
017900050627     C                   if        vindta > *blanks
018000000613     C                   add       1             rrnum
018100000801     C*
018200000801     C                   if        *in70 = *off
018300000801     C                             and
018400000801     C                             (vinflg = *blanks
018500000801     C                              or vinflg = '0'
018600000801     C                              or vinflg = '2')
018700000801     C*
018800000801     C                   clear                   vinmsg
018900000801     C                   eval      vinflg = '1'
019000010305     C*
019100050628     C                   clear                   fivab000
019200060725     C                   clear                   fivat000
019300050628     C                   exsr      impvab                                       => carico VAB
019400060725     C                   exsr      wrivab                                       => scarico VAB
019500000905     C*
019600000905     C                   else
019700000905     C                   eval      vinflg = '1'
019800050628     C                   endif
019900000905     C                   endif
020000000905     C*
020100000905     C  N70              update    tivin000
020200000905     C*
020300991022     C  N70              ENDdo
020400010202     C*
020500010202     C                   endif
020600990910
020700990910     C* Se non ci sono record con errori ...
020800000710     C                   if        §ctrno = 0
020900990910     C* ... restituisco esito OK.
021000990921     C                   eval      wrkesito = '0'
021100990910     C                   else
021200010201     C                   if        §ctrokvb > 0
021300990921     C                   eval      wrkesito = '1'
021400000710     C                   else
021500000710     C                   eval      wrkesito = '2'
021600990910     C                   endif
021700000710     C                   endif
021800990910     C*
021900990914     C                   if        %open(tivin00r)
022000990908     C                   close     tivin00r
022100990914     C                   endif
022200021113     C                   if        %open(fivabwwr)
022300021113     C                   close     fivabwwr
022400990914     C                   endif
022500021113     C                   if        %open(fivatwwr)
022600021113     C                   close     fivatwwr
022700010201     C                   endif
022800990910     C*
022900010201     C                   if        §ctrokvb > 0
023000000724     C                             and vlrpoi <> *zeros
023100010202     C                   exsr      invio
023200990920     C                   endif
023300990920     C*
023400910830     C                   ENDSR
023500000613     C***
023600010305
023700010305     C*----------------------------------------------------*
023800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
023900010305     C*----------------------------------------------------*
024000020305     C     WRIVAB        BEGSR
024100010305     C*
024200060225     C* Quindi scarico il buffer del file d testata
024300021113     C                   write     fivab000                                     => scarico il VAB
024400010305     C*
024500010305     C                   ENDSR
024600990920
024700000801     C*----------------------------------------------------*
024800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024900000801     C*----------------------------------------------------*
025000010201     C     INZVAR        BEGSR
025100000801     C*
025200040802     C                   Z-ADD     *zeros        Num5_0            5 0
025300040802     C                   MOVEL     '0'           FlgCAS            1
025400000801     C*
025500000801     C                   ENDSR
025600000801     C*----------------------------------------------------*
025700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
025800000801     C*----------------------------------------------------*
025900000801     C     DEFCAM        BEGSR
026000000801     C*
026100020619     C* Imposto i valori di default...
026200060725     C                   Z-ADD     0503107       VABCCM
026300060725     C                   Z-ADD     0503107       VATCCM
026400060725     C                   Z-ADD     050           VABLNP
026500060725     C                   Z-ADD     050           VATLNP
026600040714     C                   Z-ADD     000           VABCTR
026700060529     C                   MOVEL     '7Q'          VABCTM
026800060725     C                   MOVEL     '1'           VABCBO
026900020619     C* ... e poi verifico se sono stati passati come parametri
027000020619     C                   IF        vlrppt > *blanks
027100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
027200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027300020619     C                   EXSR      CHKNUM
027400020619     C                   IF        PiInt=*on
027500020619     C                   Z-ADD     PiVal         VABCCM
027600020619     C                   Z-ADD     PiVal         VATCCM
027700020619     C                   ENDIF
027800040506     C                   ENDIF
027900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
028000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028100020619     C                   EXSR      CHKNUM
028200020619     C                   IF        PiInt=*on
028300020619     C                   Z-ADD     PiVal         VABLNP
028400020619     C                   Z-ADD     PiVal         VATLNP
028500040506     C                   ENDIF
028600020619     C                   ENDIF
028700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
028800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028900020619     C                   EXSR      CHKNUM
029000020619     C                   IF        PiInt=*on
029100020619     C                   Z-ADD     PiVal         VABCTR
029200040506     C                   ENDIF
029300020619     C                   ENDIF
029400020619     C                   ENDIF
029500000801     C*
029600000801     C                   ENDSR
029700000801     C*----------------------------------------------------*
029800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029900000801     C*----------------------------------------------------*
030000040823     C     IMPVAB        BEGSR
030100040823     C*
030200020305     C                   EXSR      INZVAR
030300020305     C                   EXSR      DEFCAM
030400010305     C*
030500000801     C                   Z-ADD     *zeros        errore            1 0
030600000830     C                   MOVEL     datcor        VABAAS
030700020305     C                   MOVEL     datcor        VATAAS
030800040526     C                   MOVE      datcor        VABMGS
030900060529     C                   MOVE(P)   vlrpoi        VABFGS
031000060529     C                   MOVE(P)   vlrpoi        VATFGS
031100050628     C*
031200060725     C                   EVAL      VABRSD=%trim(%subst(vindta:34:35))
031300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031500020117     C* ==
031600060725     C                   EVAL      VABRD2=%trim(%subst(vindta:69:35))
031700060725     C                   EVAL      VABIND=%trim(%subst(vindta:104:35))
031800060725     C                   EVAL      VABLOD=%trim(%subst(vindta:148:35))
031900060725     C                   EVAL      VABPRD=%trim(%subst(vindta:183:2))
032000060725     C                   EVAL      VABNAS=%trim(%subst(vindta:212:15))
032100060725     C                   EVAL      VABNOT=%trim(%subst(vindta:322:35))
032200060912     C***                EVAL      VABNT2=%trim(%subst(vindta:357:35))
032300060912     C                   EVAL      VABNT2='AUTORIZZATA CONSEGNA FERMO DEPOSITO'
032400060725     C                   EVAL      VABRMA=%trim(%subst(vindta:5:4))+
032500060725     C                                    %trim(%subst(vindta:29:2))+
032600060725     C                                    %trim(%subst(vindta:21:8))
032700060725     C                   IF        %trim(%subst(vindta:29:2)) = 'A'
032800060725     C                   EVAL      VABCBO='1'
032900060725     C                   ENDIF
033000060725     C                   IF        %trim(%subst(vindta:29:2)) = 'B'
033100060725     C                   EVAL      VABCBO='4'
033200060725     C                   EVAL      FlgCAS='1'
033300060725     C                   ENDIF
033400060529     C* RMN
033500060725     C                   EVAL      PiStr=%trim(%subst(vindta:21:8))
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
036200060725     C                   EVAL      PiStr=%trim(%subst(vindta:139:9))
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
038600060725     C                   EVAL      PiStr=%trim(%subst(vindta:227:5))
038700060725     C                   EXSR      CHKNUM
038800060725     C                   IF        PiInt=*on
038900060725     C                   Z-ADD     PiVal         VABNCL
039000060725     C                   ELSE
039100060725     C                   ADD       1             errore
039200060725     C                   EVAL      vinmsg = %trimr(vinmsg)
039300060725     C                             + ' ' + 'VABNCL'
039400060725     C                   ENDIF
039500040506     C* PKB
039600060725     C                   EVAL      PiStr=%trim(%subst(vindta:232:7))
039700010201     C                   EXSR      CHKNUM
039800010201     C                   IF        PiNum=*on
039900060725     C                   Z-ADD(H)  PiVal         VABPKB
040000010201     C                   ELSE
040100010201     C                   ADD       1             errore
040200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040300010201     C                             + ' ' + 'VABPKB'
040400010201     C                   ENDIF
040500060725     C* CAS
040600060725     C                   IF        %subst(vindta:244:13)<>'000000000.000'
040700060725     C                   EVAL      VABVCA='EUR'
040800060725     C                   EVAL      PiStr=%trim(%subst(vindta:244:13))
040900060307     C                   EXSR      CHKNUM
041000060307     C                   IF        PiNum=*on
041100060725     C                   Z-ADD     PiVal         VABCAS
041200060307     C                   ELSE
041300060307     C                   ADD       1             errore
041400060307     C                   EVAL      vinmsg = %trimr(vinmsg)
041500060307     C                             + ' ' + 'VABCAS'
041600060307     C                   ENDIF
041700060725     C                   ENDIF
041800060529     C*
041900060529     C* Gestione Disk C
042000060725     C                   exsr      exeVATE
042100010205     C*
042200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042300040802     C                   IF        FlgCAS <> '0'
042400010205     C                   EVAL      VABCBO = '4'
042500010205     C                   ELSE
042600010205     C                   EVAL      VABCBO = '1'
042700010205     C                   ENDIF
042800020305     C*
042900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043000011113     C                   EXSR      CHKIMPDIV
043100010202     C*
043200000801     C* Ebbene...
043300000801     C                   ADD       1             §CTRMO
043400010201     C                   IF        errore <> *zeros
043500000801     C                   ADD       1             §CTRNO
043600000801     C                   EVAL      recko = vindta
043700000801     C                   EXCEPT    rigadett
043800000801     C                   EVAL      vinflg = '2'
043900000801     C                   ELSE
044000010201     C                   ADD       1             §CTROKVB
044100000801     C                   ENDIF
044200000801     C*
044300000801     C                   ENDSR
044400050628     C*----------------------------------------------------*
044500060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
044600050628     C*----------------------------------------------------*
044700060307     C     EXEVATE       BEGSR
044800050628     C*
044900050628     C* Valorizzo il barcode
045000050628     C                   EVAL      VATTRC='E'
045100060925     C                   EVAL      VATNOT=%subst(vindta:5:4) +
045200060925     C                                    %subst(vindta:29:1)+
045300060925     C                                    %subst(vindta:21:8)
045400050628     C                   exsr      wrivat                                       => scarico VAT
045500060912     C*
045600060912     C* Valorizzo il telefono del destinatario
045700060912     C                   EVAL      VATTRC='B'
045800060912     C                   EVAL      VATNOT=%trim(%subst(vindta:322:35))
045900060912     C                   IF        VATNOT=*blanks
046000060912     C                   EVAL      VATNOT='.'
046100060912     C                   ENDIF
046200060912     C                   exsr      wrivat                                       => scarico VAT
046300050628     C*
046400050628     C                   ENDSR
046500010201     C*----------------------------------------------------*
046600040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046700010201     C*----------------------------------------------------*
046800020305     C     WRIVAT        BEGSR
046900050628     C*
047000060223     C* Scrivo solo se valorizzato qualcosa
047100060223     C                   IF        VATNOT <> *blanks
047200040802     C                   WRITE     FIVAT000
047300060223     C                   ENDIF
047400010201     C*
047500010201     C                   ENDSR
047600010202     C*----------------------------------------------------*
047700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047800010202     C*----------------------------------------------------*
047900020305     C     PREVAT        BEGSR
048000010202     C*
048100021113     C* Compongo il nome del membro da dare al FIVATWWR
048200010202     C                   eval      parmbr = vlrhdl
048300010202     C                   movel     'M'           parmbr
048400050627     C                   eval      parccm = %subst(vlrKSC:2:7)
048500010202     C                   eval      paropz = '1'
048600010202     C* Effettuo la chiamata al CLLE preposto
048700040506     C                   call(e)   'TITVVTC'
048800010202     C                   parm                    parccm
048900010202     C                   parm                    parmbr
049000010202     C                   parm                    paropz
049100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049200010202     C                   if        %error
049300010202     C                   movel     '1'           chkcall
049400010202     C                   else
049500010202     C                   movel     '0'           chkcall
049600010202     C                   endif
049700010202     C*
049800010202     C                   ENDSR
049900000801     C*----------------------------------------------------*
050000000801     C*  CONTROLLO NUMERICITA' CAMPI
050100000801     C*----------------------------------------------------*
050200000801     C     CHKNUM        BEGSR
050300000801     C*
050400000801     C                   call(e)   'ISNUMERIC'
050500000801     C                   PARM                    PiStr            30
050600060725     C                   PARM      '.'           PiDecChr          1
050700000801     C                   PARM      *ZEROS        PiVal            30 9
050800000801     C                   PARM      '0'           PiInt             1
050900000801     C                   PARM      '0'           PiNum             1
051000000801     C                   IF        %error
051100000801     C                   EVAL      PiInt=*off
051200000801     C                   ENDIF
051300000801     C*
051400000801     C                   ENDSR
051500000801     C***
051600000801
051700011113
051800011113     C*----------------------------------------------------*
051900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052000011113     C*----------------------------------------------------*
052100011113     C     CHKIMPDIV     BEGSR
052200011113     C*
052300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052400011113     C                   Z-ADD     *zeros        wrkDec            9 9
052500011113     C*
052600011113     C* Come prima cosa effettuo considerazioni sulla divisa
052700011113     C                   IF        vabIAS > *zeros
052800011113     C                   IF        vabVAS <> 'EUR'
052900011113     C                   EVAL      vabVAS =  'ITL'
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C                   IF        vabCAS > *zeros
053400011113     C                   IF        vabVCA <> 'EUR'
053500011113     C                   EVAL      vabVCA =  'ITL'
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C                   IF        vabVMD > *zeros
054000020305     C                   IF        vabVAD <> 'EUR'
054100011113     C                   EVAL      vabVAD =  'ITL'
054200011113     C                   ENDIF
054300011113     C                   ENDIF
054400011113     C*
054500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054600011113     C                   Z-ADD     vabIAS        wrkDec
054700011113     C                   IF        wrkDec > *zeros
054800011113     C                   IF        vabVAS = 'ITL'
054900011113     C                   EVAL      vabIAS = *zeros
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055400011113     C                   Z-ADD     vabCAS        wrkDec
055500011113     C                   IF        wrkDec > *zeros
055600011113     C                   IF        vabVCA = 'ITL'
055700011113     C                   EVAL      vabCAS = *zeros
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056200011113     C                   Z-ADD     vabVMD        wrkDec
056300011113     C                   IF        wrkDec > *zeros
056400011113     C                   IF        vabVAD = 'ITL'
056500011113     C                   EVAL      vabVMD = *zeros
056600011113     C                   ENDIF
056700011113     C                   ENDIF
056800011113     C*
056900011113     C                   ENDSR
057000011113     C***
057100011113
057200011113
057300000801
057400000801
057500990920      /TITLE Invio dei dati al punto operativo.
057600010202     C     invio         BEGSR
057700990920     C*
057800021113     C* 1° invio FIVAT
057900010201     C                   reset                   dscmz
058000010201     C                   move      vlrpoi        cmzdst
058100021113     C                   eval      cmzfld = 'FIVATWWR'
058200010201     C                   eval      cmzmbd = vlrhdl
058300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058400021009     C***                if        prmfir = *blanks
058500021113     C                   eval      cmzfla = 'FIVAT00F'
058600021113     C                   eval      cmzmba = 'FIVAT00F'
058700021009     C***                else
058800021009     C***                eval      cmzfla = prmfir
058900021009     C***                eval      cmzmba = prmfir
059000021009     C***                endif
059100010201     C                   eval      cmznrr = *zeros
059200020305     C                   move      §ctrokvt      cmznrr
059300021018     C                   eval      cmzlba = vlrfl1
059400010201     C                   call(e)   'TIS711C'
059500010201     C                   parm                    dscmz
059600010201     C                   parm      *blanks       esito
059700010205     C                   if        %error
059800010205     C                             or cmzerr = '1'
059900010205     C                             or esito  = '1'
060000010205     C                   eval      wrkesito = '3'
060100010205     C                   else
060200010201     C*
060300021113     C* 2° invio FIVAB
060400010201     C                   reset                   dscmz
060500010201     C                   move      vlrpoi        cmzdst
060600010201     C                   eval      cmzfld = vlrfou
060700010201     C                   eval      cmzmbd = vlrhdl
060800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060900021009     C***                if        prmfir = *blanks
061000021113     C                   eval      cmzfla = 'FIVAB00F'
061100021113     C                   eval      cmzmba = 'FIVAB00F'
061200021009     C***                else
061300021009     C***                eval      cmzfla = prmfir
061400021009     C***                eval      cmzmba = prmfir
061500021009     C***                endif
061600010201     C                   eval      cmznrr = *zeros
061700010201     C                   move      §ctrokvb      cmznrr
061800021018     C                   eval      cmzlba = vlrfl1
061900010201     C                   call(e)   'TIS711C'
062000010201     C                   parm                    dscmz
062100010201     C                   parm      *blanks       esito
062200010201     C                   if        %error
062300010201     C                             or cmzerr = '1'
062400010201     C                             or esito  = '1'
062500010201     C                   eval      wrkesito = '3'
062600010201     C                   endif
062700010205     C                   endif
062800990920     C*
062900000613     C                   ENDSR
063000000613     C***
063100070411
063200070411     C     *pssr         BEGSR
063300070411     C*
063400070411     C                   if        %open(tivin00r)
063500070411     C                   close     tivin00r
063600070411     C                   endif
063700070411     C                   if        %open(fivabwwr)
063800070411     C                   close     fivabwwr
063900070411     C                   endif
064000070411     C                   if        %open(fivatwwr)
064100070411     C                   close     fivatwwr
064200070411     C                   endif
064300070411     C*
064400070411     C* Effettuo la chiamata al CLLE preposto
064500070411     C                   call(e)   'TITVVTC'
064600070411     C                   parm                    parccm
064700070411     C                   parm                    parmbr
064800070411     C                   parm      '2'           paropz
064900070411     C*
065000070411     C                   eval      wrkesito = '2'
065100070411     C*
065200070411     C                   seton                                        LR
065300070411     C*
065400070411     C                   ENDSR     '*CANCL'
065500070411     C***
065600070411
065700990910
065800000613     C     *inzsr        BEGSR
065900990910     C*
066000990910     C     *entry        plist
066100990920     C                   parm                    tivlrds
066200990921     C                   parm      wrkesito      esito
066300000724     C                   parm                    prmlit
066400000710     C                   parm                    prmfir
066500000613     C*
066600000830     C* CALCOLA LA DATA CORRENTE
066700000830     C                   time                    wn14             14 0
066800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066900000830     C                   z-add     wn8           g08dat
067000000830     C                   z-add     *zeros        g08inv
067100000830     C                   movel     '0'           g08err
067200000830     C                   call      'XSRDA8'
067300000830     C                   parm                    wlbda8
067400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067500000830     C*
067600000613     C                   ENDSR
067700000613     C***
067800990908
067900060725     Otitvu4p   E            riepilogo         2
068000990915     O                                              'Upload via Internet'
068100990915     O                                           +1 'Traduzione TIVIN00R -'
068200021113     O                                           +1 'FIVABWWR/FIVATWWR'
068300010201     O                                           +1 'Report testate bolle'
068400990915     O          E            riepilogo   2
068500990915     O                       wrkdata
068600990915     O                       wrkora              +1
068700990915     O                       procname            +1
068800990915     O          E            riepilogo   2
068900990915     O                                              'Cliente..................:'
069000990915     O                       VABCCM        z     +1
069100990915     O          E            riepilogo   2
069200990920     O                                              'Riferimento Strategi.....:'
069300990920     O                       vlrhdl              +1
069400990915     O          E            riepilogo   2
069500990915     O                                              'Giusti...................:'
069600010201     O                       §CTROKVB      2   +  1
069700990915     O          E            riepilogo   2
069800990915     O                                              'Sbagliati e corretti.....:'
069900971022     O                       §CTRMO        2   +  1
070000990915     O          E            riepilogo   2
070100990915     O                                              'Sbagliati e scartati.....:'
070200971022     O                       §CTRNO        2   +  1
070300000613
070400060725     Otitvu4ps  E            testdett          2
070500000613     O                                              'Upload via Internet'
070600000613     O                                           +1 'Traduzione TIVIN00R -'
070700021113     O                                           +1 'FIVABWWR/FIVATWWR'
070800010201     O                                           +1 'Report testate bolle'
070900000616     O          E            testdett    3
071000000613     O                                           +2 'N° rec'
071100000613     O                                           +3 'Anteprima contenuto'
071200000616     O          E            rigadett    2
071300000613     O                       rrnum               +2
071400000621     O                       recko               +3
071500000616     O          E            findett     2
071600000613     O                       wrkdata
071700000613     O                       wrkora              +1
071800000613     O                       procname            +1
071900000616     O          E            findett     2
072000000613     O                                              'Cliente..................:'
072100000613     O                       VABCCM        z     +1
072200000616     O          E            findett     2
072300000613     O                                              'Riferimento Strategi.....:'
072400000613     O                       vlrhdl              +1
072500000616     O          E            findett     2
072600000613     O                                              'Giusti...................:'
072700010201     O                       §CTROKVB      2   +  1
072800000616     O          E            findett     2
072900000613     O                                              'Sbagliati e corretti.....:'
073000000613     O                       §CTRMO        2   +  1
073100000616     O          E            findett     2
073200000613     O                                              'Sbagliati e scartati.....:'
073300000613     O                       §CTRNO        2   +  1
073400000512** CMD - COMANDI CL
073500060725OVRPRTF FILE(TITVU4P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
073600060725OVRPRTF FILE(TITVU4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
073700060725DLTOVR FILE(TITVU4P TITVU4PS) LVL(*)
073800000512
073900000512
