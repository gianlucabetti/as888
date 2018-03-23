000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200970703     H* FNLV13R  *---------------------------------------------------*
000300970703     H*     CONTROLLO CAP
000400921021     H*--------------------------------------------------------------*
000500970708     H* CONPORTAMENTI DEL TISI95R
000600970708     H* 1) SE TIPO CONTROLLO 7:
000700970708     H*   *CHIEDE GIA' DA SOLO LA PROVINCIA
000800970708     H*                         OBBLIGATORIA PER ITALIA (NON PER 3 E 4)
000900970708     H*   *PUO' RESTITUIRE I SEGUENTI DATI MODIFICATI IN AUTOMATICO :
001000970708     H*         CAP  NAZIONE  E  PROVINCIA
001100970708     H*         EMETTERE MSG DI AVVISO EVIDENZIANDO I DATI
001200970708     H* 2) PER ANDARE IN INTERROGAIZONE CAPPARIO IMMETTERE LA LOCALITA'
001300970708     H*    SENZA LA PROVINCIA E IL CAP
001400970708     H*--------------------------------------------------------------*
001500080414     D ERR             S             78    DIM(19) CTDATA PERRCD(1)
001600970701     D* DS PER TISI95R - CONTROLLO DI UN CAP
001700970701     D DSSI95        E DS                  EXTNAME(TISI95DS)
001800970707     D DSSI00        E DS                  EXTNAME(TISI00DS)
001900970707     D DSLV13        E DS                  EXTNAME(FNLV13DS)
002000060921     D TISI97DS      E DS
002100061030     D dcppflo       E DS
002200970707     D KPJBA         E DS
002300000721     D                SDS
002400000721     D  PARMS                 37     39  0
002500921021     C*---------------------------------------------------------------*
002600921021     C     *ENTRY        PLIST
002700970707     C                   PARM                    KPJBA
002800970707     C                   PARM                    DSLV13
002900970707     C                   PARM                    DSSI95
003000000721     C                   PARM                    FLGBAC            1
003100060921     C                   PARM                    tisi97ds
003200940218     C*---------------------------------------------------------------*
003300970707     C* TIPO LANCIO, I13TLA "C" -           CHIUSO CON LR
003400970707     C* TIPO LANCIO, I13TLA "L" - ELABORO E CHIUDO CON LR
003500970707     C* TIPO LANCIO, I13TLA " " - ELABORO E CHIUDO IN RETRN
003600970707    1C     I13TLA        IFNE      'C'
003700970707     C                   CLEAR                   O13ERR
003800970707     C                   CLEAR                   O13MSG
003900970708     C                   CLEAR                   O13ROP
004000970708     C                   CLEAR                   O13ROC
004100970708     C                   CLEAR                   O13RON
004200970708     C                   CLEAR                   O13ROL
004300970707     C**
004400970701     C                   CALL      'TISI95R'
004500970701     C                   PARM                    DSSI95
004600941019     C*
004700970703     C* NON TROVATI ERRORI
004800970707    2C     O95ERR        IFEQ      *BLANKS
004900060921     c
005000080407     c* Se richiesto controllo cappario utilizzi particolari
005100061030    3c                   if        parms=5
005200080407     c                   eval      o97uti='S'
005300060921     c                   call      'TISI97R'
005400060921     c                   parm                    dssi95
005500060921     c                   parm                    tisi97ds
005600060921     c* se c'e' errore imposto ed esco
005700060921    4c                   if        o97err<>' '
005800060921     C                   MOVEL     O97MSG        O13MSG
005900060921     c                   eval      o13err='5'
006000060921   x4c                   else
006100080521     c                   movel     o97flo        dcppflo
006200060921     c* se non utilizzabile per il network indicato errore
006300080521    5c                   if        o97uti='N'
006400080521    6c                   if        §cppall='S'
006500080411     c                   select
006600080411     c                   when      o97liv='C'
006700060921     C                   MOVEL     ERR(11)       O13MSG
006800080414     c                   eval      %subst(o13msg:40:3)=i97ntw
006900080411     c                   when      o97liv='N'
007000080411     C                   MOVEL     ERR(14)       O13MSG
007100080414     c                   eval      %subst(o13msg:41:3)=i97ntw
007200080414     c                   when      o97liv='W'
007300080414     C                   MOVEL     ERR(17)       O13MSG
007400080423     c                   eval      %subst(o13msg:9:3)=i97ntw
007500080411     c                   endsl
007600060921     C                   MOVEL     '5'           O13ERR
007700080521   x6c                   else
007800061030     c*
007900080521    7c                   if        §cppcas='S' and i95fca='S'
008000080411     c                   select
008100080411     c                   when      o97liv='C'
008200061030     C                   MOVEL     ERR(12)       O13MSG
008300080414     c                   eval      %subst(o13msg:40:3)=i97ntw
008400080411     c                   when      o97liv='N'
008500080411     C                   MOVEL     ERR(15)       O13MSG
008600080414     c                   eval      %subst(o13msg:41:3)=i97ntw
008700080414     c                   when      o97liv='W'
008800080414     C                   MOVEL     ERR(18)       O13MSG
008900080423     c                   eval      %subst(o13msg:9:3)=i97ntw
009000080411     c                   endsl
009100061030     C                   MOVEL     '5'           O13ERR
009200080521    7c                   endif
009300080521    7c                   if        §cpppas='S' and i95tpo='A'
009400080411     c                   select
009500080411     c                   when      o97liv='C'
009600080411     C                   MOVEL     ERR(13)       O13MSG
009700080414     c                   eval      %subst(o13msg:40:3)=i97ntw
009800080411     c                   when      o97liv='N'
009900080411     C                   MOVEL     ERR(16)       O13MSG
010000080414     c                   eval      %subst(o13msg:41:3)=i97ntw
010100080414     c                   when      o97liv='W'
010200080414     C                   MOVEL     ERR(19)       O13MSG
010300080423     c                   eval      %subst(o13msg:9:3)=i97ntw
010400080411     c                   endsl
010500080411     C                   MOVEL     '5'           O13ERR
010600080521    7c                   endif
010700080521    6c                   endif
010800061030    5c                   endif
010900060921     c
011000060921    4c                   endif
011100060921    3c                   endif
011200970707     C**
011300970707     C* LIVELLO AFFIDABILITA'0 : SEMPRE ERRORE
011400970707    3C     I13AF0        IFEQ      'S'
011500970708   3AC     O95LIA        IFEQ      '0'
011600970707     C**
011700970703     C* SE IL CAP L'HA TROVATO DO ERRORE DI INCONGRUENZA
011800970708    4C     O95LID        IFLT      '2'
011900970708     C                   MOVEL     ERR(2)        O13MSG
012000970707     C                   MOVEL     '5'           O13ERR
012100970703     C                   ELSE
012200970708     C                   MOVEL     ERR(1)        O13MSG
012300970707     C                   MOVEL     '5'           O13ERR
012400970707    4C                   ENDIF
012500970703     C*
012600970703     C                   GOTO      ENDC01
012700970708   3AC                   ENDIF
012800970708   X3C                   ELSE
012900970708    4C     O95LID        IFEQ      '1'
013000970708     C     O95NUF        ANDNE     'S'
013100970708     C                   MOVEL     ERR(2)        O13MSG
013200970708     C                   MOVEL     '5'           O13ERR
013300970710     C                   GOTO      ENDC01
013400970708    4C                   ENDIF
013500970708    3C                   ENDIF
013600970704     C*
013700970704     C* CAP E PROVINCIA DEVONO ESSERE CONGRUENTI
013800970707    3C     I13CNG        IFEQ      'S'
013900970707     C     I95TCN        ANDEQ     '3'
014000970707     C*
014100970707    4C     O95NCP        IFEQ      'S'
014200970704     C     I95PRV        ANDNE     O95PRV
014300970704     C     I95PRV        ANDNE     O95PRA
014400970708     C                   MOVEL     ERR(3)        O13MSG
014500970707     C                   MOVEL     '5'           O13ERR
014600970707     C                   GOTO      ENDC01
014700970707    4C                   ENDIF
014800970707    3C                   ENDIF
014900970708     C**
015000970708     C* SE LIVELLO DATI 3 O 4 E AFFIDABILITA' 2 O > PROPONGO I DATI
015100970708     C*  CHE SI SONO MODIFICATI CON MSG DI AVVISO
015200970708    1C     O95LID        IFEQ      '3'
015300970708     C     O95LID        OREQ      '4'
015400970708    2C     O95LIA        IFGE      '2'
015500970708     C*
015600970708    3C     O95PRV        IFNE      I95PRV
015700970714    3C     O95PRA        ANDNE     I95PRV
015800970708     C                   MOVEL     '4'           O13ERR
015900000721     C*
016000000721     C     PARMS         IFEQ      4
016100000721     C     FLGBAC        ANDEQ     'S'
016200000721     C                   MOVEL     ERR(10)       O13MSG
016300000721     C                   ELSE
016400970708     C                   MOVEL     ERR(8)        O13MSG
016500000721     C                   ENDIF
016600970708     C                   MOVEL     'S'           O13ROP
016700970708    3C                   ENDIF
016800970708    3C     O95NAR        IFNE      I95NAR
016900970708     C                   MOVEL     '6'           O13ERR
017000000721     C*
017100000721     C     PARMS         IFEQ      4
017200000721     C     FLGBAC        ANDEQ     'S'
017300000721     C                   MOVEL     ERR(10)       O13MSG
017400000721     C                   ELSE
017500970708     C                   MOVEL     ERR(8)        O13MSG
017600000721    3C                   ENDIF
017700000721     C*
017800970708     C                   MOVEL     'S'           O13RON
017900970708    3C                   ENDIF
018000970708    3C     O95CAP        IFNE      I95CAP
018100970708     C                   MOVEL     '5'           O13ERR
018200000721     C*
018300000721     C     PARMS         IFEQ      4
018400000721     C     FLGBAC        ANDEQ     'S'
018500000721     C                   MOVEL     ERR(10)       O13MSG
018600000721     C                   ELSE
018700970708     C                   MOVEL     ERR(8)        O13MSG
018800000721    3C                   ENDIF
018900000721     C*
019000970708     C                   MOVEL     'S'           O13ROC
019100970708    3C                   ENDIF
019200970708     C     O13ERR        IFNE      ' '
019300970708     C                   GOTO      ENDC01
019400970708    2C                   ENDIF
019500970708    2C                   ENDIF
019600970708    1C                   ENDIF
019700970710     C*
019800000713     C* VEDO SE DARE ERRORE PER LIVELLO DI AFFIDABILITA' = 2 O 1
019900000713     C*  SE NON FACCIO I CONTROLLI SPECIFICI PER LIVELLO 1
020000970710     C     O95LIA        IFEQ      '2'
020100000713     C     I13AF1        OREQ      ' '
020200000713     C     O95LIA        ANDEQ     '1'
020300000713     C*
020400000713     C     O95NUF        IFNE      'S'
020500970710     C     I13LA3        ANDEQ     'S'
020600970710     C     E13FZ3        ANDNE     1
020700970710     C                   MOVEL     '5'           O13ERR
020800970710     C     I13SZ3        IFEQ      'S'
020900970710     C                   MOVEL     ERR(9)        O13MSG
021000970710     C                   MOVEL     '1'           E13FZ3
021100970710     C                   ELSE
021200970710     C                   MOVEL     ERR(1)        O13MSG
021300970710     C                   ENDIF
021400970710     C                   GOTO      ENDC01
021500970710     C                   ENDIF
021600000713     C                   ENDIF
021700970707     C*
021800970707     C* CAP CON VIARIO
021900970707    4C     O95FLG        IFEQ      'S'
022000970707     C*
022100970707    3C     I13CNV        IFEQ      'S'
022200970707     C     E13FZV        ANDNE     1
022300970707     C                   MOVEL     '5'           O13ERR
022400970707     C* ERRORE FORZABILE SE = "S"
022500970707    5C     I13SZV        IFEQ      'S'
022600970708     C                   MOVEL     ERR(5)        O13MSG
022700970707     C                   MOVEL     1             E13FZV
022800970707     C                   ELSE
022900970708     C                   MOVEL     ERR(4)        O13MSG
023000970707    5C                   ENDIF
023100970707     C                   GOTO      ENDC01
023200970707    4C                   ENDIF
023300970707    3C                   ENDIF
023400970707     C**
023500970707     C* PROCEDURA AFFIDABILITA' =1
023600970707     C*  LA 1 VOLTA VADO IN INTERROGAZIONE
023700970707    3C     I13AF1        IFEQ      'S'
023800970707     C     O95LIA        ANDEQ     '1'
023900970707     C*
024000970707    4C     E13FZ1        IFNE      1
024100970707     C* RICHIAMO L'INTERROGAZIONE
024200970707     C                   CLEAR                   DSSI00
024300970707     C                   MOVEL     'L01'         D00OP0
024400970707     C                   MOVEL     I95LOC        D00LOC
024500970707     C                   MOVEL     I95DAT        D00DRI
024600970707     C                   MOVEL     1             E13FZ1
024700970707     C                   CALL      'TISI30R'
024800970707     C                   PARM                    KPJBA
024900970707     C                   PARM                    DSSI00
025000970707     C* SE SELEZIONE
025100970707    5C     D00CAP        IFNE      *BLANKS
025200970707     C                   MOVEL     D00CAP        O95CAP
025300970710     C                   MOVEL     'S'           O13ROC
025400970707     C                   MOVEL     D00LOC        O95LOC
025500970710     C                   MOVEL     'S'           O13ROL
025600970707     C                   MOVEL     D00NAR        O95NAR
025700970710     C                   MOVEL     'S'           O13RON
025800970707     C                   MOVEL     D00PRV        O95PRV
025900970710     C                   MOVEL     'S'           O13ROP
026000970707     C                   MOVEL     '3'           O13ERR
026100970707     C                   GOTO      ENDC01
026200970707    5C                   ENDIF
026300970707    4C                   ENDIF
026400970707     C**
026500970707     C* SE C'E' ANCORA LIVELLO AFFIDABILITA' 1 : ERRORE FORZABILE
026600970707    4C     E13FZ2        IFNE      1
026700970707     C                   MOVEL     '3'           O13ERR
026800970707     C*
026900970707    5C     I13SZ2        IFEQ      'S'
027000970708     C                   MOVEL     ERR(7)        O13MSG
027100970707     C                   MOVEL     1             E13FZ2
027200970707     C                   ELSE
027300970708     C                   MOVEL     ERR(6)        O13MSG
027400970707    5C                   ENDIF
027500970707     C                   GOTO      ENDC01
027600970707    4C                   ENDIF
027700970707     C**
027800970707     C* LA PROVINCIA DEVE PERO' ESSERE CONGRUENTE CON IL CAP ALMENO
027900970707    4C     O95NCP        IFEQ      'S'
028000970707     C     I95PRV        ANDNE     O95PRV
028100970707     C     I95PRV        ANDNE     O95PRA
028200970707     C                   MOVEL     ERR(3)        O13MSG
028300970707     C                   MOVEL     '4'           O13ERR
028400970707     C                   GOTO      ENDC01
028500970707    4C                   ENDIF
028600970707    3C                   ENDIF
028700970707     C**
028800970707   X2C                   ELSE
028900970707     C**
029000970707     C                   MOVEL     O95MSG        O13MSG
029100970707     C* ERRORE SU :
029200970707     C                   SELECT
029300970707     C     O95ERR        WHENEQ    'C'
029400970707     C     O95ERR        OREQ      'E'
029500970707     C                   MOVEL     '4'           O13ERR
029600970707     C     O95ERR        WHENEQ    'D'
029700970707     C     O95ERR        OREQ      'B'
029800970707     C     O95ERR        OREQ      'H'
029900970707     C     O95ERR        OREQ      'I'
030000970707     C                   MOVEL     '3'           O13ERR
030100970707     C     O95ERR        WHENEQ    '1'
030200970715     C                   MOVEL     '6'           O13ERR
030300970707     C                   OTHER
030400970707     C                   MOVEL     '5'           O13ERR
030500970707     C                   ENDSL
030600970707    2C                   ENDIF
030700970707    1C                   ENDIF
030800970703     C*
030900941019     C     ENDC01        TAG
031000941019     C* CHIUSURA PGM
031100970707    1C     I13TLA        IFEQ      ' '
031200941019     C                   RETURN
031300941019     C                   ELSE
031400970701     C*
031500970701     C                   CLEAR                   DSSI95
031600970701     C                   MOVEL     'C'           I95TLA
031700970701     C                   CALL      'TISI95R'
031800970701     C                   PARM                    DSSI95
031900970701     C*
032000941019     C                   SETON                                        LR
032100941019    1C                   ENDIF
032200000721**
032300970710C.A.P. incongruente con provincia e/o localita'                                1
032400970707C.A.P. inesistente                                                             2
032500970707C.A.P. incongruente con la provincia                                           3
032600970707Inserito un CAP generico di citta' che ha il Viario                            4
032700970707Inserito un CAP generico di citta' che ha il Viario: premere ENTER per FORZARE 5
032800970707Localita' INESISTENTE per CAP con eccezioni di localita'                       6
032900970707Localita' INESISTENTE per CAP con eccezioni di localita': ENTER per forzare    7
033000970714ATTENZIONE!! Presenti dati incongruenti: modificati.Enter per continuare
033100970710C.A.P. incongruente con provincia e/o localita': ENTER per forzare             9
033200000721ATTENZIONE!! Presenti dati incongruenti: modificati.                          10
033300061030C.A.P. non utilizzabile per il network xxx indicato per tutte le spedizioni
033400061030C.A.P. non utilizzabile per il network xxx indicato per spediz. con C/Assegno
033500080411C.A.P. non utilizzabile per il network xxx indicato per spediz. in porto Ass. 13
033600080414Nazione non utilizzabile per il network xxx per tutte le spedizioni           14
033700080414Nazione non utilizzabile per il network xxx per spediz. con C/Assegno         15
033800080414Nazione non utilizzabile per il network xxx per spediz. in porto Assegnato    16
033900080414Network xxx non utilizzabile per tutte le spedizioni                          17
034000080414Network xxx non utilizzabile per spedizioni con C/Assegno                     18
034100080414Network xxx non utilizzabile per spedizioni in porto assegnato                19
