000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300061012     fdpcro01l  If   E           K DISK
000400061013     fwfcro00f  uf a E           k DISK
000500061012     ftabel00f  If   E           K DISK
000600061012     fqsysprt   O    F  132        PRINTER  oflind(*INOF)
000700061012     fqsysprtx  O    F  132        PRINTER  oflind(*INOe)
000800980611      * ---------------------------------------------------------
000900061013     D msg             S             78    DIM(2) ctdata perrcd(1)              CARATT.CONTROLLO
001000061013     D CCP             S              1    DIM(9)                               CARATT.CONTROLLO
001100061012     D CAP             S              1    DIM(9)                               CAP DA CONTROLLAR
001200061012     D CAF             S              1    DIM(9)                               CAP FITTIZIO
001300061012     D nar             S              3    DIM(300)                             CAP FITTIZIO
001400061012     D iso             S              2    DIM(300)                             CAP FITTIZIO
001500061012     D pcf             S              1  0 DIM(300)                             CAP FITTIZIO
001600061012     D ecf             S              1  0 DIM(300)                             CAP FITTIZIO
001700061012     d
001800061012     D tisiE2ds      E DS
001900061012     D tisi95ds      E DS
002000061012     D ds15          E DS
002100981029
002200061012     D kcod            S                   LIKE(tblcod)
002300061012     D XX              s              3  0 inz
002400061012     d posin           s              2  0
002500061012     d lung            s              2  0
002600980625      * ---------------------------------------------------------
002700061012     c* recupero versione cappario dpda a udate
002800061012     c                   clear                   tisie2ds
002900061012     c                   eval      sie2dri=20061012
003000061012     c                   call      'TISIE2R'
003100061012     c                   parm                    tisie2ds
003200061012     c*  carico la tabella nazioni con relativo cod iso e composizione
003300061012     c*  CAP fittizio
003400061012     c                   exsr      cartab
003500061012     c
003600061012     c     sie2ver       setll     dpcro01l
003700061012     c     sie2ver       reade     dpcro01l
003800061012    1c                   dow       not %eof(dpcro01l)
003900061013     c                   clear                   primav            1
004000061013     c
004100061013     c                   z-add     1             xx
004200061013     c     croiso2       lookup    iso(XX)                                90
004300061013    2c                   if        not *in90
004400061013     c* stampa non trovato codice iso
004500061013     c* Memorizzo in file
004600061013     c                   seton                                        01
004700061013     c                   except    staerr
004800061013     c                   setoff                                       01
004900061013     c* file
005000061013     c                   movel     croptcb       crocap
005100061013     c                   movel     msg(1)        cromsg
005200061013     c                   clear                   crocont
005300061013     c                   write     wfcro000
005400061013   x2c                   else
005500061012     c
005600061012     c* CAP "normalizzato" bartolini
005700061012     c* Se c'e' uguale potengo così
005800061012     C                   MOVEl     croptcb       WCAPFI            9            *MEM CAP FITTIZIO
005900061012     c                   exsr      LETCAP
006000061012     c
006100061013    3c                   if        o95err=*blanks
006200061012     c
006300061012     c                   if        *in01 and not *in01
006400061012     c                   except    staOK
006500061012     c                   endif
006600061012     c
006700061013   x3c                   else
006800061012     c
006900061012     c* Recupero cap fittizio
007000061012     C                   MOVEA     croptcb       CAP
007100061012     C                   MOVEA     croptcb       CCP
007200061012     C*
007300061012     C* RECUPERO CAP FITTIZIO PER EVENTUALE TENTATIVO SUCCESSIVO
007400061012     C                   Z-ADD     pcf(xx)       P                 2 0
007500061012     C                   CLEAR                   CAF
007600061012     C                   MOVEA     CAP(P)        CAF(P)
007700061012     C                   ADD       ecf(xx)       P                              *CARATT.DA TENERE
007800061012     c
007900061012     c* Addiziono 1 al cal fittizio perchè nel loop sotto
008000061012     c*  di controllo parte sottraendo 1 carattere al cap fittizio
008100061012     c                   add       1             p
008200061012     c
008300061012     C     P             IFLT      10
008400061012     C                   MOVEA     *BLANKS       CAF(P)
008500061012     C                   ENDIF
008600061012     c
008700061012     c                   eval      o95err='E'
008800061012     c
008900061012     c* Cerco se esiste su cappario
009000061012    4c                   dow       o95err<>*blanks and p>pcf(xx)
009100061012     c* tolto 1 carattere per il fittizio
009200061012     c                   sub       1             p
009300061012     c                   movel     *blanks       caf(P)
009400061012     C                   MOVEA     CAF           WCAPFI            9            *MEM CAP FITTIZIO
009500061013     c
009600061013     c* La prima volta memorizzo il cap fittizio completo
009700061013     c                   if        primav=' '
009800061013     c                   movel     wcapfi        crocap
009900061013     c                   eval      primav='S'
010000061013     c                   endif
010100061012     c
010200061012     c                   exsr      LETCAP
010300061012     c*
010400061012    4c                   enddo
010500061012     c
010600061012     c                   z-add     pcf(xx)       §15pcf
010700061012     c                   z-add     ecf(xx)       §15ecf
010800061012     c
010900061012     c*Errore : non trovato nostro cap fittizio
011000061012    4c                   if        p=pcf(xx) and o95err<>' '
011100061012     c                   seton                                        02
011200061012     c                   except    staerr
011300061012     c                   setoff                                       02
011400061013     c* Scrivo file
011500061013     c* Imposto il cap e vedo se c'e' già sul file
011600061013     c     kcro          chain     wfcro00f
011700061013     c                   if        %found(wfcro00f)
011800061013     c                   add       1             crocont
011900061013     c                   update    wfcro000
012000061013     c                   else
012100061013     c                   z-add     1             crocont
012200061013     c                   movel     msg(2)        cromsg
012300061013     c                   write     wfcro000
012400061013     c                   endif
012500061013     c
012600061012   x4c                   else
012700061012     c
012800061012     c* Scrivo il cap modo bartolini
012900061012     c* 1° PARTE DEL CAP SE FITTIZIO NON INIZIa DALLA POSIZIONE 1
013000061012    5C                   IF        PCF(xx)>1
013100061012     c                   movea     *blanks       ccp(§15pcf)
013200061012     c                   movea     ccp           osie8capb
013300061012     c                   else
013400061012     c                   clear                   osie8capb         9
013500061012    5C                   ENDIF
013600061012     c*
013700061012     c* 2° PARTE --> cap fittizio
013800061012     c                   eval      %subst(osie8capb:§15pcf:§15ecf)=
013900061012     c                             %subst(wcapfi:§15pcf:§15ecf)
014000061012     c* 3° PARTE --> parte finale del cap
014100061012     c                   eval      lung= (9-§15pcf-§15ecf+1)
014200061012     c                   eval      posin=(§15pcf+§15ecf)
014300061012     c*
014400061012     c                   clear                   finecap
014500061012     c                   movea     ccp(P)        finecap           9
014600061012     c                   eval      %subst(osie8capb:posin:lung)=finecap
014700061012     c* Lo  stampo
014800061012     c                   except    staoktrad
014900061012    4c                   endif
015000061012    3c                   endif
015100061012    2c                   endif
015200061012     C*
015300061012     c     sie2ver       reade     dpcro01l
015400061012     c                   enddo
015500061012     c
015600061012     c                   SETON                                        lr
015700061012     c*--------------------------------------------------------------------------------------------*
015800061012     C* Lettura con cap per verificare se esiste il fittizio
015900061012     c*--------------------------------------------------------------------------------------------*
016000061012     c     LETCAP        BEGSR
016100061012     c* richiamo il tisi95 pasato il cap
016200061012     c                   clear                   tisi95ds
016300061012     c                   movel     '3'           i95tcn
016400061012     C                   MOVEL     wcapfi        I95CAP
016500061012     C                   MOVEL     nar(xx)       I95NAR
016600061012     C                   MOVEL     20061012      I95DAT
016700061012     c                   call      'TISI95R'
016800061012     C                   parm                    tisi95ds
016900061012c
017000061012     c                   ENDSR
017100061012     c*--------------------------------------------------------------------------------------------*
017200061012     c     CARTAB        BEGSR
017300061012     c     ktab          klist
017400061012     c                   kfld                    codut
017500061012     c                   kfld                    kcod
017600061013     c     kcro          klist
017700061013     c                   kfld                    crover
017800061013     c                   kfld                    croiso2
017900061013     c                   kfld                    crocap
018000061012     c
018100061012     c                   z-add     1             xx
018200061012     c                   z-add     1             codut             1 0
018300061012     c                   eval      kcod='15'
018400061012     c     ktab          setll     tabel00f
018500061012     c     ktab          reade     tabel00f
018600061012     c                   dow       not %eof(tabel00f)
018700061012     c                   movel     tbluni        ds15
018800061012     c                   add       1             xx
018900061012     c                   movel     tblkey        nar(xx)
019000061012     c                   movel     §15cod        iso(xx)
019100061012     c                   movel     §15pcf        pcf(xx)
019200061012     c                   movel     §15ecf        ecf(xx)
019300061012     c     ktab          reade     tabel00f
019400061012     c                   enddo
019500061012     c
019600061012     c                   ENDSR
019700980611      *------------------------------------------------------------
019800061012     Oqsysprtx  E            staok       1
019900061012     o                                              'CAP dpd     presenti:'
020000061012     O                       croiso2          +   2
020100061012     O                       croptcb          +   2
020200061012     O                       croptce          +   2
020300061012     Oqsysprtx  E            staoktrad   1
020400061012     o                                              'CAP dpd     presenti:'
020500061012     O                       croiso2          +   2
020600061012     O                       croptcb          +   2
020700061012     O                       croptce          +   2
020800061012     O               02                       +   2 'CAP dpd tradotto "modo'
020900061012     o                                        +   1 'Bartolini'
021000061012     O                       osie8capb        +   2
021100061012     Oqsysprt   E            staERR      1
021200061012     o                                              'CAP dpd NON presenti:'
021300061012     O                       croiso2          +   2
021400061012     O                       croptcb          +   2
021500061012     O                       croptce          +   2
021600061012     O               01                         100 'non trovato codice ISO'
021700061012     O               02                         100 'non trovato CAP fittizio'
021800061013**
021900061013NON TROVATO CODICE ISO NELLA TABELLA "15" DELLE NAZIONI
022000061013NON TROVATO CAP  FITTIZIO NEL CAPPARIO
