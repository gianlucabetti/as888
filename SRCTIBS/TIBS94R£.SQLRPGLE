000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200101103      * TIBS94R *----------------------------------------------------*
000300101103      *       Elimina picking Messaggerie per una filiale
000400940915      *--------------------------------------------------------------*
000500101103     FTIBS94D   CF   E             WORKSTN
000600101103     FAZORG01L  uF   E           K DISK    USROPN extfile(Wazorg)
000700101103     Ffnfvv03l  IF   E           K DISK
000800050103     Ftntbe01l  IF   E           K DISK
000801170512     Ffiapd01l  uF   E           K DISK
000900020805      *
001000020805      * DEFINIZIONE SCHIERE
001100960302     D*
001200050103     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001300040917     D KPJBA         E DS
001400040917     D CNCR80        E DS
001500101103     D og150         E DS
001600040917     D UT§DSE0F      E DS
001601170512     d dapdflr       e ds                  inz
001602170512     D fiapdds       E DS                  extname(fiapd00f)
001603170512     d fnlv36ds      e ds                  inz
001604170512     d fiapda        e ds                  extname(fiapd00f)
001605170512     d                                     prefix(a)
001606170512     d fiapdb        e ds                  extname(fiapd00f)
001607170512     d                                     prefix(b)
001608170512     d fiapd4A       e ds                  extname(fiapd40f)
001609170512     d                                     prefix(a)
001610170512     d fiapd4b       e ds                  extname(fiapd40f)
001611170512     d                                     prefix(b)
001700050103     d Dataiso         s               d   datfmt(*iso)
001800050103     d Dataeur         s               d   datfmt(*eur)
001900101103     d Datasys         s               d   inz(*sys) datfmt(*iso)
002000101103     d dateu           s              8  0
002100101103     d apdD            s              7  0
002200101103     d apdA            s              7  0
002300050103     d
002400101103     d knpg            s                   Like(fvvnpg) inz(3)
002500101103     d kTbeCod         s                   Like(TbeCod)
002600050103     d kTbeKe1         s                   Like(TbeKe1)
002700101103
002800101104     d WAZORG          s             21    inz('FILTRAGRU /AZORG01L')
002900101103     d wrkgetlista     s           4096    varying
003000041220     d
003100040917     C**********************************************************************
003200050103     c     Ktbe          Klist
003300050103     c                   Kfld                    kTbeCod
003400050103     c                   Kfld                    kTbeKe1
003500101103     c     Kfvv          Klist
003600101103     c                   Kfld                    v1cpo
003700101103     c                   Kfld                    knpg
003701170512     c     Kapd          Klist
003702170512     c                   Kfld                    tipo              1
003703170512     c                   Kfld                    apdd
003704170512     c                   move      'A'           tipo
003800170512     C************************************************* ********************
003900040917     C     *ENTRY        PLIST
004000040917     C                   PARM                    KPJBA
004100050103     c*
004200040917     C                   Z-ADD     1             CODUT
004300040917     C                   CALL      'X§PARUT'
004400040917     C                   PARM                    UT§DSE0F
004500040917     C                   MOVEL     REC80         CNCR80
004600040917     C                   MOVEL     RAGUT         VIDRSU
004700041220     C                   CLEAR                   V1CPO
004800041220     C                   CLEAR                   V1DPO
004900101103
005000101103     c                   eval      dateu=%dec(datasys)
005100101103     c
005200101104     c                   open(e)   azorg01l
005300101103     c                   if        not %open(azorg01l)
005400101103     c                   eval      %subst(Wazorg:7:4)='GRPF'
005500101103     C                   OPEN      azorg01l
005600101103     c                   endif
005700040917     c
005800040917     C     FOR01         TAG
005900000828      *
006000101103     C                   EXFMT     BS94D01
006100970102     C                   SETOFF                                       289050
006200040917     C* F3 - FINE
006300040917     C   KC              GOTO      FINE
006400050104     C* F7 - manutenzione organigramma
006500050104     c                   if        *inkg
006600050110     C                   call      'TIBS84C'
006700050104     c                   parm                    kpjba
006800050104     c                   goto      for01
006900050104     c                   endif
007000041220      *  CONTROLLI
007100040917     C                   EXSR      CTRD01
007200040917     c   90              goto      for01
007300000825      *
007400040917     c*
007500101103     c                   if        not *inkf
007600041220     c                   goto      for01
007700041220     c                   endif
007800040917     C*
007900040917      * F6  - SOTTOMETTO ELABORAZIONE
008000050412    1C     *INKF         IFEQ      *ON
008100101103     c                   exsr      Elab
008200101103     c                   endif
008300050103     c
008400000000     C     FINE          TAG
008500960129     C*
008600000000     C                   SETON                                        LR
008700900515      *-----------------------------------------------------***********
008800040917      * CONTROLLI SFL                                       *  CTRD01 *
008900900515      *-----------------------------------------------------***********
009000040917     C     CTRD01        BEGSR
009100050104     c                   setoff                                       010206
009200101104      * filiale
009300041220     C                   MOVE      V1CPO         CODFIL            3 0
009400951025     C                   EXSR      CTRFIL
009500041220    1c                   if        not *in34
009600041220     c                   movel     orgdes        V1DPO
009700040917     c
009800041220   x1C                   ELSE
009900040917     c                   seton                                        509028
010000041220     C                   MOVEL     MSG(1)        V1cMSG
010100041220     c                   goto      endct1
010200041220    1C                   ENDIF
010300101103     c
010400101103     c* Verifico se ha il picking messaggerie
010500101103     c                   movel     orgdf0        og150
010600101103     c*
010700101103     c                   if        §ogpkm<>'S'
010800101103     c                   seton                                        509028
010900101103     C                   MOVEL     MSG(2)        V1cMSG
011000101103     c                   goto      endct1
011100101103    1C                   ENDIF
011200101103     c
011300101103     c* Verifico se esite un IMA aperto con data minore = a oggi
011400101103     c     kfvv          setll     fnfvv03l
011500101103     c     kfvv          reade     fnfvv03l
011600101103     c                   dow       not %eof(fnfvv03l)
011700101103     c
011800101103     c                   if        fvvspg='A' and fvvdfv<=dateu
011900101103     c                   seton                                        509028
012000101103     C                   MOVEL     MSG(3)        V1cMSG
012100101103     c                   eval      %subst(v1cmsg:16:5)= %editc(fvvnfv:'X')
012200101103     c     *iso          movel     fvvdfv        dataiso
012300101103     c                   movel     dataiso       dataeur
012400101103     c                   movel     dataeur       data80            8 0
012500101103     c                   eval      %subst(v1cmsg:16:5)= %editc(fvvnfv:'X')
012600101103     c                   eval      %subst(v1cmsg:26:10)=
012700101103     c                             %editw(data80: '  /  /    ')
012800101103     c                   goto      endct1
012900101103    1C                   ENDIF
013000101103     c     kfvv          reade     fnfvv03l
013100101103     c                   enddo
013200101103     c
013300101103     c
013400900515      *
013500040917     C     ENDCT1        ENDSR
013600951025     C**************************************************************************
013700951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
013800951025     C**************************************************************************
013900951025     C     CTRFIL        BEGSR
014000020213
014100101103     C     CODFIL        CHAIN(n)  AZORG01L                           34
014200951025    1C  N34ORGFVA        IFNE      ' '
014300951025     C     ORGFAG        ORNE      'A'
014400951025     C     ORGFAG        ANDNE     'F'
014500951025     C                   SETON                                        34
014600951025    1C                   ENDIF
014700951025     C*
014800951025     C                   ENDSR
014900050214     C**************************************************************************
015000101103     C*  Carico p.o. 2 livello che vengono spostati insieme al terminal
015100050214     C**************************************************************************
015200101103     C     Elab          BEGSR
015201170512     c*
015202170512     c                   clear                   fnlv36ds
015203170512     c                   clear                   fiapdB
015204170512     c                   clear                   fiapdA
015205170512     c                   clear                   fiapd4b
015206170512     c                   clear                   fiapd4A
015207170512     c                   eval      ILV36PRU  = knmus
015208170512     c                   eval      ILV36NOJ  = knmeb
015209170512     c                   eval      ILV36PGM  = 'TIBS94R'
015210170512     c                   eval      ILV36CTA  = 'M'
015300170512     c* Imposto le variabili e poi aggiorno come faceva sql
015400101103     c                   eval      apdD=0000000
015500101103     c                   movel     v1cpo         apdD
015600101103     c                   eval      apdA=0009999
015700101103     c                   movel     v1cpo         apdA
015701170512     c     kapd          setll     fiapd01l
015702170512     c                   do        *hival
015703170512     c                   read      fiapd01l
015704170512     c                   if        %eof(fiapd01l) or
015705170512     c                             apdpdr > apda
015706170512     c                   leave
015707170512     c                   endif
015708170512     c                   movel     apdflr        dapdflr
015709170512      *aggiorna solo righe con impostato messaggeria e le abblenca
015710170512     c                   if        apdatb <> *blank or
015711170512     c                             §APDTPM <> 'M'
015712170512     c                   iter
015713170512     c                   endif
015714170512     c                   eval      fiapdB = fiapdds
015715170512     C                   move      *blank        §APDTPM
015716170512     c                   movel     dapdflr       apdflr
015717170512     C                   UPDATE    fiapd000
015718170512     c                   eval      fiapdA = fiapdds
015719170512     c                   exsr      variazione
015720170512     c                   enddo
015800101103      /free
016800101103
016900101103       // sistema FIDST
017000101103         wrkgetlista='UPDATE FIDST00F set dsttpm = '' '' WHERE dsttpm = ''M'' +
017100101103             and DSTFGS = ' + %editc(v1cpo:'X')  + ' and DSTFCF = '' ''' ;
017200101103
017300101103        exec  sql execute immediate: wrkgetlista ;
017400101103
017500101103
017600101103       // sistemo FNFVV
017700101103         wrkgetlista='UPDATE FNFVV00F SET FVVSPN = substr(fvvspn, 1, 15) +
017800101103             !! '' '' !! substr(fvvspn, 17) WHERE substr(fvvspn, 16, 1) =''M'' +
017900101103             and FVVFGS= ' + %editc(v1cpo:'X') + ' and FVVFCF = '' '''  ;
018000101103
018100101103        exec  sql execute immediate: wrkgetlista ;
018200101103
018300101103
018400101103       // Per ultimo chaino organigramma e lo aggiorno   di filiale e sede
018500101103       chain v1cpo azorg01l    ;
018600101103       if %found(azorg01l)  ;
018700101103        og150=orgdf0  ;
018800101103        §ogpkm=' ' ;
018900101103        orgdf0=og150  ;
019000101103        update azorg    ;
019100101103       endif  ;
019200101103
019300101103      /end-free
019400101103
019500101103     c                   close     azorg01l
019600101103     c                   eval      %subst(Wazorg:1:10)='GAITRAGRU '
019700101104     c                   open(e)   azorg01l
019800101103     c                   if        not %open(azorg01l)
019900101103     c                   eval      %subst(Wazorg:7:4)='GRPS'
020000101103     C                   OPEN      azorg01l
020100101103     c                   endif
020200101103     c
020300101103      /free
020400101103       chain v1cpo azorg01l    ;
020500101103       if %found(azorg01l)  ;
020600101103        og150=orgdf0  ;
020700101103        §ogpkm=' ' ;
020800101103        orgdf0=og150  ;
020900101103        clear orgftr  ;
021000101103        clear orgntr  ;
021100101103        update azorg    ;
021200101103       endif  ;
021300101103
021400101103        close azorg01l  ;
021500101103
021600101103      /end-free
021700050214     C                   ENDSR
021701170512     C*------------------------------------------------------------------------*
021702170512     C* richiama pgm variazioni
021703170512     C*------------------------------------------------------------------------*
021704170512     C     variazione    BEGSR
021705170512      * preparazione dati richiamo PGM variazioni per ANNULLAMENTO
021706170512      *richiama pgm registrazione variazioni
021707170512     c                   call      'FNLV36R'
021708170512     c                   parm                    fnlv36ds
021709170512     c                   parm                    fiapdb
021710170512     c                   parm                    fiapda
021711170512     c                   parm                    fiapd4b
021712170512     c                   parm                    fiapd4A
021713170512     c                   ENDsr
021800020805      **************************************************************************
021900960302** SCHIERA MSG - MESSAGGI DI ERRORE
022000101103Filiale INESISTENTE                                                           01
022100101103La filiale non ha il PICKING Messaggerie                                      02
022200101103Presente l'IMA xxxxx del xx/xx/xxxx aperto: chiuderlo prima di procedere      03
