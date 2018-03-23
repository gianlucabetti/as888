000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200150122     ftnsc06d   cf   e             workstn
000300150120     ffncastemp if   e             disk    usropn
000400150121     fwwreg00f  if   e           k disk    usropn
000500000607     D kpjba         e ds
000600150120      *
000700150120     D WLBDAT          DS
000800150120     D  G02DAT                 1      8  0
000900150120     D  G02INV                 9     16  0
001000150120     D  G02ERR                17     17
001100150120     D  G02TGI                18     22  0
001200150122     d diffe           s              5  0
001300150121     d opzione         s              1
001400150120     d coman           s             99
001500150120     d luncmd          s             15  5
001600150120     d wrkEml          s            253
001700150120     d wrkEmcc         s            253
001800150120     d wrkMsg          s           5000
001900150120     d wrkOgg          s             44
002000150120     d wrkesit         s              1
002100150121     d dataiso         s               d   datfmt(*iso)
002200150121     d dataiso2        s               d   datfmt(*iso)
002300150122     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002400150122     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
002500150122     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
002600150122     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
002700150122     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
002800150120      *_________________________________________________________________
002900140418     c     *entry        plist
003000140418     c                   parm                    kpjba
003100150122     c                   move      *date         vdata
003200140418      *
003300100720     c                   do        *hival
003400100720     c                   exfmt     video1
003500150121     c                   setoff                                       283031
003600150120     c                   clear                   $msg
003700150120     c                   if        vdata > 0
003800150120     C                   Z-ADD     vdata         G02DAT
003900150120     C                   MOVEL     *BLANK        G02ERR
004000150120     C                   CALL      'XSRDA8'
004100150120     C                   PARM                    WLBDAT
004200150120     c                   if        g02err = '1'
004300150120     c                   eval      $msg = 'Data errata'
004400150121     c                   seton                                        2830
004500150120     c                   iter
004600150120     c                   endif
004700150122     c                   movel     g02dat        vdata
004800150121     c                   movel     g02inv        dataiso
004900150121     c                   move      dataiso       datalfa          10
005000150120     c                   else
005100150120     c                   eval      $msg = 'Data obbligatoria'
005200150121     c                   seton                                        2830
005300150120     c                   iter
005400150120     c                   endif
005500150121      *seconda data
005600150121     c                   if        vdata2 > 0
005700150121     C                   Z-ADD     vdata2        G02DAT
005800150121     C                   MOVEL     *BLANK        G02ERR
005900150121     C                   CALL      'XSRDA8'
006000150121     C                   PARM                    WLBDAT
006100150121     c                   if        g02err = '1'
006200150121     c                   eval      $msg = 'Data errata'
006300150121     c                   seton                                        2831
006400150121     c                   iter
006500150121     c                   endif
006600150122     c                   movel     g02dat        vdata2
006700150121     c                   movel     g02inv        dataiso2
006800150121     c                   move      dataiso2      datalfa2         10
006900150121     c                   else
007000150122     c                   eval      vdata2 = vdata
007100150121     c                   iter
007200150121     c                   endif
007300150122     c                   if        dataiso2 < dataiso
007400150122     c                   seton                                        2831
007500150122     c                   eval      $msg = 'Data al minore di data dal.. '
007600150122     c                   iter
007700150122     c                   endif
007800150122     c     dataiso2      subdur    dataiso       diffe:*d
007900150122     c                   if        diffe > 3
008000150122     c                   seton                                        2831
008100150122     c                   eval      $msg = 'Il range di date può essere massimo -
008200150122     c                             di 3 giorni'
008300150122     c                   iter
008400150122     c                   endif
008500150122     c
008600150122     c
008700100720     c   kc              leave
008800150120      *
008900100720     c                   if        *inkf
009000150120      * primo step FNCAS
009100150120     c                   exsr      vedifncas
009200150120     c   28              iter
009300150120      * secondo step sql registrazioni
009400150120     c                   exsr      regis
009500150121     c   28              iter
009600150121      * terzo step lancio stampe controllo
009700150120     c                   exsr      stampe364
009800150121     c   28              iter
009900140418     c                   endif
010000100720     c                   enddo
010100000607     c                   seton                                        lr
010200150120     c*_________________________________________________
010300150120     c     vedifncas     begsr
010400150120     c*_________________________________________________
010500150209     c                   close     fncastemp
010600150122     C                   movea     cmd4          coman
010700150120     c                   exsr      comandi
010800150122     c                   if        not *in34
010900150122     c                   movea     cmd5          coman
011000150122     c                   exsr      comandi
011100150122     c                   endif
011200150122     C                   movea     cmd1          coman
011300150122     c                   exsr      comandi
011400150120     c                   open      fncastemp
011500150120     c     *start        setll     fncastemp
011600150122     c                   do        *hival
011700150120     c                   read      fncastemp
011800150120     c                   if        %eof(fncastemp)
011900150120     c                   leavesr
012000150120     c                   endif
012100150122     c                   if        castpi = 'M'
012200150122     c                   iter
012300150122     c                   endif
012400150121     c                   eval      opzione = '1'
012500150120     c                   exsr      inviamail
012600150120     c                   eval      $msg = 'C/assegni fermi in ricezione -
012700150120     c                             il CED è stato avvertito tramite mail'
012800150120     c                   seton                                        28
012900150122     c                   leavesr
013000150122     c                   enddo
013100150120     c                   endsr
013200150121     c*_________________________________________________
013300150121     c     regis         begsr
013400150121     c*_________________________________________________
013500150121     c                   movea     cmd2          coman
013600150121     c                   exsr      comandi
013700150121     C/EXEC SQL
013800150121     C+ INSERT INTO wwreg00F
013900150121     C+ SELECT NDREG00F.*, A.UTNPRF AS UTEINS, B.UTNPRF AS UTEMOD
014000150121     C+ FROM NDREG00F
014100150121     C+ LEFT OUTER JOIN ANUTN00F A
014200150121     C+ ON REGUTEIMM = A.UTNCUT
014300150121     C+ LEFT OUTER JOIN ANUTN00F B
014400150121     C+ ON REGUTEULMO = B.UTNCUT
014500150121     C+ WHERE REGSOCIETA = '201'
014600150121     C+ AND REGCTB = 'CG'
014700150121     C+ AND (REGDTIMM BETWEEN :dataiso AND :dataiso2 OR REGDTULMO
014800150121     C+ BETWEEN :dataiso   and :dataiso2)
014900150121     C+ AND ((REGKCCCPD = '000465' AND REGKSCCPD = '00000004') OR
015000150121     C+ (REGKCCCPA = '000465' AND REGKSCCPA = '00000004'))
015100150121     C+ AND ((LEFT(A.UTNPRF, 3) <> 'FIL' AND A.UTNPRF <> 'EDPCED')
015200150121     C+ OR (LEFT(B.UTNPRF, 3) <> 'FIL'  AND B.UTNPRF <> 'EDPCED'))
015300150121     C/END-EXEC
015400150121     c                   open      wwreg00f
015500150121     c     *loval        setll     wwreg00f
015600150121     c                   read      wwreg00f
015700150121     c                   if        %eof(wwreg00f)
015800150121     c                   leavesr
015900150121     c                   endif
016000150122     c*                  eval      opzione = '2'
016100150122     c*                  exsr      inviamail
016200150121     c                   eval      $msg = 'Registrazioni da verificare  -
016300150122     c                             scaricare il file WWREG00F libreria WRKQUERY'
016400150121     c                   seton                                        28
016500150121     c                   movea     cmd3          coman
016600150121     c                   exsr      comandi
016700150121     c                   endsr
016800150121     c*_________________________________________________
016900150121     c     stampe364     begsr
017000150121     c*_________________________________________________
017100150121     c                   eval      kpjbu = datalfa + datalfa2
017200150121     C                   EVAL      KCoAz = 'Y36C'
017300150121     C                   CALL      'BCH10'
017400150121     C                   PARM                    Kpjba
017500150122     c*                  eval      opzione = '3'
017600150122     c*                  exsr      inviamail
017700150121     c                   eval      $msg = 'Lanciate stampe CONTROLLO -
017800150121     c                             ANALITICO SQUADRATURE CONTRASSEGNI'
017900150121     c                   seton                                        28
018000150121     c
018100150121     c                   endsr
018200150120     c*_________________________________________________
018300150120     c     comandi       begsr
018400150120     c*_________________________________________________
018500150120      *
018600150120     C                   z-add     99            luncmd
018700150120     C                   CALL      'QCMDEXC'                            34
018800150120     C                   parm                    coman
018900150120     C                   parm                    luncmd
019000150120      *
019100150120     c                   endsr
019200150120     C*----------------------------------------------------*
019300150120     C*  invia mail di segnalazione assegni non versati
019400150120     C*----------------------------------------------------*
019500150120     C     Inviamail     BEGSR
019600150304     C                   eval      wrkEml= 'Contrassegni@brt.it'
019700150123     C                   eval      wrkEmcc='ced@brt.it'
019800150120
019900150120     C                   eval      wrkOgg='ATTENZIONE C/assegni'
020000150120      *
020100150121     c                   select
020200150121     c                   when      opzione = '1'
020300150120     C                   eval      wrkMsg='CONTRASSEGNI FERMI IN RICEZIONE'+
020400150120     C                             ':/N'+
020500150304     C                             'Verificare il file FNCAS00R' +
020600150304     C                             ':/N'+ 'Sped. ' + %editc(caslnp:'X') + ' ' +
020700150304     c                             %editc(casnrs:'X') + ' ' + %editc(casnsp:'X')
020800150304     c                             + ' Linea Arr. ' + %editc(caslna:'X')
020900150304     c                             + ' Importo  ' + %editc(cascas:'2')
021000150121     c                   when      opzione = '2'
021100150121     C                   eval      wrkMsg='PROBLEMI CON LE REGISTRAZIONI C/ASS'+
021200150121     C                             ':/N'+
021300150121     C                             'Verificare il file +
021400150121     C                              WWREG00F NELLA LIBRERIA WRKQUERY'
021500150121     c                   when      opzione = '3'
021600150121     C                   eval      wrkMsg='LANCIO STAMPE SQUADRATURE'+
021700150121     C                             ':/N'+
021800150121     C                             'CONTROLLO ANALITICO SQUADRATURE C/ASSEGNI'
021900150121     c                   endsl
022000150120     C*
022100150120     C                   call(e)   'TIS701C1'
022200150120     C                   parm                    wrkEml
022300150120     C                   parm                    wrkEmcc
022400150120     C                   parm                    wrkOgg
022500150120     C                   parm                    wrkMsg
022600150120     C                   parm                    wrkesit
022700150120     C                   endsr
022800000607     O*------------------------------------------------------*
022900150120**  CMD1
023000150120CPYF FROMFILE(FNCAS00R) TOFILE(QTEMP/FNCASTEMP) FROMMBR(*ALL) CRTFILE(*YES)
023100150121**  CMD2
023200150122CLRPFM FILE(*LIBL/WWREG00F)
023300150122**  CMD3
023400150122CPYF FROMFILE(WWREG00F) TOFILE(WRKQUERY/WWREG00F) MBROPT(*REPLACE)
023500150122**  CMD4
023600150122CHKOBJ  OBJ(QTEMP/FNCASTEMP) OBJTYPE(*FILE)
023700150122**  CMD5
023800150122DLTF FILE(QTEMP/FNCASTEMP)
