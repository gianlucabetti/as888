000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200150122     fcassd     cf   e             workstn
000201150120     ffncastemp if   e             disk    usropn
000202150121     fwwreg00f  if   e           k disk    usropn
000300000607     D kpjba         e ds
000301150120      *
000302150120     D WLBDAT          DS
000303150120     D  G02DAT                 1      8  0
000304150120     D  G02INV                 9     16  0
000305150120     D  G02ERR                17     17
000306150120     D  G02TGI                18     22  0
000307150122     d diffe           s              5  0
000308150121     d opzione         s              1
000309150120     d coman           s             99
000310150120     d luncmd          s             15  5
000311150120     d wrkEml          s            253
000312150120     d wrkEmcc         s            253
000313150120     d wrkMsg          s           5000
000314150120     d wrkOgg          s             44
000315150120     d wrkesit         s              1
000316150121     d dataiso         s               d   datfmt(*iso)
000317150121     d dataiso2        s               d   datfmt(*iso)
000318150122     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
000319150122     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
000320150122     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
000321150122     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
000322150122     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
000400150120      *_________________________________________________________________
000500140418     c     *entry        plist
000600140418     c                   parm                    kpjba
000700140418      *
000900100720     c                   do        *hival
001000100720     c                   exfmt     video1
001100150121     c                   setoff                                       283031
001101150120     c                   clear                   $msg
001102150120     c                   if        vdata > 0
001103150120     C                   Z-ADD     vdata         G02DAT
001104150120     C                   MOVEL     *BLANK        G02ERR
001105150120     C                   CALL      'XSRDA8'
001106150120     C                   PARM                    WLBDAT
001107150120     c                   if        g02err = '1'
001108150120     c                   eval      $msg = 'Data errata'
001109150121     c                   seton                                        2830
001110150120     c                   iter
001111150120     c                   endif
001112150122     c                   movel     g02dat        vdata
001113150121     c                   movel     g02inv        dataiso
001114150121     c                   move      dataiso       datalfa          10
001115150120     c                   else
001116150120     c                   eval      $msg = 'Data obbligatoria'
001117150121     c                   seton                                        2830
001118150120     c                   iter
001119150120     c                   endif
001120150121      *seconda data
001121150121     c                   if        vdata2 > 0
001122150121     C                   Z-ADD     vdata2        G02DAT
001123150121     C                   MOVEL     *BLANK        G02ERR
001124150121     C                   CALL      'XSRDA8'
001125150121     C                   PARM                    WLBDAT
001126150121     c                   if        g02err = '1'
001127150121     c                   eval      $msg = 'Data errata'
001128150121     c                   seton                                        2831
001129150121     c                   iter
001130150121     c                   endif
001131150122     c                   movel     g02dat        vdata2
001132150121     c                   movel     g02inv        dataiso2
001133150121     c                   move      dataiso2      datalfa2         10
001134150121     c                   else
001135150122     c                   eval      vdata2 = vdata
001137150121     c                   iter
001138150121     c                   endif
001139150122     c                   if        dataiso2 < dataiso
001140150122     c                   seton                                        2831
001141150122     c                   eval      $msg = 'Data al minore di data dal.. '
001142150122     c                   iter
001143150122     c                   endif
001144150122     c     dataiso2      subdur    dataiso       diffe:*d
001145150122     c                   if        diffe > 3
001146150122     c                   seton                                        2831
001147150122     c                   eval      $msg = 'Il range di date può essere massimo -
001148150122     c                             di 3 giorni'
001149150122     c                   iter
001150150122     c                   endif
001151150122     c
001152150122     c
001200100720     c   kc              leave
001201150120      *
001300100720     c                   if        *inkf
001402150120      * primo step FNCAS
001403150120     c                   exsr      vedifncas
001404150120     c   28              iter
001609150120      * secondo step sql registrazioni
001610150120     c                   exsr      regis
001611150121     c   28              iter
001612150121      * terzo step lancio stampe controllo
001614150120     c                   exsr      stampe364
001615150121     c   28              iter
001700140418     c                   endif
001800100720     c                   enddo
001900000607     c                   seton                                        lr
001901150120     c*_________________________________________________
001902150120     c     vedifncas     begsr
001903150120     c*_________________________________________________
001904150122     C                   movea     cmd4          coman
001905150120     c                   exsr      comandi
001906150122     c                   if        not *in34
001907150122     c                   movea     cmd5          coman
001908150122     c                   exsr      comandi
001910150122     c                   endif
001911150122     C                   movea     cmd1          coman
001912150122     c                   exsr      comandi
001913150120     c                   open      fncastemp
001914150120     c     *start        setll     fncastemp
001915150120     c                   read      fncastemp
001916150120     c                   if        %eof(fncastemp)
001917150120     c                   leavesr
001918150120     c                   endif
001919150121     c                   eval      opzione = '1'
001920150120     c                   exsr      inviamail
001921150120     c                   eval      $msg = 'C/assegni fermi in ricezione -
001922150120     c                             il CED è stato avvertito tramite mail'
001923150120     c                   seton                                        28
001924150120     c                   endsr
001925150121     c*_________________________________________________
001926150121     c     regis         begsr
001927150121     c*_________________________________________________
001928150121     c                   movea     cmd2          coman
001929150121     c                   exsr      comandi
001930150121     C/EXEC SQL
001931150121     C+ INSERT INTO wwreg00F
001932150121     C+ SELECT NDREG00F.*, A.UTNPRF AS UTEINS, B.UTNPRF AS UTEMOD
001933150121     C+ FROM NDREG00F
001934150121     C+ LEFT OUTER JOIN ANUTN00F A
001935150121     C+ ON REGUTEIMM = A.UTNCUT
001936150121     C+ LEFT OUTER JOIN ANUTN00F B
001937150121     C+ ON REGUTEULMO = B.UTNCUT
001938150121     C+ WHERE REGSOCIETA = '201'
001939150121     C+ AND REGCTB = 'CG'
001940150121     C+ AND (REGDTIMM BETWEEN :dataiso AND :dataiso2 OR REGDTULMO
001941150121     C+ BETWEEN :dataiso   and :dataiso2)
001942150121     C+ AND ((REGKCCCPD = '000465' AND REGKSCCPD = '00000004') OR
001943150121     C+ (REGKCCCPA = '000465' AND REGKSCCPA = '00000004'))
001944150121     C+ AND ((LEFT(A.UTNPRF, 3) <> 'FIL' AND A.UTNPRF <> 'EDPCED')
001945150121     C+ OR (LEFT(B.UTNPRF, 3) <> 'FIL'  AND B.UTNPRF <> 'EDPCED'))
001946150121     C/END-EXEC
001947150121     c                   open      wwreg00f
001948150121     c     *loval        setll     wwreg00f
001949150121     c                   read      wwreg00f
001950150121     c                   if        %eof(wwreg00f)
001951150121     c                   leavesr
001952150121     c                   endif
001953150122     c*                  eval      opzione = '2'
001954150122     c*                  exsr      inviamail
001955150121     c                   eval      $msg = 'Registrazioni da verificare  -
001956150122     c                             scaricare il file WWREG00F libreria WRKQUERY'
001957150121     c                   seton                                        28
001958150121     c                   movea     cmd3          coman
001959150121     c                   exsr      comandi
001960150121     c                   endsr
001961150121     c*_________________________________________________
001962150121     c     stampe364     begsr
001963150121     c*_________________________________________________
001964150121     c                   eval      kpjbu = datalfa + datalfa2
001965150121     C                   EVAL      KCoAz = 'Y36C'
001966150121     C                   CALL      'BCH10'
001967150121     C                   PARM                    Kpjba
001968150122     c*                  eval      opzione = '3'
001969150122     c*                  exsr      inviamail
001970150121     c                   eval      $msg = 'Lanciate stampe CONTROLLO -
001971150121     c                             ANALITICO SQUADRATURE CONTRASSEGNI'
001972150121     c                   seton                                        28
001973150121     c
001974150121     c                   endsr
001992150120     c*_________________________________________________
001993150120     c     comandi       begsr
001994150120     c*_________________________________________________
001995150120      *
001996150120     C                   z-add     99            luncmd
001997150120     C                   CALL      'QCMDEXC'                            34
001998150120     C                   parm                    coman
001999150120     C                   parm                    luncmd
002000150120      *
002001150120     c                   endsr
002002150120     C*----------------------------------------------------*
002003150120     C*  invia mail di segnalazione assegni non versati
002004150120     C*----------------------------------------------------*
002005150120     C     Inviamail     BEGSR
002006150120     C                   eval      wrkEml= 'Michele.malipiero@brt.it'
002007150120     C                   eval      wrkEmcc='Michele.Malipiero@brt.it'
002008150120
002009150120     C                   eval      wrkOgg='ATTENZIONE C/assegni'
002010150120      *
002011150121     c                   select
002012150121     c                   when      opzione = '1'
002013150120     C                   eval      wrkMsg='CONTRASSEGNI FERMI IN RICEZIONE'+
002014150120     C                             ':/N'+
002015150120     C                             'Verificare il file FNCAS00R'
002016150121     c                   when      opzione = '2'
002017150121     C                   eval      wrkMsg='PROBLEMI CON LE REGISTRAZIONI C/ASS'+
002018150121     C                             ':/N'+
002019150121     C                             'Verificare il file +
002020150121     C                              WWREG00F NELLA LIBRERIA WRKQUERY'
002021150121     c                   when      opzione = '3'
002022150121     C                   eval      wrkMsg='LANCIO STAMPE SQUADRATURE'+
002023150121     C                             ':/N'+
002024150121     C                             'CONTROLLO ANALITICO SQUADRATURE C/ASSEGNI'
002025150121     c                   endsl
002026150120     C*
002027150120     C                   call(e)   'TIS701C1'
002028150120     C                   parm                    wrkEml
002029150120     C                   parm                    wrkEmcc
002030150120     C                   parm                    wrkOgg
002031150120     C                   parm                    wrkMsg
002032150120     C                   parm                    wrkesit
002033150120     C                   endsr
002034000607     O*------------------------------------------------------*
002100150120**  CMD1
002200150120CPYF FROMFILE(FNCAS00R) TOFILE(QTEMP/FNCASTEMP) FROMMBR(*ALL) CRTFILE(*YES)
002300150121**  CMD2
002400150122CLRPFM FILE(*LIBL/WWREG00F)
002500150122**  CMD3
002600150122CPYF FROMFILE(WWREG00F) TOFILE(WRKQUERY/WWREG00F) MBROPT(*REPLACE)
002700150122**  CMD4
002800150122CHKOBJ  OBJ(QTEMP/FNCASTEMP) OBJTYPE(*FILE)
002900150122**  CMD5
003000150122DLTF FILE(QTEMP/FNCASTEMP)
