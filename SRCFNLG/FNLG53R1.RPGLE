000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300051202      * ?_____________________________________________________________      */
000400051202      * COMPILARE CON
000500070703      *               OVRDBF FILE(TITAS33C) TOFILE(GAITRAGRPS/TITAS33C)
000600070703      *               OVRDBF FILE(TITAS39C) TOFILE(GAITRAGRPS/TITAS39C)
000700051202      * ?_____________________________________________________________      */
000800070703     fazorg01l  if   e           k disk
000900070704     ftabel00f  if   e           k disk
001000070703     ftitas33c  if   e           k disk    usropn
001100070703     ftitas39c  if   e           k disk    usropn
001200070703     f                                     rename(titas000:titas)
001300070703     f                                     rename(titas010:titas10)
001400070703     f                                     rename(titasp00:titasp)
001500070703     fwgcpkl0f  o    e           k disk    usropn
001600051202     d kpjba         e ds
001700070703     D kpjbus          S                   LIKE(kpjbu)
001800070704     d ds3a          e ds
001900070703     d fnlg52ds      e ds
002000070703     D  Ksc                   17    156  0 dim(20)
002100070703     d cmd             s             66    dim(02) ctdata perrcd(1)
002200070703     d cmdp            s             66    dim(02) ctdata perrcd(1)
002300070703     D cmd1            S             66    DIM(01) CTDATA PERRCD(1)
002400070703     D cmd2            S             66    DIM(01) CTDATA PERRCD(1)
002500051202     d comman          s             80
002600051202     d lenght          s             15  5 inz(80)
002700051202      * ?_____________________________________________________________      */
002800051202      * ?sistema informativo filiale reale(SETRAS)                          */
002900051202     c                   if        knsif = 'FILTRA201'
003000070703     c                   if        d52mod = 'C'
003100051202     c                   Movel(p)  cmd(1)        comman
003200070703     c                   else
003300070703     c                   Movel(p)  cmd(2)        comman
003400070703     c                   endif
003500051202     c                   else
003600051202      * ?sistema informativo filiale prova(AS888)                           */
003700070703     c                   if        d52mod = 'C'
003800051202     c                   Movel(p)  cmdp(1)       comman
003900070703     c                   else
004000070703     c                   Movel(p)  cmdp(2)       comman
004100070703     c                   endif
004200051202     c                   endif
004300051202     c                   eval      lenght = %len(%trim(comman))
004400051202     c                   Call      'QCMDEXC'                            99
004500051202     c                   Parm                    comman
004600051202     c                   Parm                    lenght
004700070703      * ?elabora per elenco clienti o filiale                               */
004800070703     c                   if        d52mod = 'C'
004900070703     c  n99              Open      Titas33c
005000070703     c                   exsr      cliente
005100070703     c                   else
005200070703     c  n99              Open      Titas39c
005300070703     c                   exsr      filiale
005400070703     c                   endif
005500051202
005600070704     c                   feod      wgcpkl0f
005700070703     c                   movel     fnlg52ds      kpjbu
005800070703      * richiamo pgm di stampa
005900070703     c                   call      'FNLG53R'
006000070703     c                   parm                    kpjba
006100070703      * elimino file di work
006200070703     c                   close     wgcpkl0f
006300070703     c                   Movel(p)  cmd1(1)       comman
006400070703     c                   eval      lenght = %len(%trim(comman))
006500070703     c                   Call      'QCMDEXC'                            99
006600070703     c                   Parm                    comman
006700070703     c                   Parm                    lenght
006800070703
006900070703     c                   seton                                        lr
007000070703      * ?_______________________________________________________            */
007100070703     c     cliente       begsr
007200070703      * ?_______________________________________________________            */
007300070703     c     1             do        20            x                 2 0
007400070703     c                   if        ksc(x) = *zeros
007500070703     c                   leave
007600070703     c                   endif
007700070704     c                   z-add     ksc(x)        ccmk
007800070703
007900070703     c     kcli          setll     titas33c
008000070703     c                   do        *hival
008100070703     c     ccmk          reade     titas33c
008200070703     c                   if        %eof(titas33c)
008300070703     c                   leave
008400070703     c                   endif
008500070704      * esclude bolle di recupero
008600070704     C                   movel     '3A'          tblcod
008700070704     C                   movel(P)  tascbo        tblkey
008800070704     c     ktab          chain     tabel00f
008900070704     C                   movel     TBLUNI        DS3A
009000070704     c                   if        §3arbl = 'R'
009100070704     c                   iter
009200070704     c                   endif
009300070703      * verifica se bolla dentro il range di date richiesto
009400070703     c                   movel     tasaas        datasped          8 0
009500070703     c                   move      tasmgs        datasped
009600070703     c                   if        datasped > d52dsa
009700070703     c                   leave
009800070703     c                   endif
009900070703     c                   exsr      scrivi
010000070703     c                   enddo
010100070703
010200070703     c                   enddo
010300070703     c                   endsr
010400070703      * ?_______________________________________________________            */
010500070703     c     filiale       begsr
010600070703      * ?_______________________________________________________            */
010700070705     c     1             do        20            x                 2 0
010800070705     c                   if        ksc(x) = *zeros
010900070705     c                   leave
011000070705     c                   endif
011100070705     c                   z-add     ksc(x)        lnpk
011200070703
011300070703     c     kfil          setll     titas39c
011400070703     c                   do        *hival
011500070703     c     lnpk          reade     titas39c
011600070703     c                   if        %eof(titas39c)
011700070703     c                   leave
011800070703     c                   endif
011900070704      * esclude bolle di recupero
012000070704     C                   movel     '3A'          tblcod
012100070704     C                   movel(P)  tascbo        tblkey
012200070704     c     ktab          chain     tabel00f
012300070704     C                   movel     TBLUNI        DS3A
012400070704     c                   if        §3arbl = 'R'
012500070704     c                   iter
012600070704     c                   endif
012700070703      * verifica se bolla dentro il range di date richiesto
012800070703     c                   movel     tasaas        datasped
012900070703     c                   move      tasmgs        datasped
013000070703     c                   if        datasped > d52dsa
013100070703     c                   leave
013200070703     c                   endif
013300070703     c                   exsr      scrivi
013400070703     c                   enddo
013500070703
013600070705     c                   enddo
013700070703     c                   endsr
013800070703      * ?_____________________________________________________              */
013900070703     c     scrivi        begsr
014000070703      * ?_____________________________________________________              */
014100070703     c     taslna        chain     azorg01l
014200070703     c                   move      orgfl3        GKLDIST
014300070703     c                   move      orgcar        GKLAREA
014400070703     c                   z-add     tasAAS        GKLAAS
014500070703     c                   z-add     tasLNP        GKLLNP
014600070703     c                   z-add     tasNRS        GKLNRS
014700070703     c                   z-add     tasNSP        GKLNSP
014800070703     c                   z-add     tasMGS        GKLMGS
014900070703     c                   z-add     tasLNA        GKLLNA
015000070703     c                   move      tasCBO        GKLCBO
015100070703     c                   z-add     tasCCM        GKLCCM
015200070703     c
015300070703     c                   write     wgcpkl00
015400070703     c                   endsr
015500070703      * ?_____________________________________________________              */
015600051202     c     *inzsr        begsr
015700070703      * ?_____________________________________________________              */
015800051202     c     *entry        plist
015900051202     c                   parm                    kpjba
016000070703     c                   movel     kpjbu         fnlg52ds
016100070703      *
016200070703     c                   movel     d52dsd        aask
016300070703     c                   move      d52dsd        mgsk
016400051130      * originale
016500070703     c     kcli          klist
016600070703     c                   kfld                    ccmk              7 0
016700070703     c                   kfld                    aask              4 0
016800070703     c                   kfld                    mgsk              4 0
016900051202
017000070703     c     kfil          klist
017100070703     c                   kfld                    lnpk              3 0
017200070703     c                   kfld                    aask
017300070703     c                   kfld                    mgsk
017400070704      *
017500070704     C     Ktab          klist
017600070704     C                   kfld                    codut             1 0
017700070704     C                   kfld                    tblcod
017800070704     C                   kfld                    tblkey
017900070704     c                   z-add     1             codut
018000070704      * genero file di work in qtemp
018100070703     c                   Movel(p)  cmd2(1)       comman
018200070703     c                   eval      lenght = %len(%trim(comman))
018300070703     c                   Call      'QCMDEXC'                            99
018400070703     c                   Parm                    comman
018500070703     c                   Parm                    lenght
018600070703     c                   open      wgcpkl0f
018700070703
018800051202     c                   endsr
018900051202      *________________________________________________________
019000051202** cmd
019100070703OVRDBF FILE(TITAS33C) TOFILE(GAITRAGRU/TITAS33C)
019200070703OVRDBF FILE(TITAS39C) TOFILE(GAITRAGRU/TITAS39C)
019300051202** cmdp
019400070703OVRDBF FILE(TITAS33C) TOFILE(GAITRAGRPS/TITAS33C)
019500070703OVRDBF FILE(TITAS39C) TOFILE(GAITRAGRPS/TITAS39C)
019600070703**  CMD1
019700070704DLTF FILE(QTEMP/WGCPKL0F)
019800070703**  CMD2
019900070704CRTDUPOBJ OBJ(WGCPKL0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
