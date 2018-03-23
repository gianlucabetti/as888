000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300161110      * Il pgm estrae i record di log degli esiti palmare e gli orari
000400060125      *
000500060125      *****************************************************************
001000180302     ftitas38c  if   e           k disk
001001180302     f                                     ignore(titas000)
001002180302     f                                     ignore(titas010)
001100180302     Fbitasr0f  o    E             DISK
001200030429     F*---------------------------------------------------------------------------------
001300000607     D kpjba         e ds
001301180302     dparam            ds
001302180302     d datap                          8  0
001303180302     d datap2                         8  0
002200161110     d tmp             s             17
002201180302      *
002203180302     d                 ds
002204180302     d tasaas                  1      4  0
002205180302     d tasmgs                  5      8  0
002206180302     d tasdsp                  1      8  0
002207180302     d tasanme                 1      6  0
002300020205     c**********************************************************************
002400180302      * senza data una-tantum copia tutto il TITASP da poortare in piattaforma BI
002401180302     c                   if        datap = 0
002500180302     c     *loval        setll     titas38c
002600161026     c                   do        *hival
002700180302     c                   read      titas38c
002800180302     c                   if        %eof(titas38c)
002900161026     c                   leave
003000161026     c                   endif
003001180302     c                   write     bitasr00
003002180302     c                   enddo
003003180302      *
003006180302     c                   else
003007180302      *
003008180302     c                   eval      tasdsp = datap
003010180302      * CON data Copia Il TITASP da portare in piattaforma BI NEL RANGE DI DATE (DATA SPEDIZIONE)
003011180302     c     ktas          setll     titas38c
003012180302     c                   do        *hival
003013180302     c                   read      titas38c
003014180302     c                   if        %eof(titas38c) OR
003015180302     C                             TASDSP > DATAP2
003016180302     c                   leave
003017180302     c                   endif
003018180302     c                   write     bitasr00
003019180302     c                   enddo
003500180302     c                   endif
003501180302      *
003600161110     c                   seton                                        lr
022300020205     C**-------------------------------------------------------------**
022400000607     c     *inzsr        begsr
022500060125     C**-------------------------------------------------------------**
022701180302     c     *entry        plist
022702180302     c                   parm                    kpjba
022703180302     c                   if        kpjbu = *blank
022704180302     c                   clear                   param
022705180302     c                   else
022706180302     c                   movel     kpjbu         param
022707180302     c                   endif
023800161025      *
023900180302     c     ktas          klist
024000180302     c                   kFLD                    tasaas
024100180302     c                   kFLD                    tasmgs
026900020402     c                   endsr
