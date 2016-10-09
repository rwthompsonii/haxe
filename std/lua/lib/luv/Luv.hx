package lua.lib.luv;
import haxe.Constraints.Function;

typedef ResultStatus<T> = String->T->Void;
typedef StringStatus = ResultStatus<String>;
typedef BoolStatus = ResultStatus<Bool>;

typedef FileDescriptor = Int;
typedef Buffer = String;
typedef SuggestedSize = Int;
typedef NumRead = Int;
typedef Status = Int;
typedef UvAllocCb = Handle->SuggestedSize->Buffer->Void;
typedef UvReadCb = Stream->NumRead->Buffer->Void;
typedef UvRead2Cb = Pipe->NumRead->Buffer->Handle->Void;
typedef ReqStatus = Req->Status->Void;
typedef UvWriteCb = ReqStatus;
typedef UvConnectCb = ReqStatus;
typedef UvShutdownCb = ReqStatus;
typedef UvConnectionCb = Stream->Status->Void;
typedef UvCloseCb = Handle->Void;
typedef Events = Int;
typedef HandleStatus = Handle->Status->Void;
typedef UvPollCb = Handle->Status->Events->Void;
typedef UvTimerCb = HandleStatus;
typedef UvAsyncCb = HandleStatus;
typedef UvPrepareCb = HandleStatus;
typedef UvCheckCb = HandleStatus;
typedef UvIdleCb = HandleStatus;

typedef ExitStatus = Int;
typedef TermSignal = Int;
typedef UvExitCb = Process->ExitStatus->TermSignal;
typedef UvWalkCb = Handle->Void;
typedef Fs = Dynamic;
typedef UvFsCb = Fs->Void;
typedef UvWorkCb = Req->Void;
typedef UvAfterWorkCb = ReqStatus;
typedef Addr = Dynamic;
typedef UvGetAddrInfoCb = Req->Status->Addr->Void;

typedef Filename = String;
typedef UvFsEventCb = Handle->Filename->Events->Status->Void;
// typedef Stat = Dynamic;
typedef Prev = Stat;
typedef Curr = Stat;
typedef UvFsPollCb = Handle->Status->Prev->Curr->Void;
typedef Signum = Int;
typedef UvSignalCb = Handle->Signum->Void

typedef UvUdpSendCb = Req->Status->Void;
typedef Flags = Int;
typedef UvUdpRecvCb = Handle->NumRead->Buffer->Addr->Flags->Void;


@:luaRequire("luv")
extern class Loop {
  static function loop_close() : Bool;
  static function run(?mode : String) : Bool;
  static function loop_alive() : Bool;
  static function stop() : Void;
  static function backend_fd() : Int;
  static function backend_timeout() : Int;
  static function now() : Int;
  static function update_time() : Void;
  static function walk(cb : Handle->Void) : Void;
}

@:luaRequire("luv")
extern class Req {
  static function cancel(req:Req) : Int;
}

@:luaRequire("luv")
extern class Handle {
  function is_active() : Bool;
  function is_closing() : Bool;
  function close() : Void;
  function ref() : Void;
  function unref() : Void;
  function has_ref() : Bool;
  function send_buffer_size(size : Int) : Int;
  function recv_buffer_size(size : Int) : Int;
  function fileno() : Int;
}

@:luaRequire("luv")
extern class Timer extends Handle {
  static function new_timer() : Timer;
  @:native("new_timer") function new() : Void;

  function start(timeout : Int, repeat : Int, cb : Void->Void) : Int;
  function stop() : Int;
  function again() : Int;
  function set_repeat(repeat : Int) : Void;
  function get_repeat() : Int;
}

// handle style calls

@:luaRequire("luv")
extern class Prepare extends Handle {
  static function new_prepare() : Prepare;
  @:native("new_prepare") function new() : Void;

  function start() : Int;
  function stop() : Int;
}

@:luaRequire("luv")
extern class Check extends Handle {
  static function new_check() : Check;
  @:native("new_check") function new() : Void;

  function start(handle : Handle) : Int;
  function stop() : Int;
}

@:luaRequire("luv")
extern class Idle extends Handle {
  static function new_idle() : Idle;
  @:native("new_idle") function new() : Void;
  function start(cb : Void->Void) : Int;
  function stop(cb : Void->Void) : Int;
}

@:luaRequire("luv")
extern class Async extends Handle {
  static function new_async() : Async;
  @:native("new_async") function new() : Void;
  function send() : Int;
}


@:luaRequire("luv")
extern class Poll extends Handle {
  static function new_poll() : Async;
  @:native("new_poll") function new() : Void;

  function start(?type : Int, ?cb : Void->Void ) : Int;
  function stop() : Int;
}

@:luaRequire("luv")
extern class Signal extends Handle {
  static function new_signal() : Signal;
  @:native("new_signal") function new() : Void;

  function start(sigtype : haxe.extern.EitherType<Int,String>, ?cb : Void-> Void ) : Int;
  function stop() : Int;

}

@:luaRequire("luv")
extern class Process extends Handle {
  static function disable_stdio_inheritance() : Void;
  function spawn(path : String, options : ProcessOptions, cb : Int->Signal->Void ) : Int;
  function kill(sig:String) : Int;
}

typedef ProcessOptions = {
  args : Table<Int,String>,
  stdio : Table<Int,String>
}

typedef StreamData = haxe.extern.EitherType<String,Table<Int,String>>;
typedef SendHandle = Tcp;

@:luaRequire("luv")
extern class Stream extends Handle {
  function shutdown(?cb : Void->Void) : Int;
  function listen(backlog : Int, cb : StringStatus) : Int;
  function accept(client_stream : Stream) : Int;
  function read_start(cb : BoolStatus) : Int;
  function read_stop() : Int;
  function write(data : StreamData, ?cb : BoolStatus) : Int;
  function write2(data : StreamData, send_handle : SendHandle, cb: BoolStatus) : Int;
  function try_write(data : StreamData) : Int;
  function is_readable() : Bool;
  function is_writable() : Bool;
  function set_blocking(blocking : Bool) : Int;
}

@:luaRequire("luv")
extern class Tcp extends Stream {
  static function new_tcp() : Tcp;
  @:native("new_tcp") function new() : Void;

  function open(sock : Int) : Int;
  function nodelay(enable : Bool) : Int;
  function keepalive(enable : Bool, ?delay : Int) : Int;
  function simultaneous_accepts(enable : Bool) : Int;
  function bind(address : String, port : Int) : Int;
  function getsockname() : Int;
  function getpeername() : String;
  function connect(host : String, port : Int, cb : BoolStatus) : Int;
  function write_queue_size() : Int;
}

typedef EitherHandle = haxe.extern.EitherType<FileHandle, Handle>;

@:luaRequire("luv")
extern class Pipe extends Stream {
  static function new_pipe(ipc : Bool) : Pipe;
  @:native("new_pipe") function new(ipc : Bool) : Void;

  function open(file : EitherHandle) : Pipe;
  function bind(name : String) : Pipe;
  function connect(name : String, cb: BoolStatus) : Int;
  function getsockname() : String;
  function pending_instances(count : Int) : Int;
  function pending_count() : Int;
  function pending_type() : Int;
}

@:multiReturn
extern class WidthHeight {
  var width : Int;
  var height : Int;
}

@:luaRequire("luv")
extern class Tty extends Stream {
  static function new_tty(fd : Int, readable : Bool) : Tty;
  @:native("new_tty") function new(fd : Int, readable : Bool) : Void;

  static function reset_mode() : Int;

  function set_mode(mode : Int) : Int;
  function get_winsize() : WidthHeight;
}

@:luaRequire("luv")
extern class Udp extends Handle {
  static function new_udp() : Udp;
  @:native("new_udp") function new() : Void;

  function open(fd : Int) : Int;
  function bind(host : String, port : Int) : Int;
  function getsockname() : String;
  function set_membership(multicast_addr : String, interface_addr : String, membership : String) : Int;
  function set_multicast_loop(on : Bool) : Int;
  function set_multicast_ttl(ttl : Int ) : Int;
  function set_multicast_interface(interface_addr : String) : Int;
  function set_broadcast(on : Bool) : Int;
  function set_ttl(ttl : Int) : Int;
  function send(data : String, host : String, port : Int, cb : Bool->Void) : Int;
  function try_send(data : String, host : String, port : Int) : Int;
  function recv_start(cb : BoolStatus) : Int;
  function recv_stop() : Int;
}

typedef StartOptions = {
  watch_entry : Bool,
  stat : Bool,
  recursive : Bool
}

@:luaRequire("luv")
extern class FileSystemEvent {
  static function new_fs_event() : FileSystemEvent;
  @:native("new_fs_event") function new() : Void;

  function start(path : String, options : StartOptions, cb : BoolStatus) : Int;
  function stop() : Int;
  function getpath() : String;
}

@:luaRequire("luv")
extern class FileSystemPoll {
  static function new_fs_poll() : FileSystemPoll;
  @:native("new_fs_poll") function new() : Void;

  function start(path : String, interval : Int, cb : BoolStatus) : Int;
  function stop() : Int;
  function getpath() : String;
}

@:luaRequire("luv")
extern class FileSystem {
  @:native("fs_close")
  @:overload(function(file : FileDescriptor, cb : ResultStatus<Bool>) : Req {})
  static function close(file : FileDescriptor) : Bool;

  @:native("fs_open")
  @:overload(function(path : String, flags : Open, mode : Int, ?cb : ResultStatus<FileDescriptor>) : Req {})
  static function open(path : String, flags : Open, mode : Int) : FileDescriptor;

  @:native("fs_read")
  @:overload(function(file : FileDescriptor, len : Int, offset : Int, ?cb : StringStatus) : Req {} )
  static function read(file : FileDescriptor, len : Int, offset : Int) : String;

  @:native("fs_unlink")
  @:overload(function(file : FileDescriptor, ?cb : ResultStatus<String>) : Req {} )
  static function unlink(file : FileDescriptor, content : String) : String;

  @:native("fs_write")
  @:overload(function(file : FileDescriptor, content : String, offset : Int, ?cb : BoolStatus) : Int {})
  static function write(file : FileDescriptor, content : String, offset : Int) : Bool;

  @:native("fs_mkdir")
  @:overload(function(path : String, mode : Int, cb : ResultStatus<Bool>) : Req {})
  static function mkdir(path : String, mode :Int) : Bool;

  @:native("fs_mkdtemp")
  @:overload(function(data : String, cb : UvFsCb) : Req {})
  static function mkdtemp(data : String) : Int;

  @:native("fs_rmdir")
  @:overload(function(path : String, cb : UvFsCb) : Req {})
  static function rmdir(path : String) : Int;

  @:native("fs_scandir")
  @:overload(function(path : String, cb : UvFsCb) : Req {})
  static function scandir(path : String) : ScanDirMarker;

  @:native("fs_scandir_next")
  static function scandir_next(scandir : ScanDirMarker) : ScandirNext;

  @:native("fs_stat")
  @:overload(function(path : String, cb : ResultStatus<Stat>) : Req {})
  static function stat(path : String) : Stat;

  @:native("fs_fstat")
  @:overload(function(descriptor : FileDescriptor, cb : ResultStatus<Stat>) : Req {})
  static function fstat(descriptor : FileDescriptor) : Stat;

  @:native("fs_lstat")
  @:overload(function(path : String, cb : ResultStatus<Stat>) : Req {})
  static function lstat(path : String) : Stat;

  @:native("fs_rename")
  @:overload(function(path : String, newpath : String, cb : ResultStatus<Bool>) : Req {})
  static function rename(path : String, newpath : String) : Bool;

  @:native("fs_fsync")
  @:overload(function(descriptor : FileDescriptor, cb : ResultStatus<Bool>) : Req {})
  static function fsync(descriptor : FileDescriptor) : Bool;

  @:native("fs_fdatasync")
  @:overload(function(descriptor : FileDescriptor, cb : ResultStatus<Bool>) : Req {})
  static function fdatasync(descriptor : FileDescriptor) : Bool;

  @:native("fs_ftruncate")
  @:overload(function(descriptor : FileDescriptor, offset : Int, cb : ResultStatus<Bool>) : Req {})
  static function ftruncate(descriptor : FileDescriptor, offset : Int) : Bool;

  @:native("fs_sendfile")
  @:overload(function(fin : FileDescriptor, fout : FileDescriptor, cb : ResultStatus<Int>) : Req {})
  static function sendfile(fin : FileDescriptor, fout : FileDescriptor) : Int;

  @:native("fs_access")
  @:overload(function(path : String, mode : Int, cb : ResultStatus<Bool>) : Req {})
  static function access(path : String, mode :Int) : Bool;

  @:native("fs_chmod")
  @:overload(function(path : String, mode : Int, cb : ResultStatus<Bool>) : Req {})
  static function chmod(path : String, mode :Int) : Bool;

  @:native("fs_fchmod")
  @:overload(function(descriptor : FileDescriptor, mode : Int, cb : ResultStatus<Bool>) : Req {})
  static function fchmod(descriptor : FileDescriptor, mode :Int) : Bool;

  @:native("fs_futime")
  @:overload(function(descriptor : FileDescriptor, actime : Int, modtime : Int, cb : ResultStatus<Bool>) : Req {})
  static function futime(descriptor : FileDescriptor, actime : Int, modtime : Int) : Bool;

  @:native("fs_utime")
  @:overload(function(path : String, actime : Int, modtime : Int, cb : ResultStatus<Bool>) : Req {})
  static function utime(path : String, actime : Int, modtime : Int) : Bool;

  @:native("fs_link")
  @:overload(function(oldpath : String, newpath : String, cb : ResultStatus<Bool>) : Req {})
  static function link(oldpath : String, newpath : String) : Bool;

  @:native("fs_symlink")
  @:overload(function(oldpath : String, newpath : String, flags : Int, cb : ResultStatus<Bool>) : Req {})
  static function symlink(oldpath : String, newpath : String, flags : Int) : Bool;

  // @:native("fs_readlink")
  // @:overload(function(path : String, cb : ResultStatus<String>) : Req {})
  // static function readlink(path : String) : String;

  @:native("fs_realpath")
  @:overload(function(path : String, cb : ResultStatus<String>) : Req{})
  static function realpath(path : String) : String;

  @:native("fs_chown")
  @:overload(function(path : String, uid : Int, gid : Int, cb : ResultStatus<Bool>) : Req {})
  static function chown(path : String, uid : Int, gid : Int) : Bool;

  @:native("fs_fchown")
  @:overload(function(descriptor : FileDescriptor, uid : Int, gid : Int, cb : ResultStatus<Bool>) : Req {})
  static function fchown(descriptor : FileDescriptor, uid : Int, gid : Int) : Bool;

}

extern class ScanDirMarker {}

@:luaRequire("luv")
extern class Dns {
  @:overload(function(node : String, service : String, ?hints : AddrInfo, cb : ResultStatus<Table<Int, AddrInfo>>) : Req {})
  public static function getaddrinfo(node : String, service : String, ?hints : AddrInfo ) : Table<Int,AddrInfo>;

  @:overload(function(ip: String, port : Int, family : String, cb : ResultStatus<AddrInfo>) : Req {})
  public static function getnameinfo(info:AddrInfo) : String;
}

@:luaRequire("luv")
extern class Misc {
  public static function chdir(path : String) : Bool;

  public static function os_homedir() : String;
  public static function os_tmpdir() : String;
  public static function os_get_passwd() : String;
  public static function cpu_info() : Table<Int,CpuInfo>;

  public static function cwd() : String;
  public static function exepath() : String;
  public static function get_process_title() : String;
  public static function get_total_memory() : Int;
  public static function get_free_memory() : Int;
  public static function getpid() : Int;

  // TODO Windows only?
  public static function getuid() : Int;
  public static function setuid(from : Int, to : Int) : String;
  public static function getgid() : Int;
  public static function setgid(from : Int, to : Int) : Void;

  public static function getrusage() : ResourceUsage;
  public static function guess_handle(handle : Int) : String;
  public static function hrtime() : Float;

  // TODO: implement this
  // public static function interface_addresses() : String;

  public static function loadavg() : Float;
  public static function resident_set_memory() : Int;
  public static function set_process_title(title : String) : Bool;
  public static function uptime() : Int;
  public static function version() : Int;
  public static function version_string() : String;

  // TODO : Windows only
  public static function print_all_handles() : Table<Int,String>;
  public static function print_active_handles() : Table<Int,String>;

}

@:luaRequire("luv")
extern class Thread {
  static function new_thread() : Timer;
  @:native("new_thread") function new() : Void;
  function equal() : Bool;
  function self() : Thread;
  function join() : Bool;
  function sleep() : Void;
}

@:luaRequire("luv")
extern class Work {
  static function new_work() : Work;
  @:native("new_work") function new() : Void;
  static function queue_work(work : Work) : Bool;
}

@:multiType
extern class ScandirNext {
  var name : String;
  var type : String;
}

@:enum
abstract Open(String) {
  var ReadOnly                 = "r";
  var ReadOnlySync             = "rs";
  var ReadWrite                = "r+";
  var ReadWriteSync            = "rs+";
  var ReadWriteAppend          = "a+";
  var ReadWriteTruncate        = "w+";
  var ReadWriteTruncateNewFile = "wx+";
  var ReadWriteAppendNewFile   = "ax+";
  var WriteOnly                = "w";
  var WriteNewFile             = "wx";
  var Append                   = "a";
  var AppendNewFile            = "ax";
}

typedef Stat = {
  ino       : Int,
  ctime     : TimeStamp,
  uid       : Int,
  dev       : Int,
  nlink     : Int,
  mode      : Int,
  size      : Int,
  birthtime : TimeStamp,
  gid       : Int,
  type      : String,
  rdev      : Int,
  gen       : Int,
  blocks    : Int,
  mtime     : TimeStamp,
  atime     : TimeStamp,
  blksize   : Int,
  flags     : Int
}

typedef TimeStamp = {
  sec  : Int,
  nsec : Int
}

typedef AddrInfo = {
    ?ip       : String,
	?addr     : String,
	?port     : Int,
	?family   : String,
	?socktype : String
}

typedef CpuInfo = {
  model : String,
  times : CpuTimes,
  speed : Int
}

typedef CpuTimes = {
  sys  : Int,
  idle : Int,
  irq  : Int,
  user : Int
}

typedef ResourceUsage = {
  nivcsw   : Int,
  maxrss   : Int,
  msgrcv   : Int,
  isrss    : Int,
  inblock  : Int,
  ixrss    : Int,
  nvcsw    : Int,
  nsignals : Int,
  minflt   : Int,
  nswap    : Int,
  msgsnd   : Int,
  oublock  : Int,
  majflt   : Int,
  stime    : MicroTimeStamp,
  idrss    : Int,
  utime    : MicroTimeStamp
}

typedef MicroTimeStamp = {
  usec : Int,
  sec : Int
}
