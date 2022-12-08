pub extern "Kernel32" fn GlobalAlloc(
    uFlags: UINT,
    dwBytes: SIZE_T,
) HGLOBAL;

pub extern "Kernel32" fn GlobalFree(
    hMem: HGLOBAL,
) HGLOBAL;

pub extern "Kernel32" fn GlobalLock(
    hMem: HGLOBAL,
) LPVOID;

pub extern "Kernel32" fn GlobalUnlock(
    hMem: HGLOBAL,
) BOOL;

pub extern "User32" fn OpenClipboard(
    hWndNewOwner: HWND,
) BOOL;

pub extern "User32" fn EmptyClipboard() BOOL;

pub extern "User32" fn SetClipboardData(
    uFormat: UINT,
    hMem: HANDLE,
) HANDLE;

pub extern "User32" fn CloseClipboard() BOOL;

pub const HGLOBAL = ?*anyopaque;
pub const UINT = u32;
pub const SIZE_T = usize;
pub const LPVOID = ?*anyopaque;
pub const BOOL = c_int;
pub const HANDLE = ?*anyopaque;
pub const HWND = HANDLE;

pub const GMEM_MOVEABLE = 0x0002;
pub const GMEM_ZEROINIT = 0x0040;

pub const CF_TEXT = 1;
