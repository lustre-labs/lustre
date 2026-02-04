import { FrameBufferRenderable, TextRenderable, RGBA, CliRenderer } from "@opentui/core";

const SPINNER_FRAMES = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"] as const;

/**
 * FrameBuffer-based spinner with cleanup in destructor.
 */
class FrameBufferSpinner extends FrameBufferRenderable {
  #intervalId: ReturnType<typeof setInterval> | null = null;
  #frameIndex = 0;
  #fg: RGBA;
  #bg: RGBA;

  constructor(renderer: CliRenderer, color: string, interval_ms: number) {
    super(renderer, { width: 1, height: 1 });

    this.#fg = RGBA.fromHex(color);
    this.#bg = RGBA.fromHex("#00000000");

    // Initial frame
    this.frameBuffer.setCell(0, 0, SPINNER_FRAMES[this.#frameIndex]!, this.#fg, this.#bg);

    // Start animation
    this.#intervalId = setInterval(() => {
      this.#frameIndex = (this.#frameIndex + 1) % SPINNER_FRAMES.length;
      this.frameBuffer.setCell(0, 0, SPINNER_FRAMES[this.#frameIndex]!, this.#fg, this.#bg);
    }, interval_ms);
  }

  stop(): void {
    if (this.#intervalId) {
      clearInterval(this.#intervalId);
      this.#intervalId = null;
    }
  }

  override destroySelf(): void {
    this.stop();
    super.destroySelf();
  }
}

/**
 * Text-based spinner with cleanup in destructor.
 */
class TextSpinner extends TextRenderable {
  #intervalId: ReturnType<typeof setInterval> | null = null;
  #frameIndex = 0;

  constructor(renderer: CliRenderer, color: string, interval_ms: number) {
    super(renderer, {});

    this.content = SPINNER_FRAMES[this.#frameIndex]!;
    this.fg = color;

    // Start animation
    this.#intervalId = setInterval(() => {
      this.#frameIndex = (this.#frameIndex + 1) % SPINNER_FRAMES.length;
      this.content = SPINNER_FRAMES[this.#frameIndex]!;
    }, interval_ms);
  }

  stop(): void {
    if (this.#intervalId) {
      clearInterval(this.#intervalId);
      this.#intervalId = null;
    }
  }

  override destroySelf(): void {
    this.stop();
    super.destroySelf();
  }
}

/**
 * Creates a FrameBuffer spinner factory.
 * Returns just the node - cleanup is handled by destroySelf when OpenTUI
 * removes the node from the tree.
 */
export function create_framebuffer_spinner(color: string, interval_ms: number): (renderer: CliRenderer) => FrameBufferSpinner {
  return (renderer: CliRenderer) => new FrameBufferSpinner(renderer, color, interval_ms);
}

/**
 * Creates a Text spinner factory.
 * Returns just the node - cleanup is handled by destroySelf when OpenTUI
 * removes the node from the tree.
 */
export function create_text_spinner(color: string, interval_ms: number): (renderer: CliRenderer) => TextSpinner {
  return (renderer: CliRenderer) => new TextSpinner(renderer, color, interval_ms);
}
