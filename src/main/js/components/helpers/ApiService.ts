export interface ApiService {
  fetchJsonForStep(stepPath: number[], childPath: string, options?: RequestInit): Promise<any>
  updateTheorem(theoremJson: any): void
}
